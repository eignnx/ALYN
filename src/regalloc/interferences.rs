use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::{Debug, Display},
};

use super::{
    Instr,
    cfg::{Cfg, NodeId},
    live_sets::LiveSets,
};
use crate::{
    instr_sel::Stg,
    regalloc::{Cc, live_sets::Move},
};
use alyn_common::names::Tmp;

#[derive(Default)]
pub struct Interferences<R> {
    pub(super) graph: BTreeMap<Stg<R>, BTreeSet<Stg<R>>>,
    move_rels: BTreeSet<Move<R>>,
}

impl<R: Cc> Interferences<R> {
    pub fn new() -> Self {
        Self {
            graph: Default::default(),
            move_rels: Default::default(),
        }
    }

    pub fn all_nodes(&self) -> impl Iterator<Item = Stg<R>> {
        self.graph.keys().copied()
    }

    pub fn take_graph(self) -> BTreeMap<Stg<R>, BTreeSet<Stg<R>>> {
        self.graph
    }

    /// We don't want to forget about orphan nodes. For example:
    /// ```text
    /// label my_func:
    ///    move %x <- 123;
    ///    move %y <- %x + 456;
    ///    store MEM[%y] <- 999;
    ///
    ///    stack_load %orphan <- STACK[2];
    ///    move %orphan <- %orphan + 1;
    ///    stack_store STACK[2] <- %orphan;
    ///    ret;
    /// ```
    /// In the above, `%orphan` will have an empty set of neighbors in the interference graph.
    fn ensure_all_tmps_registered(&mut self, live_sets: &LiveSets<R>) {
        for tmp in live_sets.all_tmps() {
            self.graph.insert(tmp, Default::default());
        }
    }

    pub fn compute_interferences<I: Instr<Register = R>>(
        &mut self,
        cfg: &Cfg<I>,
        live_sets: &LiveSets<R>,
    ) {
        self.ensure_all_tmps_registered(live_sets);
        self.move_rels
            .extend(live_sets.move_instrs().into_iter().cloned());

        let mut defs = BTreeSet::new();
        let mut uses = BTreeSet::new();

        for (id, stmt) in cfg.stmts.iter().enumerate() {
            if let Some((lhs, rhs)) = stmt.try_as_pure_move() {
                // > At a move instruction `a <- c`, where variables `b1, ..., bj` are *live-out*,
                // > add interference edges `(a, b1), ..., (a, bj)` for any `bi` that is *not* the
                // > same as `c`.
                for live_out in live_sets.get_live_outs(id) {
                    if live_out != rhs {
                        self.record_interference(lhs, live_out);
                    }
                }
            } else {
                // > At any nonmove instruction that *defines* a variable `a`, where the *live-out*
                // > variables are `b1, ..., bj`, add interference edges `(a, b1), ..., (a, bj)`.
                defs.clear();
                uses.clear();
                stmt.add_defs_uses(&mut defs, &mut uses);
                for def in defs.iter().copied() {
                    for live_out in live_sets.get_live_outs(id) {
                        self.record_interference(def, live_out);
                    }
                }
            }
        }
    }

    pub fn interferes_with(&self, a: Stg<R>, b: Stg<R>) -> bool {
        if matches!((a, b), (Stg::Reg(_), Stg::Reg(_))) {
            return true;
        }
        let Some(neighbors) = self.graph.get(&a) else {
            panic!("Tmp not in interference graph: {a:?}");
        };
        neighbors.contains(&b)
    }

    pub fn record_interference(&mut self, a: Stg<R>, b: Stg<R>) {
        if a == b {
            return;
        }
        self.graph.entry(a).or_default().insert(b);
        self.graph.entry(b).or_default().insert(a);
    }

    pub fn neighbors(&self, node: &Stg<R>) -> impl Iterator<Item = Stg<R>> {
        self.graph
            .get(node)
            .map(BTreeSet::iter)
            .into_iter()
            .flatten()
            .copied()
    }
}

impl<R: Debug> Debug for Interferences<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for (tmp, neighbors) in &self.graph {
            writeln!(f, "    {tmp:?} interferes with:")?;
            for n in neighbors {
                writeln!(f, " {n:?}")?;
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<R: Debug> Display for Interferences<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "strict graph {{")?;
        for (tmp, neighbors) in &self.graph {
            write!(f, "\"{tmp:?}\" -- {{")?;
            for n in neighbors {
                write!(f, " \"{n:?}\"")?;
            }
            write!(f, "}}")?;
        }
        for mv in &self.move_rels {
            writeln!(f, "\"{:?}\" -- \"{:?} [style=dashed]\"", mv.src, mv.dst)?;
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}
