use std::{collections::{BTreeMap, BTreeSet}, fmt::{Debug, Display}};

use crate::{
    DefsUses, Instruction,
    cfg::Cfg,
    common::Stg,
    liveness::LiveSets,
};

/// Interferences
pub struct Intfs<R> {
    graph: BTreeMap<Stg<R>, BTreeSet<Stg<R>>>,
}

impl<R: Copy + Eq + Ord> Intfs<R> {
    pub fn new() -> Self {
        Self {
            graph: Default::default(),
        }
    }

    pub fn build_from<I: Instruction<Reg = R> + DefsUses>(
        cfg: &Cfg<R, I>,
        live_sets: &LiveSets<R, I>,
    ) -> Self {
        let mut this = Self::new();
        this.compute_interferences(cfg, live_sets);
        this
    }

    fn compute_interferences<I: Instruction<Reg = R> + DefsUses>(
        &mut self,
        cfg: &Cfg<R, I>,
        live_sets: &LiveSets<R, I>,
    ) {
        let mut defs = BTreeSet::new();
        let mut uses = BTreeSet::new();

        for (id, stmt) in cfg.stmts().enumerate() {
            if let Some((lhs, rhs)) = stmt.try_as_pure_move() {
                // > At a move instruction `lhs <- rhs`, where variables `v0, ..., vn` are
                // *live-out*, add interference edges `(lhs, vi)' for all `vi` not equal to `rhs`.
                // (I.e. make sure not to add an edge between `lhs` and `rhs`.)
                for live_out in live_sets.get_live_outs(id) {
                    if live_out != rhs {
                        self.record_interference(lhs, live_out);
                    }
                }
            } else {
                // > At any nonmove instruction which *defines* a variable `def`, where the
                // *live-outs* are `v0, ..., vn`, add interference edges `(def, vi)` for all `vi`.
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

    fn record_interference(&mut self, a: Stg<R>, b: Stg<R>) {
        self.graph.entry(a).or_default().insert(b);
        self.graph.entry(b).or_default().insert(a);
    }

    pub fn interferes_with(&self, a: Stg<R>, b: Stg<R>) -> bool {
        self.graph[&a].contains(&b)
    }

    pub fn neighbors(&self, node: Stg<R>) -> impl Iterator<Item = Stg<R>> {
        self.graph[&node].iter().copied()
    }
}

/// Display in DOT graph (GraphViz) format.
impl<R: Debug> Display for Intfs<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "strict graph {{")?;
        for (tmp, neighbors) in &self.graph {
            write!(f, "\"{tmp:?}\" -- {{")?;
            for n in neighbors {
                write!(f, " \"{n:?}\"")?;
            }
            write!(f, "}}")?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

