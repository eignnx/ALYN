use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
};

use super::{
    Instr,
    cfg::{Cfg, NodeId},
};
use crate::{instr_sel::Stg, names::Tmp};

#[derive(Debug, Default)]
pub struct LiveSets<R> {
    live_ins: BTreeMap<NodeId, BTreeSet<Stg<R>>>,
    live_outs: BTreeMap<NodeId, BTreeSet<Stg<R>>>,
}

impl<R: Copy + Eq + Ord + Debug> LiveSets<R> {
    pub fn new() -> Self {
        Self {
            live_ins: Default::default(),
            live_outs: Default::default(),
        }
    }

    pub fn compute_live_ins_live_outs<I: Instr<Register = R>>(&mut self, cfg: &Cfg<I>) {
        // All function parameters need to be marked as live-in in the entry.
        let entry_live_ins = self.live_ins.entry(cfg.entry).or_default();
        entry_live_ins.extend(cfg.params.iter().copied().map(Stg::Tmp));

        let mut defs_buf = BTreeSet::new();
        let mut uses_buf = BTreeSet::new();
        let mut recompute = false;
        loop {
            for (id, stmt) in cfg.stmts.iter().enumerate().rev() {
                self.compute_live_outs(id, cfg, &mut recompute);
                self.compute_live_ins(id, stmt, &mut defs_buf, &mut uses_buf, &mut recompute);
            }

            if !recompute {
                break;
            } else {
                recompute = false;
            }
        }
    }

    /// `LiveOuts[I] = union(LiveIn[p] for p in Succ[I])`
    fn compute_live_outs<I: Instr<Register = R>>(
        &mut self,
        id: NodeId,
        cfg: &Cfg<I>,
        recompute: &mut bool,
    ) {
        let live_outs = self.live_outs.entry(id).or_default();
        let old_len = live_outs.len();
        for pred in cfg.successors(id) {
            let pred_live_ins = self.live_ins.entry(pred).or_default();
            live_outs.extend(pred_live_ins.iter().copied());
        }
        let new_len = live_outs.len();
        if new_len != old_len {
            *recompute = true;
        }
    }

    /// `LiveIns[I] = Uses[I]  U  (LiveOuts[I] - Defs[I])`
    fn compute_live_ins<I: Instr<Register = R>>(
        &mut self,
        id: NodeId,
        stmt: &I,
        defs: &mut BTreeSet<Stg<R>>,
        uses: &mut BTreeSet<Stg<R>>,
        recompute: &mut bool,
    ) {
        defs.clear();
        uses.clear();
        stmt.add_defs_uses(defs, uses);

        let live_ins = self.live_ins.entry(id).or_default();
        let old_len = live_ins.len();
        live_ins.extend(uses.iter().copied());
        for live_out in self.live_outs.entry(id).or_default().iter().copied() {
            if !defs.contains(&live_out) {
                live_ins.insert(live_out);
            }
        }
        let new_len = live_ins.len();
        if new_len != old_len {
            *recompute = true;
        }
    }

    pub fn get_live_ins(&self, id: NodeId) -> impl Iterator<Item = Stg<R>> {
        self.live_ins
            .get(&id)
            .map(|set| set.iter())
            .into_iter()
            .flatten()
            .copied()
    }

    pub fn get_live_outs(&self, id: NodeId) -> impl Iterator<Item = Stg<R>> {
        self.live_outs
            .get(&id)
            .map(|set| set.iter())
            .into_iter()
            .flatten()
            .copied()
    }

    pub fn all_tmps(&self) -> impl Iterator<Item = Stg<R>> {
        let mut tmps = BTreeSet::new();
        for live_set in self.live_ins.values() {
            tmps.extend(live_set.iter().copied());
        }
        for live_set in self.live_outs.values() {
            tmps.extend(live_set.iter().copied());
        }
        tmps.into_iter()
    }

    pub fn display<'a, I: Debug>(&'a self, stmts: &'a [I]) -> impl Display + 'a {
        DisplayLiveSets {
            live_sets: self,
            stmts,
        }
    }
}

struct DisplayLiveSets<'a, I, R> {
    live_sets: &'a LiveSets<R>,
    stmts: &'a [I],
}

impl<'a, I: Debug, R: Copy + Eq + Ord + Debug> Display for DisplayLiveSets<'a, I, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, stmt) in self.stmts.iter().enumerate() {
            // Write stmt
            write!(f, "|\t{:<40} ", format!("{stmt:?}"))?;
            // Write live-ins
            write!(f, "{{")?;
            for live_in in self.live_sets.get_live_ins(i) {
                write!(f, " {live_in:?}")?;
            }
            write!(f, " }} -> ")?;
            // Write live-outs
            write!(f, "{{")?;
            for live_out in self.live_sets.get_live_outs(i) {
                write!(f, " {live_out:?}")?;
            }
            writeln!(f, " }}")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_snapshot;

    use super::super::test_datastructures::{Expr, Stmt};
    use super::*;

    #[test]
    fn simple_with_live_ins_on_entry() {
        #[rustfmt::skip]
        let cfg = Cfg::new(
            0,
            ["arg".into()],
            [
                Stmt::mov(Tmp::from("x"), 123),
                Stmt::mov(Tmp::from("y"), Expr::binop("x", "arg"))
            ],
        );
        let mut live_sets = LiveSets::new();
        live_sets.compute_live_ins_live_outs(&cfg);

        assert_snapshot!(live_sets.display(&cfg.stmts));
    }
}
