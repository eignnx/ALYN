use std::collections::{BTreeMap, BTreeSet};

use super::{
    Stmt,
    cfg::{Cfg, NodeId},
};
use crate::names::Tmp;

#[derive(Debug, Default)]
pub struct LiveSets {
    live_ins: BTreeMap<NodeId, BTreeSet<Tmp>>,
    live_outs: BTreeMap<NodeId, BTreeSet<Tmp>>,
}

impl LiveSets {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn compute_live_ins_live_outs(&mut self, cfg: &Cfg) {
        // All function parameters need to be marked as live-in in the entry.
        let entry_live_ins = self.live_ins.entry(cfg.entry).or_default();
        entry_live_ins.extend(cfg.params.iter().cloned());

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
    fn compute_live_outs(&mut self, id: NodeId, cfg: &Cfg, recompute: &mut bool) {
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
    fn compute_live_ins(
        &mut self,
        id: NodeId,
        stmt: &Stmt,
        defs: &mut BTreeSet<Tmp>,
        uses: &mut BTreeSet<Tmp>,
        recompute: &mut bool,
    ) {
        defs.clear();
        uses.clear();
        stmt.defs_uses(defs, uses);

        let live_ins = self.live_ins.entry(id).or_default();
        let old_len = live_ins.len();
        live_ins.extend(uses.iter().cloned());
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

    pub fn get_live_ins(&self, id: NodeId) -> impl Iterator<Item = Tmp> {
        self.live_ins
            .get(&id)
            .map(|set| set.iter())
            .into_iter()
            .flatten()
            .copied()
    }

    pub fn get_live_outs(&self, id: NodeId) -> impl Iterator<Item = Tmp> {
        self.live_outs
            .get(&id)
            .map(|set| set.iter())
            .into_iter()
            .flatten()
            .copied()
    }

    pub fn all_tmps(&self) -> impl Iterator<Item = Tmp> {
        let mut tmps = BTreeSet::new();
        for live_set in self.live_ins.values() {
            tmps.extend(live_set.iter().cloned());
        }
        for live_set in self.live_outs.values() {
            tmps.extend(live_set.iter().cloned());
        }
        tmps.into_iter()
    }

    pub fn display<'a>(&'a self, stmts: &'a [Stmt]) -> impl std::fmt::Display + 'a {
        DisplayLiveSets {
            live_sets: self,
            stmts,
        }
    }
}

struct DisplayLiveSets<'a> {
    live_sets: &'a LiveSets,
    stmts: &'a [Stmt],
}

impl<'a> std::fmt::Display for DisplayLiveSets<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, stmt) in self.stmts.iter().enumerate() {
            // Write stmt
            write!(f, "{:<30} ", format!("{stmt:?}"))?;
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
    use super::super::Expr;
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn simple_with_live_ins_on_entry() {
        #[rustfmt::skip]
        let cfg = Cfg::new(
            0,
            ["arg".into()],
            [
                Stmt::mov("x", 123),
                Stmt::mov("y", Expr::binop("x", "arg"))
            ],
        );
        let mut live_sets = LiveSets::new();
        live_sets.compute_live_ins_live_outs(&cfg);

        assert_snapshot!(live_sets.display(&cfg.stmts));
    }
}
