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

    /// `LiveOuts[I] = union(LiveIn[p] for p in Pred[I])`
    fn compute_live_outs(&mut self, id: NodeId, cfg: &Cfg, recompute: &mut bool) {
        let live_outs = self.live_outs.entry(id).or_default();
        let old_len = live_outs.len();
        for pred in cfg.predecessors(id) {
            let pred_live_ins = self.live_ins.entry(pred).or_default();
            live_outs.extend(pred_live_ins.iter().copied());
        }
        let new_len = live_outs.len();
        if new_len != old_len {
            *recompute = true;
        }
    }

    /// `LiveIns[I] = Uses[I]  U  (LiveOuts[I] - Defs[I])`
    fn compute_live_ins(&mut self, id: NodeId, stmt: &Stmt, defs: &mut BTreeSet<Tmp>, uses: &mut BTreeSet<Tmp>, recompute: &mut bool) {
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
            self.live_ins.get(&id)
                .map(|set| set.iter())
                .into_iter()
                .flatten()
                .copied()
    }

    pub fn get_live_outs(&self, id: NodeId) -> impl Iterator<Item = Tmp> {
            self.live_outs.get(&id)
                .map(|set| set.iter())
                .into_iter()
                .flatten()
                .copied()
    }
}

