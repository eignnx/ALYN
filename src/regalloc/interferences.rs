use std::collections::{BTreeSet, HashMap};

use crate::{names::Tmp, regalloc::Expr};
use super::{
    Stmt,
    cfg::{Cfg, NodeId},
    live_sets::LiveSets,
};


#[derive(Debug, Default)]
pub struct Interferences {
    graph: HashMap<Tmp, BTreeSet<Tmp>>,
}

impl Interferences {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn all_tmps(&self) -> impl Iterator<Item = Tmp> {
        self.graph.keys().copied()
    }

    pub fn take_graph(self) -> HashMap<Tmp, BTreeSet<Tmp>> {
        self.graph
    }

    pub fn compute_interferences(&mut self, cfg: &Cfg, live_sets: &LiveSets) {
        let mut defs = BTreeSet::new();
        let mut uses = BTreeSet::new();

        for (id, stmt) in cfg.stmts.iter().enumerate() {
            if let Stmt::Mov(lhs, rhs) = stmt {
                // > At a move instruction `a <- c`, where variables `b1, ..., bj` are *live-out*,
                // > add interference edges `(a, b1), ..., (a, bj)` for any `bi` that is *not* the
                // > same as `c`.
                for live_out in live_sets.get_live_outs(id) {
                    if &Expr::Tmp(live_out) != rhs {
                        self.record_interference(*lhs, live_out);
                    }
                }
            } else {
                // > At any nonmove instruction that *defines* a variable `a`, where the *live-out*
                // > variables are `b1, ..., bj`, add interference edges `(a, b1), ..., (a, bj)`.
                defs.clear();
                uses.clear();
                stmt.defs_uses(&mut defs, &mut uses);
                for def in defs.iter().copied() {
                    for live_out in live_sets.get_live_outs(id) {
                        self.record_interference(def, live_out);
                    }
                }
            }
        }
    }

    pub fn interferes_with(&self, a: Tmp, b: Tmp) -> bool {
        let Some(neighbors) = self.graph.get(&a) else {
            panic!("Tmp not in interference graph: {a:?}");
        };
        neighbors.contains(&b)
    }

    pub fn record_interference(&mut self, a: Tmp, b: Tmp) {
        self.graph.entry(a).or_default().insert(b);
        self.graph.entry(b).or_default().insert(a);
    }
}

