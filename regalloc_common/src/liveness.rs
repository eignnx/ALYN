use std::{collections::{BTreeMap, BTreeSet}, marker::PhantomData};

use alyn_common::names::Tmp;

use crate::{cfg::Cfg, ctrl_flow::GetCtrlFlow, stg::Stg, stmt::Stmt, DefsUses, Instruction, Register};



pub type StmtIdx = usize;

#[derive(Debug, Default)]
pub struct LiveSets<R, I> {
    live_ins: BTreeMap<StmtIdx, BTreeSet<Stg<R>>>,
    live_outs: BTreeMap<StmtIdx, BTreeSet<Stg<R>>>,
    _instr: PhantomData<I>,
}

impl<R, I> LiveSets<R, I>
where
    R: Register,
    I: Instruction<Reg = R> + GetCtrlFlow + DefsUses,
{
    pub fn new() -> Self {
        Self {
            live_ins: Default::default(),
            live_outs: Default::default(),
            _instr: PhantomData,
        }
    }

    pub fn build_from<'cfg>(
        cfg: &Cfg<'cfg, R, I>,
        entry_live_ins: impl IntoIterator<Item = Tmp>,
    ) -> Self {
        let mut this = Self::new();
        this.compute_live_ins_live_outs(cfg, entry_live_ins);
        this
    }

    const MAX_ITERS: usize = 32;

    pub fn compute_live_ins_live_outs<'cfg>(
        &mut self,
        cfg: &Cfg<'cfg, R, I>,
        entry_live_ins: impl IntoIterator<Item = Tmp>,
    ) {
        self.live_ins
            .entry(cfg.entry())
            .or_default()
            .extend(entry_live_ins.into_iter().map(Stg::Tmp));

        // All callee-save (saved) registers need to be marked live-out on exit.
        let saved_regs = R::GPR_SAVED_REGS.iter().copied().map(Stg::Reg);
        for exit_id in cfg.exits() {
            self.live_outs
                .entry(exit_id)
                .or_default()
                .extend(saved_regs.clone());
        }

        let mut recompute = false;
        for _ in 0..Self::MAX_ITERS {
            for (idx, stmt) in cfg.stmts().enumerate().rev() {
                self.compute_live_outs(idx, cfg, &mut recompute);
                self.compute_live_ins(idx, stmt, &mut recompute);
            }

            if !recompute {
                return;
            } else {
                recompute = false;
            }
        }
        panic!(
            "Max iterations reached in live set analysis: {}",
            Self::MAX_ITERS
        );
    }

    /// `LiveOuts[I] = union(LiveIn[p] for p in Succ[I])`
    fn compute_live_outs(&mut self, id: StmtIdx, cfg: &Cfg<R, I>, recompute: &mut bool) {
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

    /// `LiveIns[I] = Uses[I] ∪ (LiveOuts[I] - Defs[I])`
    fn compute_live_ins(
        &mut self,
        id: StmtIdx,
        stmt: &Stmt<I>,
        recompute: &mut bool,
    ) {
        let live_ins = self.live_ins.entry(id).or_default();
        let old_len = live_ins.len();

        let mut defs = BTreeSet::new();

        // TODO: clone uneccessary?
        for def_use in stmt.clone().defs_uses() {
            match def_use {
                crate::DefUse::Def(stg) => {
                    defs.insert(*stg);
                }
                crate::DefUse::Use(stg) => {
                    live_ins.insert(*stg);
                }
            }
        }

        for &live_out in self.live_outs.entry(id).or_default().iter() {
            if !defs.contains(&live_out) {
                live_ins.insert(live_out);
            }
        }

        let new_len = live_ins.len();
        if new_len != old_len {
            *recompute = true;
        }
    }

    pub fn live_ins(&self, stmt_idx: StmtIdx) -> &BTreeSet<Stg<R>> {
        &self.live_ins[&stmt_idx]
    }

    pub fn live_outs(&self, stmt_idx: StmtIdx) -> &BTreeSet<Stg<R>> {
        &self.live_outs[&stmt_idx]
    }
}
