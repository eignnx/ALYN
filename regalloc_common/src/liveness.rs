use std::{collections::{BTreeMap, BTreeSet}, marker::PhantomData, sync::LazyLock};

use crate::{cfg::{BbIdx, Cfg}, ctrl_flow::GetCtrlFlow, stg::Stg, DefUse, DefsUses, Instruction, Register};

/// Computes live-ins and live-outs of each basic-block in a control flow graph.
///
/// This involves computing liveness information at each statement, but that information is thrown
/// away because it can "easily" be recomputed as long as the live-ins/live-outs are known at the
/// basic-block boundaries.
///
/// Ultimately this datastructure records **global** (and not local) liveness information about a
/// control flow graph.
#[derive(Debug, Default)]
pub struct LiveSets<R, I> {
    live_ins: BTreeMap<BbIdx, BTreeSet<Stg<R>>>,
    live_outs: BTreeMap<BbIdx, BTreeSet<Stg<R>>>,
    _instr: PhantomData<I>,
}

pub fn compute_local_live_set<R, I>(
    cfg: &Cfg<R, I>,
    bb_idx: BbIdx,
    live_outs: &BTreeSet<Stg<R>>,
) -> BTreeSet<Stg<R>>
where
    R: Clone + Ord,
    I: Instruction<Reg = R> + GetCtrlFlow + DefsUses,
{
    let mut live_set = live_outs.clone();
    let mut defs_uses = Vec::new();

    for instr in cfg.bb_instrs(bb_idx).rev() {
        instr.defs_uses(&mut defs_uses);
        for def_use in defs_uses.drain(..) {
            match def_use {
                DefUse::Def(stg) => {
                    live_set.remove(&stg);
                }
                DefUse::Use(stg) => {
                    live_set.insert(stg);
                }
            }
        }
    }

    live_set
}

static DEFAULT_MAX_ITERS: usize = 16;
static MAX_ITERS: LazyLock<usize> = LazyLock::new(|| {
    if let Ok(n) = std::env::var("ALYN_MAX_LIVENESS_ITERS") {
        n.parse()
            .expect("value of env var `ALYN_MAX_LIVENESS_ITERS` must be parseable as a usize")
    } else {
        DEFAULT_MAX_ITERS
    }
});

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
        live_ins_on_entry: impl Iterator<Item = Stg<R>>,
        live_outs_on_exit: impl Iterator<Item = Stg<R>> + Clone,
    ) -> Self {
        let mut this = Self::new();
        this.compute_live_ins_live_outs(cfg, live_ins_on_entry, live_outs_on_exit);
        this
    }

    pub fn compute_live_ins_live_outs<'cfg>(
        &mut self,
        cfg: &Cfg<'cfg, R, I>,
        live_ins_on_entry: impl Iterator<Item = Stg<R>>,
        live_outs_on_exit: impl Iterator<Item = Stg<R>> + Clone,
    ) {
        self.live_ins
            .entry(cfg.entry())
            .or_default()
            .extend(live_ins_on_entry);

        for exit_id in cfg.exits() {
            self.live_outs
                .entry(exit_id)
                .or_default()
                .extend(live_outs_on_exit.clone());
        }

        let mut recompute = false;
        for _ in 0..*MAX_ITERS {
            for bb_idx in cfg.bbs() {
                self.compute_live_outs(bb_idx, cfg, &mut recompute);
                self.compute_live_ins(bb_idx, cfg, &mut recompute);
            }

            if !recompute {
                return;
            } else {
                recompute = false;
            }
        }
        panic!(
            "Max iterations reached in live set analysis: {}",
            *MAX_ITERS
        );
    }

    /// `LiveOuts[I] = union(LiveIn[p] for p in Succ[I])`
    fn compute_live_outs(&mut self, bb_idx: BbIdx, cfg: &Cfg<R, I>, recompute: &mut bool) {
        let live_outs = self.live_outs.entry(bb_idx).or_default();
        let old_len = live_outs.len();
        for pred in cfg.successor_bbs(bb_idx) {
            let pred_live_ins = self.live_ins.entry(pred).or_default();
            live_outs.extend(pred_live_ins.iter().copied());
        }
        let new_len = live_outs.len();
        if new_len != old_len {
            *recompute = true;
        }
    }

    /// `LiveIns[I] = Uses[I] ∪ (LiveOuts[I] - Defs[I])`
    ///
    /// or:
    ///
    /// `LiveIns[I] = LiveOuts[I] - Defs[I] + Uses[I]`
    fn compute_live_ins(
        &mut self,
        bb_idx: BbIdx,
        cfg: &Cfg<R, I>,
        recompute: &mut bool,
    ) {
        let live_outs = self.live_outs.entry(bb_idx).or_default();
        let new_live_ins = compute_local_live_set(cfg, bb_idx, live_outs);
        let live_ins = self.live_ins.entry(bb_idx).or_default();

        // Assumption: These sets only ever grow bigger or stay the same size. Elements are never
        // removed.
        if new_live_ins.len() != live_ins.len() {
            live_ins.extend(new_live_ins);
            *recompute = true;
        }
    }

    pub fn live_ins(&self, bb_idx: BbIdx) -> &BTreeSet<Stg<R>> {
        &self.live_ins[&bb_idx]
    }

    pub fn live_outs(&self, bb_idx: BbIdx) -> &BTreeSet<Stg<R>> {
        &self.live_outs[&bb_idx]
    }
}
