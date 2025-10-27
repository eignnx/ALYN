use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
    marker::PhantomData,
};

use crate::{
    DefsUses, Instruction, Register,
    cfg::{Cfg, ControlFlow, StmtIdx},
    common::{Stg, Stmt},
};

#[derive(Debug, Default)]

pub struct LiveSets<R, I> {
    live_ins: BTreeMap<StmtIdx, BTreeSet<Stg<R>>>,
    live_outs: BTreeMap<StmtIdx, BTreeSet<Stg<R>>>,
    _instr: PhantomData<I>,
}

impl<R, I> LiveSets<R, I>
where
    R: Register,
    I: Instruction<Reg = R>,
    I: ControlFlow,
    I: DefsUses,
{
    pub fn new() -> Self {
        Self {
            live_ins: Default::default(),
            live_outs: Default::default(),
            _instr: PhantomData,
        }
    }

    const MAX_ITERS: usize = 32;

    pub fn compute_live_ins_live_outs(&mut self, cfg: &Cfg<R, I>) {
        //// All function parameters need to be marked as live-in in the entry.
        let entry_live_ins = self.live_ins.entry(cfg.entry).or_default();
        entry_live_ins.extend(cfg.params().map(Stg::Tmp));

        // All callee-save (saved) registers need to be marked live-out on exit.
        let saved_regs = R::GPR_SAVED_REGS.iter().copied().map(Stg::Reg);
        for exit_id in cfg.exits() {
            self.live_outs
                .entry(exit_id)
                .or_default()
                .extend(saved_regs.clone());
        }

        let mut defs_buf = BTreeSet::new();
        let mut uses_buf = BTreeSet::new();
        let mut recompute = false;
        for _ in 0..Self::MAX_ITERS {
            for (idx, stmt) in cfg.stmts().enumerate().rev() {
                self.compute_live_outs(idx, cfg, &mut recompute);
                self.compute_live_ins(idx, stmt, &mut defs_buf, &mut uses_buf, &mut recompute);
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
}

impl<R: Register, I> LiveSets<R, I> {
    pub fn get_live_ins(&self, id: StmtIdx) -> impl Iterator<Item = Stg<R>> {
        self.live_ins
            .get(&id)
            .map(|set| set.iter())
            .into_iter()
            .flatten()
            .copied()
    }

    pub fn get_live_outs(&self, id: StmtIdx) -> impl Iterator<Item = Stg<R>> {
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
}

impl<R: Register, I: Instruction> LiveSets<R, I> {
    pub fn display<'a>(&'a self, stmts: &'a [I]) -> impl Display + 'a {
        DisplayLiveSets {
            live_sets: self,
            stmts,
        }
    }
}

struct DisplayLiveSets<'a, R, I> {
    live_sets: &'a LiveSets<R, I>,
    stmts: &'a [I],
}

impl<'a, R, I> Display for DisplayLiveSets<'a, R, I>
where
    R: Register,
    I: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, stmt) in self.stmts.iter().enumerate() {
            if i == 0 {
                // Write live-ins
                write!(f, "        {{")?;
                for live_in in self.live_sets.get_live_ins(i) {
                    write!(f, " {live_in:?}")?;
                }
                writeln!(f, " }}")?;
            }
            // Write stmt
            write!(f, "{i:02}: {:40}", format!("{stmt:?}"))?;
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
    //use insta::assert_snapshot;

    //use super::super::test_datastructures::{Expr, Stmt};
    //use super::*;

    //#[test]
    //fn simple_with_live_ins_on_entry() {
    //    #[rustfmt::skip]
    //    let cfg = Cfg::new(
    //        0,
    //        ["arg".into()],
    //        [
    //            Stmt::mov(Tmp::from("x"), 123),
    //            Stmt::mov(Tmp::from("y"), Expr::binop("x", "arg"))
    //        ],
    //    );
    //    let mut live_sets = LiveSets::new();
    //    live_sets.compute_live_ins_live_outs(&cfg);

    //    assert_snapshot!(live_sets.display(&cfg.stmts));
    //}
}
