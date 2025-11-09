#![feature(formatting_options)]

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use alyn_common::names::Tmp;
use regalloc_common::{
    asn::{Asn, SlotId}, cfg::{Cfg, StmtIdx}, ctrl_flow::{CtrlFlow, GetCtrlFlow}, liveness::LiveSets, slot_alloc::{InstrWrite, SlotAllocator}, stg::Stg, stmt::Stmt, DefsUses, Instruction, Register
};

pub mod diagram;
mod pad;

#[derive(Debug)]
pub enum Access<'a, R> {
    Read(&'a mut Stg<R>, InstrExePhase),
    Write(&'a mut Stg<R>, InstrExePhase),
}

impl<'a, R: Copy> Access<'a, R> {
    pub fn phase(&self) -> InstrExePhase {
        match self {
            Access::Read(_, phase) => *phase,
            Access::Write(_, phase) => *phase,
        }
    }

    pub fn stg_mut(&'a mut self) -> &'a mut Stg<R> {
        match self {
            Access::Read(stg, _) => *stg,
            Access::Write(stg, _) => *stg,
        }
    }
}

pub trait Accesses: Instruction {
    fn accesses<'a>(&'a mut self) -> Vec<Access<'a, Self::Reg>>;
}

impl<I: Accesses> Accesses for Stmt<I> {
    fn accesses<'a>(&'a mut self) -> Vec<Access<'a, Self::Reg>> {
        match self {
            Stmt::Instr(instr) => instr.accesses(),
            Stmt::Label(_) => vec![],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InstrExePhase {
    ReadArgs,
    WriteBack,
}

impl InstrExePhase {
    pub const PHASES: [Self; 2] = [Self::ReadArgs, Self::WriteBack];
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrgPt {
    stmt_idx: StmtIdx,
    phase: InstrExePhase,
}

impl PrgPt {
    pub fn new(stmt_idx: StmtIdx, phase: InstrExePhase) -> Self {
        Self { stmt_idx, phase }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiveRange {
    begin: PrgPt,
    end: PrgPt,
}

impl LiveRange {
    pub fn contains(&self, pt: PrgPt) -> bool {
        self.begin <= pt && pt <= self.end
    }
}

pub fn compute_live_ranges<R: Register, I: Instruction<Reg = R> + Accesses>(
    stmts: &[Stmt<I>],
) -> HashMap<Stg<R>, Vec<LiveRange>> {
    let mut live_ranges = HashMap::<Stg<R>, Vec<LiveRange>>::new();
    let mut last_use = HashMap::<Stg<R>, PrgPt>::new();

    for (i, stmt) in stmts.iter().enumerate().rev() {
        let i = StmtIdx::from(i);
        let Stmt::Instr(mut instr) = stmt.clone() else {
            continue;
        };
        for access in instr.accesses() {
            match access {
                Access::Read(stg, phase) => {
                    if !last_use.contains_key(&stg) {
                        last_use.insert(*stg, PrgPt::new(i, phase));
                    }
                }
                Access::Write(stg, phase) => {
                    let Some(end) = last_use.remove(&stg) else {
                        continue; // Never read from, so just ignore.
                    };
                    live_ranges.entry(*stg).or_default().push(LiveRange {
                        begin: PrgPt::new(i, phase),
                        end,
                    });
                }
            }
        }
    }

    live_ranges
}

pub fn display_bb_live_ins_outs<
    R: Register,
    I: Instruction<Reg = R> + Accesses + GetCtrlFlow + DefsUses,
>(
    cfg: &Cfg<R, I>,
    live_sets: &LiveSets<R, I>,
) {
    for bb_idx in cfg.bbs() {
        println!("live-ins: {:?}", live_sets.live_ins(bb_idx));
        println!("{:-^40}", format!("{bb_idx}"));
        for (idx, stmt) in cfg.bb_stmts_indexed(bb_idx) {
            println!("{idx}: {stmt:?}");
        }
        println!("{:-^40}", "");
        println!("live-outs: {:?}", live_sets.live_outs(bb_idx));
        println!();
    }
}

pub fn compute_live_ranges_2<
    R: Register,
    I: Instruction<Reg = R> + Accesses + GetCtrlFlow + DefsUses,
>(
    cfg: &Cfg<R, I>,
    live_sets: &LiveSets<R, I>,
) -> HashMap<Stg<R>, Vec<LiveRange>> {
    let mut live_ranges = HashMap::<Stg<R>, Vec<LiveRange>>::new();

    for bb_idx in cfg.bbs() {
        let mut live_set = live_sets.live_outs(bb_idx).clone();
        let mut live_ends = HashMap::<Stg<R>, PrgPt>::new();

        for live in live_set.iter().copied() {
            let last_idx_in_bb = cfg[bb_idx].instrs_range().end;
            // TODO: WriteBack is correct here?
            live_ends.insert(live, PrgPt::new(last_idx_in_bb, InstrExePhase::WriteBack));
        }

        for (stmt_idx, instr) in cfg.bb_instrs_indexed(bb_idx).rev() {
            for access in instr.clone().accesses() {
                match access {
                    Access::Read(stg, phase) => {
                        live_set.insert(*stg);
                        live_ends.entry(*stg).or_insert(PrgPt::new(stmt_idx, phase));
                    }
                    Access::Write(stg, phase) => {
                        live_set.remove(stg);
                        let end = live_ends.remove(stg).unwrap();
                        let begin = PrgPt::new(stmt_idx, phase);
                        live_ranges.entry(*stg).or_default().push(LiveRange { begin, end })
                    }
                }
            }
        }
    }

    live_ranges
}

pub fn reg_choice<R: Register>(tmp: Tmp, working_set: &mut HashMap<Tmp, Asn<R>>) -> Asn<R> {
    let mut slot_id = 0usize;
    let choices = R::GPRS
        .into_iter()
        .copied()
        .map(Asn::Reg)
        .chain(std::iter::from_fn(|| {
            let chosen_slot = slot_id;
            slot_id += 1;
            Some(Asn::Slot(SlotId(chosen_slot)))
        }));

    if let Some(asn) = working_set.get(&tmp) {
        *asn
    } else {
        let in_use = working_set.values().copied().collect::<HashSet<_>>();
        for choice in choices {
            if !in_use.contains(&choice) {
                working_set.insert(tmp, choice);
                return choice;
            }
        }
        unreachable!()
    }
}

pub fn linear_scan<R, I>(
    stmts: Vec<Stmt<I>>,
    live_ranges: &HashMap<Tmp, Vec<LiveRange>>,
    slot_alloc: impl SlotAllocator,
) -> Vec<Stmt<I>>
where
    R: Register,
    I: Instruction<Reg = R> + GetCtrlFlow + Accesses + InstrWrite,
{
    let mut working_set = HashMap::<Tmp, Asn<R>>::new();
    let mut new_program = Vec::new();

    for (i, mut stmt) in stmts.into_iter().enumerate() {
        if let CtrlFlow::Advance = stmt.ctrl_flow() {
            let mut spills_before = Vec::new();
            let mut spills_after = Vec::new();

            let mut accesses = stmt.accesses();
            accesses.sort_by_key(Access::phase); // Necessary?
            for stg in accesses.iter_mut().map(Access::stg_mut) {
                match stg {
                    Stg::Tmp(tmp) => {
                        let choice = reg_choice(*tmp, &mut working_set);
                        if let Asn::Reg(reg) = choice {
                            *stg = Stg::Reg(reg);
                        } else {
                            //spills_before.extend(slot_alloc.emit_stack_load(dst, src_slot_id));
                            todo!("spill")
                        }
                    }
                    Stg::Reg(_) => todo!(),
                }
            }
            new_program.extend(spills_before);
            new_program.push(stmt);
            new_program.extend(spills_after);
        } else {
            todo!();
        }
    }

    new_program
}
