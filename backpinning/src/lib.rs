#![feature(formatting_options)]

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use alyn_common::names::Tmp;
use regalloc_common::{
    DefsUses, Instruction, Register,
    asn::{Asn, SlotId},
    cfg::Cfg,
    ctrl_flow::{CtrlFlow, GetCtrlFlow},
    liveness::LiveSets,
    slot_alloc::{InstrWrite, SlotAllocator},
    stg::Stg,
    stmt::Stmt,
};

pub mod diagram;

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
    stmt_idx: usize,
    phase: InstrExePhase,
}

impl PrgPt {
    pub fn new(stmt_idx: usize, phase: InstrExePhase) -> Self {
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

pub fn compute_live_ranges<R, I: Instruction<Reg = R> + Accesses>(
    stmts: &[Stmt<I>],
) -> HashMap<Tmp, Vec<LiveRange>> {
    let mut live_ranges = HashMap::<Tmp, Vec<LiveRange>>::new();
    let mut last_use = HashMap::<Tmp, PrgPt>::new();

    for (i, stmt) in stmts.iter().enumerate().rev() {
        let Stmt::Instr(mut instr) = stmt.clone() else {
            continue;
        };
        for access in instr.accesses() {
            match access {
                Access::Read(Stg::Tmp(tmp), phase) => {
                    if !last_use.contains_key(&tmp) {
                        last_use.insert(*tmp, PrgPt::new(i, phase));
                    }
                }
                Access::Write(Stg::Tmp(tmp), phase) => {
                    let Some(end) = last_use.remove(&tmp) else {
                        continue; // Never read from, so just ignore.
                    };
                    live_ranges.entry(*tmp).or_default().push(LiveRange {
                        begin: PrgPt::new(i, phase),
                        end,
                    });
                }
                Access::Read(Stg::Reg(_), _) | Access::Write(Stg::Reg(_), _) => {}
            }
        }
    }

    live_ranges
}

pub fn compute_live_ranges_2<
    R: Register,
    I: Instruction<Reg = R> + Accesses + GetCtrlFlow + DefsUses,
>(
    cfg: &Cfg<R, I>,
) -> HashMap<Tmp, Vec<LiveRange>> {
    let mut live_ranges = HashMap::<Tmp, Vec<LiveRange>>::new();
    let mut last_use = HashMap::<Tmp, PrgPt>::new();

    let live_sets = LiveSets::build_from(cfg, []);

    for (idx, stmt) in cfg.stmts().enumerate().rev() {
        match stmt.ctrl_flow() {
            CtrlFlow::Advance => todo!(),
            CtrlFlow::Exit => todo!(),
            CtrlFlow::Jump(lbl) => todo!(),
            CtrlFlow::Switch(lbls) => todo!(),
            CtrlFlow::Branch(lbl) => todo!(),
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
