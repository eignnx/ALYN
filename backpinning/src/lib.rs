#![feature(formatting_options)]

use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
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
    JustBefore,
    ReadArgs,
    WriteBack,
    JustAfter,
}

impl Display for InstrExePhase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let char = match self {
            InstrExePhase::JustBefore => 'b',
            InstrExePhase::ReadArgs => 'R',
            InstrExePhase::WriteBack => 'W',
            InstrExePhase::JustAfter => 'a',
        };
        write!(f, "{char}")
    }
}

impl InstrExePhase {
    pub const PHASES: [Self; 4] = [
        Self::JustBefore,
        Self::ReadArgs,
        Self::WriteBack,
        Self::JustAfter,
    ];
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrgPt {
    stmt_idx: StmtIdx,
    phase: InstrExePhase,
}

impl Debug for PrgPt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{:?}", self.phase, self.stmt_idx)
    }
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
                    let here = PrgPt::new(i, phase);
                    let lr = LiveRange { begin: here, end };
                    live_ranges.entry(*stg).or_default().push(lr);
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

        let lbls_it = cfg.bb_labels(bb_idx);
        if lbls_it.len() > 0 {
            print!("[ ");
            for (i, lbl) in lbls_it.enumerate() {
                if i > 0 {
                    print!("; ");
                }
                print!("{lbl}");
            }
            println!(" ]");
        }

        for (idx, instr) in cfg.bb_instrs_indexed(bb_idx) {
            println!("{idx}: {instr:?}");
        }

        if let Some((idx, term)) = cfg.bb_terminator_indexed(bb_idx) {
            println!("{idx}: {term:?}");
        }

        println!("{:-^40}", "");
        println!("live-outs: {:?}", live_sets.live_outs(bb_idx));
        if cfg.successor_bbs(bb_idx).len() > 0 {
            print!("--> [ ");
            for (i, succ) in cfg.successor_bbs(bb_idx).enumerate() {
                if i > 0 {
                    print!("; ");
                }
                print!("{succ}");
            }
            println!(" ]");
        }
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

        // For each live-out, set it's end point within the `Bb` to be `JustAfter` the last statement
        // of the `Bb`.
        for live in live_set.iter().copied() {
            let last_idx_in_bb = cfg[bb_idx].instrs_range().end - 1.into();
            live_ends.insert(live, PrgPt::new(last_idx_in_bb, InstrExePhase::JustAfter));
        }

        // Now process (in reverse order) each instruction in the `Bb`.
        for (stmt_idx, instr) in cfg.bb_instrs_indexed(bb_idx).rev() {
            for access in instr.clone().accesses() {
                match access {
                    Access::Read(stg, phase) => {
                        live_set.insert(*stg); // `stg` must be live at this point
                        live_ends
                            .entry(*stg) // If `stg` is not in the map, insert this as last use.
                            .or_insert(PrgPt::new(stmt_idx, phase));
                    }
                    Access::Write(stg, phase) => {
                        live_set.remove(stg); // Above this point, `stg`'s value is irrelevant.
                        let end = live_ends.remove(stg).unwrap(); // Get it's end
                        let begin = PrgPt::new(stmt_idx, phase); // Here is it's start
                        live_ranges
                            .entry(*stg)
                            .or_default()
                            .push(LiveRange { begin, end })
                    }
                }
            }

            // NOTE: We're iterating over instrs in reverse order through the `Bb`! 
            println!();
            println!("  live_ends: {:?}", live_ends); // State before the instr
            println!("  live_set: {:?}", live_set);   // State before the instr
            println!("{stmt_idx}: {instr:?}\t{:?}", instr.clone().accesses());
        }

        // If there's anything left in `live_ends`, it needs it's own live range for this `Bb`.
        for (stg, end) in live_ends {
            let begin_idx = cfg.bb_instrs_indexed(bb_idx).next().unwrap().0;
            let begin = PrgPt::new(begin_idx, InstrExePhase::JustBefore);
            live_ranges.entry(stg)
                .or_default()
                .push(LiveRange { begin, end });
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
