use std::{collections::{HashMap, HashSet}, fmt::{self, Debug, Display}};

use alyn_common::names::Tmp;
use regalloc_common::{asn::{Asn, SlotId}, ctrl_flow::{CtrlFlow, GetCtrlFlow}, slot_alloc::{InstrWrite, SlotAllocator}, stg::Stg, stmt::Stmt, Instruction, Register};


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
        let Stmt::Instr(mut instr) = stmt.clone() else { continue };
        for access in instr.accesses() {
            match access {
                Access::Read(Stg::Tmp(tmp), phase) => if !last_use.contains_key(&tmp) {
                    last_use.insert(*tmp, PrgPt::new(i, phase));
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
                Access::Read(Stg::Reg(_), _) | Access::Write(Stg::Reg(_), _) => {},
            }
        }
    }

    live_ranges
}

pub fn reg_choice<R: Register>(tmp: Tmp, working_set: &mut HashMap<Tmp, Asn<R>>) -> Asn<R> {
    let mut slot_id = 0usize;
    let choices = R::GPRS.into_iter().copied().map(Asn::Reg).chain(
        std::iter::from_fn(|| {
            let chosen_slot = slot_id;
            slot_id += 1;
            Some(Asn::Slot(SlotId(chosen_slot)))
        })
    );

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

pub fn linear_scan<R, I>(stmts: Vec<Stmt<I>>, live_ranges: &HashMap<Tmp, Vec<LiveRange>>, slot_alloc: impl SlotAllocator) -> Vec<Stmt<I>>
where
    R: Register,
    I: Instruction<Reg = R> + GetCtrlFlow + Accesses + InstrWrite
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

pub struct DisplayLiveRanges<'a, I> {
    stmts: &'a [Stmt<I>],
    live_ranges: &'a HashMap<Tmp, Vec<LiveRange>>,
}

impl<'a, I> DisplayLiveRanges<'a, I> {
    pub fn new(stmts: &'a [Stmt<I>], live_ranges: &'a HashMap<Tmp, Vec<LiveRange>>) -> Self {
        Self { stmts, live_ranges }
    }
}

impl<'a, I: Debug> Display for DisplayLiveRanges<'a, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tmps = self.live_ranges.keys()
            .map(|t| (*t, format!("{t:?}").len()))
            .collect::<Vec<_>>();
        tmps.sort_unstable_by_key(|(tmp, _)| {
            self.live_ranges[tmp]
                .iter()
                .map(|range| range.begin)
                .min()
                .unwrap()
        });

        let numcol_width = self.stmts.len().ilog10() as usize + 1;

        write!(f, "  ")?;
        for (iter, (tmp, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, " ")?;
            }
            write!(f, "{:<width$}", format!("{tmp:?}"), width=len)?;
        }
        writeln!(f, "   ")?;

        write!(f, "╔═")?;
        for (iter, (_, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, "═")?;
            }
            write!(f, "{:═<width$}", "╪", width=len)?;
        }
        writeln!(f, "══╗")?;

        for (i, stmt) in self.stmts.iter().enumerate() {
            for phase in InstrExePhase::PHASES {
                let pt = PrgPt::new(i, phase);

                if phase != InstrExePhase::ReadArgs && tmps.iter().all(|(tmp, _)| {
                    let ranges = &self.live_ranges[tmp];
                    ranges.iter().all(|r| r.begin != pt && r.end != pt)
                }) {
                    continue;
                }

                write!(f, "║ ")?;

                let mut draw_x_guide = false;

                for (tmp, len) in &tmps {
                    let ranges = &self.live_ranges[tmp];
                    let contained = ranges.iter().any(|r| r.contains(pt));
                    if ranges.iter().any(|r| r.begin == pt) {
                        draw_x_guide = true;
                        write!(f, "{:┄<width$}┄", '𜸛', width=len)?;
                    } else if ranges.iter().any(|r| r.end == pt) {
                        draw_x_guide = true;
                        write!(f, "{:┄<width$}┄", '𜸽', width=len)?;
                    } else if draw_x_guide {
                        if contained {
                            write!(f, "{:┄<width$}┄", '𜸩', width=len)?;
                        } else {
                            write!(f, "{:┄<width$}┄", '┄', width=len)?;
                        }
                    } else {
                        if contained {
                            write!(f, "{: <width$} ", '𜸩', width=len)?;
                        } else {
                            write!(f, "{: <width$} ", '┊', width=len)?;
                        }
                    }
                }

                if draw_x_guide {
                    write!(f, "┈")?;
                } else {
                    write!(f, " ")?;
                }

                match phase {
                    InstrExePhase::ReadArgs => if draw_x_guide {
                        write!(f, "╫┈{i:0width$}: {stmt:?}", width=numcol_width)?;
                    } else {
                        write!(f, "╟┈{i:0width$}: {stmt:?}", width=numcol_width)?;
                    },
                    InstrExePhase::WriteBack  => write!(f, "╫┈╯")?,
                }

                writeln!(f)?;
            }
        }

        write!(f, "╚═")?;
        for (iter, (_, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, "═")?;
            }
            write!(f, "{:═<width$}", "╪", width=len)?;
        }
        writeln!(f, "══╝")?;

        write!(f, "  ")?;
        for (iter, (tmp, len)) in tmps.iter().enumerate() {
            if iter != 0 {
                write!(f, " ")?;
            }
            write!(f, "{:<width$}", format!("{tmp:?}"), width=len)?;
        }
        writeln!(f, "   ")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use alyn_common::names::Lbl;
    use regalloc_common::{ctrl_flow::{CtrlFlow, GetCtrlFlow}, Register};

    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    enum Reg {
        T0, T1, T2,
    }

    impl Register for Reg {
        const GPRS: &'static [Self] = &[Reg::T0, Reg::T1, Reg::T2];
        const GPR_SAVED_REGS: &'static [Self] = &[];
        const GPR_TEMP_REGS: &'static [Self] = &[];
        const GPR_ARG_REGS: &'static [Self] = &[];
    }

    #[derive(Clone)]
    enum Instr {
        Def(Stg<Reg>),
        Use(Stg<Reg>),
        Move(Stg<Reg>, Stg<Reg>),
        MoveImm(Stg<Reg>, i32),
        BinOp(Stg<Reg>, Stg<Reg>, Stg<Reg>),
        BinOpImm(Stg<Reg>, Stg<Reg>, i32),
        Load(Stg<Reg>, Stg<Reg>),
        Store(Stg<Reg>, Stg<Reg>),
        CmpBranch(Stg<Reg>, Stg<Reg>, Lbl),
        Jmp(Lbl),
        Ret,
    }

    impl Debug for Instr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Def(x) => write!(f, "{x:?} ← 𜱪 "),
                Self::Use(x) => write!(f, "𜱪  ← {x:?}"),
                Self::Move(dst, src) => write!(f, "{dst:?} ← {src:?}"),
                Self::MoveImm(dst, imm) => write!(f, "{dst:?} ← {imm}"),
                Self::BinOp(dst, src1, src2) => write!(f, "{dst:?} ← {src1:?} ⋄ {src2:?}"),
                Self::BinOpImm(dst, src, imm) => write!(f, "{dst:?} ← {src:?} ⋄ {imm:?}"),
                Self::Load(dst, src) => write!(f, "{dst:?} ← MEM[{src:?}]"),
                Self::Store(dst, src) => write!(f, "MEM[{dst:?}] ← {src:?}"),
                Self::CmpBranch(src1, src2, lbl) => write!(f, "branch to {lbl:?} if {src1:?} ≷ {src2:?}"),
                Self::Jmp(lbl) => write!(f, "jmp {lbl:?}"),
                Self::Ret => write!(f, "ret"),
            }
        }
    }

    impl Instruction for Instr {
        type Reg = Reg;
    }

    impl Accesses for Instr {
        fn accesses<'a>(&'a mut self) -> Vec<Access<'a, Self::Reg>> {
            match self {
                Instr::Def(dst) => vec![Access::Write(dst, InstrExePhase::WriteBack)],
                Instr::Use(src) => vec![Access::Read(src, InstrExePhase::ReadArgs)],
                Instr::Move(dst, src) => vec![
                    Access::Read(src, InstrExePhase::ReadArgs),
                    Access::Write(dst, InstrExePhase::WriteBack),
                ],
                Instr::MoveImm(dst, _) => vec![Access::Write(dst, InstrExePhase::WriteBack)],
                Instr::BinOp(dst, src1, src2) => vec![
                    Access::Read(src1, InstrExePhase::ReadArgs),
                    Access::Read(src2, InstrExePhase::ReadArgs),
                    Access::Write(dst, InstrExePhase::WriteBack),
                ],
                Self::BinOpImm(dst, src, _) => vec![
                    Access::Read(src, InstrExePhase::ReadArgs),
                    Access::Write(dst, InstrExePhase::WriteBack),
                ],
                Instr::Load(dst, addr) => vec![
                    Access::Read(addr, InstrExePhase::ReadArgs),
                    Access::Write(dst, InstrExePhase::WriteBack),
                ],
                Instr::Store(addr, src) => vec![
                    Access::Read(addr, InstrExePhase::ReadArgs),
                    Access::Read(src, InstrExePhase::ReadArgs),
                ],
                Instr::CmpBranch(src1, src2, _) => vec![
                    Access::Read(src1, InstrExePhase::ReadArgs),
                    Access::Read(src2, InstrExePhase::ReadArgs),
                ],
                Instr::Jmp(_) | Instr::Ret => vec![],
            }
        }
    }

    impl GetCtrlFlow for Instr {
        fn ctrl_flow(&self) -> CtrlFlow {
            match self {
                Instr::Def(..) |
                Instr::Use(..) |
                Instr::Move(..) |
                Instr::MoveImm(..) |
                Instr::BinOp(..) |
                Instr::BinOpImm(..) |
                Instr::Load(..) |
                Instr::Store(..) => CtrlFlow::Advance,
                Instr::CmpBranch(_, _, lbl) => CtrlFlow::Branch(*lbl),
                Instr::Jmp(lbl) => CtrlFlow::Jump(*lbl),
                Instr::Ret => CtrlFlow::Exit,
            }
        }
    }

    impl From<Instr> for Stmt<Instr> {
        fn from(instr: Instr) -> Self {
            Self::Instr(instr)
        }
    }

    #[test]
    fn test_live_range_computation() {
        use Stmt as S;
        use Instr::*;

        let stmts = vec![
            S::Label("test".into()),
            S::Instr(Def("x".into())),
            S::Instr(Def("yeet".into())),
            S::Instr(Use("x".into())),
            S::Instr(Use("yeet".into())),
            S::Instr(Def("z".into())),
            S::Instr(Use("yeet".into())),
            S::Instr(Use("z".into())),

            S::Instr(Move("w".into(), "z".into())),
            S::Instr(Use("w".into())),

            S::Instr(Move("x".into(), "w".into())),
            S::Instr(Use("x".into())),
        ];

        let live_ranges = compute_live_ranges(&stmts[..]);
        println!("{}", DisplayLiveRanges::new(&stmts[..], &live_ranges));
    }

     #[test]
    fn knr_binsearch() {
        // int binsearch(int x, int v[], int n) {
        //     int low = 0;
        //     int high = n - 1;
        //     while (low <= high) {
        //          int mid = (low + high) / 2;
        //          int elem = v[mid];
        //          if (x < elem)
        //              high = mid - 1;
        //          else if (x > elem)
        //              low = mid + 1;
        //          else
        //              return mid;
        //     }
        //     return -1;
        // }
        //
        // binsearch:
        // mov %low <- 0;
        // mov %high <- %n - 1;
        // jmp loop_cond;
        // loop_top:
        //      mov %mid <- (%low + %high) / 2;
        //      load %elem <- MEM[%v + %mid];
        //      branch if %x >= %elem to else_if
        //          %high <- %mid - 1;
        //          jmp end_if;
        //      else_if:
        //      branch if x <= elem to else
        //          %low <- %mid + 1;
        //          jmp end_if;
        //      else:
        //          ret %mid;
        //      end_if:
        // loop_cond:
        //      branch if %low <= %high to loop_top;
        //  ret -1;
        //

        use Stmt as S;
        use Instr::*;

        let low = "low".into();
        let high = "high".into();
        let n = "n".into();
        let v = "v".into();
        let x = "x".into();
        let elem = "elem".into();
        let mid = "mid".into();
        let v_plus_mid = "v_plus_mid".into();
        let retval = "retval".into();

        #[rustfmt::skip]
        let stmts = vec![
            Def(x).into(), Def(v).into(), Def(n).into(),
            S::Label(Lbl::subr("binsearch")),
            MoveImm(low, 0).into(),
            BinOpImm(high, n, 1).into(),
            Jmp("loop_cond".into()).into(),
            S::Label("loop_top".into()),
                BinOp(mid, low, high).into(),
                BinOp(v_plus_mid, v, mid).into(),
                Load(elem, v_plus_mid).into(),
                CmpBranch(x, elem, "else_if".into()).into(),
                    BinOpImm(high, mid, 1).into(),
                    Jmp("end_if".into()).into(),
                S::Label("else_if".into()),
                CmpBranch(x, elem, "else".into()).into(),
                    BinOpImm(low, mid, 1).into(),
                    Jmp("end_if".into()).into(),
                S::Label("else".into()).into(),
                    Move(retval, mid).into(),
                    Ret.into(),
                    Use(retval).into(),
                S::Label("end_if".into()).into(),
            S::Label("loop_cond".into()).into(),
                CmpBranch(low, high, "loop_top".into()).into(),
            MoveImm(retval, -1).into(),
            Ret.into(),
            Use(retval).into(),
        ];
        let live_ranges = compute_live_ranges(&stmts[..]);
        println!("{}", DisplayLiveRanges::new(&stmts[..], &live_ranges));
    }
}
