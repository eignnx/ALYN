use std::{cmp::Ordering, collections::HashMap, fmt::{self, Debug, Display}};

use alyn_common::names::Tmp;
use regalloc_common::{asn::Asn, ctrl_flow::{CtrlFlow, GetCtrlFlow}, stg::Stg, stmt::Stmt, Instruction};


#[derive(Debug, Clone, Copy)]
pub enum Access<R> {
    Read(Stg<R>, InstrExePhase),
    Write(Stg<R>, InstrExePhase),
}

pub trait Accesses: Instruction {
    fn accesses(&self) -> Vec<Access<Self::Reg>>;
}

impl<I: Accesses> Accesses for Stmt<I> {
    fn accesses(&self) -> Vec<Access<Self::Reg>> {
        match self {
            Stmt::Instr(instr) => instr.accesses(),
            Stmt::Label(_) => vec![],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InstrExePhase {
    Before,
    During,
    After,
}

impl InstrExePhase {
    pub const PHASES: [Self; 3] = [Self::Before, Self::During, Self::After];
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
        let Stmt::Instr(instr) = stmt else { continue };
        for access in instr.accesses() {
            match access {
                Access::Read(Stg::Tmp(tmp), phase) => if !last_use.contains_key(&tmp) {
                    last_use.insert(tmp, PrgPt::new(i, phase));
                }
                Access::Write(Stg::Tmp(tmp), phase) => {
                    let Some(end) = last_use.remove(&tmp) else {
                        continue; // Never read from, so just ignore.
                    };
                    live_ranges.entry(tmp).or_default().push(LiveRange {
                        begin: PrgPt::new(i, phase),
                        end,
                    });
                }
                Access::Read(Stg::Reg(_), _) | Access::Write(Stg::Reg(_), _) => todo!(),
            }
        }
    }

    live_ranges
}

pub fn linear_scan<R, I: Instruction<Reg = R> + GetCtrlFlow>(stmts: Vec<Stmt<I>>) -> Vec<Stmt<I>> {
    let mut working_set = HashMap::<Tmp, Asn<R>>::new();
    let mut new_program = Vec::new();

    for stmt in stmts.iter().rev() {
        match stmt.ctrl_flow() {
            CtrlFlow::Exit => todo!(),
            CtrlFlow::Advance => todo!(),
            CtrlFlow::Jump(_) => todo!(),
            CtrlFlow::Branch(_) => todo!(),
            CtrlFlow::Switch(_) => todo!(),
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
        tmps.sort_unstable();

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

                if phase != InstrExePhase::During && tmps.iter().all(|(tmp, _)| {
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
                    InstrExePhase::Before => write!(f, "╫┈╮")?,
                    InstrExePhase::During => write!(f, "╫┈{i:0width$}: {stmt:?}", width=numcol_width)?,
                    InstrExePhase::After  => write!(f, "╫┈╯")?,
                }
                if phase == InstrExePhase::During {
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
    use regalloc_common::{ctrl_flow::{CtrlFlow, GetCtrlFlow}, Register};

    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    enum Reg {
        T0, T1, T2,
    }

    impl Register for Reg {
        const GPRS: &'static [Self] = &[];
        const GPR_SAVED_REGS: &'static [Self] = &[];
        const GPR_TEMP_REGS: &'static [Self] = &[];
        const GPR_ARG_REGS: &'static [Self] = &[];
    }

    #[derive(Clone)]
    enum Instr {
        Def(Stg<Reg>),
        Use(Stg<Reg>),
        Move(Stg<Reg>, Stg<Reg>),
    }

    impl Debug for Instr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Def(x) => write!(f, "{x:?} ← 𜱪 "),
                Self::Use(x) => write!(f, "𜱪  ← {x:?}"),
                Self::Move(dst, src) => write!(f, "{dst:?} ← {src:?}"),
            }
        }
    }

    impl Instruction for Instr {
        type Reg = Reg;
    }

    impl Accesses for Instr {
        fn accesses(&self) -> Vec<Access<Self::Reg>> {
            match self {
                Instr::Def(dst) => vec![Access::Write(*dst, InstrExePhase::After)],
                Instr::Use(src) => vec![Access::Read(*src, InstrExePhase::During)],
                Instr::Move(dst, src) => vec![
                    Access::Write(*dst, InstrExePhase::After),
                    Access::Read(*src, InstrExePhase::Before),
                ],
            }
        }
    }

    impl GetCtrlFlow for Instr {
        fn ctrl_flow(&self) -> CtrlFlow {
            todo!()
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
}
