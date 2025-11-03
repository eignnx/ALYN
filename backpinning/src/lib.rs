use std::{collections::HashMap, fmt::{self, Debug, Display}};

use alyn_common::names::Tmp;
use regalloc_common::{asn::Asn, ctrl_flow::{CtrlFlow, GetCtrlFlow}, stg::Stg, stmt::Stmt, Instruction};


#[derive(Debug, Clone, Copy)]
pub enum Access<R> {
    Read(Stg<R>),
    Write(Stg<R>),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiveRange {
    begin: usize,
    end: usize,
}

impl LiveRange {
    pub fn contains(&self, stmt_idx: usize) -> bool {
        self.begin <= stmt_idx && stmt_idx <= self.end
    }
}

pub fn compute_live_ranges<R, I: Instruction<Reg = R> + Accesses>(
    stmts: &[Stmt<I>],
) -> HashMap<Tmp, Vec<LiveRange>> {
    let mut live_ranges = HashMap::<Tmp, Vec<LiveRange>>::new();
    let mut last_use = HashMap::<Tmp, usize>::new();

    for (i, stmt) in stmts.iter().enumerate().rev() {
        let Stmt::Instr(instr) = stmt else { continue };
        for access in instr.accesses() {
            match access {
                Access::Read(Stg::Tmp(tmp)) => if !last_use.contains_key(&tmp) {
                    last_use.insert(tmp, i);
                }
                Access::Write(Stg::Tmp(tmp)) => {
                    let Some(end) = last_use.remove(&tmp) else {
                        continue; // Never read from, so just ignore.
                    };
                    live_ranges.entry(tmp).or_default().push(LiveRange {
                        begin: i,
                        end,
                    });
                }
                Access::Read(Stg::Reg(_)) | Access::Write(Stg::Reg(_)) => todo!(),
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
            CtrlFlow::Jump(lbl) => todo!(),
            CtrlFlow::Branch(lbl) => todo!(),
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

        for (tmp, len) in &tmps {
            write!(f, "{:^width$} ", format!("{tmp:?}"), width=len)?;
        }
        writeln!(f, "{:width$}", "", width=numcol_width + 2)?;
        for (i, stmt) in self.stmts.iter().enumerate() {
            let mut draw_x_guide = false;

            for (tmp, len) in &tmps {
                let ranges = &self.live_ranges[tmp];
                let contained = ranges.iter().any(|r| r.contains(i));
                if ranges.iter().any(|r| r.begin == i) {
                    draw_x_guide = true;
                    write!(f, "{:┄<width$}┄", '𜸛', width=len)?;
                } else if ranges.iter().any(|r| r.end == i) {
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
                write!(f, "┈┼ ")?;
            } else {
                write!(f, " │ ")?;
            }
            writeln!(f, "{i:0width$}: {stmt:?}", width=numcol_width)?;
        }
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
                Instr::Def(dst) => vec![Access::Write(*dst)],
                Instr::Use(src) => vec![Access::Read(*src)],
                Instr::Move(dst, src) => vec![Access::Write(*dst), Access::Read(*src)],
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
