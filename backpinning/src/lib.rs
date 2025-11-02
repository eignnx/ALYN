use std::{collections::HashMap, fmt::Debug};

use alyn_common::names::{Lbl, Tmp};

#[derive(Debug, Clone)]
pub enum Stmt<I> {
    Instr(I),
    Label(Lbl),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SlotId(i32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Stg<R> {
    Tmp(Tmp),
    Reg(R),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Asn<R> {
    Reg(R),
    Slot(SlotId),
}

#[derive(Debug, Clone, Copy)]
pub enum Access<R> {
    Read(Stg<R>),
    Write(Stg<R>),
}

#[derive(Debug, Clone, Copy)]
pub enum CtrlFlow {
    Exit,
    Advance,
    Jump(Lbl),
    CondBranch(Lbl),
}

pub trait Instruction: Clone + Debug {
    type Reg;
    fn temporaries(&self) -> Vec<Access<Self::Reg>>;
    fn ctrl_flow(&self) -> CtrlFlow;
}

impl<I: Instruction> Instruction for Stmt<I> {
    type Reg = I::Reg;

    fn temporaries(&self) -> Vec<Access<Self::Reg>> {
        match self {
            Stmt::Instr(instr) => instr.temporaries(),
            Stmt::Label(_) => vec![],
        }
    }

    fn ctrl_flow(&self) -> CtrlFlow {
        match self {
            Stmt::Instr(instr) => instr.ctrl_flow(),
            Stmt::Label(_) => CtrlFlow::Advance,
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

pub fn compute_live_ranges<R, I: Instruction<Reg = R>>(
    stmts: &[Stmt<I>],
) -> HashMap<Tmp, Vec<LiveRange>> {
    let mut live_ranges = HashMap::<Tmp, Vec<LiveRange>>::new();
    let mut last_use = HashMap::<Tmp, usize>::new();

    for (i, stmt) in stmts.iter().enumerate().rev() {
        let Stmt::Instr(instr) = stmt else { continue };
        for access in instr.temporaries() {
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

pub fn linear_scan<R, I: Instruction<Reg = R>>(stmts: Vec<Stmt<I>>) -> Vec<Stmt<I>> {
    let mut working_set = HashMap::<Tmp, Asn<R>>::new();
    let mut new_program = Vec::new();

    for stmt in stmts.iter().rev() {
        match stmt.ctrl_flow() {
            CtrlFlow::Exit => todo!(),
            CtrlFlow::Advance => todo!(),
            CtrlFlow::Jump(lbl) => todo!(),
            CtrlFlow::CondBranch(lbl) => todo!(),
        }
    }

    new_program
}

pub fn print_program_with_live_ranges<I: Instruction>(stmts: &[Stmt<I>], live_ranges: &HashMap<Tmp, Vec<LiveRange>>) {
    let mut tmps = live_ranges.keys()
        .map(|t| (*t, format!("{t:?}").len()))
        .collect::<Vec<_>>();
    tmps.sort_unstable();

    print!("    ");
    for (tmp, len) in &tmps {
        print!("{:^width$} ", format!("{tmp:?}"), width=len);
    }
    println!();
    for (i, stmt) in stmts.iter().enumerate() {
        print!("{i:02}: ");
        for (tmp, len) in &tmps {
            let ranges = &live_ranges[tmp];
            if ranges.iter().any(|r| r.contains(i)) {
                print!("{:^width$} ", '|', width=len);
            } else {
                print!("{:^width$} ", ' ', width=len);
            }
        }
        println!("  {stmt:?}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Copy)]
    enum Reg {
        T0, T1, T2,
    }

    #[derive(Debug, Clone)]
    enum Instr {
        Def(Stg<Reg>),
        Use(Stg<Reg>),
    }

    impl Instruction for Instr {
        type Reg = Reg;

        fn temporaries(&self) -> Vec<Access<Self::Reg>> {
            match self {
                Instr::Def(dst) => vec![Access::Write(*dst)],
                Instr::Use(src) => vec![Access::Read(*src)],
            }
        }

        fn ctrl_flow(&self) -> CtrlFlow {
            match self {
                _ => CtrlFlow::Advance,
            }
        }
    }

    #[test]
    fn test_live_range_computation() {
        use Stmt as S;
        use Instr::*;

        let stmts = vec![
            S::Label("test".into()),
            S::Instr(Def(Stg::Tmp("x".into()))),
            S::Instr(Def(Stg::Tmp("y".into()))),
            S::Instr(Use(Stg::Tmp("x".into()))),
            S::Instr(Use(Stg::Tmp("y".into()))),
            S::Instr(Def(Stg::Tmp("z".into()))),
            S::Instr(Use(Stg::Tmp("y".into()))),
            S::Instr(Use(Stg::Tmp("z".into()))),

            S::Instr(Def(Stg::Tmp("qwerty".into()))),
            S::Instr(Use(Stg::Tmp("z".into()))),
            S::Instr(Use(Stg::Tmp("qwerty".into()))),

            S::Instr(Def(Stg::Tmp("x".into()))),
            S::Instr(Use(Stg::Tmp("x".into()))),
        ];


        let live_ranges = compute_live_ranges(&stmts[..]);
        print_program_with_live_ranges(&stmts[..], &live_ranges);

        println!("{live_ranges:?}");
    }
}
