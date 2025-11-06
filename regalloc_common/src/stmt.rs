use std::fmt::{self, Debug};

use alyn_common::names::Lbl;

use crate::{
    ctrl_flow::{CtrlFlow, GetCtrlFlow}, stg::Stg, DefsUses, Instruction
};

/// Wrapper around instruction so that `I` doesn't need to have it's own `Label` variant.
#[derive(Clone)]
pub enum Stmt<I> {
    Instr(I),
    Label(Lbl),
}

impl<I: Instruction> Instruction for Stmt<I> {
    type Reg = I::Reg;

    fn try_as_pure_move(&self) -> Option<(Stg<Self::Reg>, Stg<Self::Reg>)> {
        match self {
            Stmt::Instr(instr) => instr.try_as_pure_move(),
            Stmt::Label(_) => None,
        }
    }

    fn is_subr_call(&self) -> bool {
        match self {
            Stmt::Instr(instr) => instr.is_subr_call(),
            Stmt::Label(_) => false,
        }
    }
}

impl<I: GetCtrlFlow> GetCtrlFlow for Stmt<I> {
    fn ctrl_flow(&self) -> CtrlFlow {
        match self {
            Stmt::Instr(instr) => instr.ctrl_flow(),
            Stmt::Label(_) => CtrlFlow::Advance,
        }
    }
}

impl<I: DefsUses> DefsUses for Stmt<I> {
    fn defs_uses<'a>(&'a mut self) -> impl Iterator<Item=crate::DefUse<'a, Self::Reg>> {
        let mut out = Vec::new();
        match self {
            Stmt::Instr(instr) => out.extend(instr.defs_uses()),
            Stmt::Label(_) => {},
        }
        out.into_iter()
    }
}

impl<I: Debug> Debug for Stmt<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Instr(instr) => instr.fmt(f),
            Self::Label(lbl) => lbl.fmt(f),
        }
    }
}
