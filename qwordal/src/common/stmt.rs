use std::fmt::{self, Debug};

use alyn_common::names::{Lbl, Tmp};
use crate::{
    DefsUses, Instruction,
    cfg::{ControlFlow, CtrlTx},
    common::{Stg},
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

impl<I: ControlFlow> ControlFlow for Stmt<I> {
    fn ctrl_tx(&self) -> CtrlTx {
        match self {
            Stmt::Instr(instr) => instr.ctrl_tx(),
            Stmt::Label(_) => CtrlTx::Advance,
        }
    }
}

impl<I: Instruction + DefsUses> DefsUses for Stmt<I> {
    fn add_defs_uses<E: Extend<Stg<Self::Reg>>>(&self, defs: &mut E, uses: &mut E) {
        if let Self::Instr(instr) = self {
            instr.add_defs_uses(defs, uses);
        }
    }

    fn substitute_tmp_for_reg(&mut self, old: Tmp, new: Self::Reg) {
        if let Self::Instr(instr) = self {
            instr.substitute_tmp_for_reg(old, new);
        }
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
