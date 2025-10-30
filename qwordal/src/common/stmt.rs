use std::{
    collections::{BTreeSet, HashMap},
    fmt::{self, Debug},
};

use crate::{
    DefsUses, Instruction, StgSubst, ToSpill,
    common::{Asn, CtrlFlow, CtrlTx, Stg},
};
use alyn_common::names::{Lbl, Tmp};

/// Wrapper around instruction so that `I` doesn't need to have it's own `Label` variant.
#[derive(Clone)]
pub enum Stmt<I> {
    Instr(I),
    Label(Lbl),
}

impl<I> From<I> for Stmt<I> {
    fn from(instr: I) -> Self {
        Stmt::Instr(instr)
    }
}

impl<I> Stmt<I> {
    pub fn label(lbl: impl Into<Lbl>) -> Self {
        Stmt::Label(lbl.into())
    }
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

impl<I: CtrlFlow> CtrlFlow for Stmt<I> {
    fn ctrl_tx(&self) -> CtrlTx {
        match self {
            Stmt::Instr(instr) => instr.ctrl_tx(),
            Stmt::Label(_) => CtrlTx::Advance,
        }
    }
}

impl<I: DefsUses> DefsUses for Stmt<I> {
    fn add_defs_uses<E: Extend<Stg<Self::Reg>>>(&self, defs: &mut E, uses: &mut E) {
        if let Self::Instr(instr) = self {
            instr.add_defs_uses(defs, uses);
        }
    }
}

impl<I: StgSubst> StgSubst for Stmt<I> {
    fn subst_tmp(
        &mut self,
        assignments: &HashMap<Tmp, Asn<Self::Reg>>,
        spills: &mut BTreeSet<ToSpill>,
    ) {
        if let Self::Instr(instr) = self {
            instr.subst_tmp(assignments, spills);
        }
    }
}

impl<I: Debug> Debug for Stmt<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Instr(instr) => write!(f, "    {instr:?}"),
            Self::Label(lbl) => write!(f, "{lbl:?}:"),
        }
    }
}
