use std::{
    collections::{BTreeSet, HashMap},
    fmt::Debug,
};

mod stmt;

use alyn_common::names::{Lbl, Tmp};
pub use stmt::*;

use crate::{Register, ToSpill};

/// "Storage"
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, std::hash::Hash)]
pub enum Stg<R> {
    Tmp(Tmp),
    Reg(R),
}

impl<R> Stg<R> {
    pub fn try_as_tmp(self) -> Option<Tmp> {
        match self {
            Stg::Tmp(tmp) => Some(tmp),
            Stg::Reg(_) => None,
        }
    }

    pub fn try_as_reg(self) -> Option<R> {
        match self {
            Stg::Reg(reg) => Some(reg),
            Stg::Tmp(_) => None,
        }
    }
}

impl<R> From<Tmp> for Stg<R> {
    fn from(tmp: Tmp) -> Self {
        Self::Tmp(tmp)
    }
}

impl<R: Register> Stg<R> {
    pub fn subst_def(
        &mut self,
        assignments: &HashMap<Tmp, Asn<R>>,
        spills: &mut BTreeSet<ToSpill>,
    ) {
        if let Stg::Tmp(tmp) = self {
            match assignments[&*tmp] {
                Asn::Reg(reg) => *self = Stg::Reg(reg),
                Asn::Slot(slot_id) => {
                    spills.insert(ToSpill::Def(*tmp, slot_id));
                }
            }
        }
    }
    pub fn subst_use(
        &mut self,
        assignments: &HashMap<Tmp, Asn<R>>,
        spills: &mut BTreeSet<ToSpill>,
    ) {
        if let Stg::Tmp(tmp) = self {
            match assignments[&*tmp] {
                Asn::Reg(reg) => *self = Stg::Reg(reg),
                Asn::Slot(slot_id) => {
                    spills.insert(ToSpill::Use(*tmp, slot_id));
                }
            }
        }
    }
}

impl<R: Debug> Debug for Stg<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tmp(tmp) => tmp.fmt(f),
            Self::Reg(reg) => reg.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CtrlTx {
    /// Exit the current subroutine. Either a return statement, a tail-call, or maybe
    /// `system_exit`.
    Exit,

    /// Just advance to the next instruction: `$PC <- $PC + 1`
    Advance,

    /// Unconditional jump to given label.
    Jump(Lbl),

    /// Jump to one of the given labels, like in a switch statement.
    Switch(Vec<Lbl>),

    /// Either fallthrough (which would be the same as `Advance`) or branch to the given label.
    Branch(Lbl),
}

pub trait CtrlFlow {
    fn ctrl_tx(&self) -> CtrlTx;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SlotId(pub usize);

/// "Assignment"
/// The thing to which a temporary is assigned by the end of regalloc.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Asn<R> {
    Reg(R),
    /// A location on the stack relative to the base pointer.
    Slot(SlotId),
}
