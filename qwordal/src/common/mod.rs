use std::fmt::Debug;

mod stmt;

use alyn_common::names::{Lbl, Tmp};
pub use stmt::*;

/// "Storage"
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, std::hash::Hash)]
pub enum Stg<R> {
    Tmp(Tmp),
    Reg(R),
}

impl<R: Debug> Debug for Stg<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tmp(tmp) => write!(f, "{tmp:?}"),
            Self::Reg(reg) => write!(f, "{reg:?}"),
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

#[derive(Debug, Clone, Copy)]
pub struct SlotId(pub usize);

/// "Assignment"
/// The thing to which a temporary is assigned by the end of regalloc.
#[derive(Clone, Copy)]
pub enum Asn<R> {
    Reg(R),
    /// A location on the stack relative to the base pointer.
    Slot(SlotId),
}
