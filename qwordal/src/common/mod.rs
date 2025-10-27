use std::fmt::Debug;

mod stmt;

use alyn_common::names::{Tmp};
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
