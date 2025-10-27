#![feature(associated_type_defaults)]

use std::{fmt::Debug, hash::Hash};

use alyn_common::names::{Tmp};
use crate::common::{Stg};

mod alloc;
mod cfg;
mod common;
mod liveness;

pub trait Register: Copy + 'static + Debug + Ord + Eq + Hash {
    /// A list of all the available general-purpose registers.
    const GPRS: &'static [Self];
    const N_GPRS: usize = Self::GPRS.len();

    const GPR_SAVED_REGS: &'static [Self];
    const GPR_TEMP_REGS: &'static [Self];
    const GPR_ARG_REGS: &'static [Self];
}

pub trait Instruction: Debug + Clone {
    type Reg: Register;
    fn try_as_pure_move(&self) -> Option<(Stg<Self::Reg>, Stg<Self::Reg>)>;
    fn is_subr_call(&self) -> bool;
}

pub trait DefsUses: Instruction {
    fn add_defs_uses<E: Extend<Stg<Self::Reg>>>(&self, defs: &mut E, uses: &mut E);
    fn substitute_tmp_for_reg(&mut self, old: Tmp, new: Self::Reg);
}

#[cfg(test)]
mod tests {
    use super::*;

    type Unused = alloc::SlotId;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
