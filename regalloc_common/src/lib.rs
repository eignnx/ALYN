#![feature(associated_type_defaults)]

use std::{fmt::Debug, hash::Hash};

use crate::stg::Stg;

pub mod stmt;
pub mod stg;
pub mod slot_alloc;
pub mod ctrl_flow;
pub mod asn;

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
    fn try_as_pure_move(&self) -> Option<(Stg<Self::Reg>, Stg<Self::Reg>)> { None }
    fn is_subr_call(&self) -> bool { false }
}

