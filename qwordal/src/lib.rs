#![feature(associated_type_defaults)]

use std::{
    collections::{BTreeSet, HashMap},
    fmt::Debug,
    hash::Hash,
};

use crate::{alloc::Asn, common::Stg};
use alyn_common::names::Tmp;

mod alloc;
mod cfg;
pub mod common;
mod liveness;
mod spill;

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

    /// Any move with an identical source and destination is trivial and may be eliminated.
    fn is_trivial_move(&self) -> bool {
        self.try_as_pure_move().is_some_and(|(dst, src)| dst == src)
    }

    fn is_subr_call(&self) -> bool;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DefOrUse {
    Def(Tmp),
    Use(Tmp),
}

pub trait DefsUses: Instruction {
    fn add_defs_uses<E: Extend<Stg<Self::Reg>>>(&self, defs: &mut E, uses: &mut E);

    /// Given a mapping of assignments from temporaries to either registers or stack slots, this
    /// method must mutate `self` so that temporaries are replaced with their assigned registers.
    /// If a temporary has been assigned to a stack slot, then this method should skip replacement
    /// of the temporary, and instead insert a `Def` or `Use` into the spill set to indicate to
    /// calling code that a spill instruction is needed.
    fn substitute_tmp_for_reg(
        &mut self,
        assignments: &HashMap<Tmp, Asn<Self::Reg>>,
        spills: &mut BTreeSet<DefOrUse>,
    );
}

