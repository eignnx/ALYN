#![feature(associated_type_defaults)]

use std::{fmt::Debug, hash::Hash};

use crate::stg::Stg;

pub mod stmt;
pub mod stg;
pub mod slot_alloc;
pub mod ctrl_flow;
pub mod asn;
pub mod liveness;
pub mod cfg;

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

pub enum DefUseMut<'a, R> {
    Def(&'a mut Stg<R>),
    Use(&'a mut Stg<R>),
}

pub trait DefsUsesMut: Instruction {
    fn defs_uses_mut<'a>(&'a mut self) -> impl Iterator<Item=DefUseMut<'a, Self::Reg>>;
}

pub enum DefUse<R> {
    Def(Stg<R>),
    Use(Stg<R>),
}

pub trait DefsUses: Instruction {
    fn defs_uses(&self, out: &mut impl Extend<DefUse<Self::Reg>>);
}

/// Provides a default implementation of `DefsUses` if `DefsUsesMut` has already been implemented
/// and the instruction type is cheap to clone.
///
/// ```
/// # use regalloc_common::*;
/// #[derive(Debug, Clone)]
/// enum MyInstr { Mov }
///
/// #[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
/// enum MyReg { X }
///
/// impl Register for MyReg {
/// #   const GPRS: &'static [Self] = &[];
/// #   const GPR_SAVED_REGS: &'static [Self] = &[];
/// #   const GPR_TEMP_REGS: &'static [Self] = &[];
/// #   const GPR_ARG_REGS: &'static [Self] = &[];
///     /* .. */
/// }
///
/// impl Instruction for MyInstr {
/// #    type Reg = MyReg;
///     /* .. */
/// }
///
/// impl DefsUsesMut for MyInstr {
///     fn defs_uses_mut<'a>(&'a mut self) -> impl Iterator<Item=DefUseMut<'a, Self::Reg>> {
/// #       [].into_iter()
///         /* .. */
///     }
/// }
///
/// impl DefsUses for MyInstr {
///     fn defs_uses(&self, out: &mut impl Extend<DefUse<Self::Reg>>) {
///         CloneableInstr(self).defs_uses(out);
///     }
/// }
/// ```
#[derive(Debug, Clone)]
pub struct CloneableInstr<'i, I: Clone>(pub &'i I);

impl<'i, I: Instruction> Instruction for CloneableInstr<'i, I> {
    type Reg = I::Reg;

    fn try_as_pure_move(&self) -> Option<(Stg<Self::Reg>, Stg<Self::Reg>)> {
        self.0.try_as_pure_move()
    }

    fn is_subr_call(&self) -> bool {
        self.0.is_subr_call()
    }
}

impl<'i, T> DefsUses for CloneableInstr<'i, T> where T: DefsUsesMut + Clone {
    fn defs_uses(&self, out: &mut impl Extend<DefUse<Self::Reg>>) {
        for u in self.0.clone().defs_uses_mut() {
            match u {
                DefUseMut::Def(stg) => out.extend([DefUse::Def(*stg)]),
                DefUseMut::Use(stg) => out.extend([DefUse::Use(*stg)]),
            }
        }
    }
}
