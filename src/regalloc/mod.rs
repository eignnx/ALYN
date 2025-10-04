use std::{
    collections::{BTreeSet, HashMap, LinkedList},
    fmt::Debug,
};

use derive_more::From;
use internment::Intern;
use smallvec::SmallVec;

use crate::{
    instr_sel::Stg,
    names::{Lbl, Tmp},
};

pub mod cfg;
mod interferences;
mod live_sets;
mod regalloc;

pub use regalloc::*;

#[cfg(test)]
mod test_datastructures;

/// Control Transfer - how the program counter may be updated by an instruction.
///
/// A `return` statement would produce `None`, a `nop` statement would produce `Some(Advance)`.
/// Subroutine call statements should produce `Some(Advance)` since within the subroutine it
/// wouldn't seem like any jump has happened.
pub enum CtrlTx {
    /// Just advance to the next instruction: `$PC <- $PC + 1`
    Advance,

    /// Unconditional jump to given label.
    Jump(Lbl),

    /// Jump to one of the given labels, like in a switch statement.
    Switch(SmallVec<[Lbl; 2]>),

    /// Either fallthrough (same as `Advance`) or branch to the given label.
    Branch(Lbl),
}

pub trait Instr: Debug {
    type Register: Copy + Debug;

    fn add_defs_uses(
        &self,
        defs: &mut impl Extend<Stg<Self::Register>>,
        uses: &mut impl Extend<Stg<Self::Register>>,
    );

    /// `%a <- %b` is a pure move instruction from one register/temporary to another.
    /// Returns the lefthand side (`%a`) and the righthand side (`%b`) respectively.
    fn try_as_pure_move(&self) -> Option<(Tmp, Tmp)>;

    fn replace_def_occurrances(&mut self, old: Tmp, new: Stg<Self::Register>);
    fn replace_use_occurrances(&mut self, old: Tmp, new: Stg<Self::Register>);
    fn replace_occurrances(&mut self, old: Tmp, new: Stg<Self::Register>) {
        self.replace_def_occurrances(old, new);
        self.replace_use_occurrances(old, new);
    }

    /// If this instruction has a label associated with it, return it.
    fn get_label(&self) -> Option<Lbl>;

    /// A `return` statement would produce `None`, a `nop` statement would produce `Some(Advance)`.
    /// Subroutine call statements should produce `Some(Advance)` since within the subroutine it
    /// wouldn't seem like any jump has happened.
    fn ctrl_tx(&self) -> Option<CtrlTx>;

    fn mk_store_to_stack(addr: i32, src: Tmp) -> Self;
    fn mk_load_from_stack(dst: Tmp, addr: i32) -> Self;
}

/// Calling convention
pub trait Cc<R: 'static + Clone> {
    /// A list of all the available general-purpose registers.
    const GPRS: &'static [R];

    const N_GPRS: usize = Self::GPRS.len();
}
