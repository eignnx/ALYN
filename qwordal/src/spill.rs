use std::collections::{BTreeSet, HashMap};

use alyn_common::names::Tmp;

use crate::{common::{Asn, Stmt}, DefOrUse, DefsUses, Instruction, Register};

pub fn rewrite_with_spills<I: Instruction + DefsUses>(
    program: impl IntoIterator<Item = Stmt<I>> + ExactSizeIterator,
    assignments: &HashMap<Tmp, Asn<I::Reg>>,
) -> Vec<Stmt<I>> {
    let max_expected_spills: usize = <I::Reg as Register>::GPR_SAVED_REGS.len();
    let mut new_program = Vec::with_capacity(program.len() + 2 * max_expected_spills);
    let mut spills_buf = BTreeSet::new();

    let mut before_buf = Vec::new();
    let mut after_buf = Vec::new();

    let mut did_spill = false;

    let mk_fresh = |to_spill| Tmp::fresh(&format!("spill<{to_spill:?}>"));

    for mut stmt in program {
        if stmt.is_trivial_move() {
            continue;
        }

        stmt.substitute_tmp_for_reg(assignments, &mut spills_buf);
        for spill in spills_buf.iter() {
            did_spill = true;
            match spill {
                DefOrUse::Use(tmp) => {
                    // If it's a Use of X, insert Stmt::StackLoad(new_tmp, X_address) before
                    // and edit the using instruction.
                    todo!("spill needed for use {tmp}")
                },
                DefOrUse::Def(tmp) => {
                    // If it's a Def of X, edit the old stmt to Def `new_tmp`, and insert
                    // Stmt::StackStore(X_address, new_tmp) after.
                    todo!("spill needed for def {tmp}")
                },
            }
        }
        spills_buf.clear();

        new_program.extend(before_buf.drain(..));
        new_program.push(stmt);
        new_program.extend(after_buf.drain(..));
        
    }

    new_program
}
