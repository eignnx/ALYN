use std::collections::{BTreeSet, HashMap};

use alyn_common::names::Tmp;

use crate::{alloc::{InstrWrite, SlotAllocator}, common::{Asn, Stg, Stmt}, DefsUses, Instruction, Register, StgSubst, ToSpill};

pub fn rewrite_with_spills<I>(
    program: impl IntoIterator<Item = Stmt<I>> + ExactSizeIterator,
    assignments: &HashMap<Tmp, Asn<I::Reg>>,
    slot_alloc: &mut impl SlotAllocator,
) -> (Vec<Stmt<I>>, bool)
    where I: Instruction + DefsUses + InstrWrite + StgSubst
{
    let max_expected_spills: usize = <I::Reg as Register>::GPR_SAVED_REGS.len();
    let mut new_program = Vec::with_capacity(program.len() + 2 * max_expected_spills);
    let mut spills_buf = BTreeSet::new();

    let mut before_buf = Vec::new();
    let mut after_buf = Vec::new();

    let mut did_spill = false;

    let mk_fresh = |to_spill| Stg::Tmp(Tmp::fresh(&format!("spill<{to_spill:?}>")));

    for mut stmt in program.into_iter().filter(|s| !s.is_trivial_move()) {
        stmt.substitute_tmp_for_reg(assignments, &mut spills_buf);
        for spill in spills_buf.iter() {
            did_spill = true;
            match *spill {
                ToSpill::Use(tmp, src_slot_id) => {
                    // If it's a Use of X, insert Stmt::StackLoad(new_tmp, X_address) before
                    // and edit the using instruction.
                    before_buf.extend(slot_alloc.emit_stack_load(mk_fresh(tmp), src_slot_id));
                },
                ToSpill::Def(tmp, dst_slot_id) => {
                    // If it's a Def of X, edit the old stmt to Def `new_tmp`, and insert
                    // Stmt::StackStore(X_address, new_tmp) after.
                    after_buf.extend(slot_alloc.emit_stack_store(dst_slot_id, mk_fresh(tmp)));
                },
            }
        }
        spills_buf.clear();

        new_program.extend(before_buf.drain(..));
        new_program.push(stmt);
        new_program.extend(after_buf.drain(..));
    }

    (new_program, did_spill)
}
