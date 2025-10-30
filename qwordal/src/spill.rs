use std::collections::{BTreeSet, HashMap};

use alyn_common::names::Tmp;

use crate::{
    alloc::{InstrWrite, SlotAllocator}, common::{Asn, SlotId, Stg, Stmt}, DefsUses, Instruction, Register, StgSubst, ToSpill
};

pub fn rewrite_with_spills<I, It>(
    program: It,
    assignments: &HashMap<Tmp, Asn<I::Reg>>,
    slot_alloc: &mut impl SlotAllocator,
) -> (Vec<Stmt<I>>, bool)
where
    I: Instruction + DefsUses + InstrWrite + StgSubst,
    It: IntoIterator<Item = Stmt<I>>,
    It::IntoIter: ExactSizeIterator,
{
    let stmts = program.into_iter();
    let max_expected_spills: usize = <I::Reg as Register>::GPR_SAVED_REGS.len();
    let mut new_program = Vec::with_capacity(stmts.len() + 2 * max_expected_spills);
    let mut spills_buf = BTreeSet::new();

    let mut before_buf = Vec::new();
    let mut after_buf = Vec::new();

    let mut did_spill = false;

    for (id, mut stmt) in stmts.enumerate() {
        stmt.subst_tmp(assignments, &mut spills_buf);
        if stmt.is_trivial_move() {
            eprintln!("-- {id:02}: {stmt:?} ; trivial move elimination");
            continue;
        }
        for spill in spills_buf.iter() {
            did_spill = true;
            match *spill {
                ToSpill::Use(tmp, src_slot_id) => {
                    handle_use_spill(tmp, src_slot_id, &mut before_buf, slot_alloc);
                }
                ToSpill::Def(tmp, dst_slot_id) => {
                    handle_def_spill(tmp, dst_slot_id, &mut after_buf, slot_alloc);
                }
            }
        }
        spills_buf.clear();

        new_program.extend(before_buf.drain(..));
        new_program.push(stmt);
        new_program.extend(after_buf.drain(..));
    }

    (new_program, did_spill)
}

fn handle_use_spill<I>(
    tmp: Tmp,
    src_slot_id: SlotId,
    before_buf: &mut Vec<Stmt<I>>,
    slot_alloc: &mut impl SlotAllocator,
) where
    I: InstrWrite,
{
    // If it's a Use of X, insert Stmt::StackLoad(new_tmp, X_address) before
    // and edit the using instruction.
    let fresh_tmp = mk_fresh(tmp);
    before_buf.extend(slot_alloc.emit_stack_load(fresh_tmp, src_slot_id));
}

fn handle_def_spill<I>(
    tmp: Tmp,
    dst_slot_id: SlotId,
    after_buf: &mut Vec<Stmt<I>>,
    slot_alloc: &mut impl SlotAllocator,
) where
    I: InstrWrite,
{
    // If it's a Def of X, edit the old stmt to Def `new_tmp`, and insert
    // Stmt::StackStore(X_address, new_tmp) after.
    let fresh_tmp = mk_fresh(tmp);
    after_buf.extend(slot_alloc.emit_stack_store(dst_slot_id, fresh_tmp));
    //let map = HashMap::from([(tmp.into(), fresh_tmp)]);
    //stmt.substitute_tmp_for_reg(&map, &mut vec![]);
}

fn mk_fresh<R>(to_spill: Tmp) -> Stg<R> {
    Stg::Tmp(Tmp::fresh(&format!("spill<{to_spill:?}>")))
}
