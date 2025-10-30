use std::collections::{BTreeSet, HashMap};

use alyn_common::names::Tmp;

use crate::{
    Instruction, Register,
    common::{Asn, SlotId, Stg, Stmt},
    intfs::Intfs,
};

pub trait SlotAllocator {
    fn get_or_alloc_slot(&mut self, slot_id: SlotId) -> i32;

    fn emit_stack_load<I: InstrWrite>(
        &mut self,
        dst: Stg<I::Reg>,
        src_slot_id: SlotId,
    ) -> impl Iterator<Item = Stmt<I>> {
        let src_slot_idx = self.get_or_alloc_slot(src_slot_id);
        I::emit_stack_load(dst, src_slot_idx)
    }

    fn emit_stack_store<I: InstrWrite>(
        &mut self,
        dst_slot_id: SlotId,
        src: Stg<I::Reg>,
    ) -> impl Iterator<Item = Stmt<I>> {
        let dst_slot_idx = self.get_or_alloc_slot(dst_slot_id);
        I::emit_stack_store(dst_slot_idx, src)
    }
}

pub trait InstrWrite: Instruction {
    fn emit_move(dst: Stg<Self::Reg>, src: Stg<Self::Reg>) -> impl Iterator<Item = Stmt<Self>>;

    fn emit_stack_load(dst: Stg<Self::Reg>, src_slot_idx: i32) -> impl Iterator<Item = Stmt<Self>>;

    fn emit_stack_store(dst_slot_idx: i32, src: Stg<Self::Reg>)
    -> impl Iterator<Item = Stmt<Self>>;
}

fn iter_slots<R>() -> impl Iterator<Item = Asn<R>> {
    let mut i = 0;
    std::iter::from_fn(move || {
        let current = i;
        i += 1;
        Some(Asn::Slot(SlotId(current)))
    })
}

fn iter_choices<R: Register>() -> impl Iterator<Item = Asn<R>> {
    R::GPRS
        .into_iter()
        .copied()
        .map(Asn::Reg)
        .chain(iter_slots())
}

fn select_assignment<R: Register>(in_use: &BTreeSet<Asn<R>>) -> Asn<R> {
    iter_choices()
        .find(|choice| !in_use.contains(choice))
        .expect("Should always find an available stack slot")
}

pub fn color_graph_greedily<R: Register>(
    intfs: &Intfs<R>,
    elimination_ordering: Vec<Tmp>,
) -> HashMap<Tmp, Asn<R>> {
    let mut assignments = HashMap::new();
    let mut in_use: BTreeSet<Asn<R>> = BTreeSet::new();

    for node in elimination_ordering {
        in_use.clear();
        for nbr in intfs.neighbors(node.into()) {
            match nbr {
                Stg::Reg(reg) => {
                    in_use.insert(Asn::Reg(reg));
                }
                Stg::Tmp(nbr) => {
                    if let Some(&a) = assignments.get(&nbr) {
                        in_use.insert(a);
                    }
                }
            }
        }

        assignments.insert(node, select_assignment(&in_use));
    }

    assignments
}
