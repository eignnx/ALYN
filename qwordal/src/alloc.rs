use crate::{
    common::{SlotId, Stg, Stmt}, Instruction
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
    fn emit_move(
        dst: Stg<Self::Reg>,
        src: Stg<Self::Reg>,
    ) -> impl Iterator<Item = Stmt<Self>>;

    fn emit_stack_load(
        dst: Stg<Self::Reg>,
        src_slot_idx: i32,
    ) -> impl Iterator<Item = Stmt<Self>>;

    fn emit_stack_store(
        dst_slot_idx: i32,
        src: Stg<Self::Reg>,
    ) -> impl Iterator<Item = Stmt<Self>>;
}
