use crate::instr_sel::{self, Select};
use alyn_common::names::Lbl;

pub struct I8086InstrSel;

#[derive(Debug, Clone, Copy)]
#[rustfmt::skip]
pub enum Reg {
    Ax, Bx, Cx, Dx,
    Sp,
    Si, Di,
}

pub type Stg = instr_sel::Stg<Reg>;

#[derive(Debug, Clone, Copy)]
pub enum Imm {
    Int(i16),
    Lbl(Lbl),
}

/// "Addressing Mode"
#[derive(Clone)]
pub enum Am {
    Imm(Imm),
    Reg(Stg),
    /// Register indirect addressing
    RegInd(Stg),
    /// Direct addressing mode
    DirAddr(Imm),
    /// Based addressing
    Base(Stg, Imm),
    /// Based, indexed addressing
    BasedIndexed(Stg, Stg),
}

#[derive(Clone)]
pub enum Instr {
    Mov(Am, Am),
    Cmp(Am, Am),
}

impl Select for I8086InstrSel {
    type Register = Reg;

    type Instruction = Instr;

    fn stmt_to_asm(&mut self, stmt: crate::canon::Stmt) {
        todo!()
    }

    fn expr_to_asm(
        &mut self,
        rval: crate::canon::RVal,
        opt_dst: impl Into<Option<instr_sel::Stg<Self::Register>>>,
    ) -> instr_sel::Stg<Self::Register> {
        todo!()
    }

    fn render(&mut self) -> impl Iterator<Item = Instr> {
        // self.out.drain(..)
        todo!();
        [].into_iter()
    }
}
