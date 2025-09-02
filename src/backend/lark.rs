use std::{collections::{BTreeSet, BTreeMap}, mem};

use internment::Intern;
use derive_more::{From, Debug, Display};

use crate::{ir, names::Tmp};

use super::{
    Backend,
};

pub struct LarkBackend {
    regs_in_use: BTreeSet<Reg>,
    locals: BTreeMap<Tmp, RegOrSlot>,
    next_stack_local_slot: usize, // Index in bytes
}

impl Default for LarkBackend {
    fn default() -> Self {
        Self {
            regs_in_use: Default::default(),
            locals: Default::default(),
            next_stack_local_slot: 0,
        }
    }
}

impl LarkBackend {
    fn load_into_reg(&mut self, out: &mut Vec<Instr>, reg_or_slot: RegOrSlot) -> Reg {
        match reg_or_slot {
            RegOrSlot::Reg(reg) => reg,
            RegOrSlot::Slot(idx) => {
                // Emit instructions to load [SP+idx] into a register:
                let Some(dest) = self.choose_available_reg(out) else {
                    panic!("Out of registers!");
                };
                out.push(Lw(dest, Zero, Imm::Int(idx.cast_unsigned())));
                dest
            },
        }
    }

    #[rustfmt::skip]
    const GPR_TEMP_REGS: &'static [Reg] = &[
        T0, T1, T2,
        A0, A1, A2,
    ];
    #[rustfmt::skip]
    const GPR_SAVED_REGS: &'static [Reg] = &[
        S0, S1, S2,
    ];

    fn choose_available_reg(&mut self, out: &mut Vec<Instr>) -> Option<Reg> {
        for reg in Self::GPR_TEMP_REGS {
            if !self.regs_in_use.contains(reg) {
                self.regs_in_use.insert(*reg);
                return Some(*reg);
            }
        }
        for reg in Self::GPR_SAVED_REGS {
            if !self.regs_in_use.contains(reg) {
                let slot_idx = self.alloc_stack_slot();
                out.push(Sw(Sp, Imm::Int(slot_idx), *reg));
                self.regs_in_use.insert(*reg);
                return Some(*reg);
            }
        }
        None
    }

    fn alloc_stack_slot(&mut self) -> u16 {
        let idx = self.next_stack_local_slot;
        self.next_stack_local_slot += 2;
        idx as u16
    }

    fn get_or_alloc_temp(&mut self, out: &mut Vec<Instr>, tmp: Tmp) -> RegOrSlot {
        use std::collections::btree_map::Entry;
        if let Some(temp) = self.locals.get(&tmp) {
            *temp
        } else {
            let temp = self.fresh_temp(out);
            self.locals.insert(tmp, temp);
            temp
        }
    }
}

#[rustfmt::skip]
#[derive(Debug, Display, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
#[display("${self:?}")]
pub enum Reg {
    Zero,
    Rv, Ra,
    A0, A1, A2,
    S0, S1, S2,
    T0, T1, T2,
    K0, K1,
    Gp, Sp
}

#[derive(Clone, Copy, From)]
pub enum RegOrSlot {
    Reg(Reg),
    Slot(i16),
}

#[derive(Debug, Display)]
#[display("{self:?}")]
pub enum Imm {
    #[debug(":{_0}")]
    Lbl(Intern<String>),
    #[debug("{_0}")]
    Int(u16),
}

impl TryFrom<&ir::RVal> for Imm {
    type Error = ();

    fn try_from(rval: &ir::RVal) -> Result<Self, Self::Error> {
        match rval {
            ir::RVal::Byte(x) => Ok(Imm::Int(*x as u16)),
            ir::RVal::Nat(x) => Ok(Imm::Int(*x as u16)),
            ir::RVal::Int(x) => Ok(Imm::Int((*x as i16).cast_unsigned())),
            ir::RVal::Lbl(lbl) => Ok(Imm::Lbl(lbl.render().into())),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum Instr {
    #[debug("mv {_0}, {_1}")]
    Mv(Reg, Reg),

    #[debug("add {_0}, {_1}, {_2}")]
    Add(Reg, Reg, Reg),
    #[debug("addi {_0}, {_1}, {_2}")]
    AddI(Reg, Reg, Imm),

    #[debug("sub {_0}, {_1}, {_2}")]
    Sub(Reg, Reg, Reg),
    #[debug("subi {_0}, {_1}, {_2}")]
    SubI(Reg, Reg, Imm),

    #[debug("jr {_0}")]
    Jr(Reg),
    #[debug("nop")]
    Nop,
    #[debug("li {_0}, {_1}")]
    Li(Reg, Imm),
    #[debug("lw {_0}, [{_1} + {_2}]")]
    Lw(Reg, Reg, Imm),
    #[debug("sw [{_0} + {_1}], {_2}]")]
    Sw(Reg, Imm, Reg),
}

use Reg::*;
use Instr::*;

impl Backend<Vec<Instr>> for LarkBackend {
    type Temporary = RegOrSlot;
    type Instruction = Instr;

    fn stmt_to_asm(&mut self, mut out: &mut Vec<Instr>, stmt: ir::Stmt) {
        use ir::Stmt;
        match stmt {
            Stmt::Move(lval, rval) => {
                match lval {
                    ir::LVal::Tmp(tmp) => {
                        let tmp = self.get_or_alloc_temp(out, tmp);
                        match tmp {
                            RegOrSlot::Reg(lhs_reg) => {
                                self.rval_to_asm(out, lhs_reg, rval);
                            }
                            RegOrSlot::Slot(_) => todo!(),
                        }
                    },
                    ir::LVal::Param(_) => todo!(),
                    ir::LVal::Mem(rval) => todo!(),
                    ir::LVal::Global(intern) => todo!(),
                }

                    // let x_dest = self.fresh_temp(out);
                    // self.rval_to_asm(out, x_dest, *x);
                    // let y_dest = self.fresh_temp(out);
                    // self.rval_to_asm(out, y_dest, *y);
                    // let x_dest_reg = self.load_into_reg(out, x_dest);
                    // let y_dest_reg = self.load_into_reg(out, y_dest);
                    // let dest_reg = self.load_into_reg(out, dest);
                    // out.extend([Add(dest_reg, x_dest_reg, y_dest_reg)]);
            }
            Stmt::RVal(rval) => todo!(),
            Stmt::Jmp(rval, lbls) => todo!(),
            Stmt::Br { op, e1, e2, if_true, if_false } => todo!(),
            Stmt::Seq(stmt, stmt1) => todo!(),
            Stmt::Lbl(lbl) => todo!(),
            Stmt::Nop => out.extend([Nop]),
            Stmt::Ret(Some(rval)) => {
                self.rval_to_asm(out, Rv, rval);
                out.extend([Jr(Ra)]);
            }
            Stmt::Ret(None) => out.extend([Jr(Ra)]),
        }
    }

    fn rval_to_asm(&mut self, mut out: &mut Vec<Instr>, dest: impl Into<RegOrSlot>, rval: ir::RVal) {
        use ir::{RVal, Binop};
        let dest = dest.into();
        match rval {
            RVal::Byte(x) =>  {
                let dest_reg = self.load_into_reg(out, dest);
                out.extend([Li(dest_reg, Imm::Int(x as u16))])
            },
            RVal::Nat(x) => {
                let dest_reg = self.load_into_reg(out, dest);
                out.extend([Li(dest_reg, Imm::Int(x as u16))])
            }
            RVal::Int(x) => {
                let dest_reg = self.load_into_reg(out, dest);
                out.extend([Li(dest_reg, Imm::Int(x as u16))])
            }
            RVal::Lbl(lbl) => {
                let dest_reg = self.load_into_reg(out, dest);
                out.extend([Li(dest_reg, Imm::Lbl(lbl.render().into()))])
            }
            RVal::LVal(lval) => {
                match lval {
                    ir::LVal::Tmp(tmp) => {
                        let dest_reg = self.load_into_reg(out, dest);
                        let reg_or_slot = self.get_or_alloc_temp(out, tmp);
                        let src_reg = self.load_into_reg(out, reg_or_slot);
                        out.push(Mv(dest_reg, src_reg));
                    }
                    ir::LVal::Param(_) => todo!(),
                    ir::LVal::Mem(rval) => todo!(),
                    ir::LVal::Global(intern) => todo!(),
                }
            }
            RVal::Binop(binop, x, y) => match binop {
                Binop::Add => {
                    if let Ok(imm) = x.as_ref().try_into() {
                        let y_dest = self.fresh_temp(out);
                        self.rval_to_asm(out, y_dest, *y);
                        let y_dest_reg = self.load_into_reg(out, y_dest);
                        let dest_reg = self.load_into_reg(out, dest);
                        out.extend([AddI(dest_reg, y_dest_reg, imm)]);
                    } else if let Ok(imm) = y.as_ref().try_into() {
                        let x_dest = self.fresh_temp(out);
                        self.rval_to_asm(out, x_dest, *x);
                        let x_dest_reg = self.load_into_reg(out, x_dest);
                        let dest_reg = self.load_into_reg(out, dest);
                        out.extend([AddI(dest_reg, x_dest_reg, imm)]);
                    } else {
                        let x_dest = self.fresh_temp(out);
                        self.rval_to_asm(out, x_dest, *x);
                        let y_dest = self.fresh_temp(out);
                        self.rval_to_asm(out, y_dest, *y);
                        let x_dest_reg = self.load_into_reg(out, x_dest);
                        let y_dest_reg = self.load_into_reg(out, y_dest);
                        let dest_reg = self.load_into_reg(out, dest);
                        out.extend([Add(dest_reg, x_dest_reg, y_dest_reg)]);
                    }
                }
                Binop::Sub => {
                    if let Ok(imm) = y.as_ref().try_into() {
                        let x_dest = self.fresh_temp(out);
                        self.rval_to_asm(out, x_dest, *x);
                        let x_dest_reg = self.load_into_reg(out, x_dest);
                        let dest_reg = self.load_into_reg(out, dest);
                        out.extend([SubI(dest_reg, x_dest_reg, imm)]);
                    } else {
                        let x_dest = self.fresh_temp(out);
                        self.rval_to_asm(out, x_dest, *x);
                        let y_dest = self.fresh_temp(out);
                        self.rval_to_asm(out, y_dest, *y);
                        let x_dest_reg = self.load_into_reg(out, x_dest);
                        let y_dest_reg = self.load_into_reg(out, y_dest);
                        let dest_reg = self.load_into_reg(out, dest);
                        out.extend([Sub(dest_reg, x_dest_reg, y_dest_reg)]);
                    }
                }
                Binop::And => todo!(),
                Binop::Or => todo!(),
                Binop::Shr => todo!(),
                Binop::Xor => todo!(),
            }
            RVal::Unop(unop, rval) => todo!(),
            RVal::BitCast(ty, rval) => todo!(),
            RVal::Call(rval, rvals) => todo!(),
            RVal::Seq(stmt, rval) => {
                self.stmt_to_asm(out, *stmt);
                self.rval_to_asm(out, dest, *rval);
            }
        }
    }

    fn fresh_temp(&mut self, out: &mut Vec<Instr>) -> Self::Temporary {
        if let Some(reg) = self.choose_available_reg(out) {
            reg.into()
        } else {
            self.alloc_stack_slot().cast_signed().into()
        }
    }
}
