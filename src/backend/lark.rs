use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
};

use derive_more::{Debug, Display, From};
use internment::Intern;

use crate::{ir, names::{self, Lbl, Tmp}};

use super::Backend;

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

#[derive(Debug, From, Clone, Copy)]
pub enum Stg {
    #[from]
    Tmp(Tmp),
    #[from]
    Reg(Reg),
}

impl std::fmt::Display for Stg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stg::Tmp(tmp) => write!(f, "{tmp:?}"),
            Stg::Reg(reg) => write!(f, "{reg}"),
        }
    }
}

#[derive(Debug)]
pub enum Instr {
    #[debug("{_0:?}:")]
    Label(Lbl),

    #[debug("mv {_0}, {_1}")]
    Mv(Stg, Stg),

    #[debug("add {_0}, {_1}, {_2}")]
    Add(Stg, Stg, Stg),
    #[debug("addi {_0}, {_1}, {_2}")]
    AddI(Stg, Stg, Imm),

    #[debug("sub {_0}, {_1}, {_2}")]
    Sub(Stg, Stg, Stg),
    #[debug("subi {_0}, {_1}, {_2}")]
    SubI(Stg, Stg, Imm),

    #[debug("jr {_0}")]
    Jr(Stg),
    #[debug("nop")]
    Nop,
    #[debug("li {_0}, {_1}")]
    Li(Stg, Imm),
    #[debug("lw {_0}, [{_1} + {_2}]")]
    Lw(Stg, Stg, Imm),
    #[debug("sw [{_0} + {_1}], {_2}]")]
    Sw(Stg, Imm, Stg),

    #[debug("j {_0:?}")]
    J(Lbl),

    #[debug("tlt {_0}, {_1}, {_2}")]
    Tlt(Stg, Stg, Stg),
    #[debug("tge {_0}, {_1}, {_2}")]
    Tge(Stg, Stg, Stg),
    #[debug("teq {_0}, {_1}, {_2}")]
    Teq(Stg, Stg, Stg),
    #[debug("tne {_0}, {_1}, {_2}")]
    Tne(Stg, Stg, Stg),
    #[debug("tltu {_0}, {_1}, {_2}")]
    Tltu(Stg, Stg, Stg),
    #[debug("tgeu {_0}, {_1}, {_2}")]
    Tgeu(Stg, Stg, Stg),

    #[debug("bf {_0}, {_1:?}")]
    Bf(Stg, Lbl),
    #[debug("bt {_0}, {_1:?}")]
    Bt(Stg, Lbl),
}

use Instr::*;
use Reg::*;

pub struct LarkBackend<'a> {
    out: &'a mut Vec<Instr>,
    current_subr_params: BTreeMap<u8, Tmp>,
}

impl<'a> LarkBackend<'a> {
    #[rustfmt::skip]
    const GPR_TEMP_REGS: &'static [Reg] = &[
        T0, T1, T2,
        A0, A1, A2,
    ];
    #[rustfmt::skip]
    const GPR_SAVED_REGS: &'static [Reg] = &[
        S0, S1, S2,
    ];

    pub fn new(out: &'a mut Vec<Instr>) -> Self {
        Self {
            out,
            current_subr_params: Default::default(),
        }
    }

    pub fn emit(&mut self, instr: Instr) {
        self.out.push(instr);
    }

    pub fn stmt_to_asm(&mut self, stmt: ir::Stmt) {
        use ir::{Stmt, RVal::*, LVal::*, Binop::*};
        match stmt {
            // M[base + n] = src
            Stmt::Move(Mem(Binop(Add, LVal(Tmp(base)), Int(offset))), rhs) |
            Stmt::Move(Mem(Binop(Add, Int(offset), LVal(Tmp(base)))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                self.emit(Sw(base.into(), Imm::Int(offset as u16), rhs_dst));
            }
            Stmt::Move(Mem(Binop(Add, LVal(Tmp(base)), Nat(offset))), rhs) |
            Stmt::Move(Mem(Binop(Add, Nat(offset), LVal(Tmp(base)))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                self.emit(Sw(base.into(), Imm::Int(offset as u16), rhs_dst));
            }
            // M[base] = src
            Stmt::Move(Mem(LVal(Tmp(base))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                self.emit(Sw(base.into(), Imm::Int(0), rhs_dst));
            }
            Stmt::Move(Mem(lhs), rhs) => {
                let rhs_tmp = self.expr_to_asm(rhs, None);
                let lhs_tmp = self.expr_to_asm(*lhs, None);
                self.emit(Sw(lhs_tmp, Imm::Int(0), rhs_tmp));
            }

            // tmp1 = tmp2
            Stmt::Move(Tmp(lhs), LVal(Tmp(rhs))) => {
                self.emit(Mv(lhs.into(), rhs.into()));
            }

            Stmt::Move(Tmp(lhs), Int(i)) => {
                self.emit(Li(lhs.into(), Imm::Int(i as u16)));
            }

            Stmt::Move(Tmp(lhs), rhs) => {
                let _ = self.expr_to_asm(rhs, Stg::Tmp(lhs));
            }

            Stmt::Move(Global(_), _) => todo!(),

            Stmt::RVal(rval) => {
                let _ = self.expr_to_asm(rval, Stg::Reg(Zero));
            }
            Stmt::Jmp(Byte(_) | Int(_) | Nat(_) | Lbl(_), [lbl]) => self.emit(Instr::J(lbl)),
            Stmt::Jmp(rval, lbls) => todo!(),

            Stmt::Seq(Stmt::Br {e1, op, e2, if_true, if_false}, Stmt::Seq(Stmt::Lbl(lbl), rest)) if lbl == if_true => {
                let e1_tmp = self.expr_to_asm(e1, None);
                let e2_tmp = self.expr_to_asm(e2, None);
                let bool_tmp = names::Tmp::fresh("bool");
                match op {
                    ir::Relop::Eq => self.emit(Teq(bool_tmp.into(), e1_tmp.into(), e2_tmp.into())),
                    ir::Relop::LtU => self.emit(Tltu(bool_tmp.into(), e1_tmp.into(), e2_tmp.into())),
                    _ => todo!("impl relop: {op:?}"),
                }
                self.emit(Bf(bool_tmp.into(), if_false.into()));
                self.emit(Label(if_true));
                self.stmt_to_asm(*rest);
            }
            Stmt::Br {e1, op, e2, if_true, if_false} => todo!(),

            Stmt::Seq(stmt1, stmt2) => {
                self.stmt_to_asm(*stmt1);
                self.stmt_to_asm(*stmt2);
            }

            Stmt::Lbl(lbl) => self.emit(Label(lbl)),
            Stmt::Nop => self.emit(Nop),
            Stmt::Ret(Some(rval)) => {
                self.expr_to_asm(rval, Stg::Reg(Rv));
                self.emit(Jr(Ra.into()));
            }
            Stmt::Ret(None) => self.emit(Jr(Ra.into())),
        }
    }

    fn expr_to_asm(&mut self, rval: ir::RVal, dst: impl Into<Option<Stg>>) -> Stg {
        use ir::{Binop::*, RVal};
        let mk_dst = |base_name: &str| -> Stg {
            dst.into().unwrap_or_else(|| names::Tmp::fresh(base_name).into())
        };

        match rval {
            RVal::Byte(x) => {
                let dst = mk_dst("li_byte");
                self.emit(Li(dst, Imm::Int(x as u16)));
                dst
            }
            RVal::Nat(x) => {
                let dst = mk_dst("li_nat");
                self.emit(Li(dst, Imm::Int(x as u16)));
                dst
            }
            RVal::Int(x) => {
                let dst = mk_dst("li_int");
                self.emit(Li(dst, Imm::Int(x as u16)));
                dst
            }
            RVal::Lbl(lbl) => {
                let dst = mk_dst("li_lbl");
                self.emit(Li(dst, Imm::Lbl(lbl.render().into())));
                dst
            }
            RVal::LVal(lval) => match lval {
                ir::LVal::Tmp(tmp) => tmp.into(),
                ir::LVal::Mem(rval) => {
                    let addr = self.expr_to_asm(*rval, None);
                    let dst = mk_dst("load");
                    self.emit(Lw(dst, addr, Imm::Int(0)));
                    dst
                }
                ir::LVal::Global(intern) => todo!(),
            },

            RVal::Binop(op, x, RVal::Nat(n)) => self.imm_binop_to_asm(op, *x, Imm::Int(n as u16), mk_dst("imm_binop_res")),
            RVal::Binop(op, x, y) => self.binop_to_asm(op, *x, *y, mk_dst("binop_res")),

            RVal::Unop(unop, rval) => todo!(),
            RVal::BitCast(ty, rval) => todo!(),
            RVal::Call(rval, rvals) => todo!(),
            RVal::Seq(stmt, rval) => {
                self.stmt_to_asm(*stmt);
                self.expr_to_asm(*rval, mk_dst("seq_res"))
            }
        }
    }

    fn imm_binop_to_asm(&mut self, op: ir::Binop, x: ir::RVal, imm: Imm, dst: Stg) -> Stg {
        let x_dst = self.expr_to_asm(x, None);
        match op {
            ir::Binop::Add => self.emit(AddI(dst, x_dst, imm)),
            ir::Binop::Sub => self.emit(SubI(dst, x_dst, imm)),
            ir::Binop::And => todo!(),
            ir::Binop::Or => todo!(),
            ir::Binop::Shr => todo!(),
            ir::Binop::Xor => todo!(),
        }
        dst
    }

    fn binop_to_asm(&mut self, op: ir::Binop, x: ir::RVal, y: ir::RVal, dst: impl Into<Option<Stg>>) -> Stg {
        let x_dst = self.expr_to_asm(x, None);
        let y_dst = self.expr_to_asm(y, None);
        let dst = names::Tmp::fresh("binop_res").into();
        match op {
            ir::Binop::Add => self.emit(Add(dst, x_dst, y_dst)),
            ir::Binop::Sub => self.emit(Sub(dst, x_dst, y_dst)),
            ir::Binop::And => todo!(),
            ir::Binop::Or => todo!(),
            ir::Binop::Shr => todo!(),
            ir::Binop::Xor => todo!(),
        }
        dst
    }
}
