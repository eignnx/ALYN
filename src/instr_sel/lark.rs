use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
};

use derive_more::{Debug, Display, From};
use internment::Intern;

use crate::{
    canon, ir,
    names::{self, Lbl, Tmp},
};

use super::InstrSel;

#[rustfmt::skip]
#[derive(Debug, Display, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub enum Reg {
    #[display("$zero")] Zero,
    #[display("$rv")] Rv, #[display("$ra")] Ra,
    #[display("$a0")] A0, #[display("$a1")] A1, #[display("$a2")] A2,
    #[display("$s0")] S0, #[display("$s1")] S1, #[display("$s2")] S2,
    #[display("$t0")] T0, #[display("$t1")] T1, #[display("$t2")] T2,
    #[display("$k0")] K0, #[display("$k1")] K1,
    #[display("$gp")] Gp, #[display("$sp")] Sp
}

#[derive(Debug, Clone, Display)]
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

#[derive(Debug, From, Clone, Copy, Display)]
pub enum Stg {
    #[display("{_0:?}")]
    #[from]
    Tmp(Tmp),
    #[display("{_0}")]
    #[from]
    Reg(Reg),
}

#[derive(Debug, Clone)]
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
    #[debug("jal {_0}, {_1:?}")]
    Jal(Stg, Lbl),

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
    #[rustfmt::skip]
    const GPR_ARG_REGS: &'static [Reg] = &[
        A0, A1, A2
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

    fn imm_binop_to_asm(
        &mut self,
        op: canon::Binop,
        x: canon::RVal,
        imm: Imm,
        dst: impl Into<Option<Stg>>,
    ) -> Stg {
        let x_dst = self.expr_to_asm(x, None);
        let dst = dst.into();
        fn with_dst(dst: Option<Stg>, base_name: &'static str, code: impl FnOnce(Stg)) -> Stg {
            let dst = dst.unwrap_or_else(|| names::Tmp::fresh(base_name).into());
            code(dst);
            dst
        };
        match op {
            ir::Binop::Add => with_dst(dst, "addi_dst", |dst| {
                self.emit(AddI(dst, x_dst, imm));
            }),
            ir::Binop::Sub => with_dst(dst, "subi_dst", |dst| {
                self.emit(SubI(dst, x_dst, imm));
            }),
            ir::Binop::And => todo!(),
            ir::Binop::Or => todo!(),
            ir::Binop::Shr => todo!(),
            ir::Binop::Xor => todo!(),
        }
    }

    fn binop_to_asm(
        &mut self,
        op: canon::Binop,
        x: canon::RVal,
        y: canon::RVal,
        dst: impl Into<Option<Stg>>,
    ) -> Stg {
        let x_dst = self.expr_to_asm(x, None);
        let y_dst = self.expr_to_asm(y, None);
        let dst = dst.into();
        fn with_dst(dst: Option<Stg>, base_name: &'static str, code: impl FnOnce(Stg)) -> Stg {
            let dst = dst.unwrap_or_else(|| names::Tmp::fresh(base_name).into());
            code(dst);
            dst
        };
        match op {
            ir::Binop::Add => with_dst(dst, "add_dst", |dst| {
                self.emit(Add(dst, x_dst, y_dst));
            }),
            ir::Binop::Sub => with_dst(dst, "sub_dst", |dst| {
                self.emit(Sub(dst, x_dst, y_dst));
            }),
            ir::Binop::And => todo!(),
            ir::Binop::Or => todo!(),
            ir::Binop::Shr => todo!(),
            ir::Binop::Xor => todo!(),
        }
    }

    pub fn render(&self) -> &[Instr] {
        &self.out[..]
    }
}

impl<'a> InstrSel for LarkBackend<'a> {
    type Temporary = Stg;

    type Instruction = Instr;

    fn stmt_to_asm(&mut self, stmt: crate::canon::Stmt) {
        use crate::canon::{self, Binop::*, LVal::*, RVal::*, Stmt};
        match stmt {
            // M[base + n] = src
            Stmt::Move(Mem(Binop(Add, LVal(Tmp(base)), Imm(canon::Imm::Int(offset)))), rhs)
            | Stmt::Move(Mem(Binop(Add, Imm(canon::Imm::Int(offset)), LVal(Tmp(base)))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                self.emit(Sw(base.into(), self::Imm::Int(offset as u16), rhs_dst));
            }
            Stmt::Move(Mem(Binop(Add, LVal(Tmp(base)), Imm(canon::Imm::Nat(offset)))), rhs)
            | Stmt::Move(Mem(Binop(Add, Imm(canon::Imm::Nat(offset)), LVal(Tmp(base)))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                self.emit(Sw(base.into(), self::Imm::Int(offset as u16), rhs_dst));
            }
            // M[base] = src
            Stmt::Move(Mem(LVal(Tmp(base))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                self.emit(Sw(base.into(), self::Imm::Int(0), rhs_dst));
            }
            Stmt::Move(Mem(lhs), rhs) => {
                let rhs_tmp = self.expr_to_asm(rhs, None);
                let lhs_tmp = self.expr_to_asm(*lhs, None);
                self.emit(Sw(lhs_tmp, self::Imm::Int(0), rhs_tmp));
            }

            // tmp1 = tmp2
            Stmt::Move(Tmp(lhs), LVal(Tmp(rhs))) => {
                self.emit(Mv(lhs.into(), rhs.into()));
            }

            Stmt::Move(Tmp(lhs), Imm(canon::Imm::Int(i))) => {
                self.emit(Li(lhs.into(), self::Imm::Int(i as u16)));
            }

            Stmt::Move(Tmp(lhs), rhs) => {
                let _ = self.expr_to_asm(rhs, Stg::Tmp(lhs));
            }

            Stmt::Call(opt_dst, func, args) => {
                for (arg, reg) in args.into_iter().zip(Self::GPR_ARG_REGS) {
                    self.expr_to_asm(arg, Stg::Reg(*reg));
                }
                self.emit(Instr::Jal(Ra.into(), func));
                if let Some(dst) = opt_dst {
                    // Store the return value in a temporary if requested.
                    self.emit(Instr::Mv(dst.into(), Rv.into()));
                }
            }

            Stmt::Switch(expr, lbls) => todo!(),

            Stmt::Discard(rval) => {
                let _ = self.expr_to_asm(rval, Stg::Reg(Zero));
            }
            Stmt::Jmp(lbl) => self.emit(Instr::J(lbl)),

            Stmt::Br {
                e1,
                op,
                e2,
                if_true,
            } => {
                let e1_tmp = self.expr_to_asm(e1, None);
                let e2_tmp = self.expr_to_asm(e2, None);
                let bool_tmp = names::Tmp::fresh("bool");
                match op {
                    ir::Relop::Eq => self.emit(Teq(bool_tmp.into(), e1_tmp.into(), e2_tmp.into())),
                    ir::Relop::LtU => {
                        self.emit(Tltu(bool_tmp.into(), e1_tmp.into(), e2_tmp.into()))
                    }
                    _ => todo!("impl relop: {op:?}"),
                }
                self.emit(Bt(bool_tmp.into(), if_true.into()));
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

    fn expr_to_asm(&mut self, rval: crate::canon::RVal, dst: impl Into<Option<Stg>>) -> Stg {
        use crate::canon::{Binop::*, Imm::*, LVal::*, RVal::*};

        // If no destination tmp has been provided, create one using the given base name.
        // If one has been provided, use that instead.
        let mk_dst = |base_name: &str| -> Stg {
            dst.into()
                .unwrap_or_else(|| names::Tmp::fresh(base_name).into())
        };

        match rval {
            Imm(imm) => match imm {
                Byte(x) => {
                    let dst = mk_dst("li_byte");
                    self.emit(Li(dst, self::Imm::Int(x as u16)));
                    dst
                }
                Nat(x) => {
                    let dst = mk_dst("li_nat");
                    self.emit(Li(dst, self::Imm::Int(x as u16)));
                    dst
                }
                Int(x) => {
                    let dst = mk_dst("li_int");
                    self.emit(Li(dst, self::Imm::Int(x as u16)));
                    dst
                }
                Lbl(lbl) => {
                    let dst = mk_dst("li_lbl");
                    self.emit(Li(dst, self::Imm::Lbl(lbl.render().into())));
                    dst
                }
            },
            LVal(lval) => match lval {
                Tmp(tmp) => tmp.into(),
                Mem(rval) => {
                    let addr = self.expr_to_asm(*rval, None);
                    let dst = mk_dst("load");
                    self.emit(Lw(dst, addr, self::Imm::Int(0)));
                    dst
                }
            },

            Binop(op, x, Imm(Nat(n))) => {
                self.imm_binop_to_asm(op, *x, self::Imm::Int(n as u16), mk_dst("imm_binop_res"))
            }
            Binop(op, x, y) => self.binop_to_asm(op, *x, *y, mk_dst("binop_res")),

            Unop(unop, rval) => todo!(),
        }
    }
}
