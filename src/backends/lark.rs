use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
};

use derive_more::{Debug, Display, From};
use internment::Intern;

use crate::{
    canon, instr_sel, ir, names::{self, Lbl, Tmp}, regalloc::{Cc, CtrlTx}
};

use crate::instr_sel::InstrSel;

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

impl From<Reg> for instr_sel::Stg<Reg> {
    fn from(reg: Reg) -> Self {
        Stg::Reg(reg)
    }
}

impl Cc<Reg> for Reg {
    #[rustfmt::skip]
    const GPRS: &'static [Reg] = &[
        Rv, Ra,
        A0, A1, A2,
        S0, S1, S2,
        T0, T1, T2,
    ];

    #[rustfmt::skip]
    const GPR_TEMP_REGS: &'static [Reg] = &[
        T0, T1, T2,
        Rv,
        A0, A1, A2,
    ];

    #[rustfmt::skip]
    const GPR_SAVED_REGS: &'static [Reg] = &[
        S0, S1, S2, Ra,
    ];

    #[rustfmt::skip]
    const GPR_ARG_REGS: &'static [Reg] = &[
        A0, A1, A2
    ];

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

type Stg = instr_sel::Stg<Reg>;

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

    #[debug("shr {_0}, {_1}, {_2}")]
    Shr(Stg, Stg, Stg),

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

impl crate::regalloc::Instr for Instr {
    type Register = Reg;

    fn add_defs_uses(&self, defs: &mut impl Extend<Stg>, uses: &mut impl Extend<Stg>) {
        match self {
            Label(lbl) => {}
            Mv(lhs, rhs) => {
                defs.extend([*lhs]);
                uses.extend([*rhs]);
            }
            Add(dst, src1, src2) | Sub(dst, src1, src2) | Shr(dst, src1, src2) => {
                defs.extend([*dst]);
                uses.extend([*src1, *src2].into_iter());
            }
            AddI(dst, src1, imm) | SubI(dst, src1, imm) => {
                defs.extend([*dst]);
                uses.extend([*src1]);
            }
            Jr(stg) => uses.extend([*stg]),
            Nop => {}
            Li(stg, imm) => defs.extend([*stg]),
            Lw(dst, base, imm) => {
                defs.extend([*dst]);
                uses.extend([*base]);
            }
            Sw(base, imm, src) => {
                uses.extend([*base, *src].into_iter());
            }
            J(lbl) => {}
            Jal(stg, lbl) => defs.extend([*stg]),
            Tlt(dst, src1, src2)
            | Tge(dst, src1, src2)
            | Teq(dst, src1, src2)
            | Tne(dst, src1, src2)
            | Tltu(dst, src1, src2)
            | Tgeu(dst, src1, src2) => {
                defs.extend([*dst]);
                uses.extend([*src1, *src2].into_iter());
            }
            Bf(stg, lbl) | Bt(stg, lbl) => uses.extend([*stg]),
        }
    }

    fn try_as_pure_move(&self) -> Option<(Stg, Stg)> {
        match self {
            Self::Mv(lhs, stg) => Some((*lhs, *stg)),
            _ => None,
        }
    }

    fn replace_def_occurrances(&mut self, old: Tmp, new: Stg) {
        match self {
            Li(stg, _)
            | Lw(stg, _, _)
            | Mv(stg, _)
            | Jal(stg, _)
            | AddI(stg, _, _)
            | SubI(stg, _, _)
            | Add(stg, _, _)
            | Sub(stg, _, _)
            | Shr(stg, _, _)
            | Tlt(stg, _, _)
            | Tge(stg, _, _)
            | Teq(stg, _, _)
            | Tne(stg, _, _)
            | Tltu(stg, _, _)
            | Tgeu(stg, _, _) => {
                if let Stg::Tmp(tmp) = stg {
                    if *tmp == old {
                        *stg = new;
                    }
                }
            }

            Label(..) | Jr(..) | Nop | Sw(..) | J(..) | Bf(..) | Bt(..) => {}
        }
    }

    fn replace_use_occurrances(&mut self, old: Tmp, new: Stg) {
        match self {
            Mv(_, stg)
            | Jr(stg)
            | AddI(_, stg, _)
            | SubI(_, stg, _)
            | Lw(_, stg, _)
            | Bf(stg, _)
            | Bt(stg, _) => {
                if let Stg::Tmp(tmp) = stg {
                    if *tmp == old {
                        *stg = new;
                    }
                }
            }

            Add(_, stg1, stg2)
            | Sub(_, stg1, stg2)
            | Shr(_, stg1, stg2)
            | Tlt(_, stg1, stg2)
            | Tge(_, stg1, stg2)
            | Teq(_, stg1, stg2)
            | Tne(_, stg1, stg2)
            | Tltu(_, stg1, stg2)
            | Tgeu(_, stg1, stg2)
            | Sw(stg1, _, stg2) => {
                if let Stg::Tmp(tmp1) = stg1 {
                    if *tmp1 == old {
                        *stg1 = new;
                    }
                }
                if let Stg::Tmp(tmp2) = stg2 {
                    if *tmp2 == old {
                        *stg2 = new;
                    }
                }
            }

            Jal(..) | J(..) | Li(..) | Nop | Label(..) => {}
        }
    }

    fn get_label(&self) -> Option<Lbl> {
        if let Self::Label(lbl) = self {
            Some(*lbl)
        } else {
            None
        }
    }

    fn ctrl_tx(&self) -> Option<CtrlTx> {
        match self {
            // TODO: This assumes JR is used for subroutine jumps only. Does not apply if used as
            // Switch jump.
            Jr(Stg::Reg(Ra)) => None,
            Jr(_) => todo!(),

            J(lbl) => Some(CtrlTx::Jump(*lbl)),

            // Subr call jumps out, then comes back like nothing happened.
            Jal(_, lbl) => Some(CtrlTx::Advance),

            Nop | Li(..) | Lw(..) | Sw(..) | Label(..) | Mv(..) | Add(..) | AddI(..) | Sub(..)
            | SubI(..) | Shr(..) | Tlt(..) | Tge(..) | Teq(..) | Tne(..) | Tltu(..) | Tgeu(..) => {
                Some(CtrlTx::Advance)
            }

            Bf(stg, lbl) | Bt(stg, lbl) => Some(CtrlTx::Branch(*lbl)),
        }
    }

    fn mk_store_to_stack(addr: i32, src: Tmp) -> Self {
        Sw(Sp.into(), Imm::Int(addr.cast_unsigned() as u16), src.into())
    }

    fn mk_load_from_stack(dst: Tmp, addr: i32) -> Self {
        Lw(dst.into(), Sp.into(), Imm::Int(addr.cast_unsigned() as u16))
    }

    fn mk_move(dst: Stg, src: Stg) -> Self {
        Mv(dst, src)
    }
}

use Instr::*;
use Reg::*;

pub struct LarkInstrSel<'a> {
    out: &'a mut Vec<Instr>,
    current_subr_params: BTreeMap<u8, Tmp>,
}

impl<'a> LarkInstrSel<'a> {
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
            ir::Binop::Shr => with_dst(dst, "shr_dst", |dst| {
                let shamt = Stg::Tmp(Tmp::fresh("shamt"));
                self.emit(Li(shamt, imm));
                self.emit(Shr(dst, x_dst, shamt));
            }),
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
}

impl<'a> InstrSel for LarkInstrSel<'a> {
    type Register = Reg;

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
                self.emit(Jal(Ra.into(), func));
                if let Some(dst) = opt_dst {
                    // Store the return value in a temporary if requested.
                    self.emit(Mv(dst.into(), Rv.into()));
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
                    ir::Relop::Lt => self.emit(Tlt(bool_tmp.into(), e1_tmp.into(), e2_tmp.into())),
                    _ => todo!("impl relop: {op:?}"),
                }
                self.emit(Bt(bool_tmp.into(), if_true.into()));
            }

            Stmt::Lbl(lbl) => self.emit(Label(lbl)),
            Stmt::Nop => self.emit(Nop),
            Stmt::Ret(Some(rval)) => {
                self.expr_to_asm(rval, Stg::Reg(Rv));
                self.emit(Jr(Stg::from_reg(Ra)));
            }
            Stmt::Ret(None) => self.emit(Jr(Stg::from_reg(Ra))),
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

            BitCast(_ty, rval) => self.expr_to_asm(*rval, mk_dst("bitcast")), // Nothing needs to be done here?
        }
    }

    fn render(&self) -> &[Instr] {
        &self.out[..]
    }
}
