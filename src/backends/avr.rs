use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
};

use derive_more::{Debug, Display, From};
use internment::Intern;

use crate::{
    canon, instr_sel, ir, names::{self, Lbl, Tmp}, regalloc::{Cc, CtrlTx}
};

use crate::instr_sel::Select;

#[rustfmt::skip]
#[derive(Debug, Display, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub enum Reg {
    #[display("$r0")] R0,
    #[display("$r1")] R1,
    #[display("$r2")] R2,
    #[display("$r3")] R3,
    #[display("$r4")] R4,
    #[display("$r5")] R5,
    #[display("$r6")] R6,
    #[display("$r7")] R7,
    #[display("$r8")] R8,
    #[display("$r9")] R9,

    #[display("$r10")] R10,
    #[display("$r11")] R11,
    #[display("$r12")] R12,
    #[display("$r13")] R13,
    #[display("$r14")] R14,
    #[display("$r15")] R15,
    #[display("$r16")] R16,
    #[display("$r17")] R17,
    #[display("$r18")] R18,
    #[display("$r19")] R19,

    #[display("$r20")] R20,
    #[display("$r21")] R21,
    #[display("$r22")] R22,
    #[display("$r23")] R23,
    #[display("$r24")] R24,
    #[display("$r25")] R25,
    #[display("$r26")] R26,
    #[display("$r27")] R27,
    #[display("$r28")] R28,
    #[display("$r29")] R29,

    #[display("$r30")] R30,
    #[display("$r31")] R31,
}

impl From<Reg> for instr_sel::Stg<Reg> {
    fn from(reg: Reg) -> Self {
        Stg::Reg(reg)
    }
}

impl Cc<Reg> for Reg {
    #[rustfmt::skip]
    const GPRS: &'static [Reg] = &[
        // R0,  R1,  R2,  R3,  R4,  R5,  R6,  R7,  R8,  R9,  R10, R11, R12, R13, R14, R15,
        R16, R17, R18, R19, R20, R21, R22, R23, R24, R25, R26, R27, R28, R29, R30, R31,
    ];

    #[rustfmt::skip]
    const GPR_TEMP_REGS: &'static [Reg] = &[
        R16, R17, R18, R19, R20, R21, R22, R23, R24, R25, R26, R27, R28, R29, R30, R31,
    ];

    #[rustfmt::skip]
    const GPR_SAVED_REGS: &'static [Reg] = &[];

    #[rustfmt::skip]
    const GPR_ARG_REGS: &'static [Reg] = &[];

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

    /// Load immediate
    #[debug("LDI {_0}, {_1}")]
    Ldi(Stg, Imm),

    #[debug("ADD {_0}, {_1}")]
    Add(Stg, Stg),

    #[debug("SUB {_0}, {_1}")]
    Sub(Stg, Stg),

    /// LoaD direct from data Space
    #[debug("LDS {_0}, {_1}")]
    Lds(Stg, Imm),

    /// STore direct to data Space
    #[debug("STS {_0}, {_1}")]
    Sts(Imm, Stg),

    /// Move
    #[debug("MOV {_0}, {_1}")]
    Mov(Stg, Stg),

    /// Increment
    #[debug("INC {_0}")]
    Inc(Stg),

    /// Decrement
    #[debug("DEC {_0}")]
    Dec(Stg),

    /// Clear register
    #[debug("CLR {_0}")]
    Clr(Stg),

    /// Negate
    #[debug("NEG {_0}")]
    Neg(Stg),

    /// Arithmetic Shift Right
    #[debug("ASR {_0}")]
    Asr(Stg),

    /// Jump
    #[debug("JMP {_0:?}")]
    Jmp(Lbl),

    /// BRanch if Not Equal (branch if Z = 0)
    #[debug("BRNE {_0:?}")]
    Brne(Lbl),
}

impl crate::regalloc::Instr for Instr {
    type Register = Reg;

    fn add_defs_uses(&self, defs: &mut impl Extend<Stg>, uses: &mut impl Extend<Stg>) {
        match self {
            Label(lbl) => {}
            Mov(dst, src) | Add(dst, src) | Sub(dst, src) => {
                defs.extend([*dst]);
                uses.extend([*src]);
            }

            Neg(dst) | Clr(dst) | Inc(dst) | Dec(dst) | Asr(dst) => {
                defs.extend([*dst]);
                uses.extend([*dst]);
            }

            Ldi(dst, imm) | Lds(dst, imm) => defs.extend([*dst]),
            Sts(imm, src) => uses.extend([*src]),
            Jmp(lbl) | Brne(lbl) => {}
        }
    }

    fn try_as_pure_move(&self) -> Option<(Stg, Stg)> {
        match self {
            Mov(dst, src) => Some((*dst, *src)),
            _ => None,
        }
    }

    fn replace_def_occurrances(&mut self, old: Tmp, new: Stg) {
        match self {
            Label(..) | Jmp(..) | Brne(..) | Sts(..) => {}

            Inc(dst) |
            Dec(dst) |
            Clr(dst) |
            Neg(dst) |
            Asr(dst) |
            Ldi(dst, _) |
            Mov(dst, _) |
            Add(dst, _) |
            Sub(dst, _) |
            Lds(dst, _) => {
                if *dst == Stg::Tmp(old) {
                    *dst = new;
                }
            }

        }
    }

    fn replace_use_occurrances(&mut self, old: Tmp, new: Stg) {
        match self {
            Label(..) | Jmp(..) | Brne(..)  => {}

            Sts(_, stg) |
            Ldi(stg, _) |
            Lds(stg, _) |
            Inc(stg) |
            Dec(stg) |
            Clr(stg) |
            Neg(stg) |
            Asr(stg) => {
                if let Stg::Tmp(tmp) = stg {
                    if *tmp == old {
                        *stg = new;
                    }
                }
            }

            Mov(dst, src) |
            Sub(dst, src) |
            Add(dst, src) => {
                        if let Stg::Tmp(tmp1) = dst {
                            if *tmp1 == old {
                                *dst = new;
                            }
                        }
                        if let Stg::Tmp(tmp2) = src {
                            if *tmp2 == old {
                                *src = new;
                            }
                        }
                    }
        }
    }

    fn get_label(&self) -> Option<Lbl> {
        if let Label(lbl) = self {
            Some(*lbl)
        } else {
            None
        }
    }

    fn ctrl_tx(&self) -> Option<CtrlTx> {
        match self {
            Jmp(lbl) => Some(CtrlTx::Jump(*lbl)),
            Brne(lbl) => Some(CtrlTx::Branch(*lbl)),

            Label(..) |
            Ldi(..) |
            Add(..) |
            Sub(..) |
            Lds(..) |
            Sts(..) |
            Mov(..) |
            Inc(..) |
            Dec(..) |
            Clr(..) |
            Neg(..) |
            Asr(..) => Some(CtrlTx::Advance)
        }
    }

    fn emit_store_to_stack(addr: i32, src: Tmp) -> impl Iterator<Item=Self> {
        [].into_iter() // TODO
    }

    fn emit_load_from_stack(dst: Tmp, addr: i32) -> impl Iterator<Item=Self> {
        [].into_iter() // TODO
    }

    fn emit_move(dst: Stg, src: Stg) -> impl Iterator<Item=Self> {
        std::iter::once(Mov(dst, src))
    }
}

use Instr::*;
use Reg::*;

pub struct AvrInstrSel<'a> {
    out: &'a mut Vec<Instr>,
    current_subr_params: BTreeMap<u8, Tmp>,
}

impl<'a> AvrInstrSel<'a> {
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
                todo!();//self.emit(AddI(dst, x_dst, imm));
            }),
            ir::Binop::Sub => with_dst(dst, "subi_dst", |dst| {
                todo!();//self.emit(SubI(dst, x_dst, imm));
            }),
            ir::Binop::And => todo!(),
            ir::Binop::Or => todo!(),
            ir::Binop::Shr => with_dst(dst, "shr_dst", |dst| {
                let shamt = Stg::Tmp(Tmp::fresh("shamt"));
                todo!();//self.emit(Li(shamt, imm));
                todo!();//self.emit(Shr(dst, x_dst, shamt));
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
                todo!()//self.emit(Add(dst, x_dst, y_dst));
            }),
            ir::Binop::Sub => with_dst(dst, "sub_dst", |dst| {
                todo!()//self.emit(Sub(dst, x_dst, y_dst));
            }),
            ir::Binop::And => todo!(),
            ir::Binop::Or => todo!(),
            ir::Binop::Shr => todo!(),
            ir::Binop::Xor => todo!(),
        }
    }
}

impl<'a> Select for AvrInstrSel<'a> {
    type Register = Reg;

    type Instruction = Instr;

    fn stmt_to_asm(&mut self, stmt: crate::canon::Stmt) {
        use crate::canon::{self, Binop::*, LVal::*, RVal::*, Stmt};
        match stmt {
            // M[base + n] = src
            Stmt::Move(Mem(Binop(Add, LVal(Tmp(base)), Imm(canon::Imm::Int(offset)))), rhs)
            | Stmt::Move(Mem(Binop(Add, Imm(canon::Imm::Int(offset)), LVal(Tmp(base)))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                todo!();//self.emit(Sw(base.into(), self::Imm::Int(offset as u16), rhs_dst));
            }
            Stmt::Move(Mem(Binop(Add, LVal(Tmp(base)), Imm(canon::Imm::Nat(offset)))), rhs)
            | Stmt::Move(Mem(Binop(Add, Imm(canon::Imm::Nat(offset)), LVal(Tmp(base)))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                todo!();//self.emit(Sw(base.into(), self::Imm::Int(offset as u16), rhs_dst));
            }
            // M[base] = src
            Stmt::Move(Mem(LVal(Tmp(base))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                todo!();//self.emit(Sw(base.into(), self::Imm::Int(0), rhs_dst));
            }
            Stmt::Move(Mem(lhs), rhs) => {
                let rhs_tmp = self.expr_to_asm(rhs, None);
                let lhs_tmp = self.expr_to_asm(*lhs, None);
                todo!();//self.emit(Sw(lhs_tmp, self::Imm::Int(0), rhs_tmp));
            }

            // tmp1 = tmp2
            Stmt::Move(Tmp(lhs), LVal(Tmp(rhs))) => {
                todo!();//self.emit(Mv(lhs.into(), rhs.into()));
            }

            Stmt::Move(Tmp(lhs), Imm(canon::Imm::Int(i))) => {
                todo!();//self.emit(Li(lhs.into(), self::Imm::Int(i as u16)));
            }

            Stmt::Move(Tmp(lhs), rhs) => {
                let _ = self.expr_to_asm(rhs, Stg::Tmp(lhs));
            }

            Stmt::Call(opt_dst, func, args) => {
                //for (arg, reg) in args.into_iter().zip(Self::GPR_ARG_REGS) {
                //    self.expr_to_asm(arg, Stg::Reg(*reg));
                //}
                //self.emit(Jal(Ra.into(), func));
                //if let Some(dst) = opt_dst {
                //    // Store the return value in a temporary if requested.
                //    self.emit(Mv(dst.into(), Rv.into()));
                //}
                todo!()
            }

            Stmt::Switch(expr, lbls) => todo!(),

            Stmt::Discard(rval) => {
                todo!();//let _ = self.expr_to_asm(rval, Stg::Reg(Zero));
            }
            Stmt::Jmp(lbl) => todo!(),//self.emit(Instr::J(lbl)),

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
                    ir::Relop::Eq => todo!(),//self.emit(Teq(bool_tmp.into(), e1_tmp.into(), e2_tmp.into())),
                    ir::Relop::LtU => {
                        todo!();//self.emit(Tltu(bool_tmp.into(), e1_tmp.into(), e2_tmp.into()))
                    }
                    ir::Relop::Lt => todo!(),//self.emit(Tlt(bool_tmp.into(), e1_tmp.into(), e2_tmp.into())),
                    _ => todo!("impl relop: {op:?}"),
                }
                todo!();//self.emit(Bt(bool_tmp.into(), if_true.into()));
            }

            Stmt::Lbl(lbl) => self.emit(Label(lbl)),
            Stmt::Nop => todo!(),//self.emit(Nop),
            Stmt::Ret(Some(rval)) => {
                todo!();//self.expr_to_asm(rval, Stg::Reg(Rv));
                todo!();//self.emit(Jr(Stg::from_reg(Ra)));
            }
            Stmt::Ret(None) => todo!(),//self.emit(Jr(Stg::from_reg(Ra))),
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
                    todo!();//self.emit(Li(dst, self::Imm::Int(x as u16)));
                    dst
                }
                Nat(x) => {
                    let dst = mk_dst("li_nat");
                    todo!();//self.emit(Li(dst, self::Imm::Int(x as u16)));
                    dst
                }
                Int(x) => {
                    let dst = mk_dst("li_int");
                    todo!();//self.emit(Li(dst, self::Imm::Int(x as u16)));
                    dst
                }
                Lbl(lbl) => {
                    let dst = mk_dst("li_lbl");
                    todo!();//self.emit(Li(dst, self::Imm::Lbl(lbl.render().into())));
                    dst
                }
            },
            LVal(lval) => match lval {
                Tmp(tmp) => tmp.into(),
                Mem(rval) => {
                    let addr = self.expr_to_asm(*rval, None);
                    let dst = mk_dst("load");
                    todo!();//self.emit(Lw(dst, addr, self::Imm::Int(0)));
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
