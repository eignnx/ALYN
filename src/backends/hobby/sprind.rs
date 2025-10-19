use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
};

use derive_more::{Debug, Display, From};
use internment::Intern;

use crate::{
    canon,
    instr_sel::Select,
    ir,
    names::{self, Lbl, Tmp},
    regalloc::{Cc, CtrlTx},
};

#[rustfmt::skip]
#[derive(Display, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Reg {
    /// Stack pointer
    #[display("$sp")] SP,
    /// Temporary, argument 1
    #[display("$x")] X,
    /// Temporary, argument 2
    #[display("$y")] Y,
    /// Temporary, argument 3
    #[display("$z")] Z,
    /// Temporary, argument 4
    #[display("$w")] W,
    /// Temporary, return value
    #[display("$v")] V,
    /// Callee-saved
    #[display("$a")] A,
    /// Callee-saved
    #[display("$b")] B,
}

impl std::fmt::Debug for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl From<Reg> for crate::Stg<Reg> {
    fn from(reg: Reg) -> Self {
        Stg::Reg(reg)
    }
}

mod _scope_use {
    use super::{Cc, Reg::{self, *}};

    impl Cc for Reg {
        #[rustfmt::skip]
        const GPRS: &'static [Reg] = &[
            X, Y, Z, W, V,
            A, B,
        ];

        #[rustfmt::skip]
        const GPR_TEMP_REGS: &'static [Reg] = &[
            X, Y, Z, W, V,
        ];

        #[rustfmt::skip]
        const GPR_SAVED_REGS: &'static [Reg] = &[
            A, B,
        ];

        #[rustfmt::skip]
        const GPR_ARG_REGS: &'static [Reg] = &[
            X, Y, Z, W,
        ];
    }
}

#[derive(Debug, Clone, Display)]
#[display("{self:?}")]
pub enum Imm {
    #[debug("{_0}")]
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

type Stg = crate::Stg<Reg>;

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub enum Instr {
    #[debug("{_0}:")]
    Label(Lbl),

    #[debug("mov {_0}, {_1}")]
    MOV(Stg, Stg),

    #[debug("add {_0}, {_1}")]
    ADD(Stg, Stg),
    #[debug("addi {_0}, {_1}")]
    ADDI(Stg, Imm),

    #[debug("sub {_0}, {_1}")]
    SUB(Stg, Stg),
    #[debug("subi {_0}, {_1}")]
    SUBI(Stg, Imm),

    #[debug("or {_0}, {_1}")]
    OR(Stg, Stg),
    #[debug("ori {_0}, {_1}")]
    ORI(Stg, Imm),



    #[debug("lsr {_0}, {_1}")]
    LSR(Stg, Imm),

    #[debug("li {_0}, {_1}")]
    LI(Stg, Imm),
    #[debug("szi {_0}, {_1}")]
    SZI(Stg, Imm),

    #[debug("lw {_0}, [{_1} + {_2}]")]
    LW(Stg, Stg, Imm),
    #[debug("sw [{_0} + {_1}], {_2}")]
    SW(Stg, Imm, Stg),

    #[debug("call {_0}")]
    CALL(Lbl),
    #[debug("ret")]
    RET,


    #[debug("teq {_0}, {_1}")]
    TEQ(Stg, Stg),
    #[debug("tne {_0}, {_1}")]
    TNE(Stg, Stg),
    #[debug("tl {_0}, {_1}")]
    TL(Stg, Stg),
    #[debug("tge {_0}, {_1}")]
    TGE(Stg, Stg),
    #[debug("tb {_0}, {_1}")]
    TB(Stg, Stg),
    #[debug("tae {_0}, {_1}")]
    TAE(Stg, Stg),

    #[debug("b {_0}")]
    B(Lbl),
    #[debug("bf {_0}")]
    BF(Lbl),
    #[debug("bt {_0}")]
    BT(Lbl),
}


use Instr::*;

pub struct SprindInstrSel {
    out: Vec<Instr>,
    current_subr_params: BTreeMap<u8, Tmp>,
}

impl SprindInstrSel {
    pub fn new() -> Self {
        Self {
            out: Vec::new(),
            current_subr_params: Default::default(),
        }
    }

    pub fn emit(&mut self, instr: Instr) {
        self.out.push(instr);
    }

    fn with_dst(&mut self, src_dst: Stg, dst: Option<Stg>, mut code: impl FnOnce(&mut SprindInstrSel, Stg)) -> Stg {
        code(self, src_dst);
        if let Some(dst) = dst && dst != src_dst {
            self.emit(MOV(dst, src_dst));
            dst
        } else {
            src_dst
        }
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

        match op {
            ir::Binop::Add => self.with_dst(x_dst, dst, |slf, x_dst| {
                slf.emit(ADDI(x_dst, imm));
            }),
            ir::Binop::Sub => self.with_dst(x_dst, dst, |slf, x_dst| {
                slf.emit(SUBI(x_dst, imm));
            }),
            ir::Binop::Shr => self.with_dst(x_dst, dst, |slf, x_dst| {
                slf.emit(LSR(x_dst, imm));
            }),
            ir::Binop::And => todo!(),
            ir::Binop::Or => todo!(),
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
            ir::Binop::Add => self.with_dst(x_dst, dst, |slf, dst| {
                slf.emit(ADD(x_dst, y_dst));
            }),
            ir::Binop::Sub => self.with_dst(x_dst, dst, |slf, dst| {
                slf.emit(SUB(x_dst, y_dst));
            }),
            ir::Binop::And => todo!(),
            ir::Binop::Or => self.with_dst(x_dst, dst, |slf, dst| {
                slf.emit(OR(x_dst, y_dst));
            }),
            ir::Binop::Shr => todo!(),
            ir::Binop::Xor => todo!(),
        }
    }
}

impl Select for SprindInstrSel {
    type Register = Reg;

    type Instruction = Instr;

    fn stmt_to_asm(&mut self, stmt: crate::canon::Stmt) {
        use crate::canon::{self, Binop::*, LVal::*, RVal::*, Stmt};
        match stmt {
            // M[base + n] = src
            Stmt::Move(Mem(Binop(Add, LVal(Tmp(base)), Imm(canon::Imm::Int(offset)))), rhs)
            | Stmt::Move(Mem(Binop(Add, Imm(canon::Imm::Int(offset)), LVal(Tmp(base)))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                self.emit(SW(base.into(), self::Imm::Int(offset as u16), rhs_dst));
            }
            Stmt::Move(Mem(Binop(Add, LVal(Tmp(base)), Imm(canon::Imm::Nat(offset)))), rhs)
            | Stmt::Move(Mem(Binop(Add, Imm(canon::Imm::Nat(offset)), LVal(Tmp(base)))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                self.emit(SW(base.into(), self::Imm::Int(offset as u16), rhs_dst));
            }
            // M[base] = src
            Stmt::Move(Mem(LVal(Tmp(base))), rhs) => {
                let rhs_dst = self.expr_to_asm(rhs, None);
                self.emit(SW(base.into(), self::Imm::Int(0), rhs_dst));
            }
            Stmt::Move(Mem(lhs), rhs) => {
                let rhs_tmp = self.expr_to_asm(rhs, None);
                let lhs_tmp = self.expr_to_asm(*lhs, None);
                self.emit(SW(lhs_tmp, self::Imm::Int(0), rhs_tmp));
            }

            // tmp1 = tmp2
            Stmt::Move(Tmp(lhs), LVal(Tmp(rhs))) => {
                self.emit(MOV(lhs.into(), rhs.into()));
            }

            //Stmt::Move(Tmp(lhs), Imm(canon::Imm::Int(i))) => {
            //    self.emit(LI(lhs.into(), self::Imm::Int(i as u16)));
            //}

            Stmt::Move(Tmp(lhs), rhs) => {
                let _ = self.expr_to_asm(rhs, Stg::Tmp(lhs));
            }

            Stmt::Call(opt_dst, func, args) => {
                for (arg, &reg) in args.into_iter().zip(Reg::GPR_ARG_REGS) {
                    let tmp = self.expr_to_asm(arg, None);
                    self.emit(MOV(reg.into(), tmp));
                }
                self.emit(CALL(func));
                if let Some(dst) = opt_dst {
                    // Store the return value in a temporary if requested.
                    self.emit(MOV(dst.into(), Reg::V.into()));
                }
            }

            Stmt::Switch(expr, lbls) => todo!(),

            Stmt::Discard(rval) => {
                let tmp = Stg::Tmp(names::Tmp::fresh("discard"));
                let _ = self.expr_to_asm(rval, tmp);
            }
            Stmt::Jmp(lbl) => self.emit(Instr::B(lbl)),

            Stmt::Br {
                e1,
                op,
                e2,
                if_true,
            } => {
                let e1_tmp = self.expr_to_asm(e1, None);
                let e2_tmp = self.expr_to_asm(e2, None);
                match op {
                    ir::Relop::Eq => self.emit(TEQ(e1_tmp.into(), e2_tmp.into())),
                    ir::Relop::LtU => {
                        self.emit(TB(e1_tmp.into(), e2_tmp.into()))
                    }
                    ir::Relop::LteU => {
                        self.emit(TGE(e2_tmp.into(), e1_tmp.into()))
                    }
                    ir::Relop::Lt => self.emit(TL(e1_tmp.into(), e2_tmp.into())),
                    _ => todo!("impl relop: {op:?}"),
                }
                self.emit(BT(if_true.into()));
            }

            Stmt::Lbl(lbl) => self.emit(Label(lbl)),
            Stmt::Nop => self.emit(ORI(Reg::SP.into(), self::Imm::Int(0))),
            Stmt::Ret(Some(rval)) => {
                self.expr_to_asm(rval, Stg::Reg(Reg::V));
                self.emit(RET);
            }
            Stmt::Ret(None) => self.emit(RET),
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
                    self.emit(LI(dst, self::Imm::Int(x as u16)));
                    dst
                }
                Nat(x) => {
                    let dst = mk_dst("li_nat");
                    self.emit(LI(dst, self::Imm::Int(x as u16)));
                    dst
                }
                Int(x) => {
                    let dst = mk_dst("li_int");
                    self.emit(LI(dst, self::Imm::Int((x as i16).cast_unsigned())));
                    dst
                }
                Lbl(lbl) => {
                    let dst = mk_dst("li_lbl");
                    self.emit(LI(dst, self::Imm::Lbl(lbl.render().into())));
                    dst
                }
            },
            LVal(lval) => match lval {
                Tmp(tmp) => tmp.into(),
                Mem(rval) => {
                    let addr = self.expr_to_asm(*rval, None);
                    let dst = mk_dst("load");
                    self.emit(LW(dst, addr, self::Imm::Int(0)));
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

    fn render(&mut self) -> impl Iterator<Item = Instr> {
        self.out.drain(..)
    }
}
impl crate::regalloc::Instr for Instr {
    type Register = Reg;

    fn add_defs_uses(&self, defs: &mut impl Extend<Stg>, uses: &mut impl Extend<Stg>) {
        match self {
            Label(lbl) => {}
            MOV(lhs, rhs) => {
                defs.extend([*lhs]);
                uses.extend([*rhs]);
            }
            ADD(dst, src) | SUB(dst, src) | OR(dst, src) => {
                defs.extend([*dst]);
                uses.extend([*dst, *src].into_iter());
            }
            ADDI(dst, imm) | SUBI(dst, imm) => {
                defs.extend([*dst]);
                uses.extend([*dst]);
            }
            LSR(dst, shamt) => {
                defs.extend([*dst]);
                uses.extend([*dst]);
            }
            RET => {
                // Assuming this is a `ret` instruction only:
                uses.extend(Reg::GPR_SAVED_REGS.iter().copied().map(Stg::Reg));
            }
            LI(stg, imm) => defs.extend([*stg]),
            ORI(stg, imm) | SZI(stg, imm) => {
                defs.extend([*stg]);
                uses.extend([*stg]);
            }
            LW(dst, base, imm) => {
                defs.extend([*dst]);
                uses.extend([*base]);
            }
            SW(base, imm, src) => {
                uses.extend([*base, *src].into_iter());
            }
            CALL(lbl) => {
                // Assuming this is is used as a `call` instruction only:
                defs.extend(Reg::GPR_TEMP_REGS.iter().copied().map(Stg::Reg));
            }
            TL(src1, src2)
            | TGE(src1, src2)
            | TEQ(src1, src2)
            | TNE(src1, src2)
            | TB(src1, src2)
            | TAE(src1, src2) => {
                uses.extend([*src1, *src2].into_iter());
            }
            Instr::B(_) | BF(_) | BT(_) => {}
        }
    }

    fn try_as_pure_move(&self) -> Option<(Stg, Stg)> {
        match self {
            Self::MOV(lhs, stg) => Some((*lhs, *stg)),
            _ => None,
        }
    }

    fn replace_def_occurrances(&mut self, old: Tmp, new: Stg) {
        match self {
            LI(stg, _)
            | LW(stg, _, _)
            | MOV(stg, _)
            | ADD(stg, _)
            | SUB(stg, _)
            | OR(stg, _)
            | ADDI(stg, _)
            | SUBI(stg, _)
            | ORI(stg, _)
            | LSR(stg, _)
            | SZI(stg, _)
            => {
                if let Stg::Tmp(tmp) = stg {
                    if *tmp == old {
                        *stg = new;
                    }
                }
            }
            | TL(_, _)
            | TGE(_, _)
            | TEQ(_, _)
            | TNE(_, _)
            | TB(_, _)
            | TAE(_, _) 
            | CALL(_)
            | Label(..) | RET | SW(..) | Instr::B(..) | BF(..) | BT(..) => {}
        }
    }

    fn replace_use_occurrances(&mut self, old: Tmp, new: Stg) {
        match self {
            MOV(_, stg)
            | ADDI(stg, _)
            | SUBI(stg, _)
            | ORI(stg, _)
            | LSR(stg, _)
            | LI(stg, _)
            | SZI(stg, _)
            | LW(_, stg, _) => {
                if let Stg::Tmp(tmp) = stg {
                    if *tmp == old {
                        *stg = new;
                    }
                }
            }
            ADD(stg1, stg2)
            | SUB(stg1, stg2)
            | OR(stg1, stg2)
            | TL(stg1, stg2)
            | TGE(stg1, stg2)
            | TEQ(stg1, stg2)
            | TNE(stg1, stg2)
            | TB(stg1, stg2)
            | TAE(stg1, stg2)
            | SW(stg1, _, stg2) => {
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
            Label(..) | RET | Instr::B(..) | BF(..) | BT(..) | CALL(..) => {}
        }
    }

    fn try_as_lbl(&self) -> Option<Lbl> {
        if let Self::Label(lbl) = self {
            Some(*lbl)
        } else {
            None
        }
    }

    fn ctrl_tx(&self) -> Option<CtrlTx> {
        match self {
            RET => None,

            Instr::B(lbl) => Some(CtrlTx::Jump(*lbl)),

            // Subr call jumps out, then comes back like nothing happened.
            CALL(lbl) => Some(CtrlTx::Advance),

            OR(..) | ORI(..) | SZI(..) |
            LI(..) | LW(..) | SW(..) | Label(..) | MOV(..) | ADD(..) | ADDI(..) | SUB(..)
            | SUBI(..) | LSR(..) | TL(..) | TGE(..) | TEQ(..) | TNE(..) | TB(..) | TAE(..) => {
                Some(CtrlTx::Advance)
            }

            BF(lbl) | BT(lbl) => Some(CtrlTx::Branch(*lbl)),
        }
    }

    fn emit_store_to_stack(addr: i32, src: Tmp) -> impl Iterator<Item = Self> {
        std::iter::once(SW(
            Reg::SP.into(),
            Imm::Int(addr.cast_unsigned() as u16),
            src.into(),
        ))
    }

    fn emit_load_from_stack(dst: Tmp, addr: i32) -> impl Iterator<Item = Self> {
        std::iter::once(LW(
            dst.into(),
            Reg::SP.into(),
            Imm::Int(addr.cast_unsigned() as u16),
        ))
    }

    fn emit_move(dst: Stg, src: Stg) -> impl Iterator<Item = Self> {
        std::iter::once(MOV(dst, src))
    }
}
