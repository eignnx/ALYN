use std::collections::{BTreeSet, HashMap};

use alyn_common::names::{Lbl, Tmp};
use qwordal::{common::{Asn, CtrlFlow, CtrlTx, Stg}, DefsUses, Instruction, Register, StgSubst, ToSpill};



#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Reg {
    Sp,
    X0,
    X1,
    X2,
    S0,
    S1,
}

use Reg::*;

impl Register for Reg {
    const GPRS: &'static [Self] = &[X0, X1, X2, S0, S1];
    const GPR_SAVED_REGS: &'static [Self] = &[S0, S1];
    const GPR_TEMP_REGS: &'static [Self] = &[X0, X1, X2];
    const GPR_ARG_REGS: &'static [Self] = &[X0, X1, X2];
}

#[derive(Debug, Clone)]
enum Instr {
    BinOp(Stg<Reg>, Stg<Reg>, Stg<Reg>),
    Move(Stg<Reg>, Stg<Reg>),
    CmpBr(Stg<Reg>, Stg<Reg>, Lbl),
    Jump(Lbl),
}

impl Instruction for Instr {
    type Reg = Reg;

    fn try_as_pure_move(&self) -> Option<(Stg<Self::Reg>, Stg<Self::Reg>)> {
        if let Self::Move(dst, src) = self {
            Some((*dst, *src))
        } else {
            None
        }
    }

    fn is_subr_call(&self) -> bool {
        false
    }
}

impl CtrlFlow for Instr {
    fn ctrl_tx(&self) -> CtrlTx {
        match self {
            Instr::BinOp(..) | Instr::Move(..) => CtrlTx::Advance,
            Instr::CmpBr(_, _, lbl) => CtrlTx::Branch(*lbl),
            Instr::Jump(lbl) => CtrlTx::Jump(*lbl),
        }
    }
}

impl DefsUses for Instr {
    fn add_defs_uses<E: Extend<Stg<Self::Reg>>>(&self, defs: &mut E, uses: &mut E) {
        match self {
            Instr::BinOp(dst, src1, src2) => {
                defs.extend([*dst]);
                uses.extend([*src1, *src2]);
            }
            Instr::Move(dst, src) => {
                defs.extend([*dst]);
                uses.extend([*src]);
            }
            Instr::CmpBr(src1, src2, _) => {
                uses.extend([*src1, *src2]);
            }
            Instr::Jump(_) => {}
        }
    }
}

impl StgSubst for Instr {
    fn substitute_tmp_for_reg(
        &mut self,
        assignments: &HashMap<Tmp, Asn<Self::Reg>>,
        spills: &mut BTreeSet<ToSpill>,
    ) {
        match self {
            Instr::Move(dst, src) => {
                dst.subst_def(assignments, spills);
                src.subst_use(assignments, spills);
            }
            Instr::BinOp(dst, src1, src2) => {
                dst.subst_def(assignments, spills);
                src1.subst_use(assignments, spills);
                src2.subst_use(assignments, spills);
            }
            Instr::CmpBr(src1, src2, _) => {
                src1.subst_use(assignments, spills);
                src2.subst_use(assignments, spills);
            }
            Instr::Jump(_) => {}
        }
    }
}
