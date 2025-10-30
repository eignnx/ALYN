use std::{
    collections::{BTreeSet, HashMap},
    fmt::Debug,
};

use alyn_common::names::{Lbl, Tmp};
use qwordal::{
    DefsUses, Instruction, Register, StgSubst, ToSpill,
    alloc::{InstrWrite, SlotAllocator, color_graph_greedily},
    cfg::Cfg,
    common::{Asn, CtrlFlow, CtrlTx, SlotId, Stg, Stmt},
    elim_ord::simplicial_elimination_ordering,
    intfs::Intfs,
    liveness::LiveSets,
    spill::rewrite_with_spills,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Reg {
    Sp,
    X0,
    X1,
    X2,
    S0,
    S1,
}

impl Reg {
    fn into_stg(self) -> Stg<Self> {
        Stg::Reg(self)
    }
}

use Reg::*;

impl Register for Reg {
    const GPRS: &'static [Self] = &[X0, X1, X2, S0, S1];
    const GPR_SAVED_REGS: &'static [Self] = &[S0, S1];
    const GPR_TEMP_REGS: &'static [Self] = &[X0, X1, X2];
    const GPR_ARG_REGS: &'static [Self] = &[X0, X1, X2];
}

#[derive(Clone, derive_more::Display)]
enum Instr {
    #[display("load-imm {_0:?}, {_1}")]
    LoadImm(Stg<Reg>, i32),
    #[display("move {_0:?}, {_1:?}")]
    Move(Stg<Reg>, Stg<Reg>),
    #[display("binop {_0:?}, {_1:?}, {_2:?}")]
    BinOp(Stg<Reg>, Stg<Reg>, Stg<Reg>),
    #[display("cmp-br {_0:?}, {_1:?}, {_2:?}")]
    CmpBr(Stg<Reg>, Stg<Reg>, Lbl),
    #[display("jump {_0:?}")]
    Jump(Lbl),
    #[display("ret")]
    Ret,
    #[display("stack-store SP[{dst_idx}], {src:?}")]
    StackStore { dst_idx: i32, src: Stg<Reg> },
    #[display("stack-load {dst:?}, SP[{src_idx}]")]
    StackLoad { dst: Stg<Reg>, src_idx: i32 },
}

impl Debug for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
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
            Instr::LoadImm(..)
            | Instr::BinOp(..)
            | Instr::Move(..)
            | Instr::StackLoad { .. }
            | Instr::StackStore { .. } => CtrlTx::Advance,
            Instr::CmpBr(_, _, lbl) => CtrlTx::Branch(*lbl),
            Instr::Jump(lbl) => CtrlTx::Jump(*lbl),
            Instr::Ret => CtrlTx::Exit,
        }
    }
}

impl DefsUses for Instr {
    fn add_defs_uses<E: Extend<Stg<Self::Reg>>>(&self, defs: &mut E, uses: &mut E) {
        match self {
            Instr::LoadImm(dst, _) => {
                defs.extend([*dst]);
            }
            Instr::Move(dst, src) => {
                defs.extend([*dst]);
                uses.extend([*src]);
            }
            Instr::BinOp(dst, src1, src2) => {
                defs.extend([*dst]);
                uses.extend([*src1, *src2]);
            }
            Instr::CmpBr(src1, src2, _) => {
                uses.extend([*src1, *src2]);
            }
            Instr::Jump(_) | Instr::Ret => {}
            Instr::StackStore { src, .. } => {
                uses.extend([*src]);
            }
            Instr::StackLoad { dst, .. } => {
                defs.extend([*dst]);
            }
        }
    }
}

impl StgSubst for Instr {
    fn subst_tmp<'edit, 'instr: 'edit>(
        &'instr mut self,
        assignments: &HashMap<Tmp, Asn<Self::Reg>>,
    ) -> Vec<ToSpill<'edit>>
    where
        'instr: 'edit,
    {
        let mut spills = vec![];

        match self {
            Instr::LoadImm(dst, _) => {
                dst.subst_def(assignments, &mut spills);
            }
            Instr::Move(dst, src) => {
                dst.subst_def(assignments, &mut spills);
                src.subst_use(assignments, &mut spills);
            }
            Instr::BinOp(dst, src1, src2) => {
                dst.subst_def(assignments, &mut spills);
                src1.subst_use(assignments, &mut spills);
                src2.subst_use(assignments, &mut spills);
            }
            Instr::CmpBr(src1, src2, _) => {
                src1.subst_use(assignments, &mut spills);
                src2.subst_use(assignments, &mut spills);
            }
            Instr::Jump(_) | Instr::Ret => {}
            Instr::StackStore { src, .. } => {
                src.subst_use(assignments, &mut spills);
            }
            Instr::StackLoad { dst, .. } => {
                dst.subst_def(assignments, &mut spills);
            }
        }
        spills
    }
}

impl InstrWrite for Instr {
    fn emit_move(dst: Stg<Self::Reg>, src: Stg<Self::Reg>) -> impl Iterator<Item = Stmt<Self>> {
        std::iter::once(Self::Move(dst, src).into())
    }

    fn emit_stack_load(dst: Stg<Self::Reg>, src_slot_idx: i32) -> impl Iterator<Item = Stmt<Self>> {
        [Instr::StackLoad {
            dst,
            src_idx: src_slot_idx,
        }
        .into()]
        .into_iter()
    }

    fn emit_stack_store(
        dst_slot_idx: i32,
        src: Stg<Self::Reg>,
    ) -> impl Iterator<Item = Stmt<Self>> {
        [Instr::StackStore {
            dst_idx: dst_slot_idx,
            src,
        }
        .into()]
        .into_iter()
    }
}

#[derive(Default, Debug)]
struct MySlotAlloc {
    mapping: HashMap<SlotId, i32>,
}

impl SlotAllocator for MySlotAlloc {
    fn get_or_alloc_slot(&mut self, slot_id: SlotId) -> i32 {
        let next = self.mapping.len() as i32;
        *self.mapping.entry(slot_id).or_insert(next)
    }
}

#[test]
fn basic_coloring() {
    use Instr::*;

    let n = Tmp::from("n").into();
    let zero = Tmp::from("zero").into();
    let one = Tmp::from("one").into();
    let prev = Tmp::from("prev").into();
    let curr = Tmp::from("curr").into();
    let sum = Tmp::from("sum").into();

    let please_save = Tmp::from("please_save").into();

    #[rustfmt::skip]
    let asm = vec![
        Stmt::label("fibonacci"),
            Move(please_save, S0.into_stg()).into(),
            Move(n, X0.into_stg()).into(),
            LoadImm(prev, 0).into(),
            LoadImm(curr, 1).into(),
            Jump("cond".into()).into(),
            Stmt::label("loop_top"),
                BinOp(sum, curr, prev).into(),
                Move(prev, curr).into(),
                Move(curr, sum).into(),
                LoadImm(one, 1).into(),
                BinOp(n, n, one).into(),
            Stmt::label("cond"),
                LoadImm(zero, 0).into(),
                CmpBr(n, zero, "loop_top".into()).into(),
            Move(X0.into_stg(), curr).into(),
            Move(S0.into_stg(), please_save).into(),
            Ret.into(),
    ];

    let cfg = Cfg::new(0, vec!["n".into()], asm);
    let live_sets = LiveSets::build_from(&cfg);
    eprintln!("{}", live_sets.display(cfg.stmts()));
    let intfs = Intfs::build_from(&cfg, &live_sets);
    eprintln!("{intfs:?}");
    let ordering = simplicial_elimination_ordering(&intfs);
    eprintln!("elimination ordering: {ordering:?}");
    let assignments = color_graph_greedily(&intfs, ordering);
    eprintln!("Assignments:");
    for (k, v) in assignments.iter() {
        eprintln!("{k:?} --> {v:?}");
    }

    let mut slot_alloc = MySlotAlloc::default();
    let (new_program, did_spill) =
        rewrite_with_spills(cfg.take_stmts(), &assignments, &mut slot_alloc);
    dbg!(new_program);
    dbg!(did_spill);
}
