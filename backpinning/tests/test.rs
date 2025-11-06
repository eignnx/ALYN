use std::fmt::Debug;

use alyn_common::names::Lbl;
use regalloc_common::{ctrl_flow::{CtrlFlow, GetCtrlFlow}, stg::Stg, stmt::Stmt, Instruction, Register};

use backpinning::{diagram::DisplayLiveRanges, *};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Reg {
    T0, T1, T2,
}

impl Register for Reg {
    const GPRS: &'static [Self] = &[Reg::T0, Reg::T1, Reg::T2];
    const GPR_SAVED_REGS: &'static [Self] = &[];
    const GPR_TEMP_REGS: &'static [Self] = &[];
    const GPR_ARG_REGS: &'static [Self] = &[];
}

#[derive(Clone)]
enum Instr {
    Def(Stg<Reg>),
    Use(Stg<Reg>),
    Move(Stg<Reg>, Stg<Reg>),
    MoveImm(Stg<Reg>, i32),
    BinOp(Stg<Reg>, Stg<Reg>, Stg<Reg>),
    BinOpImm(Stg<Reg>, Stg<Reg>, i32),
    Load(Stg<Reg>, Stg<Reg>),
    Store(Stg<Reg>, Stg<Reg>),
    CmpBranch(Stg<Reg>, Stg<Reg>, Lbl),
    Jmp(Lbl),
    Ret,
}

impl Debug for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Def(x) => write!(f, "{x:?} ← _ "),
            Self::Use(x) => write!(f, "_ ← {x:?}"),
            Self::Move(dst, src) => write!(f, "{dst:?} ← {src:?}"),
            Self::MoveImm(dst, imm) => write!(f, "{dst:?} ← {imm}"),
            Self::BinOp(dst, src1, src2) => write!(f, "{dst:?} ← {src1:?} ± {src2:?}"),
            Self::BinOpImm(dst, src, imm) => write!(f, "{dst:?} ← {src:?} ± {imm:?}"),
            Self::Load(dst, src) => write!(f, "{dst:?} ← MEM[{src:?}]"),
            Self::Store(dst, src) => write!(f, "MEM[{dst:?}] ← {src:?}"),
            Self::CmpBranch(src1, src2, lbl) => write!(f, "branch to {lbl:?} if {src1:?} <> {src2:?}"),
            Self::Jmp(lbl) => write!(f, "jmp {lbl:?}"),
            Self::Ret => write!(f, "ret"),
        }
    }
}

impl Instruction for Instr {
    type Reg = Reg;
}

impl Accesses for Instr {
    fn accesses<'a>(&'a mut self) -> Vec<Access<'a, Self::Reg>> {
        match self {
            Instr::Def(dst) => vec![Access::Write(dst, InstrExePhase::WriteBack)],
            Instr::Use(src) => vec![Access::Read(src, InstrExePhase::ReadArgs)],
            Instr::Move(dst, src) => vec![
                Access::Read(src, InstrExePhase::ReadArgs),
                Access::Write(dst, InstrExePhase::WriteBack),
            ],
            Instr::MoveImm(dst, _) => vec![Access::Write(dst, InstrExePhase::WriteBack)],
            Instr::BinOp(dst, src1, src2) => vec![
                Access::Read(src1, InstrExePhase::ReadArgs),
                Access::Read(src2, InstrExePhase::ReadArgs),
                Access::Write(dst, InstrExePhase::WriteBack),
            ],
            Self::BinOpImm(dst, src, _) => vec![
                Access::Read(src, InstrExePhase::ReadArgs),
                Access::Write(dst, InstrExePhase::WriteBack),
            ],
            Instr::Load(dst, addr) => vec![
                Access::Read(addr, InstrExePhase::ReadArgs),
                Access::Write(dst, InstrExePhase::WriteBack),
            ],
            Instr::Store(addr, src) => vec![
                Access::Read(addr, InstrExePhase::ReadArgs),
                Access::Read(src, InstrExePhase::ReadArgs),
            ],
            Instr::CmpBranch(src1, src2, _) => vec![
                Access::Read(src1, InstrExePhase::ReadArgs),
                Access::Read(src2, InstrExePhase::ReadArgs),
            ],
            Instr::Jmp(_) | Instr::Ret => vec![],
        }
    }
}

impl GetCtrlFlow for Instr {
    fn ctrl_flow(&self) -> CtrlFlow {
        match self {
            Instr::Def(..) |
            Instr::Use(..) |
            Instr::Move(..) |
            Instr::MoveImm(..) |
            Instr::BinOp(..) |
            Instr::BinOpImm(..) |
            Instr::Load(..) |
            Instr::Store(..) => CtrlFlow::Advance,
            Instr::CmpBranch(_, _, lbl) => CtrlFlow::Branch(*lbl),
            Instr::Jmp(lbl) => CtrlFlow::Jump(*lbl),
            Instr::Ret => CtrlFlow::Exit,
        }
    }
}

impl From<Instr> for Stmt<Instr> {
    fn from(instr: Instr) -> Self {
        Self::Instr(instr)
    }
}

#[test]
fn test_live_range_computation() {
    use Stmt as S;
    use Instr::*;

    let stmts = vec![
        S::Label("test".into()),
        S::Instr(Def("x".into())),
        S::Instr(Def("yeet".into())),
        S::Instr(Use("x".into())),
        S::Instr(Use("yeet".into())),
        S::Instr(Def("z".into())),
        S::Instr(Use("yeet".into())),
        S::Instr(Use("z".into())),

        S::Instr(Move("w".into(), "z".into())),
        S::Instr(Use("w".into())),

        S::Instr(Move("x".into(), "w".into())),
        S::Instr(Use("x".into())),
    ];

    let live_ranges = compute_live_ranges(&stmts[..]);
    println!("{}", DisplayLiveRanges::new(&stmts[..], &live_ranges));
}

    #[test]
fn knr_binsearch() {
    // int binsearch(int x, int v[], int n) {
    //     int low = 0;
    //     int high = n - 1;
    //     while (low <= high) {
    //          int mid = (low + high) / 2;
    //          int elem = v[mid];
    //          if (x < elem)
    //              high = mid - 1;
    //          else if (x > elem)
    //              low = mid + 1;
    //          else
    //              return mid;
    //     }
    //     return -1;
    // }
    //
    // binsearch:
    // mov %low <- 0;
    // mov %high <- %n - 1;
    // jmp loop_cond;
    // loop_top:
    //      mov %mid <- (%low + %high) / 2;
    //      load %elem <- MEM[%v + %mid];
    //      branch if %x >= %elem to else_if
    //          %high <- %mid - 1;
    //          jmp end_if;
    //      else_if:
    //      branch if x <= elem to else
    //          %low <- %mid + 1;
    //          jmp end_if;
    //      else:
    //          ret %mid;
    //      end_if:
    // loop_cond:
    //      branch if %low <= %high to loop_top;
    //  ret -1;
    //

    use Stmt as S;
    use Instr::*;

    let low = "low".into();
    let high = "high".into();
    let n = "n".into();
    let v = "v".into();
    let x = "x".into();
    let elem = "elem".into();
    let mid = "mid".into();
    let v_plus_mid = "vmid".into();
    let retval = "retval".into();

    #[rustfmt::skip]
    let stmts = vec![
        Def(x).into(), Def(v).into(), Def(n).into(),
        S::Label(Lbl::subr("binsearch")),
        MoveImm(low, 0).into(),
        BinOpImm(high, n, 1).into(),
        Jmp("loop_cond".into()).into(),
        S::Label("loop_top".into()),
            BinOp(mid, low, high).into(),
            BinOp(v_plus_mid, v, mid).into(),
            Load(elem, v_plus_mid).into(),
            CmpBranch(x, elem, "else_if".into()).into(),
                BinOpImm(high, mid, 1).into(),
                Jmp("end_if".into()).into(),
            S::Label("else_if".into()),
            CmpBranch(x, elem, "else".into()).into(),
                BinOpImm(low, mid, 1).into(),
                Jmp("end_if".into()).into(),
            S::Label("else".into()).into(),
                Move(retval, mid).into(),
                Use(retval).into(),
                Ret.into(),
            S::Label("end_if".into()).into(),
        S::Label("loop_cond".into()).into(),
            CmpBranch(low, high, "loop_top".into()).into(),
        MoveImm(retval, -1).into(),
        Use(retval).into(),
        Ret.into(),
    ];
    let live_ranges = compute_live_ranges(&stmts[..]);
    println!("{}", DisplayLiveRanges::new(&stmts[..], &live_ranges));
}
