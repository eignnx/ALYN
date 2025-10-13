use derive_more::{Display, From};

use crate::{
    instr_sel::Stg,
    names::{Lbl, Tmp},
    regalloc::Cc,
};

use super::{CtrlTx, Instr};

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Reg {
    T0,
    T1,
    T2,
    S0,
    S1,
}

impl Cc<Reg> for Reg {
    const GPRS: &[Self] = &[Reg::T0, Reg::T1, Reg::T2, Reg::S0, Reg::S1];
    const GPR_ARG_REGS: &'static [Reg] = &[Reg::T0, Reg::T1, Reg::T2];
    const GPR_TEMP_REGS: &'static [Reg] = &[Reg::T0, Reg::T1, Reg::T2];
    const GPR_SAVED_REGS: &'static [Reg] = &[Reg::S0, Reg::S1];
}

#[derive(Clone, From)]
pub(super) enum Stmt {
    Mov(Stg<Reg>, Expr),
    Store {
        addr: Expr,
        src: Expr,
    },
    StackStore {
        addr: i32,
        src: Stg<Reg>,
    },
    Load {
        dst: Stg<Reg>,
        addr: Expr,
    },
    StackLoad {
        dst: Stg<Reg>,
        addr: i32,
    },
    Br(Expr, Lbl),
    Jmp(Lbl),
    #[from(Lbl, &str)]
    Lbl(Lbl),
    Ret(Option<Expr>),
}

impl Instr for Stmt {
    type Register = Reg;

    fn add_defs_uses(&self, defs: &mut impl Extend<Stg<Reg>>, uses: &mut impl Extend<Stg<Reg>>) {
        match self {
            Stmt::Mov(stg, expr) => {
                defs.extend([*stg]);
                expr.defs_uses(defs, uses);
            }
            Stmt::Store { addr, src } => {
                addr.defs_uses(defs, uses);
                src.defs_uses(defs, uses);
            }
            Stmt::StackStore { addr, src } => {
                uses.extend([*src]);
            }
            Stmt::Load { dst, addr } => {
                defs.extend([*dst]);
                addr.defs_uses(defs, uses);
            }
            Stmt::StackLoad { dst, addr } => {
                defs.extend([*dst]);
            }
            Stmt::Br(expr, _lbl) => expr.defs_uses(defs, uses),
            Stmt::Jmp(_) | Stmt::Lbl(_) | Stmt::Ret(None) => {}
            Stmt::Ret(Some(expr)) => expr.defs_uses(defs, uses),
        }
    }

    fn try_as_pure_move(&self) -> Option<(Stg<Reg>, Stg<Reg>)> {
        match self {
            Self::Mov(lhs, Expr::Tmp(tmp)) => Some((*lhs, Stg::Tmp(*tmp))),
            Self::Mov(lhs, Expr::Reg(reg)) => Some((*lhs, Stg::Reg(*reg))),
            _ => None,
        }
    }

    fn replace_def_occurrances(&mut self, old: Tmp, new: Stg<Reg>) {
        match self {
            Stmt::Mov(dst, expr) => {
                if *dst == old.into() {
                    *dst = new;
                }
            }
            Stmt::Store { addr, src } => {}
            Stmt::StackStore { addr, src } => {}
            Stmt::Load { dst, .. } | Stmt::StackLoad { dst, .. } => {
                if *dst == old.into() {
                    *dst = new;
                }
            }
            Stmt::Br(expr, _lbl) => {}
            Stmt::Jmp(_) | Stmt::Lbl(_) | Stmt::Ret(None) => {}
            Stmt::Ret(Some(expr)) => {}
        }
    }

    fn replace_use_occurrances(&mut self, old: Tmp, new: Stg<Reg>) {
        match self {
            Stmt::Mov(tmp, expr) => expr.replace_use_occurrances(old, new),
            Stmt::Store { addr, src } => {
                addr.replace_use_occurrances(old, new);
                src.replace_use_occurrances(old, new);
            }
            Stmt::StackStore { addr, src } => {
                if *src == old.into() {
                    *src = new;
                }
            }
            Stmt::Load { dst, addr } => addr.replace_use_occurrances(old, new),
            Stmt::StackLoad { dst, addr } => {}
            Stmt::Ret(Some(expr)) | Stmt::Br(expr, ..) => expr.replace_use_occurrances(old, new),
            Stmt::Jmp(_) | Stmt::Lbl(_) | Stmt::Ret(None) => {}
        }
    }

    fn get_label(&self) -> Option<Lbl> {
        if let Self::Lbl(lbl) = self {
            Some(*lbl)
        } else {
            None
        }
    }

    fn ctrl_tx(&self) -> Option<CtrlTx> {
        match self {
            Stmt::Mov(..)
            | Stmt::Store { .. }
            | Stmt::StackStore { .. }
            | Stmt::Load { .. }
            | Stmt::StackLoad { .. }
            | Stmt::Lbl(_) => Some(CtrlTx::Advance),
            Stmt::Br(_, lbl) => Some(CtrlTx::Branch(*lbl)),
            Stmt::Jmp(lbl) => Some(CtrlTx::Jump(*lbl)),
            Stmt::Ret(_) => None,
        }
    }

    fn emit_store_to_stack(addr: i32, src: Tmp) -> impl Iterator<Item = Self> {
        std::iter::once(Self::StackStore {
            addr,
            src: src.into(),
        })
    }

    fn emit_load_from_stack(dst: Tmp, addr: i32) -> impl Iterator<Item = Self> {
        std::iter::once(Self::StackLoad {
            dst: dst.into(),
            addr,
        })
    }

    fn emit_move(dst: Stg<Self::Register>, src: Stg<Self::Register>) -> impl Iterator<Item = Self> {
        std::iter::once(Self::Mov(dst, src.into()))
    }
}

impl Stmt {
    pub(super) fn mov(dst: impl Into<Stg<Reg>>, expr: impl Into<Expr>) -> Self {
        Self::Mov(dst.into(), expr.into())
    }

    pub(super) fn br(expr: impl Into<Expr>, offset: impl Into<Lbl>) -> Self {
        Self::Br(expr.into(), offset.into())
    }

    pub(super) fn ret(maybe_value: impl Into<Expr>) -> Self {
        Self::Ret(Some(maybe_value.into()))
    }

    pub(super) fn jmp(lbl: impl Into<Lbl>) -> Self {
        Self::Jmp(lbl.into())
    }
}

impl std::fmt::Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mov(lhs, rhs) => write!(f, "move {lhs:?} <- {rhs:?}"),
            Self::Store { addr, src } => write!(f, "store MEM[{addr:?}] <- {src:?}"),
            Self::StackStore { addr, src } => write!(f, "stack_store STACK[{addr}] <- {src:?}"),
            Self::Load { dst, addr } => write!(f, "load {dst:?} <- MEM[{addr:?}]"),
            Self::StackLoad { dst, addr } => write!(f, "stack_load {dst:?} <- STACK[{addr:?}]"),
            Self::Br(expr, lbl) => write!(f, "branch if {expr:?} to {lbl:?}"),
            Self::Jmp(lbl) => write!(f, "jump to {lbl:?}"),
            Self::Lbl(lbl) => write!(f, "label {lbl:?}:"),
            Self::Ret(None) => write!(f, "ret"),
            Self::Ret(Some(expr)) => write!(f, "ret {expr:?}"),
        }
    }
}

/// Only expressions not involving memory can be represented.
/// Loads/stores must be explicit statements.
#[derive(Clone, From, PartialEq)]
pub(super) enum Expr {
    #[from]
    Int(i32),
    #[from(&str)]
    Tmp(Tmp),
    #[from(Reg)]
    Reg(Reg),
    Binop(Box<Expr>, Box<Expr>),
}

impl From<Stg<Reg>> for Expr {
    fn from(value: Stg<Reg>) -> Self {
        match value {
            Stg::Tmp(tmp) => Expr::Tmp(tmp),
            Stg::Reg(reg) => Expr::Reg(reg),
        }
    }
}

impl Expr {
    pub(super) fn binop(x: impl Into<Expr>, y: impl Into<Expr>) -> Self {
        Self::Binop(Box::new(x.into()), Box::new(y.into()))
    }

    fn defs_uses(&self, defs: &mut impl Extend<Stg<Reg>>, uses: &mut impl Extend<Stg<Reg>>) {
        match self {
            Expr::Int(_) => {}
            Expr::Tmp(tmp) => {
                uses.extend([Stg::Tmp(*tmp)]);
            }
            Expr::Reg(reg) => {
                uses.extend([Stg::Reg(*reg)]);
            }
            Expr::Binop(expr1, expr2) => {
                expr1.defs_uses(defs, uses);
                expr2.defs_uses(defs, uses);
            }
        }
    }

    fn replace_use_occurrances(&mut self, old: Tmp, new: Stg<Reg>) {
        match self {
            Expr::Int(_) => {}
            Expr::Tmp(tmp) => {
                if *tmp == old {
                    *self = new.into();
                }
            }
            Expr::Reg(_) => {} // We'll never need a register to be replaced
            Expr::Binop(expr1, expr2) => {
                expr1.replace_use_occurrances(old, new);
                expr2.replace_use_occurrances(old, new);
            }
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{x}"),
            Self::Tmp(tmp) => write!(f, "{tmp:?}"),
            Self::Reg(reg) => write!(f, "{reg:?}"),
            Self::Binop(lhs, rhs) => write!(f, "({lhs:?} op {rhs:?})"),
        }
    }
}

#[test]
fn test() {
    use Expr as E;
    use Stmt as S;

    let _program = vec![
        S::mov(Tmp::from("a"), 0),
        "L1".into(),
        S::mov(Tmp::from("b"), E::binop("a", 1)),
        S::mov(Tmp::from("c"), E::binop("c", "b")),
        S::mov(Tmp::from("a"), E::binop("b", 2)),
        S::br(E::binop("a", 100), "L1"),
        S::ret("c"),
    ];
}
