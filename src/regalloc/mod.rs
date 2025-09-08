use std::collections::{BTreeSet, HashMap, LinkedList};

use derive_more::From;
use internment::Intern;
use smallvec::SmallVec;

use crate::names::{Lbl, Tmp};

mod cfg;
mod interferences;
mod live_sets;
mod regalloc;

/// Control Transfer - how the program counter may be updated by an instruction.
pub enum CtrlTx {
    /// Just advance to the next instruction: `$PC <- $PC + 1`
    Advance,

    /// Unconditional jump to given label.
    Jump(Lbl),

    /// Jump to one of the given labels, like in a switch statement.
    Switch(SmallVec<[Lbl; 2]>),

    /// Either fallthrough (same as `Advance`) or branch to the given label.
    Branch(Lbl),
}

pub trait Instr: std::fmt::Debug {
    fn add_defs_uses(&self, defs: &mut impl Extend<Tmp>, uses: &mut impl Extend<Tmp>);

    /// `%a <- %b` is a pure move instruction from one register/temporary to another.
    /// Returns the lefthand side (`%a`) and the righthand side (`%b`) respectively.
    fn try_as_pure_move(&self) -> Option<(Tmp, Tmp)>;

    fn replace_def_occurrances(&mut self, old: Tmp, new: Tmp);
    fn replace_use_occurrances(&mut self, old: Tmp, new: Tmp);

    /// If this instruction has a label associated with it, return it.
    fn get_label(&self) -> Option<Lbl>;

    /// A `return` statement would produce `None`, a `nop` statement would produce `Some(Advance)`.
    /// Subroutine call statements should produce `Some(Advance)` since within the subroutine it
    /// wouldn't seem like any jump has happened.
    fn ctrl_tx(&self) -> Option<CtrlTx>;

    fn mk_store_to_stack(addr: i32, src: Tmp) -> Self;
    fn mk_load_from_stack(dst: Tmp, addr: i32) -> Self;
}

#[derive(Clone, From)]
enum Stmt {
    Mov(Tmp, Expr),
    Store {
        addr: Expr,
        src: Expr,
    },
    StackStore {
        addr: i32,
        src: Tmp,
    },
    Load {
        dst: Tmp,
        addr: Expr,
    },
    StackLoad {
        dst: Tmp,
        addr: i32,
    },
    Br(Expr, Lbl),
    Jmp(Lbl),
    #[from(Lbl, &str)]
    Lbl(Lbl),
    Ret(Option<Expr>),
}

impl Instr for Stmt {
    fn add_defs_uses(&self, defs: &mut impl Extend<Tmp>, uses: &mut impl Extend<Tmp>) {
        match self {
            Stmt::Mov(tmp, expr) => {
                defs.extend([*tmp]);
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

    fn try_as_pure_move(&self) -> Option<(Tmp, Tmp)> {
        if let Self::Mov(lhs, Expr::Tmp(rhs)) = self {
            Some((*lhs, *rhs))
        } else {
            None
        }
    }

    fn replace_def_occurrances(&mut self, old: Tmp, new: Tmp) {
        match self {
            Stmt::Mov(tmp, expr) => {
                if *tmp == old {
                    *tmp = new;
                }
            }
            Stmt::Store { addr, src } => {}
            Stmt::StackStore { addr, src } => {}
            Stmt::Load { dst, .. } | Stmt::StackLoad { dst, .. } => {
                if *dst == old {
                    *dst = new;
                }
            }
            Stmt::Br(expr, _lbl) => {}
            Stmt::Jmp(_) | Stmt::Lbl(_) | Stmt::Ret(None) => {}
            Stmt::Ret(Some(expr)) => {}
        }
    }

    fn replace_use_occurrances(&mut self, old: Tmp, new: Tmp) {
        match self {
            Stmt::Mov(tmp, expr) => expr.replace_use_occurrances(old, new),
            Stmt::Store { addr, src } => {
                addr.replace_use_occurrances(old, new);
                src.replace_use_occurrances(old, new);
            }
            Stmt::StackStore { addr, src } => {
                if *src == old {
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

    fn mk_store_to_stack(addr: i32, src: Tmp) -> Self {
        Self::StackStore { addr, src }
    }

    fn mk_load_from_stack(dst: Tmp, addr: i32) -> Self {
        Self::StackLoad { dst, addr }
    }
}

impl Stmt {
    fn mov(tmp: impl Into<Tmp>, expr: impl Into<Expr>) -> Self {
        Self::Mov(tmp.into(), expr.into())
    }

    fn br(expr: impl Into<Expr>, offset: impl Into<Lbl>) -> Self {
        Self::Br(expr.into(), offset.into())
    }

    fn ret(maybe_value: impl Into<Expr>) -> Self {
        Self::Ret(Some(maybe_value.into()))
    }

    fn jmp(lbl: impl Into<Lbl>) -> Self {
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
enum Expr {
    #[from]
    Int(i32),
    #[from(&str)]
    Tmp(Tmp),
    Binop(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn binop(x: impl Into<Expr>, y: impl Into<Expr>) -> Self {
        Self::Binop(Box::new(x.into()), Box::new(y.into()))
    }

    fn defs_uses(&self, defs: &mut impl Extend<Tmp>, uses: &mut impl Extend<Tmp>) {
        match self {
            Expr::Int(_) => {}
            Expr::Tmp(tmp) => {
                uses.extend([*tmp]);
            }
            Expr::Binop(expr1, expr2) => {
                expr1.defs_uses(defs, uses);
                expr2.defs_uses(defs, uses);
            }
        }
    }

    fn replace_use_occurrances(&mut self, old: Tmp, new: Tmp) {
        match self {
            Expr::Int(_) => {}
            Expr::Tmp(tmp) => {
                if *tmp == old {
                    *tmp = new;
                }
            }
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
            Self::Binop(lhs, rhs) => write!(f, "({lhs:?} op {rhs:?})"),
        }
    }
}

#[test]
fn test() {
    use Expr as E;
    use Stmt as S;

    let _program = vec![
        S::mov("a", 0),
        "L1".into(),
        S::mov("b", E::binop("a", 1)),
        S::mov("c", E::binop("c", "b")),
        S::mov("a", E::binop("b", 2)),
        S::br(E::binop("a", 100), "L1"),
        S::ret("c"),
    ];
}
