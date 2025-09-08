use std::collections::{BTreeSet, HashMap, LinkedList};

use derive_more::From;
use internment::Intern;

use crate::names::{Lbl, Tmp};

mod cfg;
mod interferences;
mod live_sets;
mod regalloc;

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

    fn defs_uses(&self, defs: &mut BTreeSet<Tmp>, uses: &mut BTreeSet<Tmp>) {
        match self {
            Expr::Int(_) => {}
            Expr::Tmp(tmp) => {
                uses.insert(*tmp);
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

impl Stmt {
    fn defs_uses(&self, defs: &mut BTreeSet<Tmp>, uses: &mut BTreeSet<Tmp>) {
        match self {
            Stmt::Mov(tmp, expr) => {
                defs.insert(*tmp);
                expr.defs_uses(defs, uses);
            }
            Stmt::Store { addr, src } => {
                addr.defs_uses(defs, uses);
                src.defs_uses(defs, uses);
            }
            Stmt::StackStore { addr, src } => {
                uses.insert(*src);
            }
            Stmt::Load { dst, addr } => {
                defs.insert(*dst);
                addr.defs_uses(defs, uses);
            }
            Stmt::StackLoad { dst, addr } => {
                defs.insert(*dst);
            }
            Stmt::Br(expr, _lbl) => expr.defs_uses(defs, uses),
            Stmt::Jmp(_) | Stmt::Lbl(_) | Stmt::Ret(None) => {}
            Stmt::Ret(Some(expr)) => expr.defs_uses(defs, uses),
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
