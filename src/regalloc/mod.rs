use std::collections::{BTreeSet, HashMap, LinkedList};

use internment::Intern;
use derive_more::From;

use crate::names::Tmp;

mod cfg;
mod live_sets;
mod interferences;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Lbl(Intern<String>);

impl From<&str> for Lbl {
    fn from(value: &str) -> Self {
        Lbl(Intern::from_ref(value))
    }
}

/// Only expressions not involving memory can be represented.
/// Loads/stores must be explicit statements.
#[derive(Debug, From)]
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
            Expr::Int(_) => {},
            Expr::Tmp(tmp) => { uses.insert(*tmp); }
            Expr::Binop(expr1, expr2) => {
                expr1.defs_uses(defs, uses);
                expr2.defs_uses(defs, uses);
            }
        }
    }
}

#[derive(Debug, From)]
enum Stmt {
    Mov(Tmp, Expr),
    Store { addr: Expr, src: Expr },
    Load { dst: Tmp, addr: Expr },
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
            Stmt::Load { dst, addr } => {
                defs.insert(*dst);
                addr.defs_uses(defs, uses);
            }
            Stmt::Br(expr, _lbl) => expr.defs_uses(defs, uses),
            Stmt::Jmp(lbl) => {}
            Stmt::Lbl(lbl) => {}
            Stmt::Ret(None) => {}
            Stmt::Ret(Some(expr)) => expr.defs_uses(defs, uses),
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
}


#[test]
fn test() {
    use Stmt as S;
    use Expr as E;

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

