//! Intermediate Representation

use std::sync::atomic::{AtomicUsize, Ordering};

use derive_more::From;
use internment::Intern;

use crate::{
    names::{Lbl, Tmp},
    ty::Ty,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Binop {
    Add,
    Sub,
    And,
    Or,
    Shr,
    Xor,
}

#[derive(Clone, From, PartialEq)]
pub enum RVal {
    #[from]
    Byte(u8),
    #[from]
    Nat(u64),
    #[from]
    /// AKA: `Const`
    Int(i64),
    #[from]
    /// AKA: `Name`
    Lbl(Lbl),
    #[from]
    /// AKA: `Temp, Mem`
    LVal(LVal),
    Binop(Binop, Box<RVal>, Box<RVal>),
    Unop(Unop, Box<RVal>),
    BitCast(Ty, Box<RVal>),
    Call(Box<RVal>, Vec<Box<RVal>>),
    /// AKA: `ESeq`
    Seq(Box<Stmt>, Box<RVal>),
}

impl From<Tmp> for RVal {
    fn from(tmp: Tmp) -> Self {
        LVal::from(tmp).into()
    }
}

impl RVal {
    pub fn seq<const N: usize>(stmts: [Box<Stmt>; N], result: RVal) -> Self {
        let mut acc = result;
        for stmt in stmts.into_iter().rev() {
            acc = RVal::Seq(stmt, Box::new(acc));
        }
        acc
    }

    pub fn tmp(name: impl AsRef<str>) -> Self {
        Self::LVal(LVal::Tmp(name.as_ref().into()))
    }
}

#[derive(Clone, From, PartialEq)]
pub enum LVal {
    /// AKA: `Temp`
    #[from]
    Tmp(Tmp), // <-- Local
    Mem(Box<RVal>),         // <-- Deref
    Global(Intern<String>), // <-- Global
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Relop {
    Eq,
    Ne,
    Gt,
    Lt,
    Gte,
    Lte,
    GtU,
    LtU,
    GteU,
    LteU,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Unop {
    Neg,
}

#[derive(Clone, From, PartialEq)]
pub enum Stmt {
    Move(LVal, RVal),
    /// AKA: `Exp`
    RVal(RVal),
    /// AKA: `Jump`
    Jmp(RVal, Vec<Lbl>),
    /// AKA: `CJump`
    Br {
        op: Relop,
        e1: RVal,
        e2: RVal,
        if_true: Lbl,
        if_false: Lbl,
    },
    Seq(Box<Stmt>, Box<Stmt>),
    #[from]
    /// AKA: `Label`
    Lbl(Lbl),
    Nop,
    Ret(Option<RVal>),
}

impl Stmt {
    pub fn direct_jmp(lbl: impl Into<Lbl>) -> Self {
        let lbl = lbl.into();
        Self::Jmp(RVal::Lbl(lbl), vec![lbl])
    }

    pub fn seq<I>(stmts: I) -> Self
    where
        I: IntoIterator<Item = Stmt>,
        I::IntoIter: DoubleEndedIterator,
    {
        let mut iter = stmts.into_iter().rev();
        if let Some(first) = iter.next() {
            let mut acc = first;
            for stmt in iter {
                acc = Stmt::Seq(Box::new(stmt), Box::new(acc));
            }
            acc
        } else {
            Self::Nop
        }
    }
}

#[test]
fn asdf() {
    // if a > b || c < d { if_true; } else { if_false; }
    let _program: Stmt = Stmt::Seq(
        Box::new(Stmt::Br {
            op: Relop::Gt,
            e1: Tmp::from("a").into(),
            e2: Tmp::from("b").into(),
            if_true: Lbl::fresh("t"),
            if_false: Lbl::fresh("z"),
        }),
        Box::new(Stmt::Seq(
            Box::new(Lbl::fresh("z").into()),
            Box::new(Stmt::Br {
                op: Relop::Lt,
                e1: Tmp::from("c").into(),
                e2: Tmp::from("d").into(),
                if_true: Lbl::fresh("t"),
                if_false: Lbl::fresh("f"),
            }),
        )),
    );
}
