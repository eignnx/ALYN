//! Intermediate Representation
#![allow(unused)]

use crate::names::{Lbl, Tmp};
use derive_more::From;
use internment::Intern;
use std::{
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone, From)]
pub enum RVal {
    #[from]
    /// AKA: `Const`
    Int(i64),
    #[from]
    /// AKA: `Name`
    Lbl(Lbl),
    #[from]
    /// AKA: `Temp, Mem`
    LVal(LVal),
    Binop(Binop, Rc<RVal>, Rc<RVal>),
    Call(Rc<RVal>, Vec<Rc<RVal>>),
    /// AKA: `ESeq`
    Seq(Rc<Stmt>, Rc<RVal>),
}

impl From<Tmp> for RVal {
    fn from(tmp: Tmp) -> Self {
        LVal::from(tmp).into()
    }
}

impl RVal {
    pub fn seq<const N: usize>(stmts: [Rc<Stmt>; N], result: RVal) -> Self {
        let mut acc = result;
        for stmt in stmts.into_iter().rev() {
            acc = RVal::Seq(stmt, Rc::new(acc));
        }
        acc
    }
}

#[derive(Debug, Clone, From)]
pub enum LVal {
    #[from]
    /// AKA: `Temp`
    Tmp(Tmp),
    Mem(Rc<RVal>),
}

#[derive(Debug, Clone, Copy)]
pub enum Relop {
    Eq,
    Ne,
    Gt,
    Lt,
    GtU,
    LtU,
}

#[derive(Debug, Clone, From)]
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
    Seq(Rc<Stmt>, Rc<Stmt>),
    #[from]
    /// AKA: `Label`
    Lbl(Lbl),
}

impl Stmt {
    pub fn direct_jmp(lbl: Lbl) -> Self {
        Self::Jmp(lbl.into(), vec![lbl])
    }
}

#[test]
fn asdf() {
    // if a > b || c < d { if_true; } else { if_false; }
    let _program: Stmt = Stmt::Seq(
        Rc::new(Stmt::Br {
            op: Relop::Gt,
            e1: Tmp::from("a").into(),
            e2: Tmp::from("b").into(),
            if_true: Lbl::from("t"),
            if_false: Lbl::from("z"),
        }),
        Rc::new(Stmt::Seq(
            Rc::new(Lbl::from("z").into()),
            Rc::new(Stmt::Br {
                op: Relop::Lt,
                e1: Tmp::from("c").into(),
                e2: Tmp::from("d").into(),
                if_true: Lbl::from("t"),
                if_false: Lbl::from("f"),
            }),
        )),
    );
}
