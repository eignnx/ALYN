//! Intermediate Representation
#![allow(unused)]

use std::{cell::Cell, rc::Rc, sync::atomic::{AtomicUsize, Ordering}};
use internment::Intern;
use derive_more::From;

#[derive(Debug, Clone, Copy)]
pub struct Lbl(Intern<String>);

impl From<&str> for Lbl {
    fn from(name: &str) -> Self {
        Self(Intern::new(name.into()))
    }
}

pub static LBL_ID: AtomicUsize = AtomicUsize::new(0);

impl Lbl {
    pub fn fresh(base_name: &str) -> Self {
        let id = LBL_ID.fetch_add(1, Ordering::SeqCst);
        Self(Intern::new(format!("{base_name}#{id}")))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Tmp(Intern<String>);

impl From<&str> for Tmp {
    fn from(name: &str) -> Self {
        Self(Intern::new(name.into()))
    }
}

pub static TMP_ID: AtomicUsize = AtomicUsize::new(0);

impl Tmp {
    pub fn fresh(base_name: &str) -> Self {
        let id = TMP_ID.fetch_add(1, Ordering::SeqCst);
        Self(Intern::new(format!("{base_name}#{id}")))
    }
}


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
    Int(i64),
    #[from]
    Lbl(Lbl),
    #[from]
    LVal(LVal),
    Binop(Binop, Rc<RVal>, Rc<RVal>),
    Call(Rc<RVal>, Vec<Rc<RVal>>),
    Seq(Rc<Stmt>, Rc<RVal>),
}

impl From<Tmp> for RVal {
    fn from(tmp: Tmp) -> Self {
        LVal::from(tmp).into()
    }
}

impl RVal {
    pub fn seq<const N: usize>(stmts: [Rc<Stmt>; N], result: RVal) -> Self
    {
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
    RVal(RVal),
    Jmp(RVal, Vec<Lbl>),
    Br {
        op: Relop,
        e1: RVal,
        e2: RVal,
        if_true: Lbl,
        if_false: Lbl,
    },
    Seq(Rc<Stmt>, Rc<Stmt>),
    #[from]
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
                })
        ))
    );
}
