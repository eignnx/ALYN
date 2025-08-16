#![allow(unused)]

use std::rc::Rc;
use crate::ir::{Binop, LVal, Lbl, RVal, Relop, Stmt, Tmp};

pub enum IrWrap {
    RVal(RVal),
    Stmt(Stmt),
    /// A closure that produces a statement from a conditional once fed both an
    /// `if_true` label and an `if_false` label.
    ///
    /// ```text
    /// // `x > y`
    /// Cond(|t, f| Br { op: Gt, e1: x, e2: y, if_true: t, if_false: f })
    ///
    /// // `if x > y { 't: (); } else { 'f: (); }`
    /// [
    ///     Br { op: Gt, e1: x, e2: y, if_true: t, if_false: f },
    ///     Lbl("t"),
    ///     Jmp("end", ["end"]),
    ///     Lbl("f"),
    ///     Lbl("end"),
    /// ]
    /// ```
    Cond(Box<dyn Fn(Lbl, Lbl) -> Stmt>),
}

impl IrWrap {
    pub fn as_expr(&self) -> RVal {
        match self {
            IrWrap::RVal(rval) => rval.clone(),
            IrWrap::Stmt(stmt) => RVal::Seq(Rc::new(stmt.clone()), Rc::new(RVal::Int(0))),
            IrWrap::Cond(mk_stmt) => {
                let r = Tmp::fresh("r");
                let t = Lbl::fresh("t");
                let f = Lbl::fresh("f");
                RVal::seq([
                    Rc::new(Stmt::Move(r.into(), 1.into())),
                    Rc::new(mk_stmt(t, f)),
                    Rc::new(f.into()),
                    Rc::new(Stmt::Move(r.into(), 0.into())),
                    Rc::new(t.into())
                ], r.into())
            }
        }
    }

    pub fn as_stmt(&self) -> Stmt {
        match self {
            IrWrap::Stmt(stmt) => stmt.clone(),
            IrWrap::RVal(rval) => Stmt::RVal(rval.clone()),
            IrWrap::Cond(mk_stmt) => {
                let after = Lbl::fresh("after_cond");
                Stmt::Seq(
                    Rc::new(mk_stmt(after, after)),
                    Rc::new(Stmt::Lbl(after)),
                )
            }
        }
    }

    pub fn as_cond(self) -> Box<dyn Fn(Lbl, Lbl) -> Stmt> {
        match self {
            IrWrap::Cond(mk_stmt) => mk_stmt,
            IrWrap::Stmt(_) => unreachable!("Statement inside condition should never occur"),
            IrWrap::RVal(RVal::Int(0)) => Box::new(|_t, f| Stmt::direct_jmp(f)),
            IrWrap::RVal(RVal::Int(0)) => Box::new(|t, _f| Stmt::direct_jmp(t)),
            IrWrap::RVal(rval) => Box::new(move |t, f| Stmt::Br {
                op: Relop::Ne,
                e1: rval.clone(),
                e2: 0.into(),
                if_true: t,
                if_false: f,
            })
        }
    }
}
