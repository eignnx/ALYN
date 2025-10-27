#![allow(unused)]

use crate::ir::{Binop, LVal, RVal, Relop, Stmt};
use alyn_common::names::{Lbl, Tmp};

/// AKA: `exp`
pub enum IrWrap {
    /// AKA: `Ex`
    RVal(RVal),
    /// AKA: `Nx`
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
    ///
    /// AKA: `Cx`
    Cond(Box<dyn FnOnce(Lbl, Lbl) -> Stmt>),
}

impl IrWrap {
    /// AKA: `unEx`
    pub fn as_expr(self) -> RVal {
        match self {
            IrWrap::RVal(rval) => rval.clone(),
            IrWrap::Stmt(stmt) => RVal::Seq(Box::new(stmt.clone()), Box::new(RVal::Int(0))),
            IrWrap::Cond(mk_stmt) => {
                let r = Tmp::fresh("r");
                let t = Lbl::fresh("t");
                let f = Lbl::fresh("f");
                RVal::seq(
                    [
                        Box::new(Stmt::Move(r.into(), 1u8.into())),
                        Box::new(mk_stmt(t, f)),
                        Box::new(f.into()),
                        Box::new(Stmt::Move(r.into(), 0u8.into())),
                        Box::new(t.into()),
                    ],
                    r.into(),
                )
            }
        }
    }

    /// AKA: `unNx`
    pub fn as_stmt(self) -> Stmt {
        match self {
            IrWrap::Stmt(stmt) => stmt.clone(),
            IrWrap::RVal(rval) => match rval {
                RVal::Seq(stmt, rest_expr) => {
                    let rest = IrWrap::RVal(*rest_expr).as_stmt();
                    Stmt::Seq(stmt, Box::new(rest))
                }
                _ => Stmt::RVal(rval.clone()),
            },
            IrWrap::Cond(mk_stmt) => {
                let after = Lbl::fresh("after_cond");
                Stmt::Seq(Box::new(mk_stmt(after, after)), Box::new(Stmt::Lbl(after)))
            }
        }
    }

    /// AKA: `unCx`
    pub fn as_cond(self) -> Box<dyn FnOnce(Lbl, Lbl) -> Stmt> {
        match self {
            IrWrap::Cond(mk_stmt) => mk_stmt,
            IrWrap::Stmt(_) => unreachable!("Statement inside condition should never occur"),
            IrWrap::RVal(RVal::Int(0)) => Box::new(|_t, f| Stmt::direct_jmp(f)),
            IrWrap::RVal(RVal::Int(_)) => Box::new(|t, _f| Stmt::direct_jmp(t)),
            IrWrap::RVal(rval) => Box::new(move |t, f| Stmt::Br {
                op: Relop::Ne,
                e1: rval.clone(),
                e2: 0u8.into(),
                if_true: t,
                if_false: f,
            }),
        }
    }

    pub fn collect_stmts_into(self, stmts: &mut Vec<Stmt>) {
        match self.as_stmt() {
            Stmt::Seq(stmt1, stmt2) => {
                IrWrap::Stmt(*stmt1).collect_stmts_into(stmts);
                IrWrap::Stmt(*stmt2).collect_stmts_into(stmts);
            }
            // In case the RVal is a Seq of stmts.
            Stmt::RVal(rval) => IrWrap::RVal(rval).collect_stmts_into(stmts),
            other => stmts.push(other),
        }
    }
}
