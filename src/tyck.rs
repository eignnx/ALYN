use crate::ast::*;
use crate::tcx::{SymData, Tcx};
use crate::ty::Ty;
use internment::Intern;
use std::collections::HashMap;

pub enum TyckErr {
    UnknownVariable {
        var_span: Span,
        name: Intern<String>,
    },
    DerefNonPtr {
        span: Span,
        ty: Ty,
    },
    BadBinop {
        binop_span: Span,
        binop: Binop,
        operand_tys: (Ty, Ty),
        expected_tys: (String, String),
    },
    BadUnop {
        unop_span: Span,
        unop: Unop,
        operand_ty: Ty,
        expected_ty: String,
    },
    WrongArgType {
        span: Span,
        fname: Intern<String>,
        nth_arg: usize,
        actual_arg_ty: Ty,
        expected_arg_ty: Ty,
    },
    WrongNumArgs {
        span: Span,
        fname: Intern<String>,
        expected: usize,
        actual: usize,
    },
    UnknownFn {
        span: Span,
        fname: Intern<String>,
    },
    CalledNonCallable {
        span: Span,
        fname: Intern<String>,
    },
    ShadowedVarName {
        span: Span,
        varname: Intern<String>,
    },
    InvalidAssignment {
        span: Span,
        lhs_ty: Ty,
        rhs_ty: Ty,
    },
    NonBoolCond {
        span: Span,
        actual_ty: Ty,
    },
    WrongRetTy {
        span: Span,
        expected_ty: Ty,
        actual_ty: Ty,
    },
    RetValInVoidSubr {
        span: Span,
    },
    RetVoidInNonVoidSubr {
        span: Span,
    },
}

pub type TyckResult<T> = Result<T, TyckErr>;

impl Ann<LVal> {
    fn infer_ty(&mut self, tcx: &mut Tcx) -> TyckResult<Ty> {
        match &mut self.value {
            LVal::Var(name) => {
                if let Some(sym_data) = tcx.get(&name) {
                    Ok(self.set_ty(sym_data.ty.clone()))
                } else {
                    Err(TyckErr::UnknownVariable {
                        var_span: self.span,
                        name: name.clone(),
                    })
                }
            }
            LVal::Deref(inner) => {
                inner.infer_ty(tcx)?;
                let Ty::Ptr(inner_ty) = inner.ty.as_ref().unwrap() else {
                    return Err(TyckErr::DerefNonPtr {
                        span: inner.span.clone(),
                        ty: inner.ty.clone().unwrap(),
                    });
                };
                let ty = Ty::Ptr(Box::new(inner.ty.clone().unwrap()));
                Ok(self.set_ty(ty))
            }
        }
    }
}

impl Ann<RVal> {
    fn infer_ty(&mut self, tcx: &mut Tcx) -> TyckResult<Ty> {
        match &mut self.value {
            RVal::Byte(_) => Ok(self.set_ty(Ty::Byte)),
            RVal::Nat(_) => Ok(self.set_ty(Ty::Nat)),
            RVal::Int(_) => Ok(self.set_ty(Ty::Int)),
            RVal::LVal(lval) => lval.infer_ty(tcx),
            RVal::Binop(binop, x, y) => {
                x.infer_ty(tcx)?;
                y.infer_ty(tcx)?;
                binop.check_ty(&self.span, x, y).inspect(|ty| {
                    let _ = self.set_ty(ty.clone());
                })
            }
            RVal::Unop(unop, x) => {
                x.infer_ty(tcx)?;
                unop.check_ty(&self.span, x).inspect(|ty| {
                    let _ = self.set_ty(ty.clone());
                })
            }
            RVal::Call(fname, args) => {
                let Some(data) = tcx.get(&fname) else {
                    return Err(TyckErr::UnknownFn {
                        span: self.span,
                        fname: fname.clone(),
                    });
                };
                let subr_ty = data.ty.clone();
                let Ty::Subr(arg_tys, ret_ty) = subr_ty else {
                    return Err(TyckErr::CalledNonCallable {
                        span: self.span,
                        fname: fname.clone(),
                    });
                };
                let ret_ty = (*ret_ty).clone();
                if arg_tys.len() != args.len() {
                    return Err(TyckErr::WrongNumArgs {
                        span: self.span,
                        fname: fname.clone(),
                        expected: arg_tys.len(),
                        actual: args.len(),
                    });
                }
                for (i, (arg, expected_arg_ty)) in args.iter_mut().zip(arg_tys.iter()).enumerate() {
                    let actual_arg_ty = arg.infer_ty(tcx)?;
                    if &actual_arg_ty != expected_arg_ty {
                        return Err(TyckErr::WrongArgType {
                            span: arg.span,
                            fname: fname.clone(),
                            nth_arg: i + 1,
                            actual_arg_ty,
                            expected_arg_ty: expected_arg_ty.clone(),
                        });
                    }
                }
                Ok(self.set_ty(ret_ty))
            }
        }
    }
}

impl Binop {
    fn check_ty(&self, binop_span: &Span, x: &Ann<RVal>, y: &Ann<RVal>) -> TyckResult<Ty> {
        let x_ty = x.ty.as_ref().unwrap();
        let y_ty = y.ty.as_ref().unwrap();
        match self {
            Binop::Add | Binop::Sub => match (x_ty, y_ty) {
                (Ty::Byte, Ty::Byte) | (Ty::Nat, Ty::Nat) | (Ty::Int, Ty::Int) => Ok(x_ty.clone()),
                _ => Err(TyckErr::BadBinop {
                    binop_span: binop_span.clone(),
                    binop: *self,
                    operand_tys: (x_ty.clone(), y_ty.clone()),
                    expected_tys: ("{numeric}".into(), "{numeric}".into()),
                }),
            },

            Binop::Eq | Binop::Ne | Binop::Lt | Binop::Gt | Binop::Lte | Binop::Gte => {
                match (x_ty, y_ty) {
                    (Ty::Bool, Ty::Bool)
                    | (Ty::Byte, Ty::Byte)
                    | (Ty::Nat, Ty::Nat)
                    | (Ty::Int, Ty::Int) => Ok(Ty::Bool),
                    (Ty::Ptr(a), Ty::Ptr(b)) if a == b => Ok(Ty::Bool),
                    (Ty::Subr(params1, ret1), Ty::Subr(params2, ret2))
                        if params1 == params2 && ret1 == ret2 =>
                    {
                        Ok(Ty::Bool)
                    }

                    _ => Err(TyckErr::BadBinop {
                        binop_span: binop_span.clone(),
                        binop: *self,
                        operand_tys: (x_ty.clone(), y_ty.clone()),
                        expected_tys: ("some type T".into(), "the same type T".into()),
                    }),
                }
            }
        }
    }
}

impl Unop {
    fn check_ty(&self, unop_span: &Span, x: &Ann<LVal>) -> TyckResult<Ty> {
        let x_ty = x.ty.as_ref().unwrap();
        match self {
            Unop::AddrOf => Ok(Ty::Ptr(Box::new(x_ty.clone()))),
            Unop::Neg => match x_ty {
                Ty::Int => Ok(Ty::Int),
                _ => Err(TyckErr::BadUnop {
                    unop_span: unop_span.clone(),
                    unop: *self,
                    operand_ty: x_ty.clone(),
                    expected_ty: "int".into(),
                }),
            },
        }
    }
}

impl Ann<Stmt> {
    fn infer_ty(&mut self, tcx: &mut Tcx) -> TyckResult<()> {
        match &mut self.value {
            Stmt::RVal(rval) => rval.infer_ty(tcx).map(|_| ()),
            Stmt::Let(varname, rhs) => {
                let ty = rhs.infer_ty(tcx)?;
                if let Some(_prev) = tcx.insert(varname.clone(), SymData { ty }) {
                    return Err(TyckErr::ShadowedVarName {
                        span: self.span,
                        varname: varname.clone(),
                    });
                }
                Ok(())
            }
            Stmt::Assign(lhs, rhs) => {
                let rhs_ty = rhs.infer_ty(tcx)?;
                let lhs_ty = lhs.infer_ty(tcx)?;
                if rhs_ty != lhs_ty {
                    return Err(TyckErr::InvalidAssignment {
                        span: self.span,
                        lhs_ty,
                        rhs_ty,
                    });
                };
                Ok(())
            }
            Stmt::If(cond, true_branch, opt_false_branch) => {
                let cond_ty = cond.infer_ty(tcx)?;
                if cond_ty != Ty::Bool {
                    return Err(TyckErr::NonBoolCond {
                        span: cond.span.clone(),
                        actual_ty: cond_ty,
                    });
                }

                tcx.enter_scope();
                for stmt in true_branch {
                    stmt.infer_ty(tcx)?;
                }
                tcx.exit_scope();

                tcx.enter_scope();
                for stmt in opt_false_branch.iter_mut().flatten() {
                    stmt.infer_ty(tcx)?;
                }
                tcx.exit_scope();

                Ok(())
            }
            Stmt::While(cond, body) => {
                let cond_ty = cond.infer_ty(tcx)?;
                if cond_ty != Ty::Bool {
                    return Err(TyckErr::NonBoolCond {
                        span: cond.span.clone(),
                        actual_ty: cond_ty,
                    });
                }

                tcx.enter_scope();
                for stmt in body {
                    stmt.infer_ty(tcx)?;
                }
                tcx.exit_scope();

                Ok(())
            }
            Stmt::Ret(None) => {
                if tcx.get_subr_ret_ty().is_some() {
                    return Err(TyckErr::RetVoidInNonVoidSubr { span: self.span });
                }
                Ok(())
            }
            Stmt::Ret(Some(rval)) => {
                if let Some(expected_ty) = tcx.get_subr_ret_ty().cloned() {
                    let ty = rval.infer_ty(tcx)?;
                    if ty != expected_ty {
                        Err(TyckErr::WrongRetTy {
                            span: self.span,
                            expected_ty,
                            actual_ty: ty,
                        })
                    } else {
                        Ok(())
                    }
                } else {
                    Err(TyckErr::RetValInVoidSubr { span: self.span })
                }
            }
        }
    }
}

impl Ann<SubrDecl> {
    pub fn check_ty(&mut self, tcx: &mut Tcx) -> TyckResult<()> {
        tcx.enter_scope();
        tcx.set_subr_ret_ty(self.value.ret_ty.clone());
        // Define all parameters.
        for param in &self.value.params {
            tcx.insert(
                param.value.name.clone(),
                SymData {
                    ty: param.value.ty.clone(),
                },
            );
        }
        tcx.exit_scope();
        Ok(())
    }
}

impl Module {
    pub fn check_ty(&mut self, tcx: &mut Tcx) -> TyckResult<()> {
        // First read all subr decl types and put them in the tcx to allow for
        // mutual recursion.
        for subr in &mut self.decls {
            let subr_ty = subr.value.subr_ty();
            if let Some(_) = tcx.insert(subr.value.name.clone(), SymData { ty: subr_ty }) {
                return Err(TyckErr::ShadowedVarName {
                    span: subr.span.clone(),
                    varname: subr.value.name.clone(),
                });
            }
        }

        // Then type check all decls.
        for subr in &mut self.decls {
            subr.check_ty(tcx)?;
        }

        Ok(())
    }
}
