use crate::{
    ast::*,
    sym::IdentKind,
    tcx::{SymData, Tcx},
    ty::Ty,
};
use internment::Intern;
use std::collections::HashMap;

#[derive(Debug)]
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
    BadCast {
        span: Span,
        value_span: Span,
        ty_span: Span,
        inferred_ty: Ty,
        requested_ty: Ty,
    },
}

pub type TyckResult<T> = Result<T, TyckErr>;

impl Ann<LVal> {
    fn infer_ty(&mut self, tcx: &mut Tcx) -> TyckResult<Ty> {
        match &mut self.value {
            LVal::Var(name, ident_kind) => {
                if let Some(sym_data) = tcx.get(&name) {
                    *ident_kind = Some(sym_data.sym_kind);
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
                let ty = (**inner_ty).clone();
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
            RVal::LVal(lval) => lval.infer_ty(tcx).inspect(|ty| {
                let _ = self.set_ty(ty.clone());
            }),
            RVal::Binop(binop, x, y) => {
                x.infer_ty(tcx)?;
                y.infer_ty(tcx)?;
                binop.check_ty(&self.span, x, y).inspect(|ty| {
                    let _ = self.set_ty(ty.clone());
                })
            }
            RVal::AddrOf(x) => {
                let ty = x.infer_ty(tcx)?;
                Ok(self.set_ty(Ty::Ptr(Box::new(ty))))
            }
            RVal::Unop(unop, x) => {
                x.infer_ty(tcx)?;
                unop.check_ty(&self.span, x).inspect(|ty| {
                    let _ = self.set_ty(ty.clone());
                })
            }
            RVal::BitCast(ty, x) => {
                x.infer_ty(tcx)?;
                check_cast(&self.span, x, ty).inspect(|inferred_ty| {
                    let _ = self.set_ty(inferred_ty.clone());
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
        let Some(x_ty) = &x.ty else {
            panic!("x == {x:?}, x.ty == {:?}", x.ty);
        };
        let y_ty = y.ty.as_ref().unwrap();
        match self {
            Binop::Add | Binop::Sub => self.check_ty_arith(binop_span, x_ty, y_ty),

            Binop::And | Binop::Or | Binop::Shr => self.check_ty_bitwise(binop_span, x_ty, y_ty),

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

    fn check_ty_arith(&self, binop_span: &Span, x_ty: &Ty, y_ty: &Ty) -> TyckResult<Ty> {
        match (x_ty, y_ty) {
            (Ty::Byte, Ty::Byte) | (Ty::Nat, Ty::Nat) | (Ty::Int, Ty::Int) => Ok(x_ty.clone()),
            // Nat or Int +/- Ptr
            (ptr_ty @ Ty::Ptr(_), Ty::Int | Ty::Nat) | (Ty::Int | Ty::Nat, ptr_ty @ Ty::Ptr(_)) => {
                Ok(ptr_ty.clone())
            }
            // Ptr difference
            (Ty::Ptr(x_inner), Ty::Ptr(y_inner))
                if matches!(self, Self::Sub) && x_inner == y_inner =>
            {
                Ok(x_ty.clone())
            }
            _ => Err(TyckErr::BadBinop {
                binop_span: binop_span.clone(),
                binop: *self,
                operand_tys: (x_ty.clone(), y_ty.clone()),
                expected_tys: ("{numeric}".into(), "{numeric}".into()),
            }),
        }
    }

    fn check_ty_bitwise(&self, binop_span: &Span, x_ty: &Ty, y_ty: &Ty) -> TyckResult<Ty> {
        match (x_ty, y_ty) {
            (Ty::Byte, Ty::Byte) | (Ty::Nat, Ty::Nat) | (Ty::Int, Ty::Int) => Ok(x_ty.clone()),
            _ => Err(TyckErr::BadBinop {
                binop_span: binop_span.clone(),
                binop: *self,
                operand_tys: (x_ty.clone(), y_ty.clone()),
                expected_tys: ("{numeric}".into(), "{numeric}".into()),
            }),
        }
    }
}

impl Unop {
    fn check_ty(&self, unop_span: &Span, x: &Ann<RVal>) -> TyckResult<Ty> {
        let x_ty = x.ty.as_ref().unwrap();
        match self {
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

/// I think there should be a semantic cast (do-what-I-mean: +7 <--> 7 <--> 7b)
/// and a transmutation cast (do-what-I-say: -1 <--> 0xFFFF <--> 255b).
///
/// # Examples
///
/// ```txt
/// 65535 as int --> <error: out of bounds>
/// +1 as nat --> 1
///  1 as int --> +1
/// -1 as nat --> <error: out of bounds>
/// -1 as byte --> <error: out of bounds>
/// ```
fn check_cast(cast_span: &Span, x: &Ann<RVal>, ty: &Ann<Ty>) -> TyckResult<Ty> {
    match (x.ty.as_ref().unwrap(), &ty.value) {
        (ty1, ty2) if ty1 == ty2 => Ok(ty1.clone()),
        // Anything should be convertible to int
        (_, Ty::Int) => Ok(Ty::Int),
        (Ty::Byte, Ty::Int) => Ok(Ty::Int),
        (inferred, requested) => Err(TyckErr::BadCast {
            span: *cast_span,
            value_span: x.span,
            ty_span: ty.span,
            inferred_ty: inferred.clone(),
            requested_ty: requested.clone(),
        }),
    }
}

impl Ann<Stmt> {
    fn infer_ty(&mut self, tcx: &mut Tcx) -> TyckResult<()> {
        match &mut self.value {
            Stmt::RVal(rval) => rval.infer_ty(tcx).map(|_| ()),
            Stmt::Let(varname, rhs) => {
                let ty = rhs.infer_ty(tcx)?;
                if let Some(_prev) = tcx.insert(
                    varname.clone(),
                    SymData {
                        ty,
                        sym_kind: IdentKind::Local,
                    },
                ) {
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
                let Some(Ty::Void) = tcx.get_subr_ret_ty() else {
                    return Err(TyckErr::RetVoidInNonVoidSubr { span: self.span });
                };
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
    fn register_in_tcx(&self, tcx: &mut Tcx) -> Option<SymData> {
        tcx.insert(
            self.value.name.clone(),
            SymData {
                ty: self.value.subr_ty(),
                sym_kind: IdentKind::Subr,
            },
        )
    }

    pub fn check_ty(&mut self, tcx: &mut Tcx) -> TyckResult<()> {
        // Ensure self is defined so recursive calls type check.
        self.register_in_tcx(tcx);

        tcx.enter_scope();
        tcx.set_subr_ret_ty(self.value.ret_ty.clone());
        // Define all parameters.
        for (i, param) in self.value.params.iter().enumerate() {
            tcx.insert(
                param.value.name.clone(),
                SymData {
                    ty: param.value.ty.clone(),
                    sym_kind: IdentKind::Local,
                },
            );
        }

        for stmt in &mut self.value.body {
            stmt.infer_ty(tcx)?;
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
            if let Some(_) = subr.register_in_tcx(tcx) {
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
