use crate::ast::*;
use crate::ty::Ty;
use internment::Intern;
use std::collections::HashMap;

type Tcx = HashMap<Intern<String>, SymData>;

#[derive(Debug)]
struct SymData {
    ty: Ty,
}

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
            RVal::Call(intern, anns) => todo!(),
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
