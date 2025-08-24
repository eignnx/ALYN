use crate::{sym::IdentKind, ty::Ty};
use derive_more::From;
use internment::Intern;

mod impls;

/// Annotated - A `T` annotated with extra data.
#[derive(Clone)]
pub struct Ann<T> {
    pub value: T,
    pub span: Span,
    pub ty: Option<Ty>,
}

impl<T> Ann<T> {
    pub fn set_ty(&mut self, ty: Ty) -> Ty {
        self.ty = Some(ty.clone());
        ty
    }
}

pub trait MakeAnn<T>: Sized {
    fn with_span(self, span: impl Into<Span>) -> Ann<Self> {
        Ann {
            value: self,
            span: span.into(),
            ty: None,
        }
    }
}

impl<T> MakeAnn<T> for T {}

#[derive(Clone, Copy)]
pub struct Span {
    start: usize,
    end: usize,
}

impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub filename: String,
    pub decls: Vec<Ann<SubrDecl>>,
}

#[derive(Debug, Clone)]
pub struct SubrDecl {
    pub name: Intern<String>,
    pub params: Vec<Ann<Param>>,
    pub ret_ty: Ty,
    pub body: Vec<Ann<Stmt>>,
}

impl SubrDecl {
    pub fn subr_ty(&self) -> Ty {
        let mut param_tys = vec![];
        for param in &self.params {
            param_tys.push(param.ty.as_ref().cloned().unwrap());
        }
        Ty::Subr(param_tys, Box::new(self.ret_ty.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Intern<String>,
    pub ty: Ty,
}

/// `X` type parameter is for x-tra data (like type of the expression, source
/// code span)
#[derive(Clone, From)]
pub enum Stmt {
    #[from]
    RVal(Ann<RVal>),
    Let(Intern<String>, Ann<RVal>),
    Assign(Ann<LVal>, Ann<RVal>),
    If(Ann<RVal>, Vec<Ann<Stmt>>, Option<Vec<Ann<Stmt>>>),
    While(Ann<RVal>, Vec<Ann<Stmt>>),
    Ret(Option<Ann<RVal>>),
}

#[derive(Clone, From)]
pub enum RVal {
    #[from]
    Byte(u8),
    #[from]
    Nat(u64),
    #[from]
    Int(i64),
    #[from]
    LVal(Ann<LVal>),
    Binop(Binop, Box<Ann<RVal>>, Box<Ann<RVal>>),
    Unop(Unop, Box<Ann<RVal>>),
    AddrOf(Box<Ann<LVal>>),
    Call(Intern<String>, Vec<Ann<RVal>>),
}

#[derive(Clone)]
pub enum LVal {
    Var(Intern<String>, Option<IdentKind>),
    Deref(Box<Ann<RVal>>),
}

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug, Clone, Copy)]
pub enum Unop {
    Neg,
}

#[test]
fn asdf() {
    let s = 0..0;
    let _rval: RVal = RVal::LVal(
        LVal::Deref(Box::new(
            RVal::Call(
                Intern::from_ref("f"),
                vec![RVal::Int(123).with_span(s.clone()), RVal::Byte(b'$').with_span(s.clone())],
            )
            .with_span(s.clone()),
        ))
        .with_span(s),
    );
}
