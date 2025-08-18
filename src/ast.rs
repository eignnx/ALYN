use crate::ty::Ty;
use derive_more::From;
use internment::Intern;

/// Annotated - A `T` annotated with extra data.
#[derive(Debug, Clone)]
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
    fn with_span(self, span: Span) -> Ann<Self> {
        Ann {
            value: self,
            span: span,
            ty: None,
        }
    }
}

impl<T> MakeAnn<T> for T {}

#[derive(Clone, Copy)]
pub struct Span {
    pub byte_start: u32,
    pub byte_len: u32,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "src[{}..{}]",
            self.byte_start,
            self.byte_start + self.byte_len
        )
    }
}

#[derive(Debug)]
pub struct Module {
    filename: String,
    decls: Vec<Ann<SubrDecl>>,
}

#[derive(Debug, Clone)]
pub struct SubrDecl {
    name: Intern<String>,
    params: Vec<Ann<Param>>,
    ret_ty: Ty,
    body: Vec<Ann<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Param(Intern<String>, Ty);

/// `X` type parameter is for x-tra data (like type of the expression, source
/// code span)
#[derive(Debug, Clone, From)]
pub enum Stmt {
    #[from]
    RVal(Ann<RVal>),
    Let(Intern<String>, Ann<RVal>),
    Assign(Ann<LVal>, Ann<RVal>),
    If(RVal, Vec<Stmt>, Option<Vec<Stmt>>),
    While(RVal, Vec<Stmt>),
}

#[derive(Debug, Clone, From)]
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
    Unop(Unop, Box<Ann<LVal>>),
    Call(Intern<String>, Vec<Ann<RVal>>),
}

#[derive(Debug, Clone)]
pub enum LVal {
    Var(Intern<String>),
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
    AddrOf,
    Neg,
}

#[test]
fn asdf() {
    let s = Span {
        byte_start: 0,
        byte_len: 0,
    };
    let _rval: RVal = RVal::LVal(
        LVal::Deref(Box::new(
            RVal::Call(
                Intern::from_ref("f"),
                vec![RVal::Int(123).with_span(s), RVal::Byte(b'$').with_span(s)],
            )
            .with_span(s),
        ))
        .with_span(s),
    );
}
