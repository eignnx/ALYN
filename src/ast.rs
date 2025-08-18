use std::rc::Rc;
use derive_more::From;
use internment::Intern;

#[derive(Debug, Clone, From)]
pub enum Stmt {
    #[from]
    RVal(RVal),
    Assign(LVal, RVal),
    If(RVal, Vec<Stmt>, Option<Vec<Stmt>>),
    While(RVal, Vec<Stmt>),
}

#[derive(Debug, Clone, From)]
pub enum RVal {
    #[from]
    Int(i64),
    #[from]
    LVal(LVal),
    Binop(Binop, Rc<RVal>, Rc<RVal>),
    Relop(Relop, Rc<RVal>, Rc<RVal>),
    AddrOf(Rc<LVal>),
    Call(Intern<String>, Vec<RVal>),
}

#[derive(Debug, Clone)]
pub enum LVal {
    Var(Intern<String>),
    Deref(Rc<RVal>),
}

#[derive(Debug, Clone)]
pub enum Binop {
    Add, Sub,
}

#[derive(Debug, Clone)]
pub enum Relop {
    Eq, Ne, Lt, Gt, Lte, Gte
}

#[test]
fn asdf() {
    let _rval = RVal::LVal(LVal::Deref(Rc::new(
        RVal::Call(Intern::new("f"), vec![RVal::Int(123), RVal::Int(456)])
    )));
}
