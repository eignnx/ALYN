use std::fmt::{Display, Debug, Result, Formatter};
use super::*;

impl<T: Debug> Debug for Ann<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(ty) = &self.ty {
            write!(f, "{:?}:{ty}", self.value)
        } else {
            write!(f, "{:?}", self.value)
        }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Debug for RVal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Byte(x) => write!(f, "{x}b"),
            Self::Nat(x) => write!(f, "{x}"),
            Self::Int(x) if *x >= 0 => write!(f, "+{x}"),
            Self::Int(x) => write!(f, "-{x}"),
            Self::LVal(lval) => write!(f, "{lval:?}"),
            Self::Binop(op, x, y) => write!(f, "({x:?} {op} {y:?})"),
            Self::Unop(op, x) => write!(f, "({op} {x:?})"),
            Self::Call(arg0, arg1) => f.debug_tuple("Call").field(arg0).field(arg1).finish(),
        }
    }
}

impl Debug for LVal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Var(ident, Some(kind)) => write!(f, "{ident}${kind:?}"),
            Self::Var(ident, None) => write!(f, "{ident}"),
            Self::Deref(rval) => write!(f, "{rval:?}"),
        }
    }
}

impl Display for Binop {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Binop::Add => "+",
            Binop::Sub => "-",
            Binop::Eq => "==",
            Binop::Ne => "==",
            Binop::Lt => "<",
            Binop::Gt => ">",
            Binop::Lte => "<=",
            Binop::Gte => ">=",
        })
    }
}

impl Display for Unop {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Unop::AddrOf => "&",
            Unop::Neg => "-",
        })
    }
}
