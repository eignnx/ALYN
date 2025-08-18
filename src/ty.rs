use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    Void,
    Bool,
    Byte,
    Nat,
    Int,
    Ptr(Box<Ty>),
    Subr(Vec<Ty>, Box<Ty>),
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Void => write!(f, "void"),
            Ty::Bool => write!(f, "bool"),
            Ty::Byte => write!(f, "byte"),
            Ty::Nat => write!(f, "nat"),
            Ty::Int => write!(f, "int"),
            Ty::Ptr(ty) => write!(f, "*{ty}"),
            Ty::Subr(param_tys, ret_ty) => {
                write!(f, "subr(")?;
                for (i, param_ty) in param_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{param_ty}")?;
                }
                write!(f, ")")?;
                if !matches!(ret_ty.as_ref(), Ty::Void) {
                    write!(f, " -> {ret_ty}")?;
                }
                Ok(())
            }
        }
    }
}
