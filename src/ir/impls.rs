use super::{Binop, IrWrap, LVal, RVal, Relop, Stmt, Unop};
use std::fmt::{Debug, Display, Formatter, Result};

impl Debug for LVal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Param(id) => write!(f, "#{id}"),
            Self::Tmp(tmp) => write!(f, "${tmp:?}"),
            Self::Mem(rval) => write!(f, "M[{rval:?}]"),
            Self::Global(name) => write!(f, "@{name}"),
        }
    }
}

impl Debug for RVal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Byte(x) => write!(f, "{x}b"),
            Self::Nat(x) => write!(f, "{x}"),
            Self::Int(x) => write!(f, "{x:+}"),
            Self::Lbl(lbl) => write!(f, ":{lbl:?}"),
            Self::LVal(lval) => write!(f, "{lval:?}"),
            Self::Binop(op, x, y) => write!(f, "({x:?} {op} {y:?})"),
            Self::Unop(op, x) => write!(f, "({op} {x:?})"),
            Self::Call(subr, args) => {
                write!(f, "{subr:?}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg:?}")?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Self::Seq(first, second) => {
                write!(f, "{first:?};\n")?;
                write!(f, "{second:?}")?;
                Ok(())
            }
        }
    }
}

impl Display for Binop {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Binop::Add => write!(f, "+"),
            Binop::Sub => write!(f, "-"),
            Binop::And => write!(f, "&"),
            Binop::Or => write!(f, "|"),
            Binop::Shr => write!(f, ">>"),
            Binop::Xor => write!(f, "^"),
        }
    }
}

impl Display for Relop {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Relop::Eq => write!(f, "=="),
            Relop::Ne => write!(f, "!="),
            Relop::Gt => write!(f, ">"),
            Relop::Lt => write!(f, "<"),
            Relop::Gte => write!(f, ">="),
            Relop::Lte => write!(f, "<="),
            Relop::GtU => write!(f, "u(>)"),
            Relop::LtU => write!(f, "u(<)"),
            Relop::GteU => write!(f, "u(>=)"),
            Relop::LteU => write!(f, "u(<=)"),
        }
    }
}

impl Display for Unop {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Unop::Neg => write!(f, "-"),
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Stmt::Move(lval, rval) => write!(f, "{lval:?} <- {rval:?}"),
            Stmt::RVal(rval) => write!(f, "_ <- {rval:?}"),
            Stmt::Jmp(rval, lbls) => match &lbls[..] {
                [] => write!(f, "switch {rval:?} {{ }}"),
                [res_lbl] => match rval {
                    RVal::Lbl(cond_lbl) if cond_lbl == res_lbl => write!(f, "jmp {res_lbl:?}"),
                    other => panic!("Jmp({rval:?}, [{res_lbl:?}]) doesn't make sense!"),
                },
                lbls => {
                    write!(f, "switch {rval:?} {{\n")?;
                    for lbl in lbls {
                        write!(f, "    {lbl:?},")?;
                    }
                    write!(f, "}}")?;
                    Ok(())
                }
            },
            Stmt::Br {
                op,
                e1,
                e2,
                if_true,
                if_false,
            } => {
                write!(
                    f,
                    "if {e1:?} {op} {e2:?} then {if_true:?} else {if_false:?}"
                )
            }
            Stmt::Seq(first, second) => {
                write!(f, "{first:?};\n")?;
                write!(f, "{second:?}")?;
                Ok(())
            }
            Stmt::Lbl(lbl) => write!(f, ":{lbl:?}"),
            Stmt::Nop => write!(f, "nop"),
            Stmt::Ret(None) => write!(f, "ret"),
            Stmt::Ret(Some(x)) => write!(f, "ret {x:?}"),
        }
    }
}
