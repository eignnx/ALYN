use derive_more::{Debug, Display, From};

use crate::{canon, ir};
use alyn_common::names::Tmp;

/// A storage node
#[derive(From, Clone, Copy, Display, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Stg<R> {
    #[display("{_0}")]
    #[debug("{_0:?}")]
    #[from]
    Tmp(Tmp),
    #[display("{_0}")]
    #[debug("{_0:?}")]
    Reg(R),
}

impl<R> Stg<R> {
    pub fn from_reg(reg: R) -> Self {
        Self::Reg(reg)
    }

    pub fn try_as_tmp(self) -> Option<Tmp> {
        match self {
            Self::Tmp(tmp) => Some(tmp),
            Self::Reg(_) => None,
        }
    }

    pub fn try_as_reg(self) -> Option<R> {
        match self {
            Self::Tmp(_) => None,
            Self::Reg(reg) => Some(reg),
        }
    }
}

/// Something that can perform instruction selection.
pub trait Select {
    /// The type that represents a CPU register.
    type Register: std::fmt::Debug;
    type Instruction: Clone;
    fn stmt_to_asm(&mut self, stmt: canon::Stmt);
    fn expr_to_asm(
        &mut self,
        rval: canon::RVal,
        opt_dst: impl Into<Option<Stg<Self::Register>>>,
    ) -> Stg<Self::Register>;
    fn render(&mut self) -> impl Iterator<Item = Self::Instruction>;
}

#[cfg(test)]
mod tests {
    use crate::{ast::Item, backends::hobby::lark};

    use super::*;
    use insta::assert_debug_snapshot;

    fn compile_to_lark(src: &str) -> Vec<lark::Instr> {
        use crate::parse;
        let mut mod_ast = parse::ModuleParser::new().parse("<test>", src).unwrap();
        use crate::ty::Ty;
        let mut tcx = crate::tcx::Tcx::new(Ty::Void);
        mod_ast.check_ty(&mut tcx);

        let mut be = lark::LarkInstrSel::new();
        for subr in mod_ast.subr_defns() {
            let subr_lbl = crate::names::Lbl::SubrStart(subr.value.name);
            let ir_stmts = subr
                .value
                .clone()
                .to_ir()
                .into_iter()
                .map(|w| w.as_stmt())
                .collect();
            let stmts = canon::canonicalize(subr_lbl, ir_stmts);
            eprintln!("{stmts:#?}");
            for stmt in stmts {
                be.stmt_to_asm(stmt);
            }
        }
        be.render().collect()
    }

    #[test]
    fn basic() {
        assert_debug_snapshot!(compile_to_lark(
            "
            subr main() nat {
                ret 0;
            }
        "
        ));
    }

    #[test]
    fn binop() {
        assert_debug_snapshot!(compile_to_lark(
            "
            subr main() nat {
                let a = 24;
                ret 1 + a - 49;
            }
        "
        ));
    }

    #[test]
    fn simple_if() {
        assert_debug_snapshot!(compile_to_lark(
            "
            subr main() nat {
                if 1 == 1 {
                    let x = 1;
                } else {
                    let x = 2;
                }
            }
        "
        ));
    }

    #[test]
    fn simple_while() {
        assert_debug_snapshot!(compile_to_lark(
            "
            subr main() nat {
                let x = 1;
                while x < 10 {
                    x = x + 1;
                }
            }
        "
        ));
    }

    #[test]
    fn nested_call() {
        assert_debug_snapshot!(compile_to_lark(
            "
            subr add3(a: nat, b: nat, c: nat) nat {
                ret a + b + c;
            }

            subr main() nat {
                ret add3(1, add3(9, 8, 7), 2 + 3);
            }
        "
        ));
    }
}
