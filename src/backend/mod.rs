use crate::{canon, ir};

pub mod lark;

pub trait Backend {
    type Temporary;
    type Instruction;
    fn stmt_to_asm(&mut self, stmt: canon::Stmt);
    fn expr_to_asm(&mut self, rval: canon::RVal, opt_dst: impl Into<Option<Self::Temporary>>) -> Self::Temporary;
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    fn compile_to_lark(src: &str) -> Vec<lark::Instr> {
        use crate::parse;
        let mut mod_ast = parse::ModuleParser::new().parse("<test>", src).unwrap();
        use crate::ty::Ty;
        let mut tcx = crate::tcx::Tcx::new(Ty::Void);
        mod_ast.check_ty(&mut tcx);

        let mut out = vec![];
        let mut be = lark::LarkBackend::new(&mut out);
        for decl in mod_ast.decls {
            let subr_lbl = crate::names::Lbl::SubrStart(decl.value.name);
            let ir_stmts = decl.value.to_ir().into_iter().map(|w| w.as_stmt()).collect();
            let stmts = canon::canonicalize(subr_lbl, ir_stmts);
            for stmt in stmts {
                be.stmt_to_asm(stmt);
            }
        }
        be.render().to_vec()
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
