use crate::ir;

pub mod lark;

pub trait Backend<OutStream>
where
    OutStream: Extend<Self::Instruction>,
{
    type Temporary;
    type Instruction;
    fn create_backend(out: &mut OutStream) -> Self;
    fn stmt_to_asm(&mut self, stmt: ir::Stmt);
    fn expr_to_asm(&mut self, rval: ir::RVal) -> Self::Temporary;
}

pub fn run_backend<B, Tmp, Instr, Out>(out: &mut Out, stmt: ir::Stmt)
where
    B: Backend<Out, Instruction = Instr, Temporary = Tmp>,
    Out: Extend<<B as Backend<Out>>::Instruction>,
{
    let mut backend = B::create_backend(out);
    backend.stmt_to_asm(stmt)
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
            let ir = decl.value.to_ir();
            for ir_wrap in ir {
                be.stmt_to_asm(ir_wrap.as_stmt());
            }
        }
        out
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
}
