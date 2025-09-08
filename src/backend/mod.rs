use crate::ir;

pub mod lark;

pub trait Backend<OutStream>: Default
where
    OutStream: Extend<Self::Instruction>,
{
    type Temporary;
    type Instruction;
    fn stmt_to_asm(&mut self, out: &mut OutStream, stmt: ir::Stmt);
    fn rval_to_asm(
        &mut self,
        out: &mut OutStream,
        dest: impl Into<Self::Temporary>,
        rval: ir::RVal,
    );
    fn fresh_temp(&mut self, out: &mut OutStream) -> Self::Temporary;
}

pub trait Capabilities {
    const WORD_SIZE: usize;
    /// Has hardware (instruction) support for integer division.
    const HW_DIV: bool;
    /// Has hardware (instruction) support for integer multiplication.
    const HW_MUL: bool;
    /// Has hardware (instruction) support for integer modulo operation.
    const HW_MOD: bool;
}

pub fn run_backend<Tmp, Instr, Out, B>(backend: &mut B, out: &mut Out, stmt: ir::Stmt)
where
    B: Backend<Out, Instruction = Instr, Temporary = Tmp>,
    Out: Extend<<B as Backend<Out>>::Instruction>,
{
    backend.stmt_to_asm(out, stmt)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    fn compile_to_lark(src: &str) -> Vec<lark::Instr> {
        let mut be = lark::LarkBackend::default();
        use crate::parse;
        let mut mod_ast = parse::ModuleParser::new().parse("<test>", src).unwrap();
        use crate::ty::Ty;
        let mut tcx = crate::tcx::Tcx::new(Ty::Void);
        mod_ast.check_ty(&mut tcx);

        let mut out = vec![];
        for decl in mod_ast.decls {
            let ir = decl.value.to_ir();
            for ir_wrap in ir {
                be.stmt_to_asm(&mut out, ir_wrap.as_stmt());
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
}
