use crate::{
    ast::{self, Ann},
    ir::{self, IrWrap, Relop},
    names::{self, Lbl},
    sym::IdentKind,
    ty::Ty,
};

impl Ann<ast::RVal> {
    fn to_ir(self) -> IrWrap {
        match self.value {
            ast::RVal::Byte(x) => IrWrap::RVal(ir::RVal::Byte(x)),
            ast::RVal::Nat(x) => IrWrap::RVal(ir::RVal::Nat(x)),
            ast::RVal::Int(x) => IrWrap::RVal(ir::RVal::Int(x)),
            ast::RVal::LVal(lval) => lval.to_ir(),
            ast::RVal::Binop(binop, x, y) => binop.to_ir(*x, *y),
            ast::RVal::Unop(op, rval) => op.to_ir(*rval),
            ast::RVal::BitCast(ty, rval) => IrWrap::RVal(ir::RVal::BitCast(
                ty.value,
                Box::new(rval.to_ir().as_expr()),
            )),
            ast::RVal::AddrOf(x) => {
                let ir = x.to_ir();
                todo!()
            }
            ast::RVal::Call(subr_name, args) => {
                let mut ir_args = vec![];
                for arg in args {
                    ir_args.push(Box::new(arg.to_ir().as_expr()));
                }
                IrWrap::RVal(ir::RVal::Call(
                    Box::new(ir::RVal::Lbl(Lbl::SubrStart(subr_name))),
                    ir_args,
                ))
            }
        }
    }
}

impl Ann<ast::LVal> {
    fn to_ir(self) -> IrWrap {
        let make_lval = |lval| IrWrap::RVal(ir::RVal::LVal(lval));
        match self.value {
            ast::LVal::Var(ident, ident_kind) => match ident_kind.unwrap() {
                IdentKind::Subr => IrWrap::RVal(ir::RVal::Lbl(Lbl::SubrStart(ident))),
                IdentKind::Local => make_lval(ir::LVal::Tmp(names::Tmp(ident))),
                IdentKind::Global => make_lval(ir::LVal::Global(ident)),
                IdentKind::SubrRet => unreachable!(),
            },
            ast::LVal::Deref(rval) => make_lval(ir::LVal::Mem(Box::new(rval.to_ir().as_expr()))),
        }
    }
}

impl ast::Binop {
    fn to_ir(self, e1: Ann<ast::RVal>, e2: Ann<ast::RVal>) -> IrWrap {
        let Some(arg_ty) = e1.ty.clone() else {
            panic!("Type is None on node: {e1:?}");
        };
        let e1 = e1.to_ir().as_expr();
        let e2 = e2.to_ir().as_expr();

        fn make_cond(op: Relop, e1: ir::RVal, e2: ir::RVal) -> IrWrap {
            IrWrap::Cond(Box::new(move |t, f| ir::Stmt::Br {
                op,
                e1: e1.clone(),
                e2: e2.clone(),
                if_true: t,
                if_false: f,
            }))
        }

        fn make_binop(op: ir::Binop, e1: ir::RVal, e2: ir::RVal) -> IrWrap {
            IrWrap::RVal(ir::RVal::Binop(op, Box::new(e1), Box::new(e2)))
        }

        match arg_ty {
            // Treat like unsigned word:
            Ty::Bool | Ty::Byte | Ty::Nat | Ty::Ptr(..) | Ty::Subr(..) => match self {
                ast::Binop::Add => make_binop(ir::Binop::Add, e1, e2),
                ast::Binop::Sub => make_binop(ir::Binop::Sub, e1, e2),
                ast::Binop::And => make_binop(ir::Binop::And, e1, e2),
                ast::Binop::Or => make_binop(ir::Binop::Or, e1, e2),
                ast::Binop::Shr => make_binop(ir::Binop::Shr, e1, e2),
                ast::Binop::Eq => make_cond(Relop::Eq, e1, e2),
                ast::Binop::Ne => make_cond(Relop::Ne, e1, e2),
                ast::Binop::Lt => make_cond(Relop::LtU, e1, e2),
                ast::Binop::Gt => make_cond(Relop::GtU, e1, e2),
                ast::Binop::Lte => make_cond(Relop::LteU, e1, e2),
                ast::Binop::Gte => make_cond(Relop::GteU, e1, e2),
            },
            // Treat as signed word:
            Ty::Int => match self {
                ast::Binop::Add => make_binop(ir::Binop::Add, e1, e2),
                ast::Binop::Sub => make_binop(ir::Binop::Sub, e1, e2),
                ast::Binop::And => make_binop(ir::Binop::And, e1, e2),
                ast::Binop::Or => make_binop(ir::Binop::Or, e1, e2),
                ast::Binop::Shr => make_binop(ir::Binop::Shr, e1, e2),
                ast::Binop::Eq => make_cond(Relop::Eq, e1, e2),
                ast::Binop::Ne => make_cond(Relop::Ne, e1, e2),
                ast::Binop::Lt => make_cond(Relop::Lt, e1, e2),
                ast::Binop::Gt => make_cond(Relop::Gt, e1, e2),
                ast::Binop::Lte => make_cond(Relop::Lte, e1, e2),
                ast::Binop::Gte => make_cond(Relop::Gte, e1, e2),
            },
            Ty::Void => unreachable!(),
        }
    }
}

impl ast::Unop {
    fn to_ir(self, rval: Ann<ast::RVal>) -> IrWrap {
        match self {
            ast::Unop::Neg => IrWrap::RVal(ir::RVal::Unop(
                ir::Unop::Neg,
                Box::new(rval.to_ir().as_expr()),
            )),
        }
    }
}

fn to_ir_stmts<I>(stmts: I) -> impl DoubleEndedIterator<Item = ir::Stmt>
where
    I: IntoIterator<Item = Ann<ast::Stmt>>,
    I::IntoIter: DoubleEndedIterator,
{
    stmts.into_iter().map(|s| s.to_ir().as_stmt())
}

impl Ann<ast::Stmt> {
    fn to_ir(self) -> IrWrap {
        match self.value {
            ast::Stmt::RVal(rval) => IrWrap::Stmt(rval.to_ir().as_stmt()),
            ast::Stmt::Let(lhs, rhs) => {
                let lhs = ir::LVal::Tmp(names::Tmp(lhs));
                IrWrap::Stmt(ir::Stmt::Move(lhs, rhs.to_ir().as_expr()))
            }
            ast::Stmt::Assign(lhs, rhs) => {
                let ir::RVal::LVal(lhs) = lhs.to_ir().as_expr() else {
                    panic!("Gotta be a lvalue here")
                };
                IrWrap::Stmt(ir::Stmt::Move(lhs, rhs.to_ir().as_expr()))
            }
            ast::Stmt::If(cond, true_br, false_br) => {
                let cond = cond.to_ir().as_cond();
                let if_true = Lbl::fresh("if_true");
                let if_false = Lbl::fresh("if_false");
                let end = Lbl::fresh("end_if");

                let stmts = std::iter::empty()
                    .chain([cond(if_true, if_false)])
                    .chain([ir::Stmt::Lbl(if_true)])
                    .chain(to_ir_stmts(true_br))
                    .chain([ir::Stmt::direct_jmp(end)])
                    .chain([ir::Stmt::Lbl(if_false)])
                    .chain(to_ir_stmts(false_br.unwrap_or_default()))
                    .chain([ir::Stmt::Lbl(end)]);

                IrWrap::Stmt(ir::Stmt::seq(stmts))
            }
            ast::Stmt::While(cond, body) => {
                let cond = cond.to_ir().as_cond();
                let cond_lbl = Lbl::fresh("while_cond");
                let body_lbl = Lbl::fresh("while_body");
                let end_lbl = Lbl::fresh("end_while");

                let stmts = std::iter::empty()
                    .chain([ir::Stmt::Lbl(cond_lbl)])
                    .chain([cond(body_lbl, end_lbl)])
                    .chain([ir::Stmt::Lbl(body_lbl)])
                    .chain(to_ir_stmts(body))
                    .chain([ir::Stmt::direct_jmp(cond_lbl)])
                    .chain([ir::Stmt::Lbl(end_lbl)]);

                IrWrap::Stmt(ir::Stmt::seq(stmts))
            }
            ast::Stmt::Ret(None) => IrWrap::Stmt(ir::Stmt::Ret(None)),
            ast::Stmt::Ret(Some(rval)) => IrWrap::Stmt(ir::Stmt::Ret(Some(rval.to_ir().as_expr()))),
        }
    }
}

impl ast::SubrDecl {
    pub fn to_ir(self) -> Vec<IrWrap> {
        let mut ir = vec![];
        for stmt in self.body {
            ir.push(stmt.to_ir());
        }
        ir
    }
}

#[cfg(test)]
mod test {
    use crate::{ast::MakeAnn, ir, parse, tcx::Tcx, ty::Ty};
    use insta::assert_debug_snapshot;

    fn dyn_debug<'a, T>(x: T) -> Box<dyn std::fmt::Debug + 'a>
    where
        T: std::fmt::Debug + 'a,
    {
        Box::new(x)
    }

    fn parse_and_convert(src: &str) -> Result<Vec<ir::Stmt>, impl std::fmt::Debug> {
        crate::names::reset_name_ids();
        let decl = parse::SubrDeclParser::new()
            .parse("<test>", src)
            .map_err(dyn_debug)?;
        let mut decl = decl.with_span(0..100);
        let mut tcx = Tcx::new(Ty::Void);
        decl.check_ty(&mut tcx).map_err(dyn_debug)?;
        Ok(decl
            .value
            .to_ir()
            .into_iter()
            .map(|w| w.as_stmt())
            .collect::<Vec<_>>())
        .map_err(dyn_debug::<()>)
    }

    macro_rules! do_test {
        ($src:expr) => {
            assert_debug_snapshot!(parse_and_convert($src))
        };
    }

    #[test]
    fn simple_if() {
        do_test!(
            "
            subr main() {
                if 1 < 2 {
                    123;
                } else {
                    456;
                }
            }
        "
        );
    }

    #[test]
    fn subr_params() {
        do_test!(
            "
            subr min(a: int, b: int) int {
                if a < b {
                    ret a;
                } else {
                    ret b;
                }
            }
        "
        );
    }

    #[test]
    fn fib() {
        do_test!(
            "
            subr fib(n: nat) nat {
                if n < 2 {
                    ret n;
                } else {
                    ret fib(n - 1) + fib(n - 2);
                }
            }
        "
        );
    }

    #[test]
    fn matsum() {
        do_test!(
            "
            subr matsum(n: nat, a: **int, b: **int, c: **int) {
                let col = 0;
                while col < n {
                    let row = 0;
                    while row < n {
                        let ax = *(*(a + col) + row);
                        let bx = *(*(b + col) + row);
                        *(*(c + col) + row) = ax + bx;
                        row = row + 1;
                    }
                    col = col + 1;
                }
            }
        "
        );
    }

    #[test]
    fn shiftmul() {
        do_test!(
            "
            subr shiftmul(x: int, n: int) int {
                if n < +0 {
                    x = - x;
                    n = - n;
                }
                if n == +0 { ret +0; }
                let acc = +0;
                while n > +1 {
                    if n & +1 == +1 {
                        acc = acc + x;
                        n = n - +1;
                    }
                    x = x + x;
                    n = n >> +1;
                }
                ret x + acc;
            }
        "
        );
    }

    #[test]
    fn knr_binsearch() {
        // int binsearch(int x, int v[], int n) {
        //     int low = 0;
        //     int high = n - 1;
        //     while (low <= high) {
        //          int mid = (low + high) / 2;
        //          int elem = v[mid];
        //          if (x < elem)
        //              high = mid - 1;
        //          else if (x > elem)
        //              low = mid + 1;
        //          else
        //              return mid;
        //     }
        //     return -1;
        // }
        do_test!("
            subr binsearch(x: int, v: *int, n: nat) int {
                let low = 0;
                let high = n - 1;
                while low <= high {
                    let mid = (low + high) >> 1;
                    let elem = *(v + mid);
                    if x < elem {
                        high = mid - 1;
                    }
                    if x > elem {
                        low = mid + 1;
                    } else {
                        ret bitcast{int}(mid);
                    }
                }
                ret -1;
            }
        ");
    }

    #[test]
    fn simple_while() {
        do_test!("
            subr test_simple_while(n: nat) {
                while n < 100 {
                    n = n + 1;
                }
                ret;
            }
        ");
    }
}
