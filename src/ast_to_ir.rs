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
            ast::RVal::Unop(unop, ann) => todo!(),
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
                IdentKind::Subr => IrWrap::RVal(ir::RVal::Lbl(Lbl::SubrStart(ident))), // TODO: mangle subr name here?
                IdentKind::Param(idx) => make_lval(ir::LVal::Param(idx)),
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
        let arg_ty = e1.ty.as_ref().unwrap().clone();
        let e1 = e1.to_ir().as_expr();
        let e2 = e2.to_ir().as_expr();

        fn make_cond(op: Relop, e1: ir::RVal, e2: ir::RVal) -> IrWrap {
            IrWrap::Cond(Box::new(move |t, f| ir::Stmt::Br {
                op,
                e1: e1.clone(),
                e2: e1.clone(),
                if_true: t,
                if_false: f,
            }))
        }

        match arg_ty {
            // Treat like unsigned word:
            Ty::Bool | Ty::Byte | Ty::Nat | Ty::Ptr(..) | Ty::Subr(..) => match self {
                ast::Binop::Add => {
                    IrWrap::RVal(ir::RVal::Binop(ir::Binop::Add, Box::new(e1), Box::new(e2)))
                }
                ast::Binop::Sub => {
                    IrWrap::RVal(ir::RVal::Binop(ir::Binop::Sub, Box::new(e1), Box::new(e2)))
                }

                ast::Binop::Eq => make_cond(Relop::Eq, e1, e2),
                ast::Binop::Ne => make_cond(Relop::Ne, e1, e2),
                ast::Binop::Lt => make_cond(Relop::LtU, e1, e2),
                ast::Binop::Gt => make_cond(Relop::GtU, e1, e2),
                ast::Binop::Lte => make_cond(Relop::LteU, e1, e2),
                ast::Binop::Gte => make_cond(Relop::GteU, e1, e2),
            },
            // Treat as signed word:
            Ty::Int => match self {
                ast::Binop::Add => {
                    IrWrap::RVal(ir::RVal::Binop(ir::Binop::Add, Box::new(e1), Box::new(e2)))
                }
                ast::Binop::Sub => {
                    IrWrap::RVal(ir::RVal::Binop(ir::Binop::Sub, Box::new(e1), Box::new(e2)))
                }

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
                let end = Lbl::fresh("end_while");

                let stmts = std::iter::empty()
                    .chain([ir::Stmt::Lbl(cond_lbl)])
                    .chain([cond(body_lbl, end)])
                    .chain([ir::Stmt::Lbl(body_lbl)])
                    .chain(to_ir_stmts(body))
                    .chain([ir::Stmt::direct_jmp(cond_lbl)])
                    .chain([ir::Stmt::Lbl(end)]);

                IrWrap::Stmt(ir::Stmt::seq(stmts))
            }
            ast::Stmt::Ret(None) => todo!(),
            ast::Stmt::Ret(Some(rval)) => todo!(),
        }
    }
}

impl ast::SubrDecl {
    fn to_ir(self) -> IrWrap {
        todo!()
    }
}
