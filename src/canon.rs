//! Canonicalize IR

pub mod flatten {
    use crate::{ir::{LVal, RVal, Stmt}, names::Tmp};

    pub fn flatten_stmt(stmt: Stmt, out: &mut Vec<Stmt>) {
        match stmt {
            Stmt::Move(lval, rval) => {
                let lval = flatten_lval(lval, out);
                let rval = flatten_rval(rval, out);
                out.push(Stmt::Move(lval, rval));
            }
            Stmt::RVal(rval) => {
                let rval = flatten_rval(rval, out);
                out.push(Stmt::RVal(rval));
            }
            Stmt::Jmp(rval, lbls) => {
                let rval = flatten_rval(rval, out);
                out.push(Stmt::Jmp(rval, lbls));
            }
            Stmt::Br { op, e1, e2, if_true, if_false } => {
                let e1 = flatten_rval(e1, out);
                let e2 = flatten_rval(e2, out);
                out.push(Stmt::Br { op, e1, e2, if_true, if_false });
            }
            Stmt::Seq(stmt1, stmt2) => {
                flatten_stmt(*stmt1, out);
                flatten_stmt(*stmt2, out);
                // NOTE: we remove Stmt::Seq's entirely
            }
            Stmt::Lbl(_) | Stmt::Nop | Stmt::Ret(None) => out.push(stmt),
            Stmt::Ret(Some(rval)) => {
                let rval = flatten_rval(rval, out);
                out.push(Stmt::Ret(Some(rval)));
            }
        }
    }

    fn flatten_rval(rval: RVal, out: &mut Vec<Stmt>) -> RVal {
        match rval {
            RVal::Byte(_) | RVal::Nat(_) | RVal::Int(_) | RVal::Lbl(_) => rval,
            RVal::LVal(lval) => RVal::LVal(flatten_lval(lval, out)),
            RVal::Binop(binop, x, y) => {
                let x = flatten_rval(*x, out);
                let y = flatten_rval(*y, out);
                RVal::Binop(binop, Box::new(x), Box::new(y))
            },
            RVal::Call(func @ RVal::Lbl(_), args) => {
                let mut new_args = Vec::new();
                for arg in args {
                    new_args.push(Box::new(flatten_rval(*arg, out)));
                }
                let call_res: LVal = Tmp::fresh("ret_val").into();
                out.push(Stmt::Move(call_res.clone(), RVal::Call(func, new_args)));
                call_res.into()
            }
            RVal::Call(indirect_func, args) => todo!("handle indirect function calls"),
            RVal::Unop(unop, rval) => {
                let rval = flatten_rval(*rval, out);
                RVal::Unop(unop, Box::new(rval))
            }
            RVal::BitCast(ty, rval) => {
                let rval = flatten_rval(*rval, out);
                RVal::BitCast(ty, Box::new(rval))
            }
            RVal::Seq(stmt, rval) => {
                flatten_stmt(*stmt, out);
                flatten_rval(*rval, out)
            }
        }
    }

    fn flatten_lval(lval: LVal, out: &mut Vec<Stmt>) -> LVal {
        match lval {
            LVal::Tmp(tmp) => lval,
            LVal::Mem(rval) => LVal::Mem(Box::new(flatten_rval(*rval, out))),
            LVal::Global(intern) => todo!(),
        }
    }
}

pub mod basic_blocks {
    use std::{collections::BTreeMap, mem};

    use crate::{ir::{LVal, RVal, Relop, Stmt}, names::{Lbl, Tmp}};

    // Basic Block
    #[derive(Debug, PartialEq)]
    pub struct Bb {
        pub stmts: Vec<Stmt>,
        pub last: Stmt,
    }

    impl Bb {
        pub fn successors(&self) -> impl DoubleEndedIterator<Item = Lbl> {
            let mut succs = Vec::new();
            match &self.last {
                Stmt::Br { if_false, if_true, .. } => {
                    succs.push(*if_false); // We want these visited first
                    succs.push(*if_true);
                }
                Stmt::Jmp(_, lbls) => {
                    succs.extend(lbls.iter().copied());
                }
                _ => {}
            }
            succs.into_iter()
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Bbs {
        pub(super) entry: Lbl,
        pub(super) blocks: BTreeMap<Lbl, Bb>,
    }

    impl Bbs {
        pub fn labels(&self) -> impl Iterator<Item = Lbl> {
            self.blocks.keys().cloned()
        }

        pub fn entry(&self) -> Lbl {
            self.entry
        }

        #[track_caller]
        pub fn pop_bb(&mut self, block_lbl: Lbl) -> Bb {
            self.blocks.remove(&block_lbl).unwrap()
        }

        #[track_caller]
        pub fn get_any_bb_lbl(&mut self) -> Lbl {
            *self.blocks.keys().next().unwrap()
        }
    }

    #[derive(Debug, Clone, Copy)]
    enum State {
        CollectingStmts,
        DroppingTillLbl,
    }

    pub fn group_into_bbs(stmts: Vec<Stmt>) -> Bbs {
        let mut current_bb = Vec::new();
        let entry = Lbl::fresh("subr_entry");
        let mut current_bb_label = entry;
        current_bb.push(Stmt::Lbl(entry));
        let mut bbs = Bbs {
            entry,
            blocks: BTreeMap::new(),
        };

        let mut state = State::CollectingStmts;
        let mut stmts = stmts.into_iter();

        'main: while let Some(mut stmt) = stmts.next() {
            match state {
                State::CollectingStmts => {
                    match stmt {
                        Stmt::Lbl(lbl) => {
                            // End current block, insert jump to next one, create next one.
                            bbs.blocks.insert(current_bb_label, Bb {
                                stmts: mem::replace(&mut current_bb, Vec::new()),
                                last: Stmt::direct_jmp(lbl),
                            });
                            current_bb_label = lbl;
                            current_bb.push(Stmt::Lbl(lbl));
                        },

                        Stmt::Br { .. } | Stmt::Jmp(..) | Stmt::Ret(..) => {
                            bbs.blocks.insert(current_bb_label, Bb {
                                stmts: mem::replace(&mut current_bb, Vec::new()),
                                last: stmt,
                            });
                            state = State::DroppingTillLbl;
                        }

                        Stmt::Move(..) | Stmt::RVal(..) |  Stmt::Nop => current_bb.push(stmt),
                        Stmt::Seq(..) => unreachable!(),
                    }
                }
                State::DroppingTillLbl => {
                    loop {
                        if let Stmt::Lbl(lbl) = stmt {
                            current_bb_label = lbl;
                            current_bb.push(Stmt::Lbl(lbl));
                            state = State::CollectingStmts;
                            continue 'main;
                        } else {
                            drop(stmt);
                        }

                        if let Some(s) = stmts.next() {
                            stmt = s;
                        } else {
                            break 'main;
                        }
                    }
                }
            }
        }

        if !current_bb.is_empty() {
            bbs.blocks.insert(current_bb_label, Bb {
                stmts: current_bb,
                last: Stmt::Ret(None),
            });
        }

        bbs
    }

    #[test]
    fn build_bb() {
        use Stmt::*;
        use crate::ir::RVal;
        let br = Br { op: Relop::Eq, e1: RVal::Int(1), e2: RVal::Int(1), if_true: "B".into(), if_false: "A".into() };
        let stmts = vec![
            Nop,
            Nop,
            Lbl("A".into()),
            Nop,
            br.clone(),
            Nop, // dead code
            Stmt::direct_jmp("B"), // dead code
            Nop, // dead code
            Lbl("B".into()),
            Nop,
            Ret(None),
        ];
        let bbs = group_into_bbs(stmts);
        insta::assert_debug_snapshot!(bbs);
        assert_eq!(bbs, Bbs {
            entry: bbs.entry,
            blocks: [
                (bbs.entry, Bb {
                    stmts: vec![Stmt::Lbl(bbs.entry), Nop, Nop],
                    last: Stmt::direct_jmp("A"),
                }),
                ("A".into(), Bb {
                    stmts: vec![Lbl("A".into()), Nop],
                    last: br,
                }),
                ("B".into(), Bb {
                    stmts: vec![Lbl("B".into()), Nop],
                    last: Ret(None),
                }),
            ].into_iter().collect()
        });
    }
}

mod trace {
    use std::collections::BTreeSet;
    use crate::{ir::Stmt, names::Lbl};
    use super::basic_blocks::{Bb, Bbs};

    pub fn schedule_traces(mut bbs: Bbs) -> Vec<Bb> {
        let mut all_blocks = Vec::new();
        let mut to_visit = vec![bbs.entry()];
        let mut visited = BTreeSet::new();

        while let Some(mut block_lbl) = to_visit.pop() {
            'inner: loop {
                let bb = bbs.pop_bb(block_lbl);
                visited.insert(block_lbl);
                let mut unvisited_successors = bb
                    .successors()
                    .filter(|lbl| !visited.contains(lbl));
                if let Some(next_lbl) = unvisited_successors.next() {
                    block_lbl = next_lbl;
                    to_visit.extend(unvisited_successors);
                    all_blocks.push(bb);
                } else {
                    drop(unvisited_successors);
                    all_blocks.push(bb);
                    break 'inner;
                }
            }
        }

        all_blocks
    }

    #[test]
    fn build_trace() {
        use Stmt::*;
        use crate::{ir::{LVal, RVal, Relop, Stmt}, names::{Lbl, Tmp}};
        use super::basic_blocks::group_into_bbs;

        let bbs = Bbs {
            entry: "entry".into(),
            blocks: [
                ("entry".into(), Bb {
                    stmts: vec![Stmt::Lbl("entry".into()), Nop, Nop],
                    last: Stmt::direct_jmp("A"),
                }),
                ("A".into(), Bb {
                    stmts: vec![Lbl("A".into()), Nop],
                    last: Br { op: Relop::Eq, e1: RVal::Int(1), e2: RVal::Int(1), if_true: "B".into(), if_false: "C".into() },
                }),
                ("B".into(), Bb {
                    stmts: vec![Lbl("B".into()), Nop],
                    last: Ret(None),
                }),
                ("C".into(), Bb {
                    stmts: vec![Lbl("C".into()), Nop, Nop],
                    last: Br { op: Relop::Eq, e1: RVal::Int(1), e2: RVal::Int(1), if_true: "C".into(), if_false: "A".into() },
                }),
            ].into_iter().collect()
        };

        insta::assert_debug_snapshot!(schedule_traces(bbs));
    }
}
