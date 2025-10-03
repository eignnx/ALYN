//! Canonicalize IR

use crate::{ir, names::Lbl};

/// # Goals
/// - Flatten RVal::Seq and Stmt::Seq terms into a linear vector of statements.
/// - Extract RVal::Call terms into their own statements.
///     + Either Stmt::RVal(RVal::Call(..)),
///     + or Stmt::Move(Tmp(..), RVal::Call(..))
pub mod flatten {
    use crate::{
        ir::{LVal, RVal, Stmt},
        names::Tmp,
    };

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
            Stmt::Br {
                op,
                e1,
                e2,
                if_true,
                if_false,
            } => {
                let e1 = flatten_rval(e1, out);
                let e2 = flatten_rval(e2, out);
                out.push(Stmt::Br {
                    op,
                    e1,
                    e2,
                    if_true,
                    if_false,
                });
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
            }
            RVal::Call(func @ RVal::Lbl(_), args) => {
                // TODO: don't create temporary if func has ret type void
                let mut new_args = Vec::new();
                for arg in args {
                    new_args.push(Box::new(flatten_rval(*arg, out)));
                }
                let ret_val: LVal = Tmp::fresh("ret_val").into();
                out.push(Stmt::Move(ret_val.clone(), RVal::Call(func, new_args)));
                ret_val.into()
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

/// # Goals
/// Split the sequence of instructions into basic blocks.
pub mod basic_blocks {
    use std::{collections::BTreeMap, mem};

    use crate::{
        ir::{LVal, RVal, Relop, Stmt},
        names::{Lbl, Tmp},
    };

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
                Stmt::Br {
                    if_false, if_true, ..
                } => {
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
        pub fn take_bb(&mut self, block_lbl: Lbl) -> Option<Bb> {
            self.blocks.remove(&block_lbl)
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

    pub fn group_into_bbs(subr_lbl: Lbl, stmts: Vec<Stmt>) -> Bbs {
        let mut current_bb = Vec::new();
        let mut current_bb_label = subr_lbl;
        current_bb.push(Stmt::Lbl(subr_lbl));
        let mut bbs = Bbs {
            entry: subr_lbl,
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
                            bbs.blocks.insert(
                                current_bb_label,
                                Bb {
                                    stmts: mem::replace(&mut current_bb, Vec::new()),
                                    last: Stmt::direct_jmp(lbl),
                                },
                            );
                            current_bb_label = lbl;
                            current_bb.push(Stmt::Lbl(lbl));
                        }

                        Stmt::Br { .. } | Stmt::Jmp(..) | Stmt::Ret(..) => {
                            bbs.blocks.insert(
                                current_bb_label,
                                Bb {
                                    stmts: mem::replace(&mut current_bb, Vec::new()),
                                    last: stmt,
                                },
                            );
                            state = State::DroppingTillLbl;
                        }

                        Stmt::Move(..) | Stmt::RVal(..) | Stmt::Nop => current_bb.push(stmt),
                        Stmt::Seq(..) => unreachable!(),
                    }
                }
                State::DroppingTillLbl => loop {
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
                },
            }
        }

        if !current_bb.is_empty() {
            bbs.blocks.insert(
                current_bb_label,
                Bb {
                    stmts: current_bb,
                    last: Stmt::Ret(None),
                },
            );
        }

        bbs
    }

    #[test]
    fn build_bb() {
        use crate::ir::RVal;
        use Stmt::*;
        let br = Br {
            op: Relop::Eq,
            e1: RVal::Int(1),
            e2: RVal::Int(1),
            if_true: "B".into(),
            if_false: "A".into(),
        };
        let stmts = vec![
            Nop,
            Nop,
            Lbl("A".into()),
            Nop,
            br.clone(),
            Nop,                   // dead code
            Stmt::direct_jmp("B"), // dead code
            Nop,                   // dead code
            Lbl("B".into()),
            Nop,
            Ret(None),
        ];
        let bbs = group_into_bbs("test_subr".into(), stmts);
        insta::assert_debug_snapshot!(bbs);
        assert_eq!(
            bbs,
            Bbs {
                entry: bbs.entry,
                blocks: [
                    (
                        bbs.entry,
                        Bb {
                            stmts: vec![Stmt::Lbl(bbs.entry), Nop, Nop],
                            last: Stmt::direct_jmp("A"),
                        }
                    ),
                    (
                        "A".into(),
                        Bb {
                            stmts: vec![Lbl("A".into()), Nop],
                            last: br,
                        }
                    ),
                    (
                        "B".into(),
                        Bb {
                            stmts: vec![Lbl("B".into()), Nop],
                            last: Ret(None),
                        }
                    ),
                ]
                .into_iter()
                .collect()
            }
        );
    }
}

/// # Goals
/// Decide on an ordering of basic blocks that (mostly) has branch instructions followed by their
/// false label, and jumps followed by their target (if possible).
mod trace {
    use super::basic_blocks::{Bb, Bbs};
    use crate::{ir::Stmt, names::Lbl};
    use std::collections::BTreeSet;

    struct TraceScheduler {
        bbs: Bbs,
        schedule: Vec<Bb>,
        to_visit: Vec<Lbl>,
        visited: BTreeSet<Lbl>,
    }

    impl TraceScheduler {
        fn new(bbs: Bbs) -> Self {
            let entry = bbs.entry;
            Self {
                bbs,
                to_visit: vec![entry],
                schedule: Vec::new(),
                visited: BTreeSet::new(),
            }
        }

        /// # Example
        /// ```text
        /// Trace1: [A B C]
        /// Trace2: [D E A B C]
        /// Trace3: [F G H I]
        /// Trace4: [J K]
        ///
        /// Output: A B C D E F G H I J K
        /// ```
        fn schedule_traces(&mut self) {
            // Assuming there aren't disconnected basic-blocks, this will traverse the entire graph.
            // If there ARE disconnected `Bb`s, they will be discarded because they are unreachable.
            while let Some(trace_start) = self.to_visit.pop() {
                self.schedule_trace(trace_start);
            }
        }

        fn schedule_trace(&mut self, mut block_lbl: Lbl) {
            loop {
                let Some(bb) = self.bbs.take_bb(block_lbl) else {
                    // This `Bb` must have already been visited, skip to next trace.
                    return;
                };

                self.visited.insert(block_lbl);

                let mut unvisited_successors =
                    bb.successors().filter(|lbl| !self.visited.contains(lbl));

                if let Some(next_lbl) = unvisited_successors.next() {
                    // There's more to this trace, advance to next `Bb` in next iteration.
                    block_lbl = next_lbl;
                    // But save the other successors to be visited later.
                    self.to_visit.extend(unvisited_successors);
                    self.schedule.push(bb);
                } else {
                    // There are no more `Bb`s in this trace. Go request a new trace to work on.
                    drop(unvisited_successors);
                    self.schedule.push(bb);
                    return;
                }
            }
        }
    }

    pub fn schedule_traces(mut bbs: Bbs) -> Vec<Bb> {
        let mut scheduler = TraceScheduler::new(bbs);
        scheduler.schedule_traces();
        scheduler.schedule
    }

    #[test]
    fn build_trace() {
        use super::basic_blocks::group_into_bbs;
        use crate::{
            ir::{LVal, RVal, Relop, Stmt},
            names::{Lbl, Tmp},
        };
        use Stmt::*;

        let bbs = Bbs {
            entry: "entry".into(),
            blocks: [
                (
                    "entry".into(),
                    Bb {
                        stmts: vec![Stmt::Lbl("entry".into()), Nop, Nop],
                        last: Stmt::direct_jmp("A"),
                    },
                ),
                (
                    "A".into(),
                    Bb {
                        stmts: vec![Lbl("A".into()), Nop],
                        last: Br {
                            op: Relop::Eq,
                            e1: RVal::Int(1),
                            e2: RVal::Int(1),
                            if_true: "B".into(),
                            if_false: "C".into(),
                        },
                    },
                ),
                (
                    "B".into(),
                    Bb {
                        stmts: vec![Lbl("B".into()), Nop],
                        last: Ret(None),
                    },
                ),
                (
                    "C".into(),
                    Bb {
                        stmts: vec![Lbl("C".into()), Nop, Nop],
                        last: Br {
                            op: Relop::Eq,
                            e1: RVal::Int(1),
                            e2: RVal::Int(1),
                            if_true: "C".into(),
                            if_false: "A".into(),
                        },
                    },
                ),
            ]
            .into_iter()
            .collect(),
        };

        insta::assert_debug_snapshot!(schedule_traces(bbs));
    }
}

/// # Goals
/// - Remove jumps that only jump to the immediately next instruction
/// - Ensure all branch instructions fallthrough on false
mod post_process {

    use super::basic_blocks::Bb;
    use crate::{
        ir::{RVal, Relop, Stmt},
        names::Lbl,
    };

    pub fn post_process(schedule: Vec<Bb>) -> Vec<Stmt> {
        let mut output = Vec::new();
        let mut stmts = schedule.into_iter().peekable();

        while let Some(mut bb) = stmts.next() {
            if let Some(bb_next) = stmts.peek_mut() {
                process_pair(&mut output, bb, bb_next);
            } else {
                // Last `Bb`.
                // => Just add all it's statements.
                output.extend(bb.stmts.drain(..));
                output.push(bb.last);
            }
        }

        output
    }

    fn process_pair(output: &mut Vec<Stmt>, bb: Bb, bb_next: &mut Bb) {
        //  ---- bb --------
        // |     ...       |
        // | Stmt::Jmp(L) |  // This jump can be deleted.
        // ----------------
        //        |
        //        v
        // ---- bb_next ----
        // | Stmt::Lbl(L) |
        // |     ...       |
        // -----------------
        if let Stmt::Jmp(_, [l1]) = &bb.last
            && let Stmt::Lbl(l2) = bb_next.stmts.first().unwrap()
            && l1 == l2
        {
            output.extend(bb.stmts);

            // Skip `bb.last` (the uneccessary jump).
            return;
        }

        //  ---- bb --------
        // |     ...       |
        // | Stmt::Br {..} |
        // -----------------
        //        |
        //        v
        // ---- bb_next ----
        // | Stmt::Lbl(..) |
        // |     ...       |
        // -----------------
        if let Bb {
            stmts,
            last:
                last @ Stmt::Br {
                    op,
                    e1,
                    e2,
                    if_true,
                    if_false,
                },
        } = &bb
            && let Stmt::Lbl(lbl_next) = bb_next.stmts.first().unwrap()
        {
            output.extend(stmts.clone());

            if if_true == lbl_next {
                // The fallthrough case should be `if_false`.
                // => Negation needed.
                output.push(negate_branch(op, e1, e2, if_true, if_false));
                return;
            } else if if_false != lbl_next {
                // The next label is neither `if_true` nor `if_false`.
                // => Create new label `L`.
                // => Adjust branch so `if_false` is now `L`.
                // => Insert `Stmt::Lbl(L); Stmt::direct_jmp(if_false)` after the branch.
                let springboard = Lbl::fresh("springboard");
                output.push(Stmt::Br {
                    if_false: springboard,
                    op: *op,
                    e1: e1.clone(),
                    e2: e2.clone(),
                    if_true: *if_true,
                });
                output.push(Stmt::Lbl(springboard));
                output.push(Stmt::direct_jmp(*if_false));
                return;
            } else {
                // Label `if_false` follows the branch.
                // => Already canonicalized.
                output.push(last.clone());
                return;
            }
        }

        // No exceptional cases.
        // => Already canonicalized.
        output.extend(bb.stmts);
        output.push(bb.last);
    }

    fn negate_branch(op: &Relop, e1: &RVal, e2: &RVal, if_true: &Lbl, if_false: &Lbl) -> Stmt {
        let new_op = match op {
            Relop::Eq => Relop::Ne,
            Relop::Ne => Relop::Eq,

            Relop::Gt => Relop::Lte,
            Relop::Lt => Relop::Gte,
            Relop::Gte => Relop::Lt,
            Relop::Lte => Relop::Gt,

            Relop::GtU => Relop::LteU,
            Relop::LtU => Relop::GteU,
            Relop::GteU => Relop::LtU,
            Relop::LteU => Relop::GtU,
        };

        Stmt::Br {
            op: new_op,
            e1: e1.clone(),
            e2: e2.clone(),
            if_true: *if_false, // SWAPPED!
            if_false: *if_true, // SWAPPED!
        }
    }

    #[test]
    fn test_post_process() {
        let bbs = vec![
            Bb {
                stmts: vec![Stmt::Lbl("entry".into()), Stmt::Nop],
                last: Stmt::direct_jmp("x_lt_10"),
            },
            Bb {
                stmts: vec![Stmt::Lbl("x_lt_10".into()), Stmt::Nop],
                last: Stmt::Br {
                    op: Relop::Lt,
                    e1: RVal::tmp("y"),
                    e2: 0i64.into(),
                    if_true: "y_negative".into(),
                    if_false: "entry".into(),
                },
            },
            Bb {
                stmts: vec![Stmt::Lbl("y_negative".into()), Stmt::Nop],
                last: Stmt::Br {
                    op: Relop::LtU,
                    e1: RVal::tmp("x"),
                    e2: 10u64.into(),
                    if_true: "x_lt_10".into(),
                    if_false: "x_is_big".into(),
                },
            },
            Bb {
                stmts: vec![Stmt::Lbl("get_outta_here".into()), Stmt::Nop],
                last: Stmt::Ret(None),
            },
            Bb {
                stmts: vec![Stmt::Lbl("x_is_big".into()), Stmt::Nop],
                last: Stmt::direct_jmp("get_outta_here"),
            },
        ];

        insta::assert_debug_snapshot!(post_process(bbs));
    }
}

pub mod canon_ir {
    use smallvec::SmallVec;

    pub use crate::ir::{Binop, Relop, Unop};
    use crate::{
        ir,
        names::{Lbl, Tmp}, ty::Ty,
    };

    #[derive(Debug)]
    pub enum Stmt {
        Move(LVal, RVal),
        Call(Option<Tmp>, Lbl, SmallVec<[RVal; 2]>),
        // TODO: CallIndirect(Option<Tmp>, Tmp, SmallVec<[Tmp; 2]>),
        Br {
            op: Relop,
            e1: RVal,
            e2: RVal,
            if_true: Lbl,
        },
        Jmp(Lbl),
        Switch(RVal, SmallVec<[Lbl; 2]>),

        Lbl(Lbl),

        Discard(RVal),
        Nop,
        Ret(Option<RVal>),
    }

    #[derive(Debug)]
    pub enum RVal {
        LVal(LVal),
        Imm(Imm),
        Binop(Binop, Box<RVal>, Box<RVal>),
        Unop(Unop, Box<RVal>),
        BitCast(Ty, Box<RVal>),
    }

    #[derive(Debug)]
    pub enum LVal {
        Tmp(Tmp),
        Mem(Box<RVal>),
    }

    #[derive(Debug)]
    pub enum Imm {
        Byte(u8),
        Int(i64),
        Nat(u64),
        Lbl(Lbl),
    }

    impl From<ir::Stmt> for Stmt {
        fn from(stmt: ir::Stmt) -> Self {
            match stmt {
                ir::Stmt::Move(ir::LVal::Tmp(dst), ir::RVal::Call(ir::RVal::Lbl(func), args)) => {
                    let args = args.into_iter().map(|arg| (*arg).into()).collect();
                    Stmt::Call(Some(dst.into()), func, args)
                }
                ir::Stmt::RVal(ir::RVal::Call(ir::RVal::Lbl(func), args)) => {
                    let args = args.into_iter().map(|arg| (*arg).into()).collect();
                    Stmt::Call(None, func, args)
                }

                ir::Stmt::Move(lval, rval) => Stmt::Move(lval.into(), rval.into()),
                // Can almost replace `ir::Stmt::RVal(..)` with `Nop`, but MMIO may require mem loads be executed.
                ir::Stmt::RVal(rval) => Stmt::Discard(rval.into()),

                ir::Stmt::Jmp(_, []) => unreachable!("Encountered Stmt::Jmp(_, [])!\n!!! {stmt:?}"),
                ir::Stmt::Jmp(ir::RVal::Lbl(_), [lbl]) => Stmt::Jmp(lbl),
                ir::Stmt::Jmp(rval, lbls) => Stmt::Switch(rval.into(), lbls.into()),

                ir::Stmt::Br {
                    op,
                    e1,
                    e2,
                    if_true,
                    if_false: _,
                } => Stmt::Br {
                    op,
                    e1: e1.into(),
                    e2: e2.into(),
                    if_true,
                },

                ir::Stmt::Lbl(lbl) => Stmt::Lbl(lbl),

                ir::Stmt::Nop => Stmt::Nop,

                ir::Stmt::Ret(Some(rval)) => Stmt::Ret(Some(rval.into())),
                ir::Stmt::Ret(None) => Stmt::Ret(None),

                ir::Stmt::Seq(..) => unreachable!(
                    "Stmt::Seq terms weren't all removed in `flatten` step!\n!!! {stmt:?}"
                ),
            }
        }
    }

    impl From<ir::RVal> for RVal {
        fn from(rval: ir::RVal) -> Self {
            match rval {
                ir::RVal::Byte(x) => RVal::Imm(Imm::Byte(x)),
                ir::RVal::Nat(x) => RVal::Imm(Imm::Nat(x)),
                ir::RVal::Int(x) => RVal::Imm(Imm::Int(x)),
                ir::RVal::Lbl(lbl) => RVal::Imm(Imm::Lbl(lbl)),
                ir::RVal::LVal(ir::LVal::Tmp(tmp)) => RVal::LVal(LVal::Tmp(tmp)),
                ir::RVal::LVal(ir::LVal::Mem(rval)) => RVal::LVal(LVal::Mem(rval.into())),
                ir::RVal::LVal(ir::LVal::Global(rval)) => todo!(),
                ir::RVal::Binop(binop, x, y) => RVal::Binop(binop, x.into(), y.into()),
                ir::RVal::Unop(unop, rval) => RVal::Unop(unop, rval.into()),
                ir::RVal::BitCast(ty, rval) => RVal::BitCast(ty, rval.into()),
                ir::RVal::Call(rval, rvals) => unreachable!(),
                ir::RVal::Seq(stmt, rval) => unreachable!(
                    "RVal::Seq terms weren't all removed in `flatten` step!\n!!! {rval:?}"
                ),
            }
        }
    }

    impl From<Box<ir::RVal>> for Box<RVal> {
        fn from(value: Box<ir::RVal>) -> Self {
            Box::new((*value).into())
        }
    }

    impl From<ir::LVal> for LVal {
        fn from(value: ir::LVal) -> Self {
            match value {
                ir::LVal::Tmp(tmp) => LVal::Tmp(tmp),
                ir::LVal::Mem(rval) => LVal::Mem(rval.into()),
                ir::LVal::Global(intern) => todo!(),
            }
        }
    }
}

pub use canon_ir::*;

pub fn canonicalize(subr_lbl: Lbl, stmts: Vec<ir::Stmt>) -> Vec<canon_ir::Stmt> {
    let mut flattened = Vec::new();
    for stmt in stmts {
        flatten::flatten_stmt(stmt, &mut flattened);
    }

    let bbs = basic_blocks::group_into_bbs(subr_lbl, flattened);

    let schedule = trace::schedule_traces(bbs);

    let processed_stmts = post_process::post_process(schedule);

    let mut canon_stmts = Vec::new();
    for stmt in processed_stmts {
        canon_stmts.push(canon_ir::Stmt::from(stmt));
    }

    canon_stmts
}
