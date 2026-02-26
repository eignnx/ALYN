use std::{collections::BTreeSet, ops::Range};

use crate::{cfg::{BbIdx, Cfg, StmtIdx}, ctrl_flow::{CtrlFlow, GetCtrlFlow}, stmt::Stmt, Instruction};

#[derive(Debug, Clone, Copy)]
pub enum Terminator<I> {
    Instr(I),
    FallThrough,
}

impl<I> Terminator<I> {
    pub fn is_instr(&self) -> bool {
        matches!(self, Self::Instr(_))
    }
}

/// Basic Block
#[derive(Debug)]
pub struct Bb {
    /// All Stmts in the block, including labels and terminators.
    pub stmts: Range<StmtIdx>,

    /// The non-terminator, non-label statements.
    pub body_instrs: Range<StmtIdx>,

    /// The final statement. It jumps to another block or it exits.
    pub terminator: Terminator<StmtIdx>,

    /// The indices of the *statements* that the basic block goes to next (or empty if
    /// terminator does `CtrlFlow::Exit`).
    pub successors: BTreeSet<StmtIdx>,
}

impl Bb {
    /// The range of non-label instructions, including terminator if present.
    pub fn instrs_range(&self) -> Range<StmtIdx> {
        let start = self.body_instrs.start;
        let end = {
            let term_count = if self.terminator.is_instr() { 1 } else { 0 };
            self.body_instrs.end + term_count.into()
        };
        start..end
    }
}

#[derive(Debug, Clone)]
enum State {
    GatherTerminator,
    GatheringNonTermInstrs {
        bb_stmts_end: StmtIdx,
        bb_instrs_end: StmtIdx,
        terminator_idx: Terminator<StmtIdx>,
        successors: BTreeSet<StmtIdx>,
    },
    GatheringLabels {
        bb_stmts_end: StmtIdx,
        bb_instrs_end: StmtIdx,
        terminator_idx: Terminator<StmtIdx>,
        bb_labels_end: StmtIdx,
        successors: BTreeSet<StmtIdx>,
    },
}

#[derive(Debug)]
enum Consume { Do, Dont }

enum GatherResult {
    StateUpdate(Consume, State),
    // Note: Don't consume a statement when this is returned.
    BuildBb(Bb),
}

fn gather_terminator<R, I: GetCtrlFlow>(cfg: &mut Cfg<R, I>, stmt: &Stmt<I>, idx: StmtIdx) -> GatherResult {
    let mut successors = BTreeSet::new();

    let mk_ctrl_tx_res = |successors| GatherResult::StateUpdate(
        Consume::Do,
        State::GatheringNonTermInstrs {
            bb_stmts_end: idx + 1.into(), // Include idx
            bb_instrs_end: idx, // Exclude idx
            terminator_idx: Terminator::Instr(idx), // Point to idx
            successors,
        }
    );

    match stmt.ctrl_flow() {
        CtrlFlow::Exit => {
            cfg.exit_bbs.insert(cfg.bbs.len().into());
            mk_ctrl_tx_res(successors)
        }
        CtrlFlow::Jump(lbl) => {
            successors.insert(cfg.lbls_to_stmt_idxs[&lbl]);
            mk_ctrl_tx_res(successors)
        }
        CtrlFlow::Switch(lbls) => {
            successors.extend(lbls.iter().map(|lbl| cfg.lbls_to_stmt_idxs[lbl]));
            mk_ctrl_tx_res(successors)
        }
        CtrlFlow::Branch(lbl) => {
            successors.insert(cfg.lbls_to_stmt_idxs[&lbl]);
            successors.insert(idx + 1.into());
            mk_ctrl_tx_res(successors)
        }

        CtrlFlow::Advance => {
            successors.insert(idx + 1.into());
            GatherResult::StateUpdate(Consume::Dont,
                State::GatheringNonTermInstrs {
                bb_stmts_end: idx + 1.into(), // Include idx
                bb_instrs_end: idx + 1.into(), // Include idx
                terminator_idx: Terminator::FallThrough,
                successors,
            })
        }
    }

}

fn update_gather_state<R, I: GetCtrlFlow>(cfg: &mut Cfg<R, I>, state: State, stmt: &Stmt<I>, idx: StmtIdx) -> GatherResult {
    match state {
        State::GatherTerminator => gather_terminator(cfg, stmt, idx),
        State::GatheringNonTermInstrs { bb_stmts_end, bb_instrs_end, terminator_idx, successors } => {
            if let Stmt::Label(_) = stmt {
                GatherResult::StateUpdate(Consume::Dont, State::GatheringLabels {
                    bb_stmts_end,
                    bb_instrs_end,
                    terminator_idx,
                    bb_labels_end: idx + 1.into(), // Include idx
                    successors,
                })
            } else {

                match stmt.ctrl_flow() {
                    CtrlFlow::Advance => {
                        GatherResult::StateUpdate(
                            Consume::Do,
                            State::GatheringNonTermInstrs {
                                bb_stmts_end,
                                bb_instrs_end,
                                terminator_idx,
                                successors,
                            }
                        )
                    }

                    CtrlFlow::Exit |
                    CtrlFlow::Jump(_) |
                    CtrlFlow::Switch(_) |
                    CtrlFlow::Branch(_) => {
                        // No labels to be found.
                        let bb_start = idx + 1.into();
                        let instrs_start = bb_start; // Since == to bb_start, labels is empty
                        GatherResult::BuildBb(Bb {
                            stmts: bb_start..bb_stmts_end,
                            body_instrs: instrs_start..bb_instrs_end,
                            terminator: terminator_idx,
                            successors,
                        })
                    }
                }
            }
        }
        State::GatheringLabels { bb_stmts_end, bb_instrs_end, terminator_idx, bb_labels_end, successors } => {
            match stmt {
                Stmt::Label(_lbl) => {
                    GatherResult::StateUpdate(
                        Consume::Do,
                        State::GatheringLabels {
                            bb_stmts_end,
                            bb_instrs_end,
                            terminator_idx,
                            bb_labels_end,
                            successors,
                        }
                    )
                }
                Stmt::Instr(_) => {
                    let bb_start = idx + 1.into(); // Exclude index
                    GatherResult::BuildBb(Bb {
                        stmts: bb_start..bb_stmts_end,
                        body_instrs: bb_labels_end..bb_instrs_end,
                        terminator: terminator_idx,
                        successors,
                    })
                }
            }
        }
    }
}


/// # Outline
/// For each block:
///   1. Determine if there's a terminator instruction or if it's a fallthrough ending.
///   2. Gather all non-label, non-terminator instructions.
///   3. Gather all labels.
///   4. Then construct the basic block.
/// Then compute predecessors based on all the Bb's successor sets.
pub(super) fn gather_into_bbs<R, I: Instruction<Reg = R> + GetCtrlFlow>(cfg: &mut Cfg<R, I>) {
    let mut state = State::GatherTerminator;

    let mut stmts_iter = cfg.stmts
        .iter()
        .enumerate()
        .map(|(i, stmt)| (StmtIdx(i), stmt))
        .rev() // ITERATE IN REVERSE
        .peekable();

    loop {
        if let Some((idx, stmt)) = stmts_iter.peek().cloned() {
            match update_gather_state(cfg, state, stmt, idx) {
                GatherResult::StateUpdate(consume, new_state) => {
                    if let Consume::Do = consume {
                        let _ = stmts_iter.next();
                    }
                    state = new_state;
                }
                GatherResult::BuildBb(bb) => {
                    cfg.bbs.push(bb);
                    state = State::GatherTerminator;
                }
            }
        } else { // End of stmts_iter, no more statements to process.
            match state {
                State::GatherTerminator => {} // Expected new Bb, found beginning of program.
                                              // No problem.

                State::GatheringNonTermInstrs {
                    bb_stmts_end,
                    bb_instrs_end,
                    terminator_idx,
                    successors,
                }  => {
                    cfg.bbs.push(Bb {
                        stmts: 0.into() .. bb_stmts_end,
                        body_instrs: 0.into() .. bb_instrs_end,
                        terminator: terminator_idx,
                        successors,
                    });
                }

                State::GatheringLabels {
                    bb_stmts_end,
                    bb_instrs_end,
                    terminator_idx,
                    bb_labels_end,
                    successors,
                } => {
                    cfg.bbs.push(Bb {
                        stmts: 0.into() .. bb_stmts_end,
                        body_instrs: bb_labels_end .. bb_instrs_end,
                        terminator: terminator_idx,
                        successors,
                    });
                }
            }

            compute_predecessors(cfg);
            return;
        }
    }
}

fn compute_predecessors<R, I: Instruction<Reg = R> + GetCtrlFlow>(cfg: &mut Cfg<R, I>) {
    let mut preds_succs = Vec::new();
    for (bb_idx, bb) in cfg.bbs.iter().enumerate() {
        for succ_stmt_idx in &bb.successors {
            let succ_bb_idx = cfg.stmt_idx_to_bb_idx(*succ_stmt_idx);
            preds_succs.push((BbIdx(bb_idx), succ_bb_idx));
        }
    }

    for (pred, succ) in preds_succs {
        cfg.bb_predecessors.entry(succ).or_default().insert(pred);
    }
}
