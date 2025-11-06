use std::{collections::{BTreeSet, HashMap}, ops::{Index, Range}};

use alyn_common::names::Lbl;
use derive_more::{Add, From, Into};

use crate::{ctrl_flow::{CtrlFlow, GetCtrlFlow}, stg::Stg, stmt::Stmt, Instruction};

#[derive(Debug, Clone, Copy, Add, From, Into, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StmtIdx(usize);
#[derive(Debug, Clone, Copy, Add, From, Into, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BbIdx(usize);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Move<R> {
    pub dst: Stg<R>,
    pub src: Stg<R>,
    pub stmt_idx: StmtIdx,
}

pub struct Cfg<'stmts, R, I> {
    stmts: &'stmts [Stmt<I>],
    bbs: Vec<Bb>,

    entry_bb: BbIdx,
    exit_bbs: BTreeSet<BbIdx>,

    /// Maps `Lbl`s to the `StmtIdx` where the `Stmt::Label` lives in the program statement
    /// sequence.
    lbls_to_stmt_idxs: HashMap<Lbl, StmtIdx>,

    /// Maps `Lbl`s to (the index of) the `Bb` they begin.
    lbls_to_bb_idxs: HashMap<Lbl, BbIdx>,

    move_stmts: Vec<Move<R>>,
}

pub enum Terminator<I> {
    Instr(I),
    FallThrough,
}

/// Basic Block
pub struct Bb {
    /// All Stmts in the block, including labels and terminators.
    stmts: Range<StmtIdx>,

    /// The non-terminator, non-label statements.
    instrs: Range<StmtIdx>,

    /// The final statement. It jumps to another block or exits.
    terminator: Terminator<StmtIdx>,

    /// The indices of the statements that the basic block goes to next (or empty if
    /// terminator does `CtrlFlow::Exit`).
    successors: BTreeSet<StmtIdx>,
}

impl<'stmt, R, I: Instruction<Reg = R> + GetCtrlFlow> Cfg<'stmt, R, I> {
    pub fn build_from(
        stmts: &'stmt [Stmt<I>],
    ) -> Self {
        let mut this = Self {
            stmts,
            bbs: Vec::new(),
            entry_bb: 0.into(),
            exit_bbs: Default::default(),
            lbls_to_stmt_idxs: Default::default(),
            lbls_to_bb_idxs: Default::default(),
            move_stmts: Default::default(),
        };

        this.discover_labels_and_moves();
        this.gather_into_bbs();

        this
    }

    fn discover_labels_and_moves(&mut self) {
        for (idx, stmt) in self.stmts.iter().enumerate() {
            let idx = idx.into();
            match stmt {
                Stmt::Label(lbl) => {
                    self.lbls_to_stmt_idxs.insert(*lbl, idx);
                }
                Stmt::Instr(instr) => {
                    if let Some((dst, src)) = instr.try_as_pure_move() {
                        self.move_stmts.push(Move {
                            dst,
                            src,
                            stmt_idx: idx,
                        });
                    }
                }
            }
        }
    }

    fn gather_into_bbs(&mut self) {
        let mut stmts_iter = self.stmts.iter().enumerate().peekable();

        enum State {
            NewBb,
            GatheringLabels { bb_stmts_start: StmtIdx },
            GatheringNonTermInstrs { bb_stmts_start: StmtIdx, bb_instrs_start: StmtIdx },
        }

        let mut state = State::NewBb;

        while let Some((idx, stmt)) = stmts_iter.peek().cloned() {
            let idx = StmtIdx(idx);
            let curr_bb_idx = BbIdx(self.bbs.len());
            match state {
                State::NewBb => {
                    state = State::GatheringLabels { bb_stmts_start: idx };
                }

                State::GatheringLabels { bb_stmts_start } => {
                    if let Stmt::Label(lbl) = stmt {
                        self.lbls_to_bb_idxs.insert(*lbl, curr_bb_idx);
                        let _ = stmts_iter.next();
                    } else {
                        state = State::GatheringNonTermInstrs {
                            bb_stmts_start,
                            bb_instrs_start: bb_stmts_start,
                        };
                    }
                }

                State::GatheringNonTermInstrs { bb_stmts_start, bb_instrs_start } => {
                    match (stmt, stmt.ctrl_flow()) {
                        (Stmt::Instr(_), CtrlFlow::Advance) => {
                            let _ = stmts_iter.next();
                        }
                        (_, flow) => {
                            let stmts = bb_stmts_start..bb_instrs_start;
                            let instrs = bb_instrs_start..idx;
                            let mut successors = BTreeSet::<StmtIdx>::new();
                            let mut terminator = Terminator::Instr(idx);

                            match flow {
                                CtrlFlow::Advance => {
                                    let Stmt::Label(lbl) = stmt else { panic!() };
                                    successors.insert(self.lbls_to_stmt_idxs[lbl]);
                                    let _ = stmts_iter.next();
                                }
                                CtrlFlow::Exit => {
                                    self.exit_bbs.insert(curr_bb_idx);
                                }
                                CtrlFlow::Jump(lbl) => {
                                    successors.insert(self.lbls_to_stmt_idxs[&lbl]);
                                    let _ = stmts_iter.next();
                                }
                                CtrlFlow::Switch(lbls) => {
                                    successors.extend(lbls.iter().map(|lbl| self.lbls_to_stmt_idxs[lbl]));
                                    let _ = stmts_iter.next();
                                }
                                CtrlFlow::Branch(lbl) => {
                                    terminator = Terminator::FallThrough;
                                    successors.insert(self.lbls_to_stmt_idxs[&lbl]);
                                    successors.insert(idx + StmtIdx(1)); // TODO: Sure hope this doesn't
                                                                         // fall off the end!
                                }
                            }

                            self.bbs.push(Bb { stmts, instrs, terminator, successors });

                            state = State::NewBb;
                        }
                    }
                }
            }
        }
    }

    fn stmt_idx_to_bb_idx(&self, stmt_idx: StmtIdx) -> BbIdx {
        for (bb_idx, bb) in self.bbs.iter().enumerate() {
            if bb.stmts.contains(&stmt_idx) {
                return BbIdx(bb_idx);
            }
        }
        panic!("untracked statement index: {stmt_idx:?}: {:?}", self.stmts[stmt_idx.0]);
    }

    #[track_caller]
    pub fn successor_bbs(&self, bb_idx: BbIdx) -> impl Iterator<Item = BbIdx> + ExactSizeIterator {
        self[bb_idx].successors.iter().map(|stmt_idx| self.stmt_idx_to_bb_idx(*stmt_idx))
    }

    #[track_caller]
    pub fn bb_stmts(&self, bb_idx: BbIdx) -> impl Iterator<Item = &Stmt<I>> + ExactSizeIterator {
        let instr_range = self[bb_idx].instrs.clone();
        let instr_range = instr_range.start.0 .. instr_range.end.0;
        self.stmts[instr_range].iter()
    }

    /// Returns an iterator of *non-terminator*, *non-label* instructions.
    #[track_caller]
    pub fn bb_instrs(&self, bb_idx: BbIdx) -> impl Iterator<Item = &I> + ExactSizeIterator {
        self.bb_stmts(bb_idx).map(|stmt| {
            let Stmt::Instr(instr) = stmt else { unreachable!() };
            instr
        })
    }

    pub fn bb_terminator(&self, bb_idx: BbIdx) -> Terminator<&I> {
        match &self[bb_idx].terminator {
            Terminator::Instr(stmt_idx) => {
                let Stmt::Instr(instr) = &self[*stmt_idx] else { unreachable!() };
                Terminator::Instr(instr)
            }
            Terminator::FallThrough => Terminator::FallThrough,
        }
    }

    /// The labels that refer to the start of this basic block;
    pub fn bb_labels(&self, bb_idx: BbIdx) -> impl Iterator<Item = Lbl> + ExactSizeIterator {
        let bb = &self[bb_idx];
        let lbls_start = bb.stmts.start;
        let lbls_end = bb.instrs.start;
        let lbls_range = lbls_start.0 .. lbls_end.0;
        self[lbls_start..lbls_end].iter().map(|stmt| {
            let Stmt::Label(lbl) = stmt else { unreachable!() };
            *lbl
        })
    }

    pub fn stmts(&self) -> &[Stmt<I>] {
        &self.stmts[..]
    }

    pub fn entry(&self) -> BbIdx {
        self.entry_bb
    }

    pub fn exits(&self) -> impl Iterator<Item = BbIdx> {
        self.exit_bbs.iter().copied()
    }

    pub fn known_labels(&self) -> impl Iterator<Item = Lbl> {
        self.lbls_to_stmt_idxs.keys().copied()
    }

    pub fn move_stmts(&self) -> impl Iterator<Item = &Move<R>> {
        self.move_stmts.iter()
    }
}

impl<'stmts, R, I> Index<Range<StmtIdx>> for Cfg<'stmts, R, I> {
    type Output = [Stmt<I>];

    fn index(&self, index: Range<StmtIdx>) -> &Self::Output {
        &self.stmts[index.start.0 .. index.end.0]
    }
}

impl<'stmts, R, I> Index<StmtIdx> for Cfg<'stmts, R, I> {
    type Output = Stmt<I>;

    fn index(&self, index: StmtIdx) -> &Self::Output {
        &self.stmts[index.0]
    }
}

impl<'stmts, R, I> Index<BbIdx> for Cfg<'stmts, R, I> {
    type Output = Bb;

    fn index(&self, index: BbIdx) -> &Self::Output {
        &self.bbs[index.0]
    }
}
