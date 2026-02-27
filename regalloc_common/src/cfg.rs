use std::{collections::{BTreeSet, HashMap}, fmt::Debug, ops::{Index, Range}};

use alyn_common::names::Lbl;
use derive_more::{Add, From, Sub};

use crate::{cfg::bbs::{Bb, Terminator}, ctrl_flow::{CtrlFlow, GetCtrlFlow}, stg::Stg, stmt::Stmt, Instruction};

mod bbs;

#[derive(derive_more::Display, derive_more::Debug, Clone, Copy, Add, Sub, From, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display("{_0}")]
#[debug("stmt#{_0}")]
pub struct StmtIdx(usize);

#[derive(derive_more::Display, derive_more::Debug, Clone, Copy, Add, Sub, From, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display("BB{_0}")]
#[debug("BB{_0}")]
pub struct BbIdx(usize);

/// TODO: remove this from the CFG module, since it's just extra information that the CFG shouldn't
/// have to care about in general.
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

    /// The predecessors of a basic block (those blocks from which a block may be entered) are
    /// difficult to compute, so we'll save them as we find them.
    bb_predecessors: HashMap<BbIdx, BTreeSet<BbIdx>>,

    move_stmts: Vec<Move<R>>,
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
            bb_predecessors: Default::default(),
            move_stmts: Default::default(),
        };

        this.discover_labels_and_moves();
        bbs::gather_into_bbs(&mut this);

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

    fn gather_into_bbs0(&mut self) {

        #[derive(Debug)]
        enum State {
            NewBb,
            GatheringLabels { bb_stmts_start: StmtIdx },
            GatheringNonTermInstrs { bb_stmts_start: StmtIdx, bb_instrs_start: StmtIdx },
        }

        let mut state = State::NewBb;

        let mut stmts_iter = self.stmts
            .iter()
            .enumerate()
            .map(|(i, stmt)| (StmtIdx(i), stmt))
            .peekable();

        // Track all CFG edges (`BbIdx -> StmtIdx`) for computing predecessors at end of function.
        let mut global_successors = BTreeSet::<(BbIdx, StmtIdx)>::new();

        loop {
            let curr_bb_idx = BbIdx(self.bbs.len());
            if let Some((idx, stmt)) = stmts_iter.peek().cloned() {

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
                                bb_instrs_start: idx,
                            };
                        }
                    }

                    State::GatheringNonTermInstrs { bb_stmts_start, bb_instrs_start } => {
                        match (stmt, stmt.ctrl_flow()) {
                            // Regular instructions: just step forward.
                            (Stmt::Instr(_), CtrlFlow::Advance) => {
                                let _ = stmts_iter.next();
                            }

                            // Encountered a label: end current Bb, start a new one.
                            (Stmt::Label(lbl), _) => {
                                let terminator = Terminator::FallThrough;

                                // Do *not* include the current index in this Bb.
                                let stmts = bb_stmts_start..idx;
                                let body_instrs = bb_instrs_start..idx;

                                let successor = self.lbls_to_stmt_idxs[lbl];
                                global_successors.insert((curr_bb_idx, successor));
                                let successors = BTreeSet::from([successor]);

                                self.bbs.push(Bb { stmts, body_instrs, terminator, successors });

                                state = State::NewBb;
                            }

                            // Some kind of jump instruction
                            (_, flow) => {
                                let _ = stmts_iter.next();
                                let mut terminator = Terminator::Instr(idx);
                                let mut successors = BTreeSet::<StmtIdx>::new();

                                match flow {
                                    CtrlFlow::Advance => unreachable!(),
                                    CtrlFlow::Exit => {
                                        self.exit_bbs.insert(curr_bb_idx);
                                    }
                                    CtrlFlow::Jump(lbl) => {
                                        successors.insert(self.lbls_to_stmt_idxs[&lbl]);
                                    }
                                    CtrlFlow::Switch(lbls) => {
                                        successors.extend(lbls.iter().map(|lbl| self.lbls_to_stmt_idxs[lbl]));
                                    }
                                    CtrlFlow::Branch(lbl) => {
                                        terminator = Terminator::FallThrough;
                                        successors.insert(self.lbls_to_stmt_idxs[&lbl]);
                                        successors.insert(idx + StmtIdx(1)); // TODO: Sure hope this doesn't
                                                                            // fall off the end!
                                    }
                                }

                                // Add 1 to include current instr in this Bb.
                                let stmts = bb_stmts_start..(idx + 1.into());
                                // Do *not* include the terminator in the body instructions range.
                                let body_instrs = bb_instrs_start..idx;

                                for succ_stmt_idx in successors.iter() {
                                    global_successors.insert((curr_bb_idx, *succ_stmt_idx));
                                }

                                self.bbs.push(Bb { stmts, body_instrs, terminator, successors });

                                state = State::NewBb;
                            }
                        }
                    }
                }
            } else {
                let idx = StmtIdx::from(self.stmts.len());
                let successors = BTreeSet::new();
                let terminator = Terminator::FallThrough;

                match state {
                    State::NewBb => { }
                    State::GatheringLabels { bb_stmts_start } => {
                        // Do *not* include the current index in this Bb.
                        let stmts = bb_stmts_start..idx;
                        let body_instrs = idx..idx; // No non-label instrs encountered -> empty
                                                    // range.

                        self.exit_bbs.insert(curr_bb_idx);
                        self.bbs.push(Bb { stmts, body_instrs, terminator, successors });
                    }
                    State::GatheringNonTermInstrs { bb_stmts_start, bb_instrs_start } => {
                        // Do *not* include the current index in this Bb.
                        let stmts = bb_stmts_start..idx;
                        let body_instrs = bb_instrs_start..idx;

                        self.exit_bbs.insert(curr_bb_idx);
                        self.bbs.push(Bb { stmts, body_instrs, terminator, successors });
                    }
                }
                break;
            }
        }

        for (bb_idx, succ_stmt_idx) in global_successors {
            let succ_bb_idx = self.stmt_idx_to_bb_idx(succ_stmt_idx);
            self.bb_predecessors.entry(succ_bb_idx).or_default().insert(bb_idx);
        }
    }
}

impl<'stmts, R, I: std::fmt::Debug> Cfg<'stmts, R, I> {

    #[track_caller]
    fn stmt_idx_to_bb_idx(&self, stmt_idx: StmtIdx) -> BbIdx {
        for (bb_idx, bb) in self.bbs.iter().enumerate() {
            if bb.stmts.contains(&stmt_idx) {
                return BbIdx(bb_idx);
            }
        }
        panic!("untracked statement index: {stmt_idx:?}: {:?}", self.stmts[stmt_idx.0]);
    }

    #[track_caller]
    pub fn predecessor_bbs(&self, bb_idx: BbIdx) -> impl Iterator<Item = BbIdx> + ExactSizeIterator {
        self.bb_predecessors[&bb_idx].iter().cloned()
    }

    #[track_caller]
    pub fn successor_bbs(&self, bb_idx: BbIdx) -> impl Iterator<Item = BbIdx> + ExactSizeIterator {
        self[bb_idx].successors.iter().map(|stmt_idx| self.stmt_idx_to_bb_idx(*stmt_idx))
    }

    /// Returns an iterator of all statements in the given basic block. This includes labels and
    /// (if present) the terminator.
    #[track_caller]
    pub fn bb_stmts(&self, bb_idx: BbIdx) -> impl Iterator<Item = &Stmt<I>> + DoubleEndedIterator {
        let stmts_range = self[bb_idx].stmts.clone(); // Clones a `Range<StmtIdx>`, not a buffer.
        self[stmts_range].iter()
    }

    /// Returns an iterator of all statements in the given basic block. This includes labels and
    /// (if present) the terminator. Also yields the global statement index of each statement.
    #[track_caller]
    pub fn bb_stmts_indexed(&self, bb_idx: BbIdx) -> impl Iterator<Item = (StmtIdx, &Stmt<I>)> + DoubleEndedIterator {
        let stmts_range = self[bb_idx].stmts.clone(); // Clones a `Range<StmtIdx>`, not a buffer.
        // WORK-AROUND: `stmts_range_iter` ought to simplify to `stmts_range`, but that requires the
        // (currently) unstable `iter::Step` trait be implemented on `StmtIdx`.
        let stmts_range_iter = (stmts_range.start.0 .. stmts_range.end.0).map(StmtIdx);
        stmts_range_iter.zip(self[stmts_range].iter())
    }

    /// Returns an iterator of *non-label* instructions in the given basic block. This includes the
    /// terminator (if present).
    #[track_caller]
    pub fn bb_instrs(
        &self,
        bb_idx: BbIdx,
    ) -> impl Iterator<Item = &I> + DoubleEndedIterator + ExactSizeIterator {
        let instrs = self[bb_idx].instrs_range();
        self[instrs].iter().map(|stmt| {
            let Stmt::Instr(instr) = stmt else {
                panic!("expected statement, got label: {stmt:?}")
            };
            instr
        })
    }

    /// Returns an iterator of *non-label* instructions in the given basic block. This includes the
    /// terminator (if present). Also yields the *global* StmtIdx of the instruction.
    pub fn bb_instrs_indexed(
        &self,
        bb_idx: BbIdx,
    ) -> impl Iterator<Item = (StmtIdx, &I)> + DoubleEndedIterator + ExactSizeIterator {
        let bb = &self[bb_idx];
        self[bb.instrs_range()]
            .iter()
            .enumerate()
            .map(|(local_stmt_idx, stmt)| {
                let Stmt::Instr(instr) = stmt else {
                    unreachable!()
                };
                let global_stmt_idx = bb.body_instrs.start + StmtIdx(local_stmt_idx);
                (global_stmt_idx, instr)
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

    /// Returns `Some` if the terminator is not `Fallthrough`. Also returns the `StmtIdx` of the
    /// terminator in that case.
    pub fn bb_terminator_indexed(&self, bb_idx: BbIdx) -> Option<(StmtIdx, &I)> {
        match &self[bb_idx].terminator {
            Terminator::Instr(stmt_idx) => {
                assert!(stmt_idx.0 + 1 == self[bb_idx].stmts.end.0, "{} != {}", stmt_idx.0 + 1, self[bb_idx].stmts.end.0);
                let Stmt::Instr(instr) = &self[*stmt_idx] else { unreachable!() };
                Some((*stmt_idx, instr))
            }
            Terminator::FallThrough => None,
        }
    }

    /// The labels that refer to the start of this basic block;
    pub fn bb_labels(&self, bb_idx: BbIdx) -> impl Iterator<Item = Lbl> + ExactSizeIterator {
        let bb = &self[bb_idx];
        let lbls_start = bb.stmts.start;
        let lbls_end = bb.body_instrs.start;
        self[lbls_start..lbls_end].iter().map(|stmt| {
            let Stmt::Label(lbl) = stmt else { unreachable!() };
            *lbl
        })
    }

    pub fn bbs(&self) -> impl Iterator<Item = BbIdx> + ExactSizeIterator {
        (0..self.bbs.len()).rev().map(|i| i.into())
    }

    pub fn stmts(&self) -> &[Stmt<I>] {
        &self.stmts[..]
    }

    pub fn stmts_indexed(&self) -> impl Iterator<Item = (StmtIdx, &Stmt<I>)> + DoubleEndedIterator + ExactSizeIterator {
        self.stmts.iter().enumerate().map(|(i, stmt)| (StmtIdx(i), stmt))
    }

    pub fn entry(&self) -> BbIdx {
        self.entry_bb
    }

    pub fn exits(&self) -> impl Iterator<Item = BbIdx> {
        self.exit_bbs.iter().copied()
    }

    /// Get an iterator over all discovered labels in the program text.
    pub fn known_labels(&self) -> impl Iterator<Item = Lbl> {
        self.lbls_to_stmt_idxs.keys().copied()
    }

    /// Get an iterator over all "move" statements in the program text.
    pub fn move_stmts(&self) -> impl Iterator<Item = &Move<R>> {
        self.move_stmts.iter()
    }
}

/// Support `cfg[stmt_idx]` indexing.
impl<'stmts, R, I> Index<StmtIdx> for Cfg<'stmts, R, I> {
    type Output = Stmt<I>;

    fn index(&self, index: StmtIdx) -> &Self::Output {
        &self.stmts[index.0]
    }
}

/// Support `cfg[stmt_idx_start..stmt_idx_end]` indexing.
impl<'stmts, R, I> Index<Range<StmtIdx>> for Cfg<'stmts, R, I> {
    type Output = [Stmt<I>];

    fn index(&self, index: Range<StmtIdx>) -> &Self::Output {
        &self.stmts[index.start.0 .. index.end.0]
    }
}

/// Support `cfg[bb_idx]` indexing.
impl<'stmts, R, I> Index<BbIdx> for Cfg<'stmts, R, I> {
    type Output = Bb;

    fn index(&self, index: BbIdx) -> &Self::Output {
        &self.bbs[index.0]
    }
}
