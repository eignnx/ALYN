use std::collections::HashMap;

use crate::{
    Instruction,
    common::{Lbl, Stg, Stmt, Tmp},
};

#[derive(Debug, Clone)]
pub enum CtrlTx {
    /// Exit the current subroutine. Either a return statement, a tail-call, or maybe
    /// `system_exit`.
    Exit,

    /// Just advance to the next instruction: `$PC <- $PC + 1`
    Advance,

    /// Unconditional jump to given label.
    Jump(Lbl),

    /// Jump to one of the given labels, like in a switch statement.
    Switch(Vec<Lbl>),

    /// Either fallthrough (which would be the same as `Advance`) or branch to the given label.
    Branch(Lbl),
}

pub trait ControlFlow {
    fn ctrl_tx(&self) -> CtrlTx;
}

pub type StmtIdx = usize;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Move<R> {
    pub dst: Stg<R>,
    pub src: Stg<R>,
    pub stmt_idx: StmtIdx,
}

pub struct Cfg<R, I: ControlFlow> {
    stmts: Vec<Stmt<I>>,
    pub entry: usize,
    exits: Vec<StmtIdx>,
    edges: Vec<(StmtIdx, StmtIdx)>,
    params: Vec<Tmp>,
    labels: HashMap<Lbl, StmtIdx>,
    move_stmts: Vec<Move<R>>,
}

impl<R, I: Instruction<Reg = R> + ControlFlow> Cfg<R, I> {
    pub fn new(
        entry: StmtIdx,
        params: impl IntoIterator<Item = Tmp>,
        stmts: impl IntoIterator<Item = Stmt<I>>,
    ) -> Self {
        let mut this = Self {
            params: params.into_iter().collect(),
            stmts: stmts.into_iter().collect(),
            edges: Default::default(),
            entry,
            exits: Default::default(),
            labels: Default::default(),
            move_stmts: Default::default(),
        };

        this.discover_labels_and_moves();
        this.discover_edges();

        this
    }

    fn discover_labels_and_moves(&mut self) {
        for (idx, stmt) in self.stmts.iter().enumerate() {
            match stmt {
                Stmt::Label(lbl) => {
                    self.labels.insert(lbl, idx);
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

    fn discover_edges(&mut self) {
        for (idx, stmt) in self.stmts.iter().enumerate() {
            match stmt {
                Stmt::Label(lbl) => {
                    self.labels.insert(lbl, idx);
                }
                Stmt::Instr(instr) => match instr.ctrl_tx() {
                    CtrlTx::Exit => self.exits.push(idx),
                    CtrlTx::Advance => self.edges.push((idx, idx + 1)),
                    CtrlTx::Jump(lbl) => self.edges.push((idx, self.label_to_stmt_idx(lbl))),
                    CtrlTx::Branch(lbl) => {
                        self.edges.push((idx, self.label_to_stmt_idx(lbl)));
                        self.edges.push((idx, idx + 1));
                    }
                    CtrlTx::Switch(lbls) => {
                        for lbl in &lbls {
                            self.edges.push((idx, self.label_to_stmt_idx(lbl)));
                        }
                    }
                },
            }
        }
    }

    #[track_caller]
    fn label_to_stmt_idx(&self, label: Lbl) -> StmtIdx {
        let Some(idx) = self.labels.get(&label) else {
            panic!("Label not defined: {label:?}");
        };
        *idx
    }

    pub fn successors(&self, node_id: StmtIdx) -> impl Iterator<Item = StmtIdx> {
        // TODO: faster impl here
        self.edges
            .iter()
            .filter_map(move |(from, to)| (*from == node_id).then_some(*to))
    }

    pub fn predecessors(&self, node_id: StmtIdx) -> impl Iterator<Item = StmtIdx> {
        // TODO: faster impl here
        self.edges
            .iter()
            .filter_map(move |(from, to)| (*to == node_id).then_some(*from))
    }

    pub fn stmts(&self) -> impl DoubleEndedIterator<Item = &Stmt<I>> + ExactSizeIterator {
        self.stmts.iter()
    }

    pub fn exits(&self) -> impl Iterator<Item = StmtIdx> {
        self.exits.iter().copied()
    }

    pub fn params(&self) -> impl Iterator<Item = Tmp> {
        self.params.iter().copied()
    }
}
