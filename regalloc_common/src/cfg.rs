use std::collections::HashMap;

use alyn_common::names::Lbl;

use crate::{ctrl_flow::{CtrlFlow, GetCtrlFlow}, stg::Stg, stmt::Stmt, Instruction};

pub type StmtIdx = usize;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Move<R> {
    pub dst: Stg<R>,
    pub src: Stg<R>,
    pub stmt_idx: StmtIdx,
}

pub struct Cfg<'stmts, R, I: GetCtrlFlow> {
    stmts: &'stmts [Stmt<I>],
    entry: usize,
    exits: Vec<StmtIdx>,
    edges: Vec<(StmtIdx, StmtIdx)>,
    labels: HashMap<Lbl, StmtIdx>,
    move_stmts: Vec<Move<R>>,
}

impl<'stmts, R, I: Instruction<Reg = R> + GetCtrlFlow> Cfg<'stmts, R, I> {
    pub fn build_from(
        entry: StmtIdx,
        stmts: &'stmts [Stmt<I>],
    ) -> Self {
        let mut this = Self {
            stmts,
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
                    self.labels.insert(*lbl, idx);
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
                    self.labels.insert(*lbl, idx);
                }
                Stmt::Instr(instr) => match instr.ctrl_flow() {
                    CtrlFlow::Exit => self.exits.push(idx),
                    CtrlFlow::Advance => self.edges.push((idx, idx + 1)),
                    CtrlFlow::Jump(lbl) => self.edges.push((idx, self.label_to_stmt_idx(lbl))),
                    CtrlFlow::Branch(lbl) => {
                        self.edges.push((idx, self.label_to_stmt_idx(lbl)));
                        self.edges.push((idx, idx + 1));
                    }
                    CtrlFlow::Switch(lbls) => {
                        for lbl in &lbls {
                            self.edges.push((idx, self.label_to_stmt_idx(*lbl)));
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

    #[track_caller]
    pub fn successors(&self, node_id: StmtIdx) -> impl Iterator<Item = StmtIdx> {
        let mut out = Vec::new();
        match &self.stmts[node_id].ctrl_flow() {
            CtrlFlow::Exit => {}
            CtrlFlow::Advance => out.push(node_id + 1),
            CtrlFlow::Jump(lbl) => out.push(self.label_to_stmt_idx(*lbl)),
            CtrlFlow::Switch(lbls) => {
                out.extend(lbls.iter().map(|lbl| self.label_to_stmt_idx(*lbl)))
            }
            CtrlFlow::Branch(lbl) => out.push(self.label_to_stmt_idx(*lbl)),
        }
        out.into_iter()
    }

    pub fn predecessors(&self, node_id: StmtIdx) -> impl Iterator<Item = StmtIdx> {
        // TODO: faster impl here?
        self.edges
            .iter()
            .filter_map(move |(from, to)| (*to == node_id).then_some(*from))
    }

    pub fn stmts(&self) -> impl DoubleEndedIterator<Item = &Stmt<I>> + ExactSizeIterator {
        self.stmts.iter()
    }

    pub fn entry(&self) -> StmtIdx {
        self.entry
    }

    pub fn exits(&self) -> impl Iterator<Item = StmtIdx> {
        self.exits.iter().copied()
    }

    pub fn known_labels(&self) -> impl Iterator<Item = Lbl> {
        self.labels.keys().copied()
    }

    pub fn move_stmts(&self) -> impl Iterator<Item = &Move<R>> {
        self.move_stmts.iter()
    }
}
