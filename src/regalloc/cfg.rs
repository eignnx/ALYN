use crate::regalloc::CtrlTx;
use alyn_common::names::{Lbl, Tmp};
use std::collections::{BTreeMap, BTreeSet, HashMap};

use super::Instr;

pub type NodeId = usize;

/// Control Flow Graph
pub struct Cfg<I> {
    pub params: Vec<Tmp>,
    pub stmts: Vec<I>,
    pub edges: Vec<(NodeId, NodeId)>,
    pub entry: NodeId,
    pub exits: BTreeSet<NodeId>,
    labels: HashMap<Lbl, NodeId>,
    live_ins_on_entry: BTreeSet<Tmp>,
}

impl<I: Instr> Cfg<I> {
    pub fn new(
        entry: NodeId,
        params: impl IntoIterator<Item = Tmp>,
        stmts: impl IntoIterator<Item = I>,
    ) -> Self {
        let mut this = Self {
            params: params.into_iter().collect(),
            stmts: stmts.into_iter().collect(),
            edges: Default::default(),
            entry,
            exits: Default::default(),
            labels: Default::default(),
            live_ins_on_entry: Default::default(),
        };

        this.discover_labels();
        this.discover_edges();

        this
    }

    pub fn iter_stmts(&self) -> impl Iterator<Item = &I> {
        self.stmts.iter()
    }

    #[track_caller]
    fn label_to_node_id(&self, label: Lbl) -> NodeId {
        let Some(node_id) = self.labels.get(&label) else {
            panic!("Label not defined: {label:?}");
        };
        *node_id
    }

    fn discover_labels(&mut self) {
        for (id, stmt) in self.stmts.iter().enumerate() {
            if let Some(lbl) = stmt.try_as_lbl() {
                self.labels.insert(lbl, id);
            }
        }
    }

    fn discover_edges(&mut self) {
        for (id, stmt) in self.stmts.iter().enumerate() {
            if let Some(tx) = stmt.ctrl_tx() {
                match tx {
                    CtrlTx::Advance => self.edges.push((id, id + 1)),
                    CtrlTx::Jump(lbl) => self.edges.push((id, self.label_to_node_id(lbl))),
                    CtrlTx::Branch(lbl) => {
                        self.edges.push((id, id + 1)); // For fallthrough
                        self.edges.push((id, self.label_to_node_id(lbl)));
                    }
                    CtrlTx::Switch(lbls) => {
                        for lbl in lbls {
                            self.edges.push((id, self.label_to_node_id(lbl)));
                        }
                    }
                }
            } else {
                self.exits.insert(id);
            }
        }
    }

    pub fn successors(&self, node_id: NodeId) -> impl Iterator<Item = NodeId> {
        self.edges
            .iter()
            .filter_map(move |(from, to)| (*from == node_id).then_some(*to))
    }

    pub fn predecessors(&self, node_id: NodeId) -> impl Iterator<Item = NodeId> {
        self.edges
            .iter()
            .filter_map(move |(from, to)| (*to == node_id).then_some(*from))
    }

    pub fn exits(&self) -> impl Iterator<Item = &I> {
        self.iter_stmts()
            .enumerate()
            .filter_map(|(id, stmt)| self.exits.contains(&id).then_some(stmt))
    }
}
