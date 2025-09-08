use crate::{names::Tmp, regalloc::Lbl};
use std::collections::{BTreeMap, BTreeSet, HashMap};

use super::{Expr, Stmt};

pub type NodeId = usize;

/// Control Flow Graph
pub struct Cfg {
    pub stmts: Vec<Stmt>,
    pub edges: Vec<(NodeId, NodeId)>,
    pub entry: NodeId,
    pub exits: Vec<NodeId>,
    labels: HashMap<Lbl, NodeId>,
    live_ins_on_entry: BTreeSet<Tmp>,
}

impl Cfg {
    pub fn new(entry: NodeId, stmts: impl IntoIterator<Item = Stmt>) -> Self {
        let mut this = Self {
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

    #[track_caller]
    fn label_to_node_id(&self, label: Lbl) -> NodeId {
        let Some(node_id) = self.labels.get(&label) else {
            panic!("Label not defined: {label:?}");
        };
        *node_id
    }

    fn discover_labels(&mut self) {
        for (id, stmt) in self.stmts.iter().enumerate() {
            if let Stmt::Lbl(lbl) = stmt {
                self.labels.insert(*lbl, id);
            }
        }
    }

    fn discover_edges(&mut self) {
        for (id, stmt) in self.stmts.iter().enumerate() {
            match stmt {
                Stmt::Mov(..)
                | Stmt::Store { .. }
                | Stmt::StackStore { .. }
                | Stmt::Load { .. }
                | Stmt::StackLoad { .. }
                | Stmt::Lbl(..) => self.edges.push((id, id + 1)),
                Stmt::Ret(..) => self.exits.push(id),
                Stmt::Jmp(lbl) => {
                    self.edges.push((id, self.label_to_node_id(*lbl)));
                }
                Stmt::Br(expr, lbl) => {
                    self.edges.push((id, id + 1)); // For fallthrough
                    self.edges.push((id, self.label_to_node_id(*lbl)));
                }
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
}
