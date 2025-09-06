use std::collections::{BTreeSet, BTreeMap};
use smallvec::{SmallVec, smallvec};

use crate::{names::Tmp, regalloc::live_sets::LiveSets};

use super::{
    cfg::Cfg,
    interferences::Interferences,
};

trait MachineEnv {
    type Reg: 'static;
    const REGS: &'static [Self::Reg];
    const N_GPRS: usize = Self::REGS.len();
    type Instr;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct NodeGroup(SmallVec<[Tmp; 2]>);

impl From<Tmp> for NodeGroup {
    fn from(tmp: Tmp) -> Self {
        Self(smallvec![tmp])
    }
}

impl NodeGroup {
    #[track_caller]
    fn get_one(&self) -> &Tmp {
        self.0.first().expect("Empty NodeGroup!")
    }

    fn iter(&self) -> impl Iterator<Item = Tmp> {
        self.0.iter().copied()
    }
}

type NodeEntry = (NodeGroup, BTreeSet<NodeGroup>);

struct ColorGraph {
    graph: BTreeMap<NodeGroup, BTreeSet<NodeGroup>>
}

impl ColorGraph {
    fn from_interferences(interferences: Interferences) -> Self {
        let graph = interferences
            .take_graph()
            .into_iter()
            .map(|(tmp, neighbors)| {
                let neighbors = neighbors.into_iter()
                    .map(NodeGroup::from)
                    .collect();
                (NodeGroup::from(tmp), neighbors)
            }).collect();
        Self { graph }
    }

    fn insert(&mut self, ng: NodeGroup, neighbors: BTreeSet<NodeGroup>) {
        self.graph.insert(ng, neighbors);
    }

    #[track_caller]
    fn degree(&self, ng: &NodeGroup) -> usize {
        let Some(neighbors) = self.graph.get(ng) else {
            panic!("Unknown NodeGroup: {ng:?}");
        };

        // Only count node groups that haven't been removed from the graph.
        neighbors
            .iter()
            .filter(|neighbor| self.graph.contains_key(*neighbor))
            .count()
    }

    #[track_caller]
    fn is_sig_degree<const N: usize>(&self, ng: &NodeGroup) -> bool {
        self.degree(ng) >= N
    }

    #[track_caller]
    fn is_insig_degree<const N: usize>(&self, ng: &NodeGroup) -> bool {
        self.degree(ng) < N
    }

    fn coalesce(&mut self, ng1: &NodeGroup, ng2: &NodeGroup) {
        todo!()
    }

    fn take(&mut self, ng: &NodeGroup) -> NodeEntry {
        let Some(entry) = self.graph.remove_entry(ng) else {
            panic!("Unknown NodeGroup: {ng:?}");
        };
        entry
    }

    fn take_some_insig_node<const N: usize>(&mut self) -> Option<NodeEntry> {
        let mut key = None;
        for ng in self.graph.keys() {
            if self.is_insig_degree::<N>(ng) {
                key = Some(ng.clone());
                break;
            }
        }
        Some(self.take(&key?))
    }

    fn try_take_some_node(&mut self) -> Option<NodeEntry> {
        // Arbitrarily remove the first node.
        self.graph.pop_first() 
    }

    fn active_neighbors_of(&self, ng: &NodeGroup) -> impl Iterator<Item = &NodeGroup> {
        let Some(neighbors) = self.graph.get(ng) else {
            panic!("Unknown NodeGroup: {ng:?}");
        };
        neighbors
            .iter()
            .filter(|neighbor| self.graph.contains_key(*neighbor))
    }
}

enum CtxStackEntry {
    Selectable(NodeEntry),
    MaybeSpill(NodeEntry),
}

/// Register Allocator
pub struct RegAlloc<const N_GPRS: usize> {
}

impl<const N_GPRS: usize> RegAlloc<N_GPRS> {
    fn allocate_registers(&mut self, cfg: &mut Cfg) {
        loop {
            let mut color_graph = self.build_phase(cfg);
            let mut stack = self.simplify_phase(&mut color_graph);
            let assignments = self.select_phase(&mut color_graph, &mut stack);
        }
    }

    fn build_phase(&mut self, cfg: &Cfg) -> ColorGraph {
        let mut live_sets = LiveSets::new();
        live_sets.compute_live_ins_live_outs(cfg);
        let mut interferences = Interferences::new();
        interferences.compute_interferences(cfg, &live_sets);
        ColorGraph::from_interferences(interferences)
    }

    fn simplify_phase(&mut self, color_graph: &mut ColorGraph) -> Vec<CtxStackEntry> {
        let mut stack = Vec::new();

        loop {
            // First remove all easy nodes and save them to the stack.
            while let Some(entry) = color_graph.take_some_insig_node::<N_GPRS>() {
                stack.push(CtxStackEntry::Selectable(entry));
            }

            // Then if we're not done, we'll have to (potentially) spill a node.
            // Choose a node to remove and mark for spillage, then loop again
            // to see if that freed up more easy nodes.
            if let Some(entry) = color_graph.try_take_some_node() {
                stack.push(CtxStackEntry::MaybeSpill(entry));
            } else {
                // Graph must be empty, so return the stack.
                return stack;
            }
        }
    }


    fn select_phase(&mut self, color_graph: &mut ColorGraph, stack: &mut Vec<CtxStackEntry>) -> BTreeMap<Tmp, usize> {
        let mut assignments = BTreeMap::new();

        while let Some(ctx_stack_entry) = stack.pop() {
            match ctx_stack_entry {
                CtxStackEntry::Selectable((ng, neighbors)) => {
                    color_graph.insert(ng.clone(), neighbors.clone());
                    if let Some((ng, reg_id)) = self.select_once(color_graph, &assignments, ng, neighbors) {
                        for tmp in ng.iter() {
                            assignments.insert(tmp, reg_id);
                        }
                    } else {
                        unreachable!()
                    }
                }
                CtxStackEntry::MaybeSpill((ng, neighbors)) => {
                    color_graph.insert(ng.clone(), neighbors.clone());
                    if let Some((ng, reg_id)) = self.select_once(color_graph, &assignments, ng.clone(), neighbors) {
                        for tmp in ng.iter() {
                            assignments.insert(tmp, reg_id);
                        }
                    } else {
                        todo!("Handle spill of NodeGroup: {ng:?}");
                    }
                }
            }
        }

        assignments
    }

    fn select_once(
        &mut self,
        color_graph: &mut ColorGraph,
        assignments: &BTreeMap<Tmp, usize>,
        ng: NodeGroup,
        neighbors: BTreeSet<NodeGroup>
    ) -> Option<(NodeGroup, usize)>
    {
        let mut reg_ids_in_use = BTreeSet::new();
        for neighbor in color_graph.active_neighbors_of(&ng) {
            let a_neighbor = neighbor.get_one();
            let in_use = *assignments.get(a_neighbor).unwrap();
            reg_ids_in_use.insert(in_use);
        }
        for reg_id in 0..N_GPRS {
            if !reg_ids_in_use.contains(&reg_id) {
                return Some((ng, reg_id));
            }
        }
        None
    }
}
