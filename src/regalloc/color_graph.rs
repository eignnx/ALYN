use core::fmt;
use std::collections::{BTreeMap, BTreeSet};

use crate::{
    instr_sel::Stg,
    regalloc::{Cc, Instr, cfg::Cfg, interferences::Interferences, live_sets::LiveSets},
};

pub type NodeEntry<R> = (Stg<R>, BTreeSet<Stg<R>>);

pub struct ColorGraph<R> {
    interferences: Interferences<R>,
    move_relations: BTreeMap<Move<R>, usize>,
}

impl<R> From<Interferences<R>> for ColorGraph<R> {
    fn from(interferences: Interferences<R>) -> Self {
        Self {
            interferences,
            move_relations: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Move<R> {
    dst: Stg<R>,
    src: Stg<R>,
}

impl<R> ColorGraph<R>
where
    R: fmt::Debug + Ord + Eq + Copy + Cc<R> + 'static,
{
    pub fn new<I>(cfg: &Cfg<I>, live_sets: &LiveSets<R>) -> Self
    where
        I: Instr<Register = R>,
    {
        let mut interferences = Interferences::new();
        interferences.compute_interferences(cfg, live_sets);

        let mut move_relations = BTreeMap::<Move<R>, usize>::new();

        for (stmt_id, stmt) in cfg.stmts.iter().enumerate() {
            match stmt.try_as_pure_move() {
                Some((Stg::Reg(_), Stg::Reg(_))) | None => {}
                Some((dst, src)) => {
                    move_relations.insert(Move { dst, src }, stmt_id);
                }
            }
        }

        Self {
            interferences,
            move_relations,
        }
    }

    pub fn are_move_related(&self, x: Stg<R>, y: Stg<R>) -> bool {
        self.move_relations.contains_key(&Move {src: x, dst: y}) || self.move_relations.contains_key(&Move {src: y, dst: x})
    }

    pub fn insert(&mut self, n: Stg<R>, neighbors: BTreeSet<Stg<R>>) {
        self.interferences.graph.insert(n, neighbors);
    }

    /// Only count as a neighbor if the node is still in the graph. Instances of neighbors whose
    /// node is not in `self.graph` are skipped.
    pub fn active_neighbors_of(&self, n: &Stg<R>) -> impl Iterator<Item = &Stg<R>> {
        let Some(neighbors) = self.interferences.graph.get(n) else {
            panic!("Unknown Stg<R>: {n:?}");
        };
        neighbors
            .iter()
            .filter(|neighbor| self.interferences.graph.contains_key(*neighbor))
    }

    #[track_caller]
    pub fn degree(&self, n: &Stg<R>) -> usize {
        self.active_neighbors_of(n).count()
    }

    #[track_caller]
    pub fn is_sig_degree(&self, n: &Stg<R>) -> bool {
        self.degree(n) >= R::N_GPRS
    }

    #[track_caller]
    pub fn is_insig_degree(&self, n: &Stg<R>) -> bool {
        self.degree(n) < R::N_GPRS
    }

    pub fn coalesce(&mut self, n1: &Stg<R>, n2: &Stg<R>) {
        todo!()
    }

    pub fn take_node(&mut self, n: &Stg<R>) -> NodeEntry<R> {
        let Some(entry) = self.interferences.graph.remove_entry(n) else {
            panic!("Unknown storage node: {n:?}");
        };
        entry
    }

    /// Removes an arbitrary node (and its neighbor set) as long as the node has fewer than N
    /// neighbors. Returns `None` if no such node can be found.
    pub fn take_some_insig_node(&mut self) -> Option<NodeEntry<R>> {
        let mut key: Option<Stg<R>> = None;
        for n in self.interferences.graph.keys() {
            if self.is_insig_degree(n) {
                key = Some(n.clone());
                break;
            }
        }
        Some(self.take_node(&key?))
    }

    pub fn choose_node_to_spill(&mut self) -> Option<NodeEntry<R>> {
        let max_node = self
            .interferences
            .graph
            .keys()
            .max_by_key(|n| self.degree(n))
            .cloned()?;
        Some(self.take_node(&max_node))
    }
}

impl<R: fmt::Debug> fmt::Debug for ColorGraph<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "strict graph {{")?;
        for (tmp, neighbors) in &self.interferences.graph {
            let tmp = format!("{tmp:?}").replace('%', "").replace('.', "_");
            write!(f, "    {tmp} -- {{")?;
            for n in neighbors {
                let n = format!("{n:?}").replace('%', "").replace('.', "_");
                write!(f, " {n}")?;
            }
            writeln!(f, " }}")?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

