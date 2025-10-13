use std::{collections::{BTreeMap, BTreeSet}, fmt};

use crate::{
    instr_sel::Stg,
    regalloc::{cfg::Cfg, interferences::Interferences, live_sets::{LiveSets, Move}, Cc, Instr},
};

pub type NodeEntry<R> = (Stg<R>, BTreeSet<Stg<R>>);

pub struct ColorGraph<R> {
    interferences: Interferences<R>,
    move_rels: BTreeSet<Move<R>>,
}

impl<R> From<Interferences<R>> for ColorGraph<R> {
    fn from(interferences: Interferences<R>) -> Self {
        Self {
            interferences,
            move_rels: Default::default(),
        }
    }
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

        let mut move_rels = BTreeSet::<Move<R>>::new();

        for (stmt_id, stmt) in cfg.stmts.iter().enumerate() {
            match stmt.try_as_pure_move() {
                Some((Stg::Reg(_), Stg::Reg(_))) | None => {}
                Some((dst, src)) => {
                    move_rels.insert(Move { dst, src, instr_id: stmt_id });
                }
            }
        }

        Self {
            interferences,
            move_rels,
        }
    }

    pub fn are_move_related(&self, x: Stg<R>, y: Stg<R>) -> bool {
        self.move_rels.iter()
            .any(|mv| mv.dst == x && mv.src == y || mv.dst == y && mv.src == x)
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
            write!(f, "    {:?} -- {{", DotEsc(tmp))?;
            for n in neighbors {
                write!(f, " {:?}", DotEsc(n))?;
            }
            writeln!(f, " }}")?;
        }

        writeln!(f)?;

        for mv in &self.move_rels {
            writeln!(f, "    {:?} -- {:?} [style=dashed]", DotEsc(&mv.src), DotEsc(&mv.dst))?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

struct DotEsc<T: fmt::Debug>(T);

impl<T: fmt::Debug> fmt::Debug for DotEsc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = format!("{:?}", self.0);
        let text = text.replace("%", "\\%");
        write!(f, "\"{text}\"")
    }
}
