use std::{collections::{BTreeMap, BTreeSet}, fmt};

use crate::{
    instr_sel::Stg,
    regalloc::{cfg::Cfg, interferences::Interferences, live_sets::{LiveSets, Move}, Cc, Instr}, utils::current_revision_summary,
};

pub type NodeEntry<R> = (Stg<R>, BTreeSet<Stg<R>>);

pub struct ColorGraph<R> {
    interferences: Interferences<R>,
    move_rels: BTreeSet<Move<R>>,
    coalescence_map: BTreeMap<Stg<R>, Stg<R>>,
}

impl<R> From<Interferences<R>> for ColorGraph<R> {
    fn from(interferences: Interferences<R>) -> Self {
        Self {
            interferences,
            move_rels: Default::default(),
            coalescence_map: Default::default(),
        }
    }
}

impl<R: Cc> ColorGraph<R> {
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
            coalescence_map: Default::default(),
        }
    }

    pub fn are_move_related(&self, x: Stg<R>, y: Stg<R>) -> bool {
        self.move_rels.iter()
            .any(|mv| mv.dst == x && mv.src == y || mv.dst == y && mv.src == x)
    }

    pub fn move_rels(&self) -> impl Iterator<Item=&Move<R>> {
        self.move_rels.iter()
    }

    pub fn freeze_some_move_rel(&mut self) -> Option<Move<R>> {
        self.move_rels.pop_first()
    }

    pub fn insert(&mut self, n: Stg<R>, neighbors: BTreeSet<Stg<R>>) {
        self.interferences.graph.insert(n, neighbors);
    }

    /// Only count as a neighbor if the node is still in the graph. Instances of neighbors whose
    /// node is not in `self.graph` are skipped.
    pub fn active_neighbors_of(&self, n: impl Into<Stg<R>>) -> impl Iterator<Item = &Stg<R>> {
        let n = n.into();
        let Some(neighbors) = self.interferences.graph.get(&n) else {
            panic!("Unknown Stg<R>: {n:?}");
        };
        neighbors
            .iter()
            .filter(|neighbor| self.interferences.graph.contains_key(*neighbor))
    }

    #[track_caller]
    pub fn degree(&self, n: impl Into<Stg<R>>) -> usize {
        self.active_neighbors_of(n.into()).count()
    }

    #[track_caller]
    pub fn is_sig_degree(&self, n: impl Into<Stg<R>>) -> bool {
        self.degree(n) >= R::N_GPRS
    }

    #[track_caller]
    pub fn is_insig_degree(&self, n: impl Into<Stg<R>>) -> bool {
        self.degree(n) < R::N_GPRS
    }

    pub fn coalesce(&mut self, n1: &Stg<R>, n2: &Stg<R>) {
        match (*n1, *n2) {
            (t1 @ Stg::Tmp(_), t2 @ Stg::Tmp(_)) => {
                self.coalescence_map.insert(t1, t2);
            }
            (tmp @ Stg::Tmp(_), reg @ Stg::Reg(_)) | (reg @ Stg::Reg(_), tmp @ Stg::Tmp(_)) => {
                self.coalescence_map.insert(tmp, reg);
            }
            (Stg::Reg(_), Stg::Reg(_)) => unreachable!(),
        }
    }

    pub fn safe_to_coalesce(&self, n1: &Stg<R>, n2: &Stg<R>) -> bool {
        !self.interferes_with(*n1, *n2) && (
            self.safe_by_briggs_test(n1, n2) || self.safe_by_georges_test(n1, n2)
        )
    }

    /// Nodes `a` and `b` can be coalesced if the resulting node `ab` will have fewer than K neighbors of
    /// significant degree (i.e. having >= K edges).
    fn safe_by_briggs_test(&self, n1: &Stg<R>, n2: &Stg<R>) -> bool {
        let mut combined_neighbors = BTreeSet::<&Stg<R>>::new();
        combined_neighbors.extend(self.active_neighbors_of(*n1));
        combined_neighbors.extend(self.active_neighbors_of(*n2));
        let n_sig_deg_nbrs = combined_neighbors
            .into_iter()
            .filter(|nbr| self.degree(**nbr) - 1 >= R::N_GPRS) // since n1 and n2 merge, subtract 1
            .count();
        n_sig_deg_nbrs < R::N_GPRS
    }

    /// Nodes `a` and `b` can be coalesced if, for every neighbor `t` of `a`, either:
    ///   - `t` already interferese with `b` or
    ///   - `t` is of insignificant degree.
    fn safe_by_georges_test(&self, n1: &Stg<R>, n2: &Stg<R>) -> bool {
        self.active_neighbors_of(*n1).all(|&n1_nbr| {
            self.interferes_with(n1_nbr, *n2) || self.is_insig_degree(n1_nbr)
        })
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
            if self.is_insig_degree(*n) {
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
            .max_by_key(|n| self.degree(**n))
            .cloned()?;
        Some(self.take_node(&max_node))
    }

    pub fn interferes_with(&self, x: impl Into<Stg<R>>, y: impl Into<Stg<R>>) -> bool {
        self.interferences.interferes_with(x.into(), y.into())
    }
}

impl<R: Cc> fmt::Debug for ColorGraph<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "strict graph {{")?;
        writeln!(f, "    concentrate=true")?;
        writeln!(f, r##"    label="{}""##, current_revision_summary())?;

        writeln!(f)?;

        writeln!(f, "    subgraph GPRS {{")?;
        writeln!(f, r##"        node [shape=circle fontsize="20pt" style=filled fillcolor="#eee" penwidth="2"]"##)?;
        for gpr in R::GPRS {
            writeln!(f, "        {:?}", DotEsc(Stg::Reg(*gpr)))?;
        }
        writeln!(f, "    }}")?;

        writeln!(f)?;

        writeln!(f, r##"    node [fontname="Arial:bold" fontsize="15pt"]"##)?;
        writeln!(f, r##"    node [shape=rect]"##)?;
        for &node in self.interferences.graph.keys() {
            write!(f, "    {:?} -- {{", DotEsc(node))?;
            for nbr in self.active_neighbors_of(node) {
                if node.try_as_reg().is_some() && nbr.try_as_reg().is_some() { continue }
                write!(f, " {:?}", DotEsc(nbr))?;
            }
            writeln!(f, " }}")?;
        }

        writeln!(f)?;

        writeln!(f, "    subgraph MoveRelations {{")?;
        writeln!(f, r##"        edge [style=dashed penwidth="2" color="#999"]"##)?;
        for mv in &self.move_rels {
            writeln!(f, "        {:?} -- {:?}", DotEsc(&mv.src), DotEsc(&mv.dst))?;
        }
        writeln!(f, "    }}")?;
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
