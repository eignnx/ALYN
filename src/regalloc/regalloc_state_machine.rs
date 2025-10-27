use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet},
};

use crate::{
    instr_sel::Stg,
    regalloc::{Cc, Instr, cfg::Cfg, interferences::Interferences, live_sets::LiveSets},
};
use alyn_common::names::Tmp;

struct ColorGraph<R> {
    interferences: HashMap<Tmp, HashSet<Tmp>>,
    moves: HashMap<Tmp, BTreeSet<Stg<R>>>,
}

impl<R> ColorGraph<R>
where
    R: Cc,
{
    fn new<I>(cfg: &Cfg<I>, live_sets: &LiveSets<R>) -> Self
    where
        I: Instr<Register = R>,
        R: Cc,
    {
        let mut intfs = Interferences::new();
        intfs.compute_interferences(cfg, live_sets);
        let mut this = Self {
            interferences: Default::default(),
            moves: Default::default(),
        };
        for (stg, nbrs) in intfs.graph {
            let Stg::Tmp(tmp) = stg else { continue };
            for nbr in nbrs {
                let Stg::Tmp(nbr) = nbr else { continue };
                this.record_interference(tmp, nbr);
            }
        }

        for instr in cfg.iter_stmts() {
            if let Some((a, b)) = instr.try_as_pure_move() {
                match (a, b) {
                    (Stg::Tmp(tmp), other) | (other, Stg::Tmp(tmp)) => {
                        this.record_move(tmp, other);
                    }
                    (Stg::Reg(_), Stg::Reg(_)) => {}
                }
            }
        }

        this
    }

    fn interferes_with(&self, a: Tmp, b: Tmp) -> bool {
        self.interferences[&a].contains(&b)
    }

    fn record_interference(&mut self, a: Tmp, b: Tmp) {
        self.interferences.entry(a).or_default().insert(b);
        self.interferences.entry(b).or_default().insert(a);
    }

    fn record_move(&mut self, a: Tmp, b: Stg<R>) {
        self.moves.entry(a).or_default().insert(b);
        if let Stg::Tmp(b) = b {
            self.moves.entry(b).or_default().insert(a.into());
        }
    }

    fn is_move_related_to(&self, a: Tmp, b: Stg<R>) -> bool {
        self.moves[&a].contains(&b)
    }

    fn move_relations(&self, a: Tmp) -> impl Iterator<Item = Stg<R>> {
        self.moves[&a].iter().copied()
    }

    fn take_some_insig_node(&mut self) -> Option<(Tmp, HashSet<Tmp>)> {
        let selected = self.select_some_insig_node()?;
        let e = self.interferences.remove_entry(&selected).unwrap();
        for nbrs in self.interferences.values_mut() {
            nbrs.remove(&selected);
        }

        self.remove_moves_related_to(selected);

        Some(e)
    }

    fn select_some_insig_node(&self) -> Option<Tmp> {
        self.interferences
            .keys()
            .filter(|tmp| !self.moves.contains_key(*tmp))
            .filter(|tmp| self.degree(**tmp) < R::N_GPRS)
            .max_by_key(|tmp| self.degree(**tmp))
            .copied()
    }

    fn reinsert_entry(&mut self, tmp: Tmp, neighbors: HashSet<Tmp>) {
        for nbr in neighbors {
            self.record_interference(tmp, nbr);
        }
    }

    fn degree(&self, tmp: Tmp) -> usize {
        self.interferences[&tmp].len()
    }

    fn remove_moves_related_to(&mut self, to_remove: Tmp) {
        self.moves.remove(&to_remove);

        for ys in self.moves.values_mut() {
            ys.remove(&to_remove.into());
        }
    }
}

type NodeEntry = (Tmp, HashSet<Tmp>);

enum RegAllocState<I, R> {
    Setup {
        params: Vec<Tmp>,
        program: Vec<I>,
    },
    Build {
        cfg: Cfg<I>,
    },
    GraphSimplifyStep {
        cfg: Cfg<I>,
        graph: ColorGraph<R>,
        node_stack: Vec<NodeEntry>,
        assignments: HashMap<Tmp, Stg<R>>,
    },
    Coalesce {
        cfg: Cfg<I>,
        graph: ColorGraph<R>,
        node_stack: Vec<NodeEntry>,
        assignments: HashMap<Tmp, Stg<R>>,
    },
    Freeze {
        cfg: Cfg<I>,
        graph: ColorGraph<R>,
        node_stack: Vec<NodeEntry>,
        assignments: HashMap<Tmp, Stg<R>>,
    },
    ChoosePotentialSpill {
        cfg: Cfg<I>,
        graph: ColorGraph<R>,
        node_stack: Vec<NodeEntry>,
        assignments: HashMap<Tmp, Stg<R>>,
    },
    PopAndAssign {
        cfg: Cfg<I>,
        graph: ColorGraph<R>,
        node_stack: Vec<NodeEntry>,
        assignments: HashMap<Tmp, Stg<R>>,
    },
    SpillAndRewrite {
        cfg: Cfg<I>,
        to_spill: Tmp,
    },
    Done {
        program: Vec<I>,
        assignments: HashMap<Tmp, R>,
    },
}

pub const MAX_REGALLOC_ITERS: usize = 16;

pub fn allocate_registers<I, R>(params: Vec<Tmp>, program: Vec<I>) -> (Vec<I>, HashMap<Tmp, R>)
where
    I: Instr<Register = R>,
    R: Cc,
{
    let mut state = RegAllocState::Setup { program, params };
    for _ in 0..MAX_REGALLOC_ITERS {
        state = state.step();
        if let RegAllocState::Done {
            program,
            assignments,
        } = state
        {
            return (program, assignments);
        }
    }
    panic!("regalloc: max regalloc iterations exceeded: {MAX_REGALLOC_ITERS}");
}

impl<I, R> RegAllocState<I, R>
where
    I: Instr<Register = R>,
    R: Cc,
{
    pub fn step(self) -> Self {
        match self {
            Self::Setup { program, params } => {
                let cfg = Cfg::new(0, params, program);
                let cfg = Self::precolor(cfg);
                Self::Build { cfg }
            }

            Self::Build { cfg } => {
                let mut live_sets = LiveSets::new();
                live_sets.compute_live_ins_live_outs(&cfg);
                let graph = ColorGraph::new(&cfg, &live_sets);
                let node_stack = Vec::new();
                let assignments = HashMap::new();
                Self::GraphSimplifyStep {
                    cfg,
                    graph,
                    node_stack,
                    assignments,
                }
            }

            Self::GraphSimplifyStep {
                cfg,
                mut graph,
                mut node_stack,
                assignments,
            } => {
                if let Some(entry) = graph.take_some_insig_node() {
                    node_stack.push(entry);
                    todo!()
                } else {
                    todo!()
                }
            }

            Self::Coalesce {
                cfg,
                graph,
                node_stack,
                assignments,
            } => {
                todo!()
            }

            Self::Freeze {
                cfg,
                graph,
                node_stack,
                assignments,
            } => {
                todo!()
            }

            Self::ChoosePotentialSpill {
                cfg,
                graph,
                node_stack,
                assignments,
            } => {
                todo!()
            }

            Self::PopAndAssign {
                cfg,
                graph,
                node_stack,
                assignments,
            } => {
                todo!()
            }

            Self::SpillAndRewrite { cfg, to_spill } => {
                todo!()
            }

            Self::Done {
                program,
                assignments,
            } => {
                todo!()
            }
        }
    }

    fn precolor(cfg: Cfg<I>) -> Cfg<I> {
        todo!()
    }
}
