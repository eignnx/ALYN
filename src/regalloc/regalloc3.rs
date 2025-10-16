//! Register Allocation via Coloring of Chordal Graphs
//! by Fernando Magno Quintat˜ao Pereira and Jens Palsbe

use std::{collections::{BTreeMap, BTreeSet}, marker::PhantomData};

use priority_queue::PriorityQueue;

use crate::{
    instr_sel::Stg,
    names::Tmp,
    regalloc::{cfg::Cfg, interferences::Interferences, live_sets::LiveSets, Cc, Instr},
};

fn simplicial_elimination_ordering<R>(graph: &Interferences<R>) -> Vec<Tmp>
where
    R: std::fmt::Debug + Copy + Eq + Ord + std::hash::Hash + Cc<R> + 'static,
{
    let mut ordering = Vec::new();

    let mut curr = graph
        .all_nodes()
        .filter_map(|node| match node {
            Stg::Reg(_) => None,
            Stg::Tmp(tmp) => Some(tmp),
        })
        .next()
        .unwrap();

    let mut weights: PriorityQueue<Tmp, usize> = graph
        .all_nodes()
        .filter_map(|node| match node {
            Stg::Tmp(tmp) => Some(tmp),
            Stg::Reg(_) => None,
        })
        .filter(|t| *t != curr)
        .map(|tmp| {
            // Count precolored nodes:
            let weight = graph
                .neighbors(&Stg::Tmp(tmp))
                .filter(|nbr| matches!(nbr, Stg::Reg(_)))
                .count();
            (tmp, weight)
        })
        .collect();


    while !weights.is_empty() {
        ordering.push(curr);
        for nbr in graph.neighbors(&Stg::Tmp(curr)) {
            let Stg::Tmp(nbr) = nbr else { continue };
            weights.change_priority_by(&nbr, |w| *w += 1);
        }
        (curr, _) = weights.pop().unwrap();
    }

    ordering.push(curr);

    ordering
}

/// If spill is necessary, the Tmp will *not* be added to the assignments map.
fn color_graph_greedily<R>(
    graph: &Interferences<R>,
    ordering: Vec<Tmp>,
) -> BTreeMap<Tmp, R>
where
    R: std::fmt::Debug + Copy + Eq + Ord + Cc<R> + 'static,
{
    let mut assignments = BTreeMap::new();

    for node in ordering {
        let mut in_use = BTreeSet::new();
        for nbr in graph.neighbors(&Stg::Tmp(node)) {
            match nbr {
                Stg::Reg(reg) => _ = in_use.insert(reg),
                Stg::Tmp(nbr) => {
                    if let Some(reg) = assignments.get(&nbr).copied() {
                        _ = in_use.insert(reg);
                    }
                }
            }
        }

        if let Some(&reg) = R::GPRS.into_iter().find(|reg| !in_use.contains(reg)) {
            assignments.insert(node, reg);
        } else {
            eprintln!("Spill needed for {node:?}");
        }
    }

    assignments
}

pub struct RegAlloc<I, R> {
    params: Vec<Tmp>,
    program: Vec<I>,
    _reg: PhantomData<R>,
}

impl<I, R> RegAlloc<I, R>
where I: Instr<Register = R> + Clone,
      R: Copy + Ord + Eq + std::fmt::Debug + std::hash::Hash + Cc<R> + 'static
{
    fn new(params: Vec<Tmp>, program: Vec<I>) -> Self {
        Self {
            params,
            program,
            _reg: PhantomData,
        }
    }

    fn allocate_registers(&mut self) -> Allocation<I, R> {
        let intfs = self.build();
        let ordering = simplicial_elimination_ordering(&intfs);
        let assignments = color_graph_greedily(&intfs, ordering);

        for tmp in intfs.all_nodes().filter_map(Stg::try_as_tmp) {
            if let Some(reg) = assignments.get(&tmp) {

            } else {
            }
        }
        todo!()
    }

    fn build(&mut self) -> Interferences<R> {
        let mut cfg = Cfg::new(0, self.params.iter().copied(), self.program.iter().cloned());
        let mut live_sets = LiveSets::new();
        live_sets.compute_live_ins_live_outs(&cfg);
        let mut intfs = Interferences::new();
        intfs
    }

}

pub struct Allocation<I, R> {
    program: Vec<I>,
    assignments: BTreeMap<Tmp, R>,
}


#[cfg(test)]
mod tests {
    use super::super::test_datastructures::{Expr as E, Expr, Reg, Stmt as S, Stmt};
    use super::*;

    fn compute_assignments(params: Vec<Tmp>, program: Vec<Stmt>) {
        crate::names::reset_name_ids();
        eprintln!("<<<<<<<<<<<<< GPRS = {:?} >>>>>>>>>>>>>", Reg::GPRS);
        let mut ra = RegAlloc::new(params, program);
        let alloc = ra.allocate_registers();
        eprintln!("ASSIGNMENTS:");
        for (tmp, reg_id) in &alloc.assignments {
            eprintln!("  {tmp:?} -> ${reg_id}");
        }
        eprintln!("FINAL CODE:");
        for stmt in &alloc.program {
            let mut rendered = format!("{stmt:?}");
            for (tmp, reg_id) in &alloc.assignments {
                let tmp_rendered = format!("{tmp:?}");
                let reg_rendered = format!("${reg_id}");
                rendered = rendered.replace(&tmp_rendered, &reg_rendered);
            }
            eprintln!("|\t{rendered}");
        }
    }

#[test]
fn basic_test() {
    #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
    enum Reg { R, G, B }

    impl Cc<Reg> for Reg {
        const GPRS: &'static [Reg] = &[Reg::R, Reg::G, Reg::B];
        const GPR_SAVED_REGS: &'static [Reg] = &[Reg::B];
        const GPR_TEMP_REGS: &'static [Reg] = &[Reg::G];
        const GPR_ARG_REGS: &'static [Reg] = &[Reg::R];
    }

    fn entry<const N: usize>(node: &str, nbrs: [&str; N]) -> (Stg<Reg>, BTreeSet<Stg<Reg>>) {
        let node = Stg::Tmp(node.into());
        let nbrs = nbrs.into_iter().map(|name| Stg::Tmp(name.into())).collect();
        (node, nbrs)
    }

    let graph = BTreeMap::<Stg<Reg>, BTreeSet<Stg<Reg>>>::from_iter([
        entry("a", ["c", "b"]),
        entry("c", ["a", "b"]),
        entry("b", ["a", "c", "e", "f"]),
        entry("e", ["b", "f", "g"]),
        entry("f", ["b", "e", "g"]),
        entry("g", ["e", "f"]),

        entry("z", ["e", "f", "g"]),
    ]);

    // let seo = simplicial_elimination_ordering(&graph);
    // eprintln!("seo: {seo:?}");

    // let assignments = color_graph_greedily(&graph, seo);
    // eprintln!("assignments: {assignments:#?}");
}
}
