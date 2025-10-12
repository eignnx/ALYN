//! Register Allocation via Coloring of Chordal Graphs
//! by Fernando Magno Quintat˜ao Pereira and Jens Palsbe

use std::collections::{BTreeMap, BTreeSet};

use priority_queue::PriorityQueue;

use crate::{
    instr_sel::Stg,
    names::Tmp,
    regalloc::{Cc, interferences::Interferences},
};

fn simplicial_elimination_ordering<R>(graph: &BTreeMap<Stg<R>, BTreeSet<Stg<R>>>) -> Vec<Tmp>
where
    R: std::fmt::Debug + Copy + Eq + Ord + std::hash::Hash,
{
    let mut ordering = Vec::new();

    let mut curr = *graph
        .keys()
        .filter_map(|node| match node {
            Stg::Reg(_) => None,
            Stg::Tmp(tmp) => Some(tmp),
        })
        .next()
        .unwrap();

    let mut weights: PriorityQueue<Tmp, usize> = graph
        .keys()
        .filter_map(|&node| match node {
            Stg::Tmp(tmp) => Some(tmp),
            Stg::Reg(_) => None,
        })
        .filter(|t| *t != curr)
        .map(|tmp| {
            // Count precolored nodes:
            let weight = graph
                .get(&Stg::Tmp(tmp))
                .unwrap()
                .iter()
                .filter(|nbr| matches!(nbr, Stg::Reg(_)))
                .count();
            (tmp, weight)
        })
        .collect();


    while !weights.is_empty() {
        ordering.push(curr);
        for nbr in graph.get(&Stg::Tmp(curr)).unwrap() {
            let Stg::Tmp(nbr) = nbr else { continue };
            weights.change_priority_by(nbr, |w| *w += 1);
        }
        (curr, _) = weights.pop().unwrap();
    }

    ordering.push(curr);

    ordering
}

/// If spill is necessary, the Tmp will *not* be added to the assignments map.
fn color_graph_greedily<R>(
    graph: &BTreeMap<Stg<R>, BTreeSet<Stg<R>>>,
    ordering: Vec<Tmp>,
) -> BTreeMap<Tmp, R>
where
    R: std::fmt::Debug + Copy + Eq + Ord + Cc<R> + 'static,
{
    let mut assignments = BTreeMap::new();

    for node in ordering {
        let mut in_use = BTreeSet::new();
        for nbr in graph.get(&Stg::Tmp(node)).unwrap() {
            match nbr {
                Stg::Reg(reg) => _ = in_use.insert(*reg),
                Stg::Tmp(nbr) => {
                    if let Some(reg) = assignments.get(nbr).copied() {
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

    let seo = simplicial_elimination_ordering(&graph);
    eprintln!("seo: {seo:?}");

    let assignments = color_graph_greedily(&graph, seo);
    eprintln!("assignments: {assignments:#?}");
}
