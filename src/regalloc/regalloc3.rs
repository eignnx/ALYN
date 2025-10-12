//! Register Allocation via Coloring of Chordal Graphs
//! by Fernando Magno Quintat˜ao Pereira and Jens Palsbe

use std::collections::{BTreeMap, BTreeSet};

use priority_queue::PriorityQueue;

use crate::{instr_sel::Stg, names::Tmp, regalloc::{interferences::Interferences, Cc}};

fn simplicial_elimination_ordering<R>(graph: &BTreeMap<Stg<R>, BTreeSet<Stg<R>>>) -> Vec<Stg<R>> 
where R: std::fmt::Debug + Copy + Eq + Ord + std::hash::Hash
{
    let mut ordering = Vec::new();
    let mut weights: PriorityQueue<_, _> = graph
        .keys()
        .map(|&node| (node, 0usize))
        .collect();

    let mut curr = *graph.keys().next().unwrap();

    while !weights.is_empty() {
        ordering.push(curr);
        for nbr in graph.get(&curr).unwrap() {
            weights.change_priority_by(nbr, |w| *w += 1);
        }
        (curr, _) = weights.pop().unwrap();
    }

    ordering
}

fn color_graph_greedily<R>(graph: &BTreeMap<Stg<R>, BTreeSet<Stg<R>>>, seo: Vec<Stg<R>>) -> BTreeMap<Tmp, R>
where R: std::fmt::Debug + Copy + Eq + Ord + Cc<R> + 'static
{
    let mut assignments = BTreeMap::new();

    for node in seo {
        let Stg::Tmp(node) = node else {
            continue;
        };
        let mut in_use = BTreeSet::new();
        for nbr in graph.get(&Stg::Tmp(node)).unwrap() {
            match nbr {
                Stg::Reg(reg) => _ = in_use.insert(*reg),
                Stg::Tmp(nbr) => if let Some(reg) = assignments.get(nbr).copied() {
                    _ = in_use.insert(reg);
                }
            }
        }


        if let Some(&reg) = R::GPRS.into_iter().find(|reg| !in_use.contains(reg)) {
            assignments.insert(node, reg);
        } else {
            panic!("Spill needed for {node:?}");
        }
    }

    assignments
}
