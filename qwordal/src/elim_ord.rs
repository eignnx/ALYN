use alyn_common::names::Tmp;
use priority_queue::PriorityQueue;

use crate::{Register, common::Stg, intfs::Intfs};

pub fn simplicial_elimination_ordering<R: Register>(intfs: &Intfs<R>) -> Vec<Tmp> {
    let mut ordering = Vec::new();

    let mut curr = intfs.nodes().filter_map(Stg::try_as_tmp).next().unwrap();

    let mut weights: PriorityQueue<Tmp, usize> = intfs
        .nodes()
        .filter_map(Stg::try_as_tmp)
        .filter(|t| *t != curr)
        .map(|tmp| {
            // Count precolored nodes:
            let weight = intfs
                .neighbors(tmp.into())
                .filter_map(Stg::try_as_reg)
                .count();
            (tmp, weight)
        })
        .collect();

    while !weights.is_empty() {
        ordering.push(curr);
        for nbr in intfs.neighbors(curr.into()) {
            if let Stg::Tmp(nbr) = nbr {
                weights.change_priority_by(&nbr, |w| *w += 1);
            }
        }
        (curr, _) = weights.pop().unwrap();
    }

    ordering.push(curr);

    ordering
}
