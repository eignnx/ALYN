//! Register Allocation via Coloring of Chordal Graphs
//! by Fernando Magno Quintat˜ao Pereira and Jens Palsbe

use std::{
    collections::{BTreeMap, BTreeSet},
    marker::PhantomData,
};

use priority_queue::PriorityQueue;

use crate::{
    instr_sel::Stg,
    regalloc::{Cc, Instr, cfg::Cfg, interferences::Interferences, live_sets::LiveSets},
};
use alyn_common::names::Tmp;

fn simplicial_elimination_ordering<R: Cc>(graph: &Interferences<R>) -> Vec<Tmp> {
    let mut ordering = Vec::new();

    let mut curr = graph
        .all_nodes()
        .filter_map(Stg::try_as_tmp)
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

/// An Assignment
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Asn<R> {
    /// The value will live the given register.
    Reg(R),
    /// The value will live on the stack. It's location on the stack will be decided upon later.
    /// For now, it has a unique ID only.
    Slot(usize),
}

fn iter_slots<R>() -> impl Iterator<Item = Asn<R>> {
    let mut i = 0;
    std::iter::from_fn(move || {
        let current = i;
        i += 1;
        Some(Asn::Slot(current))
    })
}

fn iter_choices<R: Cc>() -> impl Iterator<Item = Asn<R>> {
    R::GPRS
        .into_iter()
        .copied()
        .map(Asn::Reg)
        .chain(iter_slots())
}

fn select_assignment<R: Cc>(in_use: &BTreeSet<Asn<R>>) -> Asn<R> {
    let Some(choice) = iter_choices().find(|choice| !in_use.contains(choice)) else {
        unreachable!("Should always find an available stack slot");
    };
    choice
}

fn color_graph_greedily<R: Cc>(
    graph: &Interferences<R>,
    ordering: Vec<Tmp>,
) -> BTreeMap<Tmp, Asn<R>> {
    let mut assignments = BTreeMap::new();
    let mut in_use: BTreeSet<Asn<R>> = BTreeSet::new();

    for node in ordering {
        in_use.clear();
        for nbr in graph.neighbors(&Stg::Tmp(node)) {
            match nbr {
                Stg::Reg(reg) => {
                    in_use.insert(Asn::Reg(reg));
                }
                Stg::Tmp(nbr) => {
                    if let Some(a) = assignments.get(&nbr).copied() {
                        _ = in_use.insert(a);
                    }
                }
            }
        }

        assignments.insert(node, select_assignment(&in_use));
    }

    assignments
}

pub trait SlotAllocator {
    fn get_or_choose_bp_offset_for_slot_id(&mut self, slot_id: usize) -> i32;
}

pub struct RegAlloc<I, R, S: SlotAllocator> {
    params: Vec<Tmp>,
    program: Vec<I>,
    slot_alloc: S,
    _reg: PhantomData<R>,
}

impl<I, R: Cc, S: SlotAllocator> RegAlloc<I, R, S>
where
    I: Instr<Register = R>,
{
    fn new(params: Vec<Tmp>, program: Vec<I>, slot_alloc: S) -> Self {
        Self {
            params,
            program,
            slot_alloc,
            _reg: PhantomData,
        }
    }

    fn build(&mut self) -> Interferences<R> {
        let mut cfg = Cfg::new(0, self.params.iter().copied(), self.program.iter().cloned());
        let mut live_sets = LiveSets::new();
        live_sets.compute_live_ins_live_outs(&cfg);
        let mut intfs = Interferences::new();
        intfs
    }

    pub fn allocate_registers(&mut self) -> Allocation<I, R> {
        let intfs = self.build();
        let ordering = simplicial_elimination_ordering(&intfs);
        let assignments = color_graph_greedily(&intfs, ordering);

        let mut new_program = Vec::<I>::new();

        for mut instr in self.program.drain(..) {
            if let Some((lhs, rhs)) = instr.try_as_pure_move() {
                let lhs = get_assignment_for(lhs, &assignments);
                let rhs = get_assignment_for(rhs, &assignments);
                if lhs == rhs {
                    continue;
                }
            }

            let mut defs = BTreeSet::new();
            let mut uses = BTreeSet::new();
            instr.add_defs_uses(&mut defs, &mut uses);

            let mut write_backs = Vec::new();

            for d in defs {
                let Stg::Tmp(tmp) = d else { continue };
                match get_assignment_for(d, &assignments) {
                    Asn::Reg(reg) => {
                        instr.replace_def_occurrances(tmp, Stg::Reg(reg));
                    }
                    Asn::Slot(id) => {
                        let addr = self.slot_alloc.get_or_choose_bp_offset_for_slot_id(id);
                        write_backs.extend(I::emit_store_to_stack(addr, tmp));
                    }
                }
            }

            new_program.push(instr);
            new_program.extend(write_backs);
        }

        todo!()
    }
}

fn get_assignment_for<R: Cc>(stg: Stg<R>, assignments: &BTreeMap<Tmp, Asn<R>>) -> Asn<R> {
    match stg {
        Stg::Reg(reg) => Asn::Reg(reg),
        Stg::Tmp(tmp) => assignments[&tmp],
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
        enum Reg {
            R,
            G,
            B,
        }

        impl Cc for Reg {
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
