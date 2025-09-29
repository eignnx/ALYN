use smallvec::{SmallVec, smallvec};
use std::{
    collections::{BTreeMap, BTreeSet},
    io::Write,
};

use crate::{names::Tmp, regalloc::live_sets::LiveSets};

use super::{Instr, cfg::Cfg, interferences::Interferences};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// A storage node
pub enum Stg<R> {
    Tmp(Tmp),
    Reg(R),
}

impl<R> From<Tmp> for Stg<R> {
    fn from(tmp: Tmp) -> Self {
        Self::Tmp(tmp)
    }
}

impl<R: std::fmt::Debug> std::fmt::Debug for Stg<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stg::Tmp(tmp) => write!(f, "{tmp:?}"),
            Stg::Reg(reg) => write!(f, "{reg:?}"),
        }
    }
}

type NodeEntry<R> = (Stg<R>, BTreeSet<Stg<R>>);

struct ColorGraph<R> {
    graph: BTreeMap<Stg<R>, BTreeSet<Stg<R>>>,
}

impl<R: std::fmt::Debug> std::fmt::Debug for ColorGraph<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "strict graph {{")?;
        for (tmp, neighbors) in &self.graph {
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

impl<R> ColorGraph<R> 
where R: std::fmt::Debug + Ord + Clone
{
    fn from_interferences(interferences: Interferences) -> Self {
        let graph = interferences
            .take_graph()
            .into_iter()
            .map(|(tmp, neighbors)| {
                let neighbors = neighbors.into_iter().map(Stg::from).collect();
                (tmp.into(), neighbors)
            })
            .collect();
        Self { graph }
    }

    fn insert(&mut self, ng: Stg<R>, neighbors: BTreeSet<Stg<R>>) {
        self.graph.insert(ng, neighbors);
    }

    #[track_caller]
    fn degree(&self, ng: &Stg<R>) -> usize {
        let Some(neighbors) = self.graph.get(ng) else {
            panic!("Unknown storage node: {ng:?}");
        };

        // Only count node groups that haven't been removed from the graph.
        neighbors
            .iter()
            .filter(|neighbor| self.graph.contains_key(*neighbor))
            .count()
    }

    #[track_caller]
    fn is_sig_degree<const N: usize>(&self, ng: &Stg<R>) -> bool {
        self.degree(ng) >= N
    }

    #[track_caller]
    fn is_insig_degree<const N: usize>(&self, ng: &Stg<R>) -> bool {
        self.degree(ng) < N
    }

    fn coalesce(&mut self, ng1: &Stg<R>, ng2: &Stg<R>) {
        todo!()
    }

    fn take(&mut self, ng: &Stg<R>) -> NodeEntry<R> {
        let Some(entry) = self.graph.remove_entry(ng) else {
            panic!("Unknown storage node: {ng:?}");
        };
        entry
    }

    fn take_some_insig_node<const N: usize>(&mut self) -> Option<NodeEntry<R>> {
        let mut key: Option<Stg<R>> = None;
        for ng in self.graph.keys() {
            if self.is_insig_degree::<N>(ng) {
                key = Some(ng.clone());
                break;
            }
        }
        Some(self.take(&key?))
    }

    fn choose_node_to_spill(&mut self) -> Option<NodeEntry<R>> {
        let max_ng = self
            .graph
            .keys()
            .max_by_key(|ng| self.degree(ng))
            .cloned()?;
        eprintln!(
            "Max degree node: {max_ng:?} (degree {})",
            self.degree(&max_ng)
        );
        Some(self.take(&max_ng))
    }

    fn active_neighbors_of(&self, ng: &Stg<R>) -> impl Iterator<Item = &Stg<R>> {
        let Some(neighbors) = self.graph.get(ng) else {
            panic!("Unknown Stg<R>: {ng:?}");
        };
        neighbors
            .iter()
            .filter(|neighbor| self.graph.contains_key(*neighbor))
    }
}

pub struct RegAllocation<I: Instr> {
    cfg: Cfg<I>,
    assignments: BTreeMap<Tmp, usize>,
}

/// Register Allocator
pub struct RegAlloc<const N_GPRS: usize, R> {
    /// Maps temporaries to an offset from the frame pointer.
    stack_slots_allocated: BTreeMap<Stg<R>, i32>,
}

impl<const N_GPRS: usize, R> RegAlloc<N_GPRS, R> 
where R: std::fmt::Debug + Ord + Clone
{
    fn new() -> Self {
        Self {
            stack_slots_allocated: Default::default(),
        }
    }

    const MAX_ITERS: usize = 16;

    fn allocate_registers<I: Instr>(&mut self, mut cfg: Cfg<I>) -> RegAllocation<I> {
        for _ in 0..Self::MAX_ITERS {
            eprintln!("-----------------------");
            let (live_sets, mut color_graph) = self.build_phase(&mut cfg);
            eprintln!("COLOR GRAPH:\n{color_graph:?}");
            let mut stack = self.simplify_phase(&mut color_graph);
            match self.select_phase(&mut color_graph, &mut stack) {
                Err(ng_to_spill) => {
                    eprintln!("Perform Spill");
                    cfg = self.spill_and_rewrite(cfg, ng_to_spill);
                }
                Ok(assignments) => return RegAllocation { cfg, assignments },
            }
        }
        panic!("Register allocation failed: iteration limit exceeded");
    }

    fn build_phase<I: Instr>(&mut self, cfg: &Cfg<I>) -> (LiveSets, ColorGraph<R>) {
        let mut live_sets = LiveSets::new();
        eprintln!("computing live sets...");
        live_sets.compute_live_ins_live_outs(cfg);
        eprintln!("LIVE SETS:\n{}", live_sets.display(&cfg.stmts[..]));
        let mut interferences = Interferences::new();
        eprintln!("computing interferences...");
        interferences.compute_interferences(cfg, &live_sets);
        eprintln!("converting to color graph...");
        let cg = ColorGraph::from_interferences(interferences);
        (live_sets, cg)
    }

    fn simplify_phase(&mut self, color_graph: &mut ColorGraph<R>) -> Vec<NodeEntry<R>> {
        let mut stack = Vec::new();

        loop {
            // First remove all easy nodes and save them to the stack.
            while let Some(entry) = color_graph.take_some_insig_node::<N_GPRS>() {
                eprintln!(">> simplify: {:?}", entry.0);
                stack.push(entry);
            }

            // Then if we're not done, we'll have to (potentially) spill a node.
            // Choose a node to remove and mark for spillage, then loop again
            // to see if that freed up more easy nodes.
            if let Some(entry) = color_graph.choose_node_to_spill() {
                eprintln!(">> simplify (potentially spill): {:?}", entry.0);
                stack.push(entry);
            } else {
                // Graph must be empty, so return the stack.
                eprintln!("Done simplifying graph");
                return stack;
            }
        }
    }

    fn select_phase(
        &mut self,
        color_graph: &mut ColorGraph<R>,
        stack: &mut Vec<NodeEntry<R>>,
    ) -> Result<BTreeMap<Tmp, usize>, Stg<R>> {
        let mut assignments = BTreeMap::new();

        while let Some((stg, neighbors)) = stack.pop() {
            color_graph.insert(stg.clone(), neighbors.clone());
            if let Some((ng, reg_id)) =
                self.select_once(color_graph, &assignments, stg.clone(), neighbors)
            {
                match ng {
                    Stg::Tmp(tmp) => { assignments.insert(tmp, reg_id); }
                    Stg::Reg(_) => {}
                }
            } else {
                // Return the problematic node group for rewriting in the caller.
                eprintln!("  * STOP! SPILL NEEDED FOR: {stg:?}");
                return Err(stg);
            }
        }

        Ok(assignments)
    }

    fn select_once(
        &mut self,
        color_graph: &mut ColorGraph<R>,
        assignments: &BTreeMap<Tmp, usize>,
        stg: Stg<R>,
        neighbors: BTreeSet<Stg<R>>,
    ) -> Option<(Stg<R>, usize)> {
        let mut reg_ids_in_use = BTreeSet::new();
        for neighbor in color_graph.active_neighbors_of(&stg) {
            if let Stg::Tmp(tmp) = neighbor {
                let in_use = *assignments.get(tmp).unwrap();
                reg_ids_in_use.insert(in_use);
            } else {
                // TODO: anything here?
            }
        }
        for reg_id in 0..N_GPRS {
            if !reg_ids_in_use.contains(&reg_id) {
                return Some((stg, reg_id));
            }
        }
        None
    }

    fn spill_and_rewrite<I: Instr>(&mut self, mut cfg: Cfg<I>, to_spill: Stg<R>) -> Cfg<I> {
        // To spill X we need to find all mentions of X. If it's a Use of X,
        // insert Stmt::StackLoad(new_tmp, X_address) before and edit the using
        // instruction.
        // If it's a Def of X, edit the old stmt to Def `new_tmp`, and insert
        // Stmt::StackStore(X_address, new_tmp) after.

        let mut new_stmts = Vec::new();

        let mut defs = BTreeSet::new();
        let mut uses = BTreeSet::new();

        for (id, mut stmt) in cfg.stmts.into_iter().enumerate() {
            defs.clear();
            uses.clear();
            stmt.add_defs_uses(&mut defs, &mut uses);
            let slot_addr = self.get_or_insert_stack_slot(to_spill.clone());
            let stmt_ref = &mut stmt;

            let mut insert_after = Vec::new();
            let mut changed = false;

            if let Stg::Tmp(tmp) = to_spill {
                let mut new_tmp = None;
                if uses.contains(&tmp) {
                    let dst = *new_tmp.get_or_insert_with(|| Tmp::fresh(tmp.as_str()));
                    let new_stmt = I::mk_load_from_stack(dst, slot_addr);
                    eprintln!("++ inserting stmt:\t{new_stmt:?}");
                    new_stmts.push(new_stmt);
                    stmt_ref.replace_use_occurrances(tmp, dst);
                    changed = true;
                }
                if defs.contains(&tmp) {
                    let src = *new_tmp.get_or_insert_with(|| Tmp::fresh(tmp.as_str()));
                    stmt_ref.replace_def_occurrances(tmp, src);
                    insert_after.push(I::mk_store_to_stack(slot_addr, src));
                    changed = true;
                }
            }

            if changed {
                eprintln!("++ rewrite stmt:\t{stmt:?}");
            } else {
                eprintln!("   same stmt:   \t{stmt:?}");
            }
            new_stmts.push(stmt);
            for new_stmt in &insert_after {
                eprintln!("++ insert stmt: \t{new_stmt:?}");
            }
            new_stmts.extend(insert_after);
        }
        Cfg::new(cfg.entry, cfg.params, new_stmts)
    }

    fn get_or_insert_stack_slot(&mut self, ng: Stg<R>) -> i32 {
        let next_idx = (self.stack_slots_allocated.len() * 2) as i32; // Assume all values are two bytes.
        self.stack_slots_allocated.entry(ng).or_insert(next_idx);
        next_idx
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_datastructures::{Expr as E, Expr, Stmt as S, Stmt};
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    enum Reg {
        R1, R2, R3
    }

    fn compute_assignments<const N: usize>(params: Vec<Tmp>, program: Vec<Stmt>) {

        crate::names::reset_name_ids();
        eprintln!("<<<<<<<<<<<<< N_GPRS = {N} >>>>>>>>>>>>>");
        let cfg = Cfg::new(0, params, program);
        let mut ra = RegAlloc::<N, Reg>::new();
        let alloc = ra.allocate_registers(cfg);
        eprintln!("ASSIGNMENTS:");
        for (tmp, reg_id) in &alloc.assignments {
            eprintln!("  {tmp:?} -> ${reg_id}");
        }
        eprintln!("FINAL CODE:");
        for stmt in &alloc.cfg.stmts {
            let mut rendered = format!("{stmt:?}");
            for (tmp, reg_id) in &alloc.assignments {
                let tmp_rendered = format!("{tmp:?}");
                let reg_rendered = format!("${reg_id}");
                rendered = rendered.replace(&tmp_rendered, &reg_rendered);
            }
            eprintln!("{rendered}");
        }
    }

    #[test]
    fn knr_power() {
        // int power(int base, int n) {
        //     int p = 1;
        //     for (int i = 1; i <= n; ++i) {
        //         p = p * base;
        //     }
        //     return p;
        // }
        //
        // power:
        // p <- 1;
        // i <- 1;
        // loop_cond:
        //     br i > n, loop_end;
        //     p <- p * base;
        //     i <- i + 1;
        // loop_end;
        // ret p;
        #[rustfmt::skip]
        let program = vec![
            S::mov("p", 1),
            S::mov("i", 1),
            "loop_cond".into(),
                S::Br(E::binop("i", "n"), "loop_end".into()),
                S::mov("p", E::binop("p", "base")),
                S::mov("i", E::binop("i", 1)),
            "loop_end".into(),
            S::ret("p"),
        ];

        compute_assignments::<3>(vec!["base".into(), "n".into()], program.clone());
        eprintln!("==============================");
        eprintln!("==============================");
        eprintln!("==============================");
        compute_assignments::<2>(vec!["base".into(), "n".into()], program.clone());
    }

    #[test]
    fn knr_binsearch() {
        // int binsearch(int x, int v[], int n) {
        //     int low = 0;
        //     int high = n - 1;
        //     while (low <= high) {
        //          int mid = (low + high) / 2;
        //          int elem = v[mid];
        //          if (x < elem)
        //              high = mid - 1;
        //          else if (x > elem)
        //              low = mid + 1;
        //          else
        //              return mid;
        //     }
        //     return -1;
        // }
        //
        // binsearch:
        // mov %low <- 0;
        // mov %high <- %n - 1;
        // jmp loop_cond;
        // loop_top:
        //      mov %mid <- (%low + %high) / 2;
        //      load %elem <- MEM[%v + %mid];
        //      branch if %x >= %elem to else_if
        //          %high <- %mid - 1;
        //          jmp end_if;
        //      else_if:
        //      branch if x <= elem to else
        //          %low <- %mid + 1;
        //          jmp end_if;
        //      else:
        //          ret %mid;
        //      end_if:
        // loop_cond:
        //      branch if %low <= %high to loop_top;
        //  ret -1;
        //
        #[rustfmt::skip]
        let program = vec![
            "binsearch".into(),
            S::mov("low", 0),
            S::mov("high", E::binop("n", 1)),
            S::jmp("loop_cond"),
            "loop_top".into(),
                 S::mov("mid", E::binop(E::binop("low", "high"), 2)),
                 S::Load { dst: "elem".into(), addr: E::binop("v", "mid") },
                 S::Br(E::binop("x", "elem"),  "else_if".into()),
                     S::mov("high", E::binop("mid", 1)),
                     S::jmp("end_if"),
                 "else_if".into(),
                 S::Br(E::binop("x", "elem"), "else".into()),
                     S::mov("low", E::binop("mid", 1)),
                     S::jmp("end_if"),
                 "else".into(),
                     S::ret("mid"),
                 "end_if".into(),
            "loop_cond".into(),
                 S::Br(E::binop("low", "high"), "loop_top".into()),
            S::ret(-1),
        ];
        compute_assignments::<3>(vec!["x".into(), "v".into(), "n".into()], program.clone());
        eprintln!("=======================================");
        eprintln!("=======================================");
        eprintln!("=======================================");
        compute_assignments::<4>(vec!["x".into(), "v".into(), "n".into()], program.clone());
    }
}
