use derive_more::From;
use smallvec::{SmallVec, smallvec};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    io::Write,
    marker::PhantomData,
};

use crate::{
    instr_sel::Stg,
    names::Tmp,
    regalloc::{Cc, live_sets::LiveSets},
};

use super::{Instr, cfg::Cfg, interferences::Interferences};

type NodeEntry<R> = (Stg<R>, BTreeSet<Stg<R>>);

struct ColorGraph<R> {
    interferences: Interferences<R>,
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

impl<R> From<Interferences<R>> for ColorGraph<R> {
    fn from(interferences: Interferences<R>) -> Self {
        Self { interferences }
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
        Self { interferences }
    }

    fn insert(&mut self, n: Stg<R>, neighbors: BTreeSet<Stg<R>>) {
        self.interferences.graph.insert(n, neighbors);
    }

    /// Only count as a neighbor if the node is still in the graph. Instances of neighbors whose
    /// node is not in `self.graph` are skipped.
    fn active_neighbors_of(&self, n: &Stg<R>) -> impl Iterator<Item = &Stg<R>> {
        let Some(neighbors) = self.interferences.graph.get(n) else {
            panic!("Unknown Stg<R>: {n:?}");
        };
        neighbors
            .iter()
            .filter(|neighbor| self.interferences.graph.contains_key(*neighbor))
    }

    #[track_caller]
    fn degree(&self, n: &Stg<R>) -> usize {
        self.active_neighbors_of(n).count()
    }

    #[track_caller]
    fn is_sig_degree(&self, n: &Stg<R>) -> bool {
        self.degree(n) >= R::N_GPRS
    }

    #[track_caller]
    fn is_insig_degree(&self, n: &Stg<R>) -> bool {
        self.degree(n) < R::N_GPRS
    }

    fn coalesce(&mut self, n1: &Stg<R>, n2: &Stg<R>) {
        todo!()
    }

    fn take_node(&mut self, n: &Stg<R>) -> NodeEntry<R> {
        let Some(entry) = self.interferences.graph.remove_entry(n) else {
            panic!("Unknown storage node: {n:?}");
        };
        entry
    }

    /// Removes an arbitrary node (and its neighbor set) as long as the node has fewer than N
    /// neighbors. Returns `None` if no such node can be found.
    fn take_some_insig_node(&mut self) -> Option<NodeEntry<R>> {
        let mut key: Option<Stg<R>> = None;
        for n in self.interferences.graph.keys() {
            if self.is_insig_degree(n) {
                key = Some(n.clone());
                break;
            }
        }
        Some(self.take_node(&key?))
    }

    fn choose_node_to_spill(&mut self) -> Option<NodeEntry<R>> {
        let max_node = self
            .interferences
            .graph
            .keys()
            .max_by_key(|n| self.degree(n))
            .cloned()?;
        Some(self.take_node(&max_node))
    }
}

pub struct RegAllocation<I: Instr, R> {
    pub cfg: Cfg<I>,
    pub assignments: BTreeMap<Tmp, R>,
}

/// Register Allocator
pub struct RegAlloc<R> {
    /// Maps temporaries to an offset from the frame pointer.
    stack_slots_allocated: BTreeMap<Tmp, i32>,
    _phantom: PhantomData<R>,
}

impl<R> RegAlloc<R>
where
    R: fmt::Debug + Ord + Eq + Copy + Cc<R> + 'static,
{
    pub fn new() -> Self {
        Self {
            stack_slots_allocated: Default::default(),
            _phantom: PhantomData,
        }
    }

    pub const MAX_ITERS: usize = 16;

    pub fn allocate_registers<I: Instr<Register = R>>(
        &mut self,
        mut cfg: Cfg<I>,
    ) -> RegAllocation<I, R> {
        for _ in 0..Self::MAX_ITERS {
            eprintln!("-----------------------");
            let (live_sets, mut color_graph) = self.build_phase(&mut cfg);
            eprintln!("COLOR GRAPH:\n{color_graph:?}");
            let mut node_stack = self.simplify_phase(&mut color_graph);
            match self.select_phase(&mut color_graph, &mut node_stack) {
                Ok(assignments) => return RegAllocation { cfg, assignments },
                Err(to_spill) => {
                    eprintln!("Perform Spill");
                    cfg = self.spill_and_rewrite(cfg, to_spill);
                }
            }
        }
        panic!(
            "Register allocation failed: iteration limit ({}) exceeded",
            Self::MAX_ITERS
        );
    }

    /// Computes live-sets and builds a color graph given a control-flow graph representing a
    /// program.
    fn build_phase<I: Instr<Register = R>>(
        &mut self,
        cfg: &Cfg<I>,
    ) -> (LiveSets<R>, ColorGraph<R>) {
        let mut live_sets = LiveSets::new();
        eprintln!("computing live sets...");
        live_sets.compute_live_ins_live_outs(cfg);
        eprintln!("LIVE SETS:\n{}", live_sets.display(&cfg.stmts[..]));
        eprintln!("initializing color graph...");
        let cg = ColorGraph::new(cfg, &live_sets);
        (live_sets, cg)
    }

    /// To "simplify" a node is to remove it from the color graph and push it onto a stack for use
    /// later. Returns the stack of Stg nodes (and their neighbor sets) that have been removed from
    /// the graph.
    fn simplify_phase(&mut self, color_graph: &mut ColorGraph<R>) -> Vec<NodeEntry<R>> {
        let mut node_stack = Vec::new();

        loop {
            // First remove all easy nodes and save them to the stack.
            while let Some(entry) = color_graph.take_some_insig_node() {
                eprintln!(">> simplify: {:?}", entry.0);
                node_stack.push(entry);
            }

            // Then if we're not done, we'll have to (potentially) spill a node.
            // Choose a node to remove and mark for spillage, then loop again
            // to see if that freed up more easy nodes.
            if let Some(entry) = color_graph.choose_node_to_spill() {
                eprintln!(">> simplify (potentially spill): {:?}", entry.0);
                node_stack.push(entry);
            } else {
                // Graph must be empty, so return the stack.
                // Done simplifying graph.
                return node_stack;
            }
        }
    }

    /// Pops Stg nodes off of the node stack and selects a color for each one. If any cannot be
    /// colored, returns `Err` of the problematic node.
    fn select_phase(
        &mut self,
        color_graph: &mut ColorGraph<R>,
        node_stack: &mut Vec<NodeEntry<R>>,
    ) -> Result<BTreeMap<Tmp, R>, Tmp> {
        let mut assignments = BTreeMap::new();

        while let Some((stg, neighbors)) = node_stack.pop() {
            color_graph.insert(stg.clone(), neighbors.clone());
            let Stg::Tmp(tmp) = stg else {
                continue;
            };
            let Some(reg) = self.select_once(color_graph, &assignments, stg.clone()) else {
                // Return the problematic node for rewriting in the caller.
                eprintln!("  * STOP! SPILL NEEDED FOR: {tmp:?}");
                return Err(tmp);
            };
            assignments.insert(tmp, reg);
        }

        Ok(assignments)
    }

    /// Given the current color-graph and the current assignments of nodes, chooses a color
    /// (register) for the given node.
    fn select_once(
        &mut self,
        color_graph: &mut ColorGraph<R>,
        assignments: &BTreeMap<Tmp, R>,
        stg: Stg<R>,
    ) -> Option<R> {
        let mut regs_in_use = BTreeSet::<R>::new();

        // Collect all neighbors of `stg` in the color-graph.
        for neighbor in color_graph.active_neighbors_of(&stg) {
            match neighbor {
                Stg::Tmp(tmp) => {
                    // If the neighbor is a Tmp that has already been assigned, its assigned
                    // register is "in use".
                    if let Some(in_use) = assignments.get(tmp).cloned() {
                        regs_in_use.insert(in_use);
                    }
                    // If the Tmp hasn't been assigned a register, skip.
                }
                // A Reg as a neighbor is obviously "in use" because `stg` cannot be assigned that
                // register since it conflicts.
                Stg::Reg(reg) => {
                    regs_in_use.insert(reg.clone());
                }
            }
        }

        // Find the first general-purpose register that's not in use.
        R::GPRS
            .into_iter()
            .find(|gpr| !regs_in_use.contains(*gpr))
            .cloned()
    }

    fn spill_and_rewrite<I: Instr<Register = R>>(
        &mut self,
        mut cfg: Cfg<I>,
        to_spill: Tmp,
    ) -> Cfg<I> {
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
            let slot_addr = self.get_or_insert_stack_slot(to_spill);
            let stmt_ref = &mut stmt;

            let mut insert_after = Vec::new();
            let mut changed = false;

            let mut new_tmp = None;
            if uses.contains(&to_spill.into()) {
                let dst = *new_tmp.get_or_insert_with(|| Tmp::fresh(to_spill.as_str()));
                let new_stmt = I::mk_load_from_stack(dst, slot_addr);
                eprintln!("++ inserting stmt:\t{new_stmt:?}");
                new_stmts.push(new_stmt);
                stmt_ref.replace_use_occurrances(to_spill, dst.into());
                changed = true;
            }
            if defs.contains(&to_spill.into()) {
                let src = *new_tmp.get_or_insert_with(|| Tmp::fresh(to_spill.as_str()));
                stmt_ref.replace_def_occurrances(to_spill, src.into());
                insert_after.push(I::mk_store_to_stack(slot_addr, src));
                changed = true;
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

    fn get_or_insert_stack_slot(&mut self, n: Tmp) -> i32 {
        let next_idx = (self.stack_slots_allocated.len() * 2) as i32; // Assume all values are two bytes.
        self.stack_slots_allocated.entry(n).or_insert(next_idx);
        next_idx
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_datastructures::{Expr as E, Expr, Reg, Stmt as S, Stmt};
    use super::*;

    fn compute_assignments(params: Vec<Tmp>, program: Vec<Stmt>) {
        crate::names::reset_name_ids();
        eprintln!("<<<<<<<<<<<<< GPRS = {:?} >>>>>>>>>>>>>", Reg::GPRS);
        let cfg = Cfg::new(0, params, program);
        let mut ra = RegAlloc::<Reg>::new();
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
            eprintln!("|\t{rendered}");
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
            S::mov(Tmp::from("p"), 1),
            S::mov(Tmp::from("i"), 1),
            "loop_cond".into(),
                S::Br(E::binop("i", "n"), "loop_end".into()),
                S::mov(Tmp::from("p"), E::binop("p", "base")),
                S::mov(Tmp::from("i"), E::binop("i", 1)),
            "loop_end".into(),
            S::ret("p"),
        ];

        compute_assignments(vec!["base".into(), "n".into()], program.clone());
        eprintln!("==============================");
        eprintln!("==============================");
        eprintln!("==============================");
        compute_assignments(vec!["base".into(), "n".into()], program.clone());
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
            S::mov(Tmp::from("low"), 0),
            S::mov(Tmp::from("high"), E::binop("n", 1)),
            S::jmp("loop_cond"),
            "loop_top".into(),
                 S::mov(Tmp::from("mid"), E::binop(E::binop("low", "high"), 2)),
                 S::Load { dst: Tmp::from("elem").into(), addr: E::binop("v", "mid") },
                 S::Br(E::binop("x", "elem"),  "else_if".into()),
                     S::mov(Tmp::from("high"), E::binop("mid", 1)),
                     S::jmp("end_if"),
                 "else_if".into(),
                 S::Br(E::binop("x", "elem"), "else".into()),
                     S::mov(Tmp::from("low"), E::binop("mid", 1)),
                     S::jmp("end_if"),
                 "else".into(),
                     S::ret("mid"),
                 "end_if".into(),
            "loop_cond".into(),
                 S::Br(E::binop("low", "high"), "loop_top".into()),
            S::ret(-1),
        ];
        compute_assignments(vec!["x".into(), "v".into(), "n".into()], program.clone());
        eprintln!("=======================================");
        eprintln!("=======================================");
        eprintln!("=======================================");
        compute_assignments(vec!["x".into(), "v".into(), "n".into()], program.clone());
    }
}
