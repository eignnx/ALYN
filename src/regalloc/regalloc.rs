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
    regalloc::{
        Cc,
        color_graph::{ColorGraph, NodeEntry},
        live_sets::LiveSets,
    },
};

use super::{Instr, cfg::Cfg, interferences::Interferences};

pub struct RegAllocation<I: Instr, R> {
    pub program: Vec<I>,
    pub assignments: BTreeMap<Tmp, R>,
}

impl<I, R> RegAllocation<I, R> 
where I: Instr<Register=R>, R: Eq + Copy
{
    pub fn new(cfg: Cfg<I>, assignments: BTreeMap<Tmp, R>) -> Self {
        let mut program = vec![];
        for (i, mut instr) in cfg.stmts.into_iter().enumerate() {
            for (tmp, reg) in &assignments {
                instr.replace_occurrances(*tmp, Stg::Reg(*reg));
            }
            if let Some((dst, src)) = instr.try_as_pure_move() && dst == src {
                eprintln!("-- {i}: {instr:?} # Eliminating redundant move");
                continue;
            } else {
                program.push(instr);
            }
        }

        Self {
            program,
            assignments,
        }
    }
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

    pub fn allocate_registers<I: Instr<Register = R> + Clone>(
        &mut self,
        params: impl IntoIterator<Item = Tmp>,
        stmts: Vec<I>,
    ) -> RegAllocation<I, R> {
        let mut cfg = self.precolor(Cfg::new(0, params, stmts));

        for i in 0..Self::MAX_ITERS {
            eprintln!("============= Iteration #{} ============", i + 1);
            let mut color_graph = self.build_phase(&mut cfg);
            let mut node_stack = self.simplify_phase(&mut color_graph);
            self.coalesce_phase();
            //self.freeze_phase();
            match self.select_phase(&mut color_graph, &mut node_stack) {
                Ok(assignments) => return RegAllocation::new(cfg, assignments),
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

    fn precolor<I: Instr<Register = R> + Clone>(&mut self, mut cfg: Cfg<I>) -> Cfg<I> {
        let mut prologue: Vec<I> = vec![];
        let mut epilogue = vec![];

        //// All saved registers need to be saved to their own temporaries (hopefully to be coalesced
        //// away later).
        //let mut tmps = vec![];
        //for &reg in R::GPR_SAVED_REGS {
        //    let fresh_tmp = Tmp::fresh(format!("saved<{reg:?}>").as_str()).into();
        //    tmps.push((fresh_tmp, reg));
        //    prologue.extend(I::emit_move(fresh_tmp, Stg::Reg(reg)));
        //}
        //for (tmp, reg) in tmps.into_iter().rev() {
        //    epilogue.extend(I::emit_move(Stg::Reg(reg), tmp));
        //}

        // Move values in arg registers into the temporaries representing parameters.
        for (&param, &reg) in cfg.params.iter().zip(R::GPR_ARG_REGS) {
            prologue.extend(I::emit_move(param.into(), Stg::Reg(reg)));
        }


        let mut new_stmts = vec![];
        new_stmts.push(cfg.stmts.first().expect("first instr will be subr name").clone());
        new_stmts.extend(prologue);

        // Skip subr label
        for (id, instr) in cfg.stmts.drain(..).enumerate().skip(1) {
            if cfg.exits.contains(&id) {
                new_stmts.extend(epilogue.iter().cloned());
            }
            new_stmts.push(instr);
        }

        // Need to rebuild CFG so that jump indices are up to date with added prologue/epilogues.
        Cfg::new(cfg.entry, cfg.params, new_stmts)
    }

    /// Computes live-sets and builds a color graph given a control-flow graph representing a
    /// program.
    fn build_phase<I: Instr<Register = R>>(&mut self, cfg: &Cfg<I>) -> ColorGraph<R> {
        let mut live_sets = LiveSets::new();
        eprintln!("computing live sets...");
        live_sets.compute_live_ins_live_outs(cfg);
        eprintln!("LIVE SETS:\n{}", live_sets.display(&cfg.stmts[..]));
        eprintln!("initializing color graph...");
        let cg = ColorGraph::new(cfg, &live_sets);
        eprintln!("COLOR GRAPH:\n{cg:?}");
        cg
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

    fn coalesce_phase(&mut self) {}

    /// Pops Stg nodes off of the node stack and selects a color for each one. If any cannot be
    /// colored, returns `Err` of the problematic node.
    fn select_phase(
        &mut self,
        color_graph: &mut ColorGraph<R>,
        node_stack: &mut Vec<NodeEntry<R>>,
    ) -> Result<BTreeMap<Tmp, R>, Tmp> {
        let mut assignments = BTreeMap::new();

        while let Some((stg, neighbors)) = node_stack.pop() {
            color_graph.insert(stg, neighbors);
            let Stg::Tmp(tmp) = stg else {
                continue;
            };
            let Some(reg) = self.select_once(color_graph, &assignments, tmp) else {
                // Return the problematic node for rewriting in the caller.
                eprintln!("  * STOP! SPILL NEEDED FOR: {tmp:?}");
                return Err(tmp);
            };
            assignments.insert(tmp, reg);
        }

        Ok(assignments)
    }

    /// Given the current color-graph and the current assignments of nodes, chooses a color
    /// (register) for the given temporary.
    fn select_once(
        &mut self,
        color_graph: &mut ColorGraph<R>,
        assignments: &BTreeMap<Tmp, R>,
        tmp: Tmp,
    ) -> Option<R> {
        let mut regs_in_use = BTreeSet::<R>::new();

        // Collect all neighbors of `tmp` in the color-graph.
        for neighbor in color_graph.active_neighbors_of(tmp) {
            match neighbor {
                Stg::Tmp(tmp) => {
                    // If the neighbor is a Tmp that has already been assigned, its assigned
                    // register is "in use".
                    if let Some(in_use) = assignments.get(tmp).cloned() {
                        regs_in_use.insert(in_use);
                    }
                    // If the Tmp hasn't been assigned a register, skip.
                }
                // A Reg as a neighbor is obviously "in use" because `tmp` cannot be assigned that
                // register since it conflicts.
                Stg::Reg(reg) => {
                    regs_in_use.insert(reg.clone());
                }
            }
        }

        eprintln!("!!!!!!!!!!!!!!!!!!! {tmp}: in_use = {regs_in_use:?}");

        // First try coalescing:
        if let Some(chosen) = R::GPRS
            .into_iter()
            .copied()
            .filter(|gpr| color_graph.are_move_related(tmp.into(), Stg::Reg(*gpr)))
            .find(|gpr| !regs_in_use.contains(gpr)) {
                eprintln!("~ coalescing {tmp:?} and {chosen:?}");
                return Some(chosen);
        }

        // Otherwise just find the first GPR that's not in use.
        R::GPRS
            .into_iter()
            .find(|gpr| !regs_in_use.contains(*gpr))
            .inspect(|gpr| eprintln!("* assigning  {tmp:?} --> {gpr:?}"))
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

            let mk_fresh = || {
                Tmp::fresh(&format!("spilled<{to_spill:?}>"))
            };

            let mut new_tmp = None;
            if uses.contains(&to_spill.into()) {
                let dst = *new_tmp.get_or_insert_with(mk_fresh);
                new_stmts.extend(I::emit_load_from_stack(dst, slot_addr).inspect(|new_stmt| {
                    eprintln!("++ inserting stmt:\t{new_stmt:?}");
                }));
                stmt_ref.replace_use_occurrances(to_spill, dst.into());
                changed = true;
            }
            if defs.contains(&to_spill.into()) {
                let src = *new_tmp.get_or_insert_with(mk_fresh);
                stmt_ref.replace_def_occurrances(to_spill, src.into());
                insert_after.extend(I::emit_store_to_stack(slot_addr, src));
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
    use crate::names::Lbl;
    use crate::utils::current_revision_summary;

    use super::super::test_datastructures::{Expr as E, Expr, Reg, Stmt as S, Stmt};
    use super::*;

    fn compute_assignments(params: Vec<Tmp>, program: Vec<Stmt>) {
        crate::names::reset_name_ids();
        eprintln!("{}", current_revision_summary());
        eprintln!("GPRS = {:?}", Reg::GPRS);
        let mut ra = RegAlloc::<Reg>::new();
        let mut alloc = ra.allocate_registers(params, program);
        eprintln!("ASSIGNMENTS:");
        for (tmp, reg_id) in &alloc.assignments {
            eprintln!("  {tmp:?} -> ${reg_id}");
        }
        eprintln!("FINAL CODE:");
        for stmt in alloc.program {
            eprintln!("|\t{stmt:?}");
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
            Lbl::subr("power").into(),
            S::mov(Tmp::from("p"), 1),
            S::mov(Tmp::from("i"), 1),
            "loop_cond".into(),
                S::Br(E::binop("i", "n"), "loop_end".into()),
                S::mov(Tmp::from("p"), E::binop("p", "base")),
                S::mov(Tmp::from("i"), E::binop("i", 1)),
                S::jmp("loop_cond"),
            "loop_end".into(),
            S::mov(Reg::T0, "p"),
            S::Ret
        ];

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
            Lbl::subr("binsearch").into(),
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
                    S::mov(Reg::T0, "mid"),
                    S::Ret,
                "end_if".into(),
            "loop_cond".into(),
                S::Br(E::binop("low", "high"), "loop_top".into()),
            S::mov(Reg::T0, -1),
            S::Ret,
        ];
        compute_assignments(vec!["x".into(), "v".into(), "n".into()], program.clone());
    }
}
