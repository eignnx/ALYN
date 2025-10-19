#![feature(deref_patterns)]
#![allow(incomplete_features)]
#![allow(unused)]

use std::path::{Path, PathBuf};

use clap::Parser;

use crate::{
    instr_sel::{Select, Stg},
    names::{Lbl, Tmp},
    regalloc::Instr, utils::current_revision_summary,
};

mod ast;
mod ast_to_ir;
mod backends;
mod canon;
mod frame;
mod instr_sel;
mod ir;
mod names;
mod parse;
mod regalloc;
mod sym;
mod tcx;
mod ty;
mod tyck;
mod utils;
mod cli_args;

fn main() {
    let opts = cli_args::CliArgs::parse();

    let fname = opts.src_file;

    let Ok(src) = std::fs::read_to_string(&fname) else {
        panic!("Could not read file `{}`", fname.display());
    };

    let mut backend = opts.target_arch.instantiate();
    backend.compile(&fname, &src[..]);
}

pub struct Compiler<'a, ISel: Select> {
    instr_select: &'a mut ISel,
}

impl<'a, ISel: Select> Compiler<'a, ISel>
where
    ISel::Register: regalloc::Cc,
    ISel::Instruction: Instr<Register = ISel::Register>,
{
    pub fn new(instr_select: &'a mut ISel) -> Self {
        Self { instr_select }
    }

    pub fn compile(&mut self, fname: &Path, src: &str) {
        eprintln!("{}", current_revision_summary());
        let fname = fname.display().to_string();

        let mut module = match parse::ModuleParser::new().parse(&fname[..], &src[..]) {
            Ok(m) => m,
            Err(e) => panic!("parse error: {e:?}"),
        };

        let mut tcx = tcx::Tcx::new(ty::Ty::Void);
        if let Err(e) = module.check_ty(&mut tcx) {
            panic!("type error: {e:?}");
        }

        for subr in module.subr_defns() {
            eprintln!("========================================");
            eprintln!("           Item `{}`", subr.value.name);
            eprintln!("========================================");
            let decl = subr.value.clone();
            let subr_name = names::Lbl::SubrStart(decl.name);

            let params: Vec<_> = decl
                .params
                .iter()
                .map(|ann_tmp| Tmp::from(ann_tmp.value.name))
                .collect();

            eprintln!(">------------ IR Statements -----------<");
            let ir_wrap = decl.to_ir();
            let ir = ir_wrap
                .into_iter()
                .map(|w| w.as_stmt())
                .inspect(|stmt| {
                    eprintln!("{stmt:?}");
                })
                .collect();
            eprintln!(">--------------------------------------<");
            let canon_stmts = canon::canonicalize(subr_name, ir);

            eprintln!("Canonicalized statements:");
            for stmt in canon_stmts {
                eprintln!("|   {stmt:?}");
                self.instr_select.stmt_to_asm(stmt);
            }

            let asm_before_regalloc = self.instr_select.render().collect();

            let mut ralloc = regalloc::RegAlloc::<ISel::Register>::new();
            let reg_allocation = ralloc.allocate_registers(params, asm_before_regalloc);

            eprintln!("ASSIGNMENTS:");
            for (tmp, reg_id) in &reg_allocation.assignments {
                eprintln!("  {tmp:?} -> {reg_id:?}");
            }

            println!("\n;;; SUBROUTINE");
            for stmt in reg_allocation.program {

                if let Some(lbl) = stmt.try_as_lbl() {
                    if let Lbl::SubrStart(_) = lbl {
                        println!();
                    }
                    println!("{stmt:?}");
                } else {
                    println!("    {stmt:?}");
                }
            }
        }
    }
}
