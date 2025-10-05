#![feature(deref_patterns)]
#![allow(incomplete_features)]
#![allow(unused)]

use std::path::{Path, PathBuf};

use crate::{
    instr_sel::{InstrSel, Stg, lark::LarkInstrSel},
    names::Tmp,
    regalloc::Instr,
};

mod ast;
mod ast_to_ir;
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

fn main() {
    let Some(fname) = std::env::args().nth(1) else {
        panic!("Please provide a filename");
    };

    let Ok(src) = std::fs::read_to_string(PathBuf::from(&fname[..])) else {
        panic!("Could not read file `{fname}`");
    };

    let mut out = Vec::new();
    let instr_select = LarkInstrSel::new(&mut out);
    let mut compiler = Compiler::new(instr_select);
    compiler.compile(&fname[..], &src[..]);
}

pub struct Compiler<ISel: InstrSel> {
    instr_select: ISel,
}

impl<ISel: InstrSel> Compiler<ISel>
where
    ISel::Register: Ord + Eq + Copy + regalloc::Cc<ISel::Register> + 'static,
    ISel::Instruction: Instr<Register = ISel::Register>,
{
    pub fn new(instr_select: ISel) -> Self {
        Self { instr_select }
    }

    pub fn compile(&mut self, fname: &str, src: &str) {
        let mut module = match parse::ModuleParser::new().parse(&fname[..], &src[..]) {
            Ok(m) => m,
            Err(e) => panic!("parse error: {e:?}"),
        };

        let mut tcx = tcx::Tcx::new(ty::Ty::Void);
        for mut decl in &mut module.decls {
            if let Err(e) = decl.check_ty(&mut tcx) {
                panic!("type error: {e:?}");
            };
        }

        for decl in module.decls {
            eprintln!("========================================");
            eprintln!("           Item `{}`", decl.value.name);
            eprintln!("========================================");
            let decl = decl.value;
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
            let asm_before_regalloc = self.instr_select.render().to_vec();

            let mut ralloc = regalloc::RegAlloc::<ISel::Register>::new();
            let cfg = regalloc::cfg::Cfg::new(
                0, // TODO: is this correct?
                params,
                asm_before_regalloc,
            );
            let reg_allocation = ralloc.allocate_registers(cfg);

            eprintln!("ASSIGNMENTS:");
            for (tmp, reg_id) in &reg_allocation.assignments {
                eprintln!("  {tmp:?} -> ${reg_id:?}");
            }

            println!("\n;;; SUBROUTINE");
            for tgt_lang_stmt in reg_allocation.cfg.iter_stmts() {
                let mut stmt = tgt_lang_stmt.clone();
                for (tmp, reg) in reg_allocation.assignments.iter() {
                    stmt.replace_occurrances(*tmp, Stg::Reg(*reg));
                }
                println!("{stmt:?}");
            }
        }
    }
}
