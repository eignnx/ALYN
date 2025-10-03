#![feature(deref_patterns)]
#![allow(incomplete_features)]
#![allow(unused)]

use std::path::{Path, PathBuf};

use crate::{instr_sel::InstrSel, names::Tmp, regalloc::Instr};

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
        let decl = decl.value;
        let subr_name = names::Lbl::SubrStart(decl.name);

        let params: Vec<_> =
            decl.params.iter()
            .map(|ann_tmp| Tmp::from(ann_tmp.value.name))
            .collect();

        let ir_wrap = decl.to_ir();
        let ir = ir_wrap.into_iter().map(|w| w.as_stmt()).collect();
        let canon_stmts = canon::canonicalize(subr_name, ir);

        let mut out = Vec::new();
        let mut backend = instr_sel::lark::LarkBackend::new(&mut out);
        for stmt in canon_stmts {
            backend.stmt_to_asm(stmt);
        }

        let mut ralloc = regalloc::RegAlloc::<instr_sel::lark::Reg>::new();
        let cfg = regalloc::cfg::Cfg::new(
            0, // TODO: is this correct?
            params,
            backend.render().to_vec(),
        );
        let reg_allocation = ralloc.allocate_registers(cfg);

        println!("\n;;; SUBROUTINE");
        for tgt_lang_stmt in reg_allocation.cfg.iter_stmts() {
            let mut stmt = tgt_lang_stmt.clone();
            for (tmp, reg) in reg_allocation.assignments.iter() {
                stmt.replace_occurrances(*tmp, (*reg).into());
            }
            println!("{stmt:?}");
        }
    }
}
