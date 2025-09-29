#![feature(deref_patterns)]
#![allow(incomplete_features)]

#![allow(unused)]

use std::path::{Path, PathBuf};

use crate::instr_sel::InstrSel;

mod ast;
mod ast_to_ir;
mod instr_sel;
mod frame;
mod ir;
mod names;
mod parse;
mod regalloc;
mod sym;
mod tcx;
mod ty;
mod tyck;
mod canon;

fn main() {
    let Some(fname) = std::env::args().nth(1) else {
        panic!("Please provide a filename");
    };

    let Ok(src) = std::fs::read_to_string(PathBuf::from(&fname[..])) else {
        panic!("Could not read file `{fname}`");
    };

    let Ok(mut module) = parse::ModuleParser::new().parse(&fname[..], &src[..]) else {
        panic!("parse error");
    };

    let mut tcx = tcx::Tcx::new(ty::Ty::Void);
    for mut decl in &mut module.decls {
        let Ok(_) = decl.check_ty(&mut tcx) else {
            panic!("type error");
        };
    }

    for decl in module.decls {
        let subr_name = names::Lbl::SubrStart(decl.value.name);
        let ir_wrap = decl.value.to_ir();
        let ir = ir_wrap.into_iter().map(|w| w.as_stmt()).collect();
        let canon_stmts = canon::canonicalize(subr_name, ir);

        let mut out = Vec::new();
        let mut backend = instr_sel::lark::LarkBackend::new(&mut out);
        for stmt in canon_stmts {
            backend.stmt_to_asm(stmt);
        }

        // TODO: regalloc

        for tgt_lang_stmt in backend.render() {
            println!("{tgt_lang_stmt:?}");
        }
    }
}
