#![feature(deref_patterns)]
#![allow(incomplete_features)]

#![allow(unused)]

mod ast;
mod ast_to_ir;
mod backend;
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
    println!("Hello, world!");
}
