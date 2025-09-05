#![allow(unused)]

mod ast;
mod ast_to_ir;
mod frame;
mod ir;
mod names;
mod parse;
mod sym;
mod tcx;
mod ty;
mod tyck;
mod backend;
mod regalloc;

fn main() {
    println!("Hello, world!");
}
