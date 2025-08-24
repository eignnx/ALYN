#![allow(unused)]
use lalrpop_util::lalrpop_mod;

mod ast;
mod ast_to_ir;
mod frame;
mod ir;
mod names;
mod sym;
mod tcx;
mod ty;
mod tyck;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
);

fn main() {
    println!("Hello, world!");
    dbg!(grammar::RValParser::new().parse("*x + (127b - +324) - -4 == **qwerty123 + asdf() - zxcv(1, 2)"));
}
