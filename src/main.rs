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
}

#[test]
fn test() {
    use insta::assert_debug_snapshot;
    assert_debug_snapshot!(grammar::RValParser::new().parse("<fname>", "&*x()"));
    assert_debug_snapshot!(grammar::RValParser::new().parse("<fname>", "*x + (127b - +324) - -4 == **qwerty123 + asdf() - zxcv(1, 2)"));
    assert_debug_snapshot!(grammar::StmtParser::new().parse("<fname>", "*lhs = 123 + *ptr;"));
    assert_debug_snapshot!(grammar::StmtParser::new().parse("<fname>", "let lhs = 123 + *ptr;"));
    assert_debug_snapshot!(grammar::StmtParser::new().parse("<fname>", "if 1 == 2 { lhs = 123; } else { ret asfd(); }"));
    assert_debug_snapshot!(grammar::SubrDeclParser::new().parse("<fname>", "subr blah() { ret; }"));
    assert_debug_snapshot!(grammar::SubrDeclParser::new().parse("<fname>", "subr sumfib(n: nat) nat { ret n + sumfib(n - 1); }"));
    assert_debug_snapshot!(grammar::SubrDeclParser::new().parse("<fname>", "subr main(args: **byte) { ret; }"));
    assert_debug_snapshot!(grammar::SubrDeclParser::new().parse("<fname>", "subr min(a: int, b: int) int { if a > b { ret b; } else { ret a; } }"));
    assert_debug_snapshot!(grammar::ModuleParser::new().parse("mathy.alyn",
        "
        subr min(a: int, b: int) int {
            if a > b {
                ret b;
            } else {
                ret a;
            }
        }

        subr fib(n: nat) nat {
            if n < 2 {
                ret 1;
            } else {
                ret fib(n - 1) + fib(n - 2);
            }
        }
        "
    ));
}
