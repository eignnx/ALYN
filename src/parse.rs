use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
);

pub use grammar::*;

#[cfg(test)]
mod test {
    use super::*;
    use insta::assert_debug_snapshot;

    macro_rules! test_parse {
        ($rule:ident, $src:expr) => {
            assert_debug_snapshot!(grammar::$rule::new().parse("<test>", $src));
        };
        ($rule:ident, $filename:expr, $src:expr) => {
            assert_debug_snapshot!(grammar::$rule::new().parse($filename, $src));
        };
    }

    #[test]
    fn bit_and() {
        test_parse!(RValParser, "x & y");
    }

    #[test]
    fn addr_deref_call() {
        test_parse!(RValParser, "&*x()");
    }

    #[test]
    fn pluses_and_minuses() {
        test_parse!(
            RValParser,
            "
            x - -y + z
        "
        );
    }

    #[test]
    fn operator_precedence() {
        test_parse!(
            RValParser,
            "
            x & -y + -z == 1 - 2 | 3
        "
        );
    }

    #[test]
    fn big_long_term() {
        test_parse!(
            RValParser,
            "
            *x + (127b - (+324)) - (-4) == **qwerty123 + asdf() - zxcv(1, 2)
        "
        );
    }

    #[test]
    fn type_transmutation_cast() {
        test_parse!(RValParser, "1 + 2 & bitcast{nat}(-3) & 4 + 5");
    }

    #[test]
    fn bitcast_with_nested_expr() {
        test_parse!(RValParser, "bitcast{nat}(12b + 40b)");
    }

    #[test]
    fn assignment() {
        test_parse!(StmtParser, "*lhs = 123 + *ptr;");
    }

    #[test]
    fn let_stmt() {
        test_parse!(StmtParser, "let lhs = 123 + *ptr;");
    }

    #[test]
    fn if_stmt() {
        test_parse!(StmtParser, "if 1 == 2 { lhs = 123; } else { ret asfd(); }");
    }

    #[test]
    fn simple_subr() {
        test_parse!(SubrDeclParser, "subr blah() { ret; }");
    }

    #[test]
    fn sumfac() {
        test_parse!(
            SubrDeclParser,
            "
            subr sumfac(n: nat) nat {
                ret n + sumfac(n - 1);
            }
        "
        );
    }

    #[test]
    fn main_with_args() {
        test_parse!(SubrDeclParser, "subr main(args: **byte) { ret; }");
    }

    #[test]
    fn min_subr() {
        test_parse!(
            SubrDeclParser,
            "
            subr min(a: int, b: int) int {
                if a > b {
                    ret b;
                } else {
                    ret a;
                }
            }
        "
        );
    }

    #[test]
    fn mathy_module() {
        test_parse!(
            ModuleParser,
            "mathy.alyn",
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
        );
    }
}
