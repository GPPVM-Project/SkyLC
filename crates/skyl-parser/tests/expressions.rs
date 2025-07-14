#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use skyl_data::{Ast, Expression, Statement};
    use skyl_driver::errors::CompilerErrorReporter;
    use skyl_lexer::Lexer;
    use skyl_parser::Parser;

    fn parse(source: String) -> (Ast, Rc<RefCell<CompilerErrorReporter>>) {
        let reporter = Rc::new(RefCell::new(CompilerErrorReporter::new()));
        let mut lexer = Lexer::new(source);
        let tokens = lexer.scan_tokens(Rc::clone(&reporter));
        let mut parser = Parser::new();
        let ast = parser.parse(Rc::clone(&reporter), tokens);

        return (ast, reporter);
    }

    #[test]
    fn expect_literal_expression() {
        let sources = vec![
            "1.0;",
            "1;",
            "true;",
            "false;",
            "\"Hello World\";",
            "'Hello World';",
        ];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Literal(_, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_variable_expression() {
        let sources = vec!["x;", "foo_test;", "foo__;", "foo__foo;"];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Variable(_, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_group_expression() {
        let sources = vec![
            "(x);",
            "(foo_test);",
            "(foo__);",
            "(foo__foo);",
            "(((2.0 + 5.2)));",
        ];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Group(_, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_unary_expression() {
        let sources = vec![
            "-x;",
            "-true;",
            "not \"Hello\";",
            "not foo__foo;",
            "not 0;",
            "-12.0;",
            "- -12.0;",
            "- - -12.0;",
            "- - -12.0;",
        ];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Unary(_, _, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_arithmetic_expression() {
        let sources = vec![
            "x + 2;",
            "x + y;",
            "2 + 2;",
            "-5 * 2;",
            "-5 * -2;",
            "5 * -2;",
            "x / 2;",
            "(-x) + y;",
            "-(2) + 2;",
            "(5 * 2) / -(2 + 5);",
            "(((1) - 3) + 4) / 2;",
            "((((1) - 2) - 3) - 4) - 5;",
        ];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Arithmetic(_, _, _, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_logical_expression() {
        let sources = vec![
            "x and 2;",
            "x or y;",
            "2 and 2;",
            "-5 or 2;",
            "-5 and -2;",
            "5 or -2;",
            "x and 2;",
            "(-x) or y;",
            "-(2) or 2;",
            "(5 * 2) and -(2 + 5);",
            "not (((1) - 3) + 4) and 2;",
            "not ((((1) - 2) - 3) - 4) or 5;",
            "not true and false and true and 2 and 2;",
            "not true or false or not true or 2 or not 2;",
            "not  ((((1) - 2) - 3) - 4) or 5;",
            "not ((((1) - 2) - 3) - 4) or 5;",
        ];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Logical(_, _, _, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_postfix_expression() {
        let sources = vec!["y++;", "x--;"];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::PostFix(_, _, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_ternary_expression() {
        let sources = vec![
            "x if y and 5 else 1;",
            "x + 5 if y and 5 + 2 else z if y else j if k else l;",
        ];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Ternary(_, _, _, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_assign_expression() {
        let sources = vec![
            "x = z if y and 5 else 1;",
            "x = z;",
            "x = 5 and 5;",
            "x = (a.b.c.d or a and a or a.b.c.d) if (x and x) else (y and y or z);",
        ];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Assign(_, _, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_get_expression() {
        let sources = vec![
            "(z if y and 5 else 1).x;",
            "x.y.z.w;",
            "(5 and 5).a.b.c.d;",
            "((((5 and 5).a).b.c).d and 5).e;",
        ];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Get(_, _, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_set_expression() {
        let sources = vec![
            "(z if y and 5 else 1).x = 2;",
            "x.y.z.w = 3;",
            "(5 and 5).a.b.c.d = 4;",
            "((((5 and 5).a).b.c).d and 5).e = 5;",
        ];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Set(_, _, _, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }

    #[test]
    fn expect_call_expression() {
        let sources = vec![
            "(z if y and 5 else 1).x().z();",
            "x.y().z.w().a().b(1 + 2, 3, 'Hello');",
            "(5 and 5).a.b.c.d(1 + 2 and 4, 1, 1, 1, 1, 1, 1);",
            "((((5 and 5).a).b.c).d and 5).e(((((5 if 5 else 4).a).b.c).d and 5).e());",
        ];

        for s in sources {
            let (ast, reporter) = parse(s.into());
            if reporter.borrow().has_errors() {
                for error in reporter.borrow().get_errors() {
                    println!("{}", error.msg);
                }
                panic!("Has errors");
            }

            if !matches!(
                &ast.statements[0],
                Statement::Expression(Expression::Call(_, _, _, _))
            ) {
                println!("{:?}", &ast.statements[0]);
                panic!("Expect different ast.")
            }
        }
    }
}
