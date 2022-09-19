use lattice_lang::{lex, parse, ParseError};
use lattice_protocol::{Expr, Expression, ExpressionType, Operator, Span, Statement};

#[test]
fn test_binary_add() {
    let source = br#"1 + 2"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);
    let expected = vec![Statement::Expression(Expression {
        expr: Expr::Binary {
            left: Box::new(Expression {
                expr: Expr::Integer(1),
                span: Span::new(0, 1),
                expr_type: Some(ExpressionType::Int),
            }),
            operator: Operator::Plus,
            right: Box::new(Expression {
                expr: Expr::Integer(2),
                span: Span::new(4, 5),
                expr_type: Some(ExpressionType::Int),
            }),
        },
        span: Span::new(2, 3),
        expr_type: Some(ExpressionType::Int),
    })];

    res.statements
        .into_iter()
        .zip(expected.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_binary_multiply() {
    let source = br#"1 * 2"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);
    let expected = vec![Statement::Expression(Expression {
        expr: Expr::Binary {
            left: Box::new(Expression {
                expr: Expr::Integer(1),
                span: Span::new(0, 1),
                expr_type: Some(ExpressionType::Int),
            }),
            operator: Operator::Multiply,
            right: Box::new(Expression {
                expr: Expr::Integer(2),
                span: Span::new(4, 5),
                expr_type: Some(ExpressionType::Int),
            }),
        },
        span: Span::new(2, 3),
        expr_type: Some(ExpressionType::Int),
    })];

    res.statements
        .into_iter()
        .zip(expected.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_binary_multiple() {
    let source = br#"1 * 2; 1 + 2
1 > 3"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);
    let expected = vec![
        Statement::Expression(Expression {
            expr: Expr::Binary {
                left: Box::new(Expression {
                    expr: Expr::Integer(1),
                    expr_type: Some(ExpressionType::Int),
                    span: Span::new(0, 1),
                }),
                operator: Operator::Multiply,
                right: Box::new(Expression {
                    expr: Expr::Integer(2),
                    expr_type: Some(ExpressionType::Int),
                    span: Span::new(4, 5),
                }),
            },
            expr_type: Some(ExpressionType::Int),
            span: Span::new(2, 3),
        }),
        Statement::Expression(Expression {
            expr: Expr::Binary {
                left: Box::new(Expression {
                    expr: Expr::Integer(1),
                    expr_type: Some(ExpressionType::Int),
                    span: Span::new(7, 8),
                }),
                operator: Operator::Plus,
                right: Box::new(Expression {
                    expr: Expr::Integer(2),
                    expr_type: Some(ExpressionType::Int),
                    span: Span::new(11, 12),
                }),
            },
            expr_type: Some(ExpressionType::Int),
            span: Span::new(9, 10),
        }),
        Statement::Expression(Expression {
            expr: Expr::Binary {
                left: Box::new(Expression {
                    expr: Expr::Integer(1),
                    expr_type: Some(ExpressionType::Int),
                    span: Span::new(13, 14),
                }),
                operator: Operator::GreaterThan,
                right: Box::new(Expression {
                    expr: Expr::Integer(3),
                    expr_type: Some(ExpressionType::Int),
                    span: Span::new(17, 18),
                }),
            },
            expr_type: Some(ExpressionType::Int),
            span: Span::new(15, 16),
        }),
    ];

    res.statements
        .into_iter()
        .zip(expected.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_error_multiple_missing_semicolon() {
    let source = br#"1 * 2 3 + 4"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);
    let expected = vec![Statement::Expression(Expression {
        expr: Expr::Binary {
            left: Box::new(Expression {
                expr: Expr::Integer(1),
                expr_type: Some(ExpressionType::Int),
                span: Span::new(0, 1),
            }),
            operator: Operator::Multiply,
            right: Box::new(Expression {
                expr: Expr::Integer(2),
                expr_type: Some(ExpressionType::Int),
                span: Span::new(4, 5),
            }),
        },
        expr_type: Some(ExpressionType::Int),
        span: Span::new(2, 3),
    })];

    res.statements
        .into_iter()
        .zip(expected.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));

    let expected_error = vec![ParseError::MissingCharacter(
        "Missing semicolon or end of line".into(),
        Span::new(4, 5),
    )];

    res.errors
        .into_iter()
        .zip(expected_error.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_unary_operations() {
    let source = br#" -1 * 2; !true == false;"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);
    let expected = vec![
        Statement::Expression(Expression {
            expr: Expr::Binary {
                left: Box::new(Expression {
                    expr: Expr::Unary {
                        operator: Operator::Minus,
                        expr: Box::new(Expression {
                            expr: Expr::Integer(1),
                            expr_type: Some(ExpressionType::Int),
                            span: Span::new(2, 3),
                        }),
                    },
                    expr_type: Some(ExpressionType::Int),
                    span: Span::new(1, 3),
                }),
                operator: Operator::Multiply,
                right: Box::new(Expression {
                    expr: Expr::Integer(2),
                    expr_type: Some(ExpressionType::Int),
                    span: Span::new(6, 7),
                }),
            },
            expr_type: Some(ExpressionType::Int),
            span: Span::new(4, 5),
        }),
        Statement::Expression(Expression {
            expr: Expr::Binary {
                left: Box::new(Expression {
                    expr: Expr::Unary {
                        operator: Operator::Not,
                        expr: Box::new(Expression {
                            expr: Expr::Bool(true),
                            expr_type: Some(ExpressionType::Bool),
                            span: Span::new(10, 14),
                        }),
                    },
                    expr_type: Some(ExpressionType::Bool),
                    span: Span::new(9, 14),
                }),
                operator: Operator::Equal,
                right: Box::new(Expression {
                    expr: Expr::Bool(false),
                    expr_type: Some(ExpressionType::Bool),
                    span: Span::new(18, 23),
                }),
            },
            expr_type: Some(ExpressionType::Bool),
            span: Span::new(15, 17),
        }),
    ];

    res.statements
        .into_iter()
        .zip(expected.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_grouping() {
    let source = br#"3 * (5 + 6 * 4) - 4"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);
    let expected = vec![Statement::Expression(Expression {
        expr: Expr::Binary {
            left: Box::new(Expression {
                expr: Expr::Binary {
                    left: Box::new(Expression {
                        expr: Expr::Integer(3),
                        expr_type: Some(ExpressionType::Int),
                        span: Span::new(0, 1),
                    }),
                    operator: Operator::Multiply,
                    right: Box::new(Expression {
                        expr: Expr::Grouping(Box::new(Expression {
                            expr: Expr::Binary {
                                left: Box::new(Expression {
                                    expr: Expr::Integer(5),
                                    expr_type: Some(ExpressionType::Int),
                                    span: Span::new(5, 6),
                                }),
                                operator: Operator::Plus,
                                right: Box::new(Expression {
                                    expr: Expr::Binary {
                                        left: Box::new(Expression {
                                            expr: Expr::Integer(6),
                                            expr_type: Some(ExpressionType::Int),
                                            span: Span::new(9, 10),
                                        }),
                                        operator: Operator::Multiply,
                                        right: Box::new(Expression {
                                            expr: Expr::Integer(4),
                                            expr_type: Some(ExpressionType::Int),
                                            span: Span::new(13, 14),
                                        }),
                                    },
                                    expr_type: Some(ExpressionType::Int),
                                    span: Span::new(11, 12),
                                }),
                            },
                            expr_type: Some(ExpressionType::Int),
                            span: Span::new(7, 8),
                        })),
                        expr_type: Some(ExpressionType::Int),
                        span: Span::new(5, 14),
                    }),
                },
                expr_type: Some(ExpressionType::Int),
                span: Span::new(2, 3),
            }),
            operator: Operator::Minus,
            right: Box::new(Expression {
                expr: Expr::Integer(4),
                expr_type: Some(ExpressionType::Int),
                span: Span::new(18, 19),
            }),
        },
        expr_type: Some(ExpressionType::Int),
        span: Span::new(16, 17),
    })];

    res.statements
        .into_iter()
        .zip(expected.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_multiple_errors() {
    let source = br#"1 + 2 (4 + 5) 3 + 4 + "#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);

    let expected_error = vec![
        ParseError::MissingCharacter("Missing semicolon or end of line".into(), Span::new(4, 5)),
        ParseError::MissingCharacter("Missing semicolon or end of line".into(), Span::new(12, 13)),
        ParseError::IncompleteExpression("Missing operand".into(), Span::new(20, 21)),
        ParseError::UnsupportedOperation(
            Span::new(20, 21),
            Span::new(14, 19),
            ExpressionType::Int,
            Span::new(20, 21),
            ExpressionType::Undefined,
        ),
        ParseError::MissingCharacter("Missing semicolon or end of line".into(), Span::new(20, 21)),
    ];

    res.errors
        .into_iter()
        .zip(expected_error.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_single_operator() {
    let source = br#"+"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);

    let expected_error = vec![ParseError::IncompleteExpression(
        "Missing operand".into(),
        Span::new(0, 1),
    )];

    res.errors
        .into_iter()
        .zip(expected_error.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_pipes_chain() {
    let source = br#"1 + 2 | 2 + 3 | 3"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);
    let expected = vec![Statement::Expression(Expression {
        expr: Expr::Pipe(vec![
            Expression {
                expr: Expr::Binary {
                    left: Box::new(Expression {
                        expr: Expr::Integer(1),
                        expr_type: Some(ExpressionType::Int),
                        span: Span::new(0, 1),
                    }),
                    operator: Operator::Plus,
                    right: Box::new(Expression {
                        expr: Expr::Integer(2),
                        expr_type: Some(ExpressionType::Int),
                        span: Span::new(4, 5),
                    }),
                },
                expr_type: Some(ExpressionType::Int),
                span: Span::new(2, 3),
            },
            Expression {
                expr: Expr::Binary {
                    left: Box::new(Expression {
                        expr: Expr::Integer(2),
                        expr_type: Some(ExpressionType::Int),
                        span: Span::new(8, 9),
                    }),
                    operator: Operator::Plus,
                    right: Box::new(Expression {
                        expr: Expr::Integer(3),
                        expr_type: Some(ExpressionType::Int),
                        span: Span::new(12, 13),
                    }),
                },
                expr_type: Some(ExpressionType::Int),
                span: Span::new(10, 11),
            },
            Expression {
                expr: Expr::Integer(3),
                expr_type: Some(ExpressionType::Int),
                span: Span::new(16, 17),
            },
        ]),
        expr_type: Some(ExpressionType::Int),
        span: Span::new(0, 17),
    })];

    res.statements
        .into_iter()
        .zip(expected.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_variable_assignment() {
    let source = br#"$a = 1 + 2"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);
    let expected = vec![Statement::Expression(Expression {
        expr: Expr::Assignment {
            var: b"a",
            expr: Box::new(Expression {
                expr: Expr::Binary {
                    left: Box::new(Expression {
                        expr: Expr::Integer(1),
                        expr_type: Some(ExpressionType::Int),
                        span: Span::new(5, 6),
                    }),
                    operator: Operator::Plus,
                    right: Box::new(Expression {
                        expr: Expr::Integer(2),
                        expr_type: Some(ExpressionType::Int),
                        span: Span::new(9, 10),
                    }),
                },
                expr_type: Some(ExpressionType::Int),
                span: Span::new(7, 8),
            }),
        },
        expr_type: Some(ExpressionType::Int),
        span: Span::new(3, 4),
    })];

    res.statements
        .into_iter()
        .zip(expected.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_variable_assignment_error() {
    let source = br#"a = 1 + 2"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);
    let expected_error = vec![ParseError::IncorrectAssignment(
        "Assignment to non var".into(),
        Span::new(2, 3),
    )];

    res.errors
        .into_iter()
        .zip(expected_error.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}

#[test]
fn test_command_call() {
    let source = br#"command a --flag | command b -g"#;
    let tokens = lex(source).unwrap();

    let res = parse(tokens);
    let expected = vec![Statement::Expression(Expression {
        expr: Expr::Pipe(vec![
            Expression {
                expr: Expr::Call {
                    name: b"command",
                    arguments: vec![
                        Expression {
                            expr: Expr::String(b"a"),
                            expr_type: Some(ExpressionType::String),
                            span: Span::new(8, 9),
                        },
                        Expression {
                            expr: Expr::Flag(b"--flag"),
                            expr_type: None,
                            span: Span::new(10, 16),
                        },
                    ],
                },
                expr_type: None,
                span: Span::new(0, 7),
            },
            Expression {
                expr: Expr::Call {
                    name: b"command",
                    arguments: vec![
                        Expression {
                            expr: Expr::String(b"b"),
                            expr_type: Some(ExpressionType::String),
                            span: Span::new(27, 28),
                        },
                        Expression {
                            expr: Expr::Flag(b"-g"),
                            expr_type: None,
                            span: Span::new(29, 31),
                        },
                    ],
                },
                expr_type: None,
                span: Span::new(19, 26),
            },
        ]),
        expr_type: None,
        span: Span::new(0, 26),
    })];

    res.statements
        .into_iter()
        .zip(expected.into_iter())
        .for_each(|(r, e)| assert_eq!(r, e));
}
