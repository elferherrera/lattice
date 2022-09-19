use lattice_protocol::{Expr, Expression, ExpressionType, Operator, Span, Statement};

use crate::{ParseError, Token, TokenType};

#[derive(Debug)]
pub struct ParseResult<'source> {
    pub statements: Vec<Statement<'source>>,
    pub errors: Vec<ParseError>,
}

pub fn parse(tokens: Vec<Token<'_>>) -> ParseResult<'_> {
    let mut statements: Vec<Statement> = Vec::new();
    let mut errors: Vec<ParseError> = Vec::new();
    let mut current_offset = 0;

    while let Some(token) = tokens.get(current_offset) {
        if let TokenType::EOF = token.token_type {
            current_offset += 1;
        } else {
            let (statement, mut decl_errors) = process_declaration(&tokens, &mut current_offset);
            statements.push(statement);

            if !decl_errors.is_empty() {
                errors.append(&mut decl_errors)
            }
        }
    }

    ParseResult { statements, errors }
}

fn process_declaration<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Statement<'source>, Vec<ParseError>) {
    // Module declaration
    // Module imports
    // Function declaration
    // Var Declaration

    process_statement(tokens, current_offset)
}

fn process_statement<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Statement<'source>, Vec<ParseError>) {
    // Process Block
    // Process Closure
    // Process While
    // Process For
    // Process If
    // Process Return
    // Process Continue
    // Process Break

    process_expression_statement(tokens, current_offset)
}

fn process_expression_statement<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Statement<'source>, Vec<ParseError>) {
    let (expr, mut errors) = process_expression(tokens, current_offset);

    let check_error = check_current_token(
        &[TokenType::EOL, TokenType::Semicolon, TokenType::EOF],
        tokens,
        current_offset,
        "Missing semicolon or end of line".into(),
    );

    if let Some(error) = check_error {
        errors.push(error)
    }

    (Statement::Expression(expr), errors)
}

fn process_expression<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    process_assignment(tokens, current_offset)
}

fn process_assignment<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    let (expr, mut errors) = process_pipe(tokens, current_offset);

    if token_match(&[TokenType::Equal], tokens, current_offset).is_some() {
        let equal_token = tokens
            .get(*current_offset)
            .expect("It has been checked in match there is a token");
        *current_offset += 1;

        let (right, mut expression_errors) = process_expression(tokens, current_offset);
        if !expression_errors.is_empty() {
            errors.append(&mut expression_errors)
        }

        let name = if let Expr::Var(name) = expr.expr {
            name
        } else {
            errors.push(ParseError::IncorrectAssignment(
                "Assignment to non var".into(),
                equal_token.span,
            ));

            b"Unknown"
        };

        return (
            Expression {
                expr_type: right.expr_type,
                expr: Expr::Assignment {
                    var: name,
                    expr: Box::new(right),
                },
                span: equal_token.span,
            },
            errors,
        );
    }

    (expr, errors)
}

fn process_pipe<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    let (expr, mut errors) = process_logic_or(tokens, current_offset);

    let mut pipes: Vec<Expression> = vec![expr];
    while let Some(token) = token_match(&[TokenType::Pipe], tokens, current_offset) {
        let operator = match token.token_type {
            TokenType::Pipe => Some(Operator::Pipe),
            _ => None,
        };

        if operator.is_some() {
            *current_offset += 1;
            let (logic_or_expr, mut logic_or_errors) = process_logic_or(tokens, current_offset);
            errors.append(&mut logic_or_errors);
            pipes.push(logic_or_expr);
        }
    }

    let expr = if pipes.len() > 1 {
        let expr_type = pipes
            .last()
            .expect("There is more than 1 element")
            .expr_type;

        Expression {
            span: pipes[..].into(),
            expr: Expr::Pipe(pipes),
            expr_type,
        }
    } else {
        pipes
            .into_iter()
            .next()
            .expect("There is at least one expression in the vector")
    };

    (expr, errors)
}

fn process_logic_or<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    let (mut expr, mut errors) = process_logic_and(tokens, current_offset);

    while let Some(token) = token_match(&[TokenType::Or], tokens, current_offset) {
        let operator = match token.token_type {
            TokenType::Or => Some(Operator::Or),
            _ => None,
        }
        .expect("Already checked that is one of these types");

        *current_offset += 1;
        let span = token.span;
        let (right, mut logic_and_errors) = process_logic_and(tokens, current_offset);
        errors.append(&mut logic_and_errors);

        let expr_type = match check_supported_operation(span, &expr, &right) {
            Ok(expr_type) => Some(expr_type),
            Err(type_error) => {
                errors.push(type_error);
                None
            }
        };

        expr = Expression {
            expr: Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            },
            expr_type,
            span,
        };
    }

    (expr, errors)
}

fn process_logic_and<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    let (mut expr, mut errors) = process_equality(tokens, current_offset);

    while let Some(token) = token_match(&[TokenType::And], tokens, current_offset) {
        let operator = match token.token_type {
            TokenType::And => Some(Operator::And),
            _ => None,
        }
        .expect("Already checked that is one of these types");

        *current_offset += 1;
        let span = token.span;
        let (right, mut equality_errors) = process_equality(tokens, current_offset);
        errors.append(&mut equality_errors);

        let expr_type = match check_supported_operation(span, &expr, &right) {
            Ok(expr_type) => Some(expr_type),
            Err(type_error) => {
                errors.push(type_error);
                None
            }
        };

        expr = Expression {
            expr: Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            },
            expr_type,
            span,
        };
    }

    (expr, errors)
}

fn process_equality<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    let (mut expr, mut errors) = process_comparison(tokens, current_offset);

    while let Some(token) = token_match(
        &[TokenType::EqualEqual, TokenType::BangEqual],
        tokens,
        current_offset,
    ) {
        let operator = match token.token_type {
            TokenType::EqualEqual => Some(Operator::Equal),
            TokenType::BangEqual => Some(Operator::NotEqual),
            _ => None,
        }
        .expect("Already checked that is one of these types");

        *current_offset += 1;
        let span = token.span;
        let (right, mut comparison_errors) = process_comparison(tokens, current_offset);
        errors.append(&mut comparison_errors);

        let expr_type = match check_supported_operation(span, &expr, &right) {
            Ok(expr_type) => Some(expr_type),
            Err(type_error) => {
                errors.push(type_error);
                None
            }
        };

        expr = Expression {
            expr: Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            },
            expr_type,
            span,
        };
    }

    (expr, errors)
}

fn process_comparison<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    let (mut expr, mut errors) = process_term(tokens, current_offset);

    while let Some(token) = token_match(
        &[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ],
        tokens,
        current_offset,
    ) {
        let operator = match token.token_type {
            TokenType::Greater => Some(Operator::GreaterThan),
            TokenType::GreaterEqual => Some(Operator::GreaterThanOrEqual),
            TokenType::Less => Some(Operator::LessThan),
            TokenType::LessEqual => Some(Operator::LessThanOrEqual),
            _ => None,
        }
        .expect("Already checked that is one of these types");

        *current_offset += 1;
        let span = token.span;
        let (right, mut term_errors) = process_term(tokens, current_offset);
        errors.append(&mut term_errors);

        let expr_type = match check_supported_operation(span, &expr, &right) {
            Ok(expr_type) => Some(expr_type),
            Err(type_error) => {
                errors.push(type_error);
                None
            }
        };

        expr = Expression {
            expr: Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            },
            expr_type,
            span,
        };
    }

    (expr, errors)
}

fn process_term<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    let (mut expr, mut errors) = process_factor(tokens, current_offset);

    while let Some(token) =
        token_match(&[TokenType::Plus, TokenType::Minus], tokens, current_offset)
    {
        let operator = match token.token_type {
            TokenType::Plus => Some(Operator::Plus),
            TokenType::Minus => Some(Operator::Minus),
            _ => None,
        }
        .expect("Already checked that is one of these types");

        *current_offset += 1;
        let span = token.span;
        let (right, mut factor_errors) = process_factor(tokens, current_offset);
        errors.append(&mut factor_errors);

        let expr_type = match check_supported_operation(span, &expr, &right) {
            Ok(expr_type) => Some(expr_type),
            Err(type_error) => {
                errors.push(type_error);
                None
            }
        };

        expr = Expression {
            expr: Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            },
            expr_type,
            span,
        };
    }

    (expr, errors)
}

fn process_factor<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    let (mut expr, mut errors) = process_unary(tokens, current_offset);

    while let Some(token) =
        token_match(&[TokenType::Star, TokenType::Slash], tokens, current_offset)
    {
        let operator = match token.token_type {
            TokenType::Star => Some(Operator::Multiply),
            TokenType::Slash => Some(Operator::Divide),
            _ => None,
        }
        .expect("Already checked that is one of these types");

        *current_offset += 1;
        let span = token.span;
        let (right, mut unary_errors) = process_unary(tokens, current_offset);
        errors.append(&mut unary_errors);

        let expr_type = match check_supported_operation(span, &expr, &right) {
            Ok(expr_type) => Some(expr_type),
            Err(type_error) => {
                errors.push(type_error);
                None
            }
        };

        expr = Expression {
            expr: Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            },
            expr_type,
            span,
        };
    }

    (expr, errors)
}

fn process_unary<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    if let Some(token) = tokens.get(*current_offset) {
        let operator = match token.token_type {
            TokenType::Bang => Some(Operator::Not),
            TokenType::Minus => Some(Operator::Minus),
            _ => None,
        };

        if let Some(operator) = operator {
            let start = token.span;

            *current_offset += 1;
            let (expr, errors) = process_unary(tokens, current_offset);

            return (
                Expression {
                    span: start.add(&expr.span),
                    expr_type: expr.expr_type,
                    expr: Expr::Unary {
                        operator,
                        expr: Box::new(expr),
                    },
                },
                errors,
            );
        }
    }

    process_call(tokens, current_offset)
}

fn process_call<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    let (expr, mut errors) = process_primary(tokens, current_offset);

    if let Expr::Identifier(call_name) = expr.expr {
        let mut arguments = Vec::new();
        while let Some(token) = tokens.get(*current_offset) {
            if matches!(
                token.token_type,
                TokenType::LeftParen
                    | TokenType::Int(..)
                    | TokenType::Float(..)
                    | TokenType::Var(..)
                    | TokenType::Flag(..)
                    | TokenType::String(..)
                    | TokenType::Interpolation { .. }
            ) {
                let (arg_expression, mut primary_errors) = process_primary(tokens, current_offset);
                errors.append(&mut primary_errors);
                arguments.push(arg_expression)
            } else {
                break;
            }
        }

        let call_expression = Expression {
            expr: Expr::Call {
                name: call_name,
                arguments,
            },
            span: expr.span,
            expr_type: None,
        };

        return (call_expression, errors);
    }

    (expr, errors)
}

fn process_primary<'source>(
    tokens: &[Token<'source>],
    current_offset: &mut usize,
) -> (Expression<'source>, Vec<ParseError>) {
    if let Some(token) = tokens.get(*current_offset) {
        *current_offset += 1;
        match &token.token_type {
            TokenType::Int(v) => (
                Expression {
                    expr: Expr::Integer(*v),
                    span: token.span,
                    expr_type: Some(ExpressionType::Int),
                },
                Vec::new(),
            ),
            TokenType::Float(v) => (
                Expression {
                    expr: Expr::Float(*v),
                    span: token.span,
                    expr_type: Some(ExpressionType::Float),
                },
                Vec::new(),
            ),
            TokenType::String(v) => (
                Expression {
                    expr: Expr::String(v),
                    span: token.span,
                    expr_type: Some(ExpressionType::String),
                },
                Vec::new(),
            ),
            TokenType::Var(v) => (
                Expression {
                    expr: Expr::Var(v),
                    span: token.span,
                    expr_type: None,
                },
                Vec::new(),
            ),
            TokenType::Flag(v) => (
                Expression {
                    expr: Expr::Flag(v),
                    span: token.span,
                    expr_type: None,
                },
                Vec::new(),
            ),
            TokenType::True => (
                Expression {
                    expr: Expr::Bool(true),
                    span: token.span,
                    expr_type: Some(ExpressionType::Bool),
                },
                Vec::new(),
            ),
            TokenType::False => (
                Expression {
                    expr: Expr::Bool(false),
                    span: token.span,
                    expr_type: Some(ExpressionType::Bool),
                },
                Vec::new(),
            ),
            TokenType::Identifier(v) => (
                Expression {
                    expr: Expr::Identifier(v),
                    span: token.span,
                    expr_type: None,
                },
                Vec::new(),
            ),
            TokenType::Range { start, end } => (
                Expression {
                    expr: Expr::Range {
                        start: *start,
                        end: *end,
                    },
                    span: token.span,
                    expr_type: Some(ExpressionType::Int),
                },
                Vec::new(),
            ),
            TokenType::CellPath { value, path } => (
                Expression {
                    expr: Expr::CellPath {
                        value: *value,
                        path: path.clone(),
                    },
                    span: token.span,
                    expr_type: None,
                },
                Vec::new(),
            ),
            TokenType::Interpolation {
                string,
                replacements,
            } => (
                Expression {
                    expr: Expr::Interpolation {
                        string,
                        replacements: replacements.to_vec(),
                    },
                    span: token.span,
                    expr_type: Some(ExpressionType::String),
                },
                Vec::new(),
            ),
            TokenType::LeftParen => {
                let (expr, mut errors) = process_expression(tokens, current_offset);
                let check_error = check_current_token(
                    &[TokenType::RightParen],
                    tokens,
                    current_offset,
                    "Missing right parenthesis".into(),
                );

                if let Some(check_error) = check_error {
                    errors.push(check_error)
                }

                (
                    Expression {
                        span: (&expr).into(),
                        expr_type: expr.expr_type,
                        expr: Expr::Grouping(Box::new(expr)),
                    },
                    errors,
                )
            }
            _ => {
                let offset = 2.min(*current_offset);
                let span = match tokens.get(*current_offset - offset) {
                    Some(inner_token) => inner_token.span,
                    None => Span::new(0, 0),
                };

                (
                    Expression {
                        expr: Expr::Garbage,
                        span,
                        expr_type: None,
                    },
                    vec![ParseError::IncompleteExpression(
                        "Missing operand".into(),
                        span,
                    )],
                )
            }
        }
    } else {
        (
            Expression {
                expr: Expr::Garbage,
                span: Span::new(0, 0),
                expr_type: None,
            },
            vec![ParseError::UnknownState(
                "Reached process primary without tokens".into(),
                Span::new(0, 0),
            )],
        )
    }
}

fn token_match<'source>(
    token_type: &[TokenType],
    tokens: &'source [Token<'source>],
    current_offset: &mut usize,
) -> Option<&'source Token<'source>> {
    if let Some(token) = tokens.get(*current_offset) {
        for compare_type in token_type {
            if &token.token_type == compare_type {
                return Some(token);
            }
        }
    }

    None
}

fn check_current_token(
    token_type: &[TokenType],
    tokens: &[Token],
    current_offset: &mut usize,
    error_message: String,
) -> Option<ParseError> {
    let offset = if *current_offset == tokens.len() {
        2
    } else {
        1
    };

    let span = match tokens.get(*current_offset - offset) {
        Some(token) => token.span,
        None => Span::new(0, 0),
    };

    if let Some(token) = tokens.get(*current_offset) {
        for compare_type in token_type {
            if &token.token_type == compare_type {
                *current_offset += 1;
                return None;
            }
        }
    }

    Some(ParseError::MissingCharacter(error_message, span))
}

// If expression types are different then and error is added to the errors list
fn check_supported_operation(
    span: Span,
    left: &Expression,
    right: &Expression,
) -> Result<ExpressionType, ParseError> {
    match (&left.expr_type, &right.expr_type) {
        (Some(lt), Some(rt)) if lt != rt => Err(ParseError::UnsupportedOperation(
            span,
            left.into(),
            *lt,
            right.into(),
            *rt,
        )),
        (Some(lt), None) => Err(ParseError::UnsupportedOperation(
            span,
            left.into(),
            *lt,
            right.into(),
            ExpressionType::Undefined,
        )),
        (None, Some(rt)) => Err(ParseError::UnsupportedOperation(
            span,
            left.into(),
            ExpressionType::Undefined,
            right.into(),
            *rt,
        )),
        (None, None) => Err(ParseError::UnsupportedOperation(
            span,
            left.into(),
            ExpressionType::Undefined,
            right.into(),
            ExpressionType::Undefined,
        )),
        (Some(lt), Some(_)) => Ok(*lt),
    }
}
