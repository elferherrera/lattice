use miette::SourceSpan;

use crate::{Expr, Expression};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<Span> for SourceSpan {
    fn from(s: Span) -> Self {
        Self::new(s.start.into(), (s.end - s.start).into())
    }
}

impl<'source> From<&Expression<'source>> for Span {
    fn from(expr: &Expression) -> Self {
        match &expr.expr {
            Expr::Garbage
            | Expr::Bool(_)
            | Expr::Integer(_)
            | Expr::Float(_)
            | Expr::String(_)
            | Expr::Var(_)
            | Expr::Flag(_)
            | Expr::Identifier(_) => expr.span,
            Expr::Grouping(inner_expr) => inner_expr.as_ref().into(),
            Expr::Unary {
                expr: inner_expr, ..
            } => {
                let operator_span = expr.span;
                let span: Span = inner_expr.as_ref().into();

                operator_span.add(&span)
            }
            Expr::Binary { left, right, .. } => {
                let left_span: Span = left.as_ref().into();
                let right_span: Span = right.as_ref().into();

                left_span.add(&right_span)
            }
            Expr::Call { .. } => expr.span,
            _ => {
                println!("Span Expression: {:?}", expr);
                todo!()
            }
        }
    }
}

impl<'source> From<&[Expression<'source>]> for Span {
    fn from(expressions: &[Expression]) -> Self {
        match (expressions.first(), expressions.last()) {
            (Some(first), Some(last)) => {
                let first_span: Span = first.into();
                let last_span: Span = last.into();

                first_span.add(&last_span)
            }
            (Some(first), None) => first.into(),
            (None, Some(last)) => last.into(),
            _ => Span::new(0, 0),
        }
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn add(&self, other: &Self) -> Self {
        let start = self.start.min(other.start);
        let end = self.end.max(other.end);

        Self { start, end }
    }
}
