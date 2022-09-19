use crate::{ExpressionType, Operator, Span};

#[derive(Debug, PartialEq)]
pub struct Expression<'source> {
    pub expr: Expr<'source>,
    pub expr_type: Option<ExpressionType>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum Expr<'source> {
    Garbage,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(&'source [u8]),
    Var(&'source [u8]),
    Flag(&'source [u8]),
    Identifier(&'source [u8]),
    Grouping(Box<Expression<'source>>),
    Assignment {
        var: &'source [u8],
        expr: Box<Expression<'source>>,
    },
    Call {
        name: &'source [u8],
        arguments: Vec<Expression<'source>>,
    },
    Range {
        start: Option<i64>,
        end: Option<i64>,
    },
    CellPath {
        value: Option<&'source [u8]>,
        path: Vec<PathType<'source>>,
    },
    Interpolation {
        string: &'source [u8],
        replacements: Vec<StringReplace<'source>>,
    },
    Unary {
        operator: Operator,
        expr: Box<Expression<'source>>,
    },
    Binary {
        left: Box<Expression<'source>>,
        operator: Operator,
        right: Box<Expression<'source>>,
    },
    Pipe(Vec<Expression<'source>>),
    List(Vec<Expression<'source>>),
    Table(Vec<Expression<'source>>, Vec<Vec<Expression<'source>>>),
    Record(Vec<(Expression<'source>, Expression<'source>)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathType<'source> {
    Column(&'source [u8]),
    Row(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringReplace<'source> {
    pub value: &'source [u8],
    pub span: Span,
}
