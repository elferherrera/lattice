use crate::Expression;

#[derive(Debug, PartialEq)]
pub enum Statement<'source> {
    ModDecl,
    ModImport,
    FunDecl,
    VarDecl,
    Block,
    Closure,
    While,
    For,
    If,
    Return,
    Break,
    Continue,
    Expression(Expression<'source>),
}
