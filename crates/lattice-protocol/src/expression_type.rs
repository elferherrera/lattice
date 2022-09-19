use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpressionType {
    Undefined,
    Bool,
    Int,
    Float,
    String,
}

impl Display for ExpressionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionType::Undefined => write!(f, "undefined"),
            ExpressionType::Bool => write!(f, "bool"),
            ExpressionType::Int => write!(f, "int"),
            ExpressionType::Float => write!(f, "float"),
            ExpressionType::String => write!(f, "string"),
        }
    }
}
