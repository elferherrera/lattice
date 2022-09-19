use lattice_protocol::{ExpressionType, Span};
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, PartialEq, Eq)]
pub enum ParseError {
    #[error("Unknown state")]
    #[diagnostic(code(lattice::parser::unknown_state), url(docsrs))]
    UnknownState(String, #[label = "{0}"] Span),

    #[error("Missing string quotes")]
    #[diagnostic(
        code(lattice::parser::missing_parenthesis),
        url(docsrs),
        help("Try adding closing quotes")
    )]
    MissingQuotes(#[label = "missing string quotes"] Span),

    #[error("Missing bracket")]
    #[diagnostic(
        code(lattice::parser::missing_bracket),
        url(docsrs),
        help("Try adding missing bracket")
    )]
    MissingBracket(#[label = "missing bracket"] Span),

    #[error("Value not found in interpolation")]
    #[diagnostic(
        code(lattice::parser::missing_bracket),
        url(docsrs),
        help("Add value to interpolate in string")
    )]
    EmptyInterpolation(#[label = "missing value in interpolation"] Span),

    #[error("Unable to parse integer")]
    #[diagnostic(
        code(lattice::parser::missing_bracket),
        url(docsrs),
        help("Change to an integer value")
    )]
    IntegerParse(#[label = "Expected integer to parse"] Span),

    #[error("Missing character")]
    #[diagnostic(code(lattice::parser::missing_character), url(docsrs))]
    MissingCharacter(String, #[label = "{0}"] Span),

    #[error("Incomplete expression")]
    #[diagnostic(code(lattice::parser::incomplete_expression), url(docsrs))]
    IncompleteExpression(String, #[label = "{0}"] Span),

    #[error("Incorrect assignment")]
    #[diagnostic(code(lattice::parser::incorrect_assignment), url(docsrs))]
    IncorrectAssignment(String, #[label = "{0}"] Span),

    #[error("Types mismatched for operation.")]
    #[diagnostic(
        code(lattice::parser::unsupported_operation),
        url(docsrs),
        help("Change {2} or {4} to be the right types and try again.")
    )]
    UnsupportedOperation(
        #[label = "doesn't support these values."] Span,
        #[label("{2}")] Span,
        ExpressionType,
        #[label("{4}")] Span,
        ExpressionType,
    ),
}
