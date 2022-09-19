use crate::parse_error::ParseError;
use lattice_protocol::{PathType, Span, StringReplace};

#[derive(Debug, PartialEq)]
pub enum TokenType<'source> {
    // Single character
    Pipe,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Star,
    Slash,
    Semicolon,
    Colon,
    EOL,
    // One or two characters
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    EqualLike,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    DefType,
    // Keywords
    False,
    True,
    Alias,
    Let,
    And,
    Or,
    If,
    Else,
    For,
    In,
    Def,
    Return,
    While,
    Break,
    Continue,
    // Literals
    Int(i64),
    Float(f64),
    String(&'source [u8]),
    Var(&'source [u8]),
    Comment(&'source [u8]),
    Flag(&'source [u8]),
    Identifier(&'source [u8]),
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
    // Extra
    EOF,
}

impl<'source> From<&u8> for TokenType<'source> {
    fn from(char: &u8) -> Self {
        match char {
            b'(' => Self::LeftParen,
            b')' => Self::RightParen,
            b'{' => Self::LeftBrace,
            b'}' => Self::RightBrace,
            b'[' => Self::LeftBracket,
            b']' => Self::RightBracket,
            b',' => Self::Comma,
            b'.' => Self::Dot,
            b'-' => Self::Minus,
            b'+' => Self::Plus,
            b'*' => Self::Star,
            b'/' => Self::Slash,
            b';' => Self::Semicolon,
            b':' => Self::Colon,
            b'\n' => Self::EOL,
            _ => Self::EOF,
        }
    }
}

impl<'source> TokenType<'source> {
    fn try_keyword(chars: &[u8]) -> Option<Self> {
        match chars {
            b"alias" => Some(TokenType::Alias),
            b"let" => Some(TokenType::Let),
            b"false" => Some(TokenType::False),
            b"true" => Some(TokenType::True),
            b"and" => Some(TokenType::And),
            b"or" => Some(TokenType::Or),
            b"if" => Some(TokenType::If),
            b"else" => Some(TokenType::Else),
            b"for" => Some(TokenType::For),
            b"in" => Some(TokenType::In),
            b"def" => Some(TokenType::Def),
            b"return" => Some(TokenType::Return),
            b"while" => Some(TokenType::While),
            b"break" => Some(TokenType::Break),
            b"continue" => Some(TokenType::Continue),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token<'source> {
    pub token_type: TokenType<'source>,
    pub span: Span,
}

impl<'source> Token<'source> {
    pub fn new(token_type: TokenType<'source>, span: Span) -> Self {
        Token { token_type, span }
    }
}

pub fn lex(source: &[u8]) -> Result<Vec<Token>, ParseError> {
    let mut current_offset = 0;
    let mut tokens: Vec<Token> = Vec::new();

    while let Some(c) = source.get(current_offset) {
        match c {
            b'(' | b')' | b'{' | b'}' | b'[' | b']' | b',' | b'+' | b'*' | b'/' | b';' | b':'
            | b'\n' => {
                tokens.push(Token::new(
                    TokenType::from(c),
                    Span::new(current_offset, current_offset + 1),
                ));
                current_offset += 1;
            }
            b'|' => process_bar(source, &mut current_offset, &mut tokens),
            b'=' => process_double_char(
                source,
                &mut current_offset,
                &mut tokens,
                TokenType::Equal,
                vec![
                    (&b'=', TokenType::EqualEqual),
                    (&b'~', TokenType::EqualLike),
                ],
            ),
            b'>' => process_double_char(
                source,
                &mut current_offset,
                &mut tokens,
                TokenType::Greater,
                vec![(&b'=', TokenType::GreaterEqual)],
            ),
            b'<' => process_double_char(
                source,
                &mut current_offset,
                &mut tokens,
                TokenType::Less,
                vec![(&b'=', TokenType::LessEqual)],
            ),
            b'!' => process_double_char(
                source,
                &mut current_offset,
                &mut tokens,
                TokenType::Bang,
                vec![(&b'=', TokenType::BangEqual)],
            ),
            b'&' => process_double_char(
                source,
                &mut current_offset,
                &mut tokens,
                TokenType::And,
                vec![(&b'&', TokenType::And)],
            ),
            b'#' => process_comment(source, &mut current_offset, &mut tokens),
            b'"' => process_string(source, &mut current_offset, &mut tokens)?,
            b'-' => process_flag(source, &mut current_offset, &mut tokens),
            b'.' => process_dot(source, &mut current_offset, &mut tokens)?,
            b'f' => {
                if let Some(c) = source.get(current_offset + 1) {
                    if c == &b'"' {
                        current_offset += 1;
                        process_interpolation(source, &mut current_offset, &mut tokens)?;
                        continue;
                    }
                }

                process_item(source, &mut current_offset, &mut tokens)?
            }
            b' ' => current_offset += 1,
            _ => process_item(source, &mut current_offset, &mut tokens)?,
        }
    }

    tokens.push(Token::new(
        TokenType::EOF,
        Span::new(current_offset, current_offset),
    ));

    Ok(tokens)
}

fn process_item<'source>(
    source: &'source [u8],
    current_offset: &mut usize,
    tokens: &mut Vec<Token<'source>>,
) -> Result<(), ParseError> {
    let start = *current_offset;
    *current_offset += 1;

    while let Some(c) = source.get(*current_offset) {
        if !c.is_ascii_alphanumeric() && c != &b'_' && c != &b'.' {
            break;
        }
        *current_offset += 1;
    }

    let chars = &source[start..*current_offset];

    if let Some(c) = chars.first() {
        if c == &b'$' {
            process_cell_path(chars, start, *current_offset, tokens);
            return Ok(());
        }
    }

    if let Some(token_type) = TokenType::try_keyword(chars) {
        tokens.push(Token::new(token_type, Span::new(start, *current_offset)));
        return Ok(());
    }

    let value = String::from_utf8_lossy(chars);

    if let Ok(val) = value.parse::<i64>() {
        tokens.push(Token::new(
            TokenType::Int(val),
            Span::new(start, *current_offset),
        ));
        return Ok(());
    }

    if let Ok(val) = value.parse::<f64>() {
        tokens.push(Token::new(
            TokenType::Float(val),
            Span::new(start, *current_offset),
        ));
        return Ok(());
    }

    if let Some(index) = chars.iter().position(|c| c == &b'.') {
        if let Some(c) = chars.get(index + 1) {
            if c == &b'.' {
                let left = &chars[..index];
                let right = &chars[index + 2..];

                let left = String::from_utf8_lossy(left)
                    .parse::<i64>()
                    .map_err(|_| ParseError::IntegerParse(Span::new(start, index)))?;

                let right = if right.is_empty() {
                    None
                } else {
                    let right = String::from_utf8_lossy(right).parse::<i64>().map_err(|_| {
                        ParseError::IntegerParse(Span::new(index + 2, *current_offset))
                    })?;
                    Some(right)
                };

                tokens.push(Token::new(
                    TokenType::Range {
                        start: Some(left),
                        end: right,
                    },
                    Span::new(start, *current_offset),
                ));
                return Ok(());
            }
        }
    }

    if let Some(prev) = tokens.last() {
        match prev.token_type {
            TokenType::LeftBracket
            | TokenType::Comma
            | TokenType::String(..)
            | TokenType::Identifier(..) => {
                tokens.push(Token::new(
                    TokenType::String(chars),
                    Span::new(start, *current_offset),
                ));
                return Ok(());
            }
            _ => {}
        }
    }

    tokens.push(Token::new(
        TokenType::Identifier(chars),
        Span::new(start, *current_offset),
    ));

    Ok(())
}

fn process_cell_path<'source>(
    chars: &'source [u8],
    start: usize,
    end: usize,
    tokens: &mut Vec<Token<'source>>,
) {
    if !chars.contains(&b'.') {
        let value = &chars[1..];
        tokens.push(Token::new(TokenType::Var(value), Span::new(start, end)));

        return;
    }

    let mut parts = chars.split(|c| c == &b'.');

    let value = parts.next().and_then(|part| {
        if part.is_empty() {
            None
        } else {
            Some(&part[1..])
        }
    });

    let path: Vec<PathType> = parts
        .map(|part| {
            let string_part = String::from_utf8_lossy(part);
            if let Ok(row) = string_part.parse::<usize>() {
                PathType::Row(row)
            } else {
                PathType::Column(part)
            }
        })
        .collect();

    tokens.push(Token::new(
        TokenType::CellPath { value, path },
        Span::new(start, end),
    ));
}

fn process_comment<'source>(
    source: &'source [u8],
    current_offset: &mut usize,
    tokens: &mut Vec<Token<'source>>,
) {
    *current_offset += 1;
    let start = *current_offset;

    while let Some(c) = source.get(*current_offset) {
        *current_offset += 1;
        if c == &b'\n' {
            break;
        }
    }

    let end = if *current_offset == source.len() {
        source.len()
    } else {
        *current_offset - 1
    };

    let comment = &source[start..end];
    tokens.push(Token::new(
        TokenType::Comment(comment),
        Span::new(start, end),
    ));
}

fn process_string<'source>(
    source: &'source [u8],
    current_offset: &mut usize,
    tokens: &mut Vec<Token<'source>>,
) -> Result<(), ParseError> {
    let start = *current_offset;

    let is_raw = match (
        source.get(*current_offset),
        source.get(*current_offset + 1),
        source.get(*current_offset + 2),
    ) {
        (Some(&b'"'), Some(&b'"'), Some(&b'"')) => {
            *current_offset += 3;
            true
        }
        _ => {
            *current_offset += 1;
            false
        }
    };

    let mut incomplete = true;
    while let Some(c) = source.get(*current_offset) {
        if c == &b'"' {
            if is_raw {
                match (
                    source.get(*current_offset + 1),
                    source.get(*current_offset + 2),
                ) {
                    (Some(c1), Some(c2)) if c1 == &b'"' && c2 == &b'"' => {
                        incomplete = false;
                        *current_offset += 3;
                        break;
                    }
                    _ => {}
                }
            }

            if !is_raw {
                incomplete = false;
                *current_offset += 1;
                break;
            }
        }

        *current_offset += 1;
    }

    if incomplete {
        return Err(ParseError::MissingQuotes(Span::new(start, start + 1)));
    }

    let string = if is_raw {
        &source[start + 3..*current_offset - 3]
    } else {
        &source[start + 1..*current_offset - 1]
    };

    tokens.push(Token::new(
        TokenType::String(string),
        Span::new(start, *current_offset),
    ));

    Ok(())
}

fn process_flag<'source>(
    source: &'source [u8],
    current_offset: &mut usize,
    tokens: &mut Vec<Token<'source>>,
) {
    let start = *current_offset;
    *current_offset += 1;

    if let Some(c) = source.get(*current_offset) {
        if c.is_ascii_digit() || c == &b' ' || c == &b'$' {
            tokens.push(Token::new(
                TokenType::Minus,
                Span::new(start, *current_offset),
            ));

            return;
        }

        if c == &b'>' {
            *current_offset += 1;
            tokens.push(Token::new(
                TokenType::DefType,
                Span::new(start, *current_offset),
            ));

            return;
        }
    }

    while let Some(c) = source.get(*current_offset) {
        if c.is_ascii_whitespace() {
            break;
        }
        *current_offset += 1;
    }

    let flag = &source[start..*current_offset];
    tokens.push(Token::new(
        TokenType::Flag(flag),
        Span::new(start, *current_offset),
    ));
}

fn process_dot<'source>(
    source: &'source [u8],
    current_offset: &mut usize,
    tokens: &mut Vec<Token<'source>>,
) -> Result<(), ParseError> {
    let start = *current_offset;
    *current_offset += 1;

    if let Some(c) = source.get(*current_offset) {
        // This is an empty left size range, eg: ..123
        if c == &b'.' {
            *current_offset += 1;
            let right_size_start = *current_offset;
            while let Some(a) = source.get(*current_offset) {
                if !a.is_ascii_digit() {
                    break;
                }
                *current_offset += 1;
            }

            let digits = &source[right_size_start..*current_offset];
            match String::from_utf8_lossy(digits).parse::<i64>() {
                Ok(val) => {
                    tokens.push(Token::new(
                        TokenType::Range {
                            start: None,
                            end: Some(val),
                        },
                        Span::new(start, *current_offset),
                    ));
                    return Ok(());
                }
                Err(_) => {
                    return Err(ParseError::IntegerParse(Span::new(
                        right_size_start,
                        *current_offset,
                    )))
                }
            }
        }
    }

    if let Some(prev) = tokens.last() {
        // This is a CellPath that doesn't have a value name
        // Chars should be `.a.b.0`
        if let TokenType::RightParen = prev.token_type {
            while let Some(c) = source.get(*current_offset) {
                if !c.is_ascii_alphanumeric() && c != &b'_' && c != &b'.' {
                    break;
                }
                *current_offset += 1;
            }

            let chars = &source[start..*current_offset];

            process_cell_path(chars, start, *current_offset, tokens);
        }
    }

    Ok(())
}

fn process_interpolation<'source>(
    source: &'source [u8],
    current_offset: &mut usize,
    tokens: &mut Vec<Token<'source>>,
) -> Result<(), ParseError> {
    let start = *current_offset;

    let is_raw = match (
        source.get(*current_offset),
        source.get(*current_offset + 1),
        source.get(*current_offset + 2),
    ) {
        (Some(&b'"'), Some(&b'"'), Some(&b'"')) => {
            *current_offset += 3;
            true
        }
        _ => {
            *current_offset += 1;
            false
        }
    };

    let mut incomplete = true;
    let mut open = false;
    let mut inner_start = 0;
    let mut value_start: Option<usize> = None;
    let mut replacements: Vec<StringReplace> = Vec::new();
    while let Some(c) = source.get(*current_offset) {
        if c == &b'{' {
            match open {
                false => {
                    open = true;
                    inner_start = *current_offset;
                }
                true => {
                    return Err(ParseError::MissingBracket(Span::new(
                        inner_start,
                        inner_start + 1,
                    )));
                }
            }
        }

        if open && c == &b'$' {
            value_start = Some(*current_offset);
        }

        if c == &b'}' {
            match open {
                true => {
                    open = false;

                    match value_start {
                        None => {
                            return Err(ParseError::EmptyInterpolation(Span::new(
                                inner_start + 1,
                                inner_start + 2,
                            )))
                        }
                        Some(start) => {
                            let chars = &source[start + 1..*current_offset];
                            let empty = chars.iter().position(|c| c == &b' ');

                            let end = match empty {
                                Some(idx) => idx + start + 1,
                                None => *current_offset,
                            };

                            let value = &source[start + 1..end];

                            value_start = None;
                            replacements.push(StringReplace {
                                value,
                                span: Span::new(inner_start, *current_offset + 1),
                            });
                        }
                    }
                }
                false => {
                    return Err(ParseError::MissingBracket(Span::new(
                        *current_offset,
                        *current_offset + 1,
                    )));
                }
            }
        }

        if c == &b'"' {
            if is_raw {
                match (
                    source.get(*current_offset + 1),
                    source.get(*current_offset + 2),
                ) {
                    (Some(c1), Some(c2)) if c1 == &b'"' && c2 == &b'"' => {
                        incomplete = false;
                        *current_offset += 3;
                        break;
                    }
                    _ => {}
                }
            }

            if !is_raw {
                *current_offset += 1;
                incomplete = false;
                break;
            }
        }

        *current_offset += 1;
    }

    if open {
        return Err(ParseError::MissingBracket(Span::new(
            inner_start,
            inner_start + 1,
        )));
    }

    if incomplete {
        return Err(ParseError::MissingQuotes(Span::new(start, start + 1)));
    }

    let string = if is_raw {
        &source[start + 3..*current_offset - 3]
    } else {
        &source[start + 1..*current_offset - 1]
    };

    tokens.push(Token::new(
        TokenType::Interpolation {
            string,
            replacements,
        },
        Span::new(start - 1, *current_offset),
    ));

    Ok(())
}

fn process_double_char<'source>(
    source: &'source [u8],
    current_offset: &mut usize,
    tokens: &mut Vec<Token<'source>>,
    single_option: TokenType<'source>,
    options: Vec<(&u8, TokenType<'source>)>,
) {
    let idx = *current_offset;
    let prev_idx = idx;
    *current_offset += 1;

    for (double_char, double_option) in options {
        if let Some(c) = source.get(*current_offset) {
            if c == double_char {
                let idx = *current_offset;
                *current_offset += 1;

                tokens.push(Token::new(double_option, Span::new(prev_idx, idx + 1)));

                return;
            }
        }
    }

    tokens.push(Token::new(single_option, Span::new(prev_idx, idx + 1)));
}

fn process_bar<'source>(
    source: &'source [u8],
    current_offset: &'source mut usize,
    tokens: &'source mut Vec<Token>,
) {
    // If the next character is `|`, it's either `|` or `||`
    let idx = *current_offset;
    let prev_idx = idx;
    *current_offset += 1;

    // If the next character is `|`, we're looking at a `||`.
    if let Some(c) = source.get(*current_offset) {
        if *c == b'|' {
            let idx = *current_offset;
            *current_offset += 1;

            tokens.push(Token::new(TokenType::Or, Span::new(prev_idx, idx + 1)));

            return;
        }
    }

    if let Some(prev_token) = tokens.last_mut() {
        match prev_token.token_type {
            TokenType::EOL | TokenType::Comment(..) => {
                *prev_token = Token::new(TokenType::Pipe, Span::new(prev_idx, idx + 1));
            }
            _ => tokens.push(Token::new(TokenType::Pipe, Span::new(prev_idx, idx + 1))),
        }
    } else {
        tokens.push(Token::new(TokenType::Pipe, Span::new(prev_idx, idx + 1)));
    }
}
