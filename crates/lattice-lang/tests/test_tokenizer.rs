use lattice_lang::{lex, ParseError, Token, TokenType};
use lattice_protocol::{PathType, Span, StringReplace};

#[test]
fn test_single_double() {
    let source = b"== || | <= >= =! != > < &&";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::EqualEqual, Span::new(0, 2)),
            Token::new(TokenType::Or, Span::new(3, 5)),
            Token::new(TokenType::Pipe, Span::new(6, 7)),
            Token::new(TokenType::LessEqual, Span::new(8, 10)),
            Token::new(TokenType::GreaterEqual, Span::new(11, 13)),
            Token::new(TokenType::Equal, Span::new(14, 15)),
            Token::new(TokenType::Bang, Span::new(15, 16)),
            Token::new(TokenType::BangEqual, Span::new(17, 19)),
            Token::new(TokenType::Greater, Span::new(20, 21)),
            Token::new(TokenType::Less, Span::new(22, 23)),
            Token::new(TokenType::And, Span::new(24, 26)),
            Token::new(TokenType::EOF, Span::new(26, 26)),
        ]
    );
}

#[test]
fn test_single_chars() {
    let source = b"  , ; : - + * / \n";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Comma, Span::new(2, 3)),
            Token::new(TokenType::Semicolon, Span::new(4, 5)),
            Token::new(TokenType::Colon, Span::new(6, 7)),
            Token::new(TokenType::Minus, Span::new(8, 9)),
            Token::new(TokenType::Plus, Span::new(10, 11)),
            Token::new(TokenType::Star, Span::new(12, 13)),
            Token::new(TokenType::Slash, Span::new(14, 15)),
            Token::new(TokenType::EOL, Span::new(16, 17)),
            Token::new(TokenType::EOF, Span::new(17, 17)),
        ]
    );
}

#[test]
fn test_keywords() {
    let source = b"let for else while def if";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Let, Span::new(0, 3)),
            Token::new(TokenType::For, Span::new(4, 7)),
            Token::new(TokenType::Else, Span::new(8, 12)),
            Token::new(TokenType::While, Span::new(13, 18)),
            Token::new(TokenType::Def, Span::new(19, 22)),
            Token::new(TokenType::If, Span::new(23, 25)),
            Token::new(TokenType::EOF, Span::new(25, 25)),
        ]
    );
}

#[test]
fn test_value_operations() {
    let source = b"$a + $b - $c";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Var(b"a"), Span::new(0, 2)),
            Token::new(TokenType::Plus, Span::new(3, 4)),
            Token::new(TokenType::Var(b"b"), Span::new(5, 7)),
            Token::new(TokenType::Minus, Span::new(8, 9)),
            Token::new(TokenType::Var(b"c"), Span::new(10, 12)),
            Token::new(TokenType::EOF, Span::new(12, 12)),
        ]
    );
}

#[test]
fn test_value_operations_no_space() {
    let source = b"$a+$b-$c=$d";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Var(b"a"), Span::new(0, 2)),
            Token::new(TokenType::Plus, Span::new(2, 3)),
            Token::new(TokenType::Var(b"b"), Span::new(3, 5)),
            Token::new(TokenType::Minus, Span::new(5, 6)),
            Token::new(TokenType::Var(b"c"), Span::new(6, 8)),
            Token::new(TokenType::Equal, Span::new(8, 9)),
            Token::new(TokenType::Var(b"d"), Span::new(9, 11)),
            Token::new(TokenType::EOF, Span::new(11, 11)),
        ]
    );
}

#[test]
fn test_value_create_comment() {
    let source = b"# A comment \n # Another comment";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Comment(b" A comment "), Span::new(1, 12)),
            Token::new(TokenType::Comment(b" Another comment"), Span::new(15, 31)),
            Token::new(TokenType::EOF, Span::new(31, 31)),
        ]
    );
}

#[test]
fn test_simple_string() {
    let source = br#""this is a string" + "another string""#;
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::String(b"this is a string"), Span::new(0, 18)),
            Token::new(TokenType::Plus, Span::new(19, 20)),
            Token::new(TokenType::String(b"another string"), Span::new(21, 37)),
            Token::new(TokenType::EOF, Span::new(37, 37)),
        ]
    );
}

#[test]
fn test_string_missing_quotes() {
    let source = br#""this is a string + "another string""#;
    let tokens = lex(source);

    assert!(matches!(
        tokens,
        Err(ParseError::MissingQuotes(Span { start: 35, end: 36 }))
    ));
}

#[test]
fn test_string_raw() {
    let source = br#" """this is raw string""" + "" + """another "one" string""" "#;
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::String(b"this is raw string"), Span::new(1, 25)),
            Token::new(TokenType::Plus, Span::new(26, 27)),
            Token::new(TokenType::String(b""), Span::new(28, 30)),
            Token::new(TokenType::Plus, Span::new(31, 32)),
            Token::new(
                TokenType::String(br#"another "one" string"#),
                Span::new(33, 59)
            ),
            Token::new(TokenType::EOF, Span::new(60, 60)),
        ]
    );
}

#[test]
fn test_string_raw_missing_quotes() {
    let source = br#" """this is raw string + """another "one" string""" "#;
    let tokens = lex(source);

    assert!(matches!(
        tokens,
        Err(ParseError::MissingQuotes(Span { start: 48, end: 49 }))
    ));
}

#[test]
fn test_number_tokens() {
    let source = b"2 + 3 -4/10.0 + 1.0+0.0";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Int(2), Span::new(0, 1)),
            Token::new(TokenType::Plus, Span::new(2, 3)),
            Token::new(TokenType::Int(3), Span::new(4, 5)),
            Token::new(TokenType::Minus, Span::new(6, 7)),
            Token::new(TokenType::Int(4), Span::new(7, 8)),
            Token::new(TokenType::Slash, Span::new(8, 9)),
            Token::new(TokenType::Float(10.0), Span::new(9, 13)),
            Token::new(TokenType::Plus, Span::new(14, 15)),
            Token::new(TokenType::Float(1.0), Span::new(16, 19)),
            Token::new(TokenType::Plus, Span::new(19, 20)),
            Token::new(TokenType::Float(0.0), Span::new(20, 23)),
            Token::new(TokenType::EOF, Span::new(23, 23)),
        ]
    );
}

#[test]
fn test_flag_and_command() {
    let source = b"let a = command val --flag -f";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Let, Span::new(0, 3)),
            Token::new(TokenType::Identifier(b"a"), Span::new(4, 5)),
            Token::new(TokenType::Equal, Span::new(6, 7)),
            Token::new(TokenType::Identifier(b"command"), Span::new(8, 15)),
            Token::new(TokenType::String(b"val"), Span::new(16, 19)),
            Token::new(TokenType::Flag(b"--flag"), Span::new(20, 26)),
            Token::new(TokenType::Flag(b"-f"), Span::new(27, 29)),
            Token::new(TokenType::EOF, Span::new(29, 29)),
        ]
    );
}

#[test]
fn test_pipe_chain_new_line() {
    let source = br#"let a = command val --flag -f
| command b # This is a comment gets ignored
| command c
| command d
"#;
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Let, Span::new(0, 3)),
            Token::new(TokenType::Identifier(b"a"), Span::new(4, 5)),
            Token::new(TokenType::Equal, Span::new(6, 7)),
            Token::new(TokenType::Identifier(b"command"), Span::new(8, 15)),
            Token::new(TokenType::String(b"val"), Span::new(16, 19)),
            Token::new(TokenType::Flag(b"--flag"), Span::new(20, 26)),
            Token::new(TokenType::Flag(b"-f"), Span::new(27, 29)),
            Token::new(TokenType::Pipe, Span::new(30, 31)),
            Token::new(TokenType::Identifier(b"command"), Span::new(32, 39)),
            Token::new(TokenType::String(b"b"), Span::new(40, 41)),
            Token::new(TokenType::Pipe, Span::new(75, 76)),
            Token::new(TokenType::Identifier(b"command"), Span::new(77, 84)),
            Token::new(TokenType::String(b"c"), Span::new(85, 86)),
            Token::new(TokenType::Pipe, Span::new(87, 88)),
            Token::new(TokenType::Identifier(b"command"), Span::new(89, 96)),
            Token::new(TokenType::String(b"d"), Span::new(97, 98)),
            Token::new(TokenType::EOL, Span::new(98, 99)),
            Token::new(TokenType::EOF, Span::new(99, 99)),
        ]
    );
}

#[test]
fn test_string_array() {
    let source = b"let a = [one two three]";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Let, Span::new(0, 3)),
            Token::new(TokenType::Identifier(b"a"), Span::new(4, 5)),
            Token::new(TokenType::Equal, Span::new(6, 7)),
            Token::new(TokenType::LeftBracket, Span::new(8, 9)),
            Token::new(TokenType::String(b"one"), Span::new(9, 12)),
            Token::new(TokenType::String(b"two"), Span::new(13, 16)),
            Token::new(TokenType::String(b"three"), Span::new(17, 22)),
            Token::new(TokenType::RightBracket, Span::new(22, 23)),
            Token::new(TokenType::EOF, Span::new(23, 23)),
        ]
    );
}

#[test]
fn test_cell_path() {
    let source = b"$e.0 $f.a.1 $a.b.c.0";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(
                TokenType::CellPath {
                    value: Some(b"e"),
                    path: vec![PathType::Row(0)]
                },
                Span::new(0, 4)
            ),
            Token::new(
                TokenType::CellPath {
                    value: Some(b"f"),
                    path: vec![PathType::Column(b"a"), PathType::Row(1),]
                },
                Span::new(5, 11)
            ),
            Token::new(
                TokenType::CellPath {
                    value: Some(b"a"),
                    path: vec![
                        PathType::Column(b"b"),
                        PathType::Column(b"c"),
                        PathType::Row(0),
                    ]
                },
                Span::new(12, 20)
            ),
            Token::new(TokenType::EOF, Span::new(20, 20)),
        ]
    );
}

#[test]
fn test_cell_path_from_expression() {
    let source = b"(command a).a.b.0";
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::LeftParen, Span::new(0, 1)),
            Token::new(TokenType::Identifier(b"command"), Span::new(1, 8)),
            Token::new(TokenType::String(b"a"), Span::new(9, 10)),
            Token::new(TokenType::RightParen, Span::new(10, 11)),
            Token::new(
                TokenType::CellPath {
                    value: None,
                    path: vec![
                        PathType::Column(b"a"),
                        PathType::Column(b"b"),
                        PathType::Row(0),
                    ]
                },
                Span::new(11, 17)
            ),
            Token::new(TokenType::EOF, Span::new(17, 17)),
        ]
    );
}

#[test]
fn test_string_interpolation() {
    let source = br#" f"this: {$a} { $b } " "#;
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(
                TokenType::Interpolation {
                    string: b"this: {$a} { $b } ",
                    replacements: vec![
                        StringReplace {
                            value: b"a",
                            span: Span::new(9, 13)
                        },
                        StringReplace {
                            value: b"b",
                            span: Span::new(14, 20)
                        },
                    ]
                },
                Span::new(1, 22)
            ),
            Token::new(TokenType::EOF, Span::new(23, 23)),
        ]
    );
}

#[test]
fn test_string_interpolation_missing_bracket() {
    let source = br#" f"this: {$a is a string " "#;
    let tokens = lex(source);

    assert!(matches!(
        tokens,
        Err(ParseError::MissingBracket(Span { start: 9, end: 10 }))
    ));
}

#[test]
fn test_string_interpolation_missing_value() {
    let source = br#" f"this: { a } is a string " "#;
    let tokens = lex(source);

    assert!(matches!(
        tokens,
        Err(ParseError::EmptyInterpolation(Span { start: 10, end: 11 }))
    ));
}

#[test]
fn test_string_interpolation_missing_quotes() {
    let source = br#" f"this: {$a} is a string "#;
    let tokens = lex(source);

    assert!(matches!(
        tokens,
        Err(ParseError::MissingQuotes(Span { start: 2, end: 3 }))
    ));
}

#[test]
fn test_raw_string_interpolation() {
    let source = br#" f"""this: "{$a}" "{ $b }" """ "#;
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(
                TokenType::Interpolation {
                    string: br#"this: "{$a}" "{ $b }" "#,
                    replacements: vec![
                        StringReplace {
                            value: b"a",
                            span: Span::new(12, 16)
                        },
                        StringReplace {
                            value: b"b",
                            span: Span::new(19, 25)
                        },
                    ]
                },
                Span::new(1, 30)
            ),
            Token::new(TokenType::EOF, Span::new(31, 31)),
        ]
    );
}

#[test]
fn test_raw_string_interpolation_missing_bracket() {
    let source = br#" f""""this: {$a is a string """ "#;
    let tokens = lex(source);

    assert!(matches!(
        tokens,
        Err(ParseError::MissingBracket(Span { start: 12, end: 13 }))
    ));
}

#[test]
fn test_raw_string_interpolation_missing_quotes() {
    let source = br#" f"""this: {$a} is a string "#;
    let tokens = lex(source);

    assert!(matches!(
        tokens,
        Err(ParseError::MissingQuotes(Span { start: 2, end: 3 }))
    ));
}

#[test]
fn test_empty_left_range() {
    let source = br#" ..123 "#;
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(
                TokenType::Range {
                    start: None,
                    end: Some(123)
                },
                Span::new(1, 6)
            ),
            Token::new(TokenType::EOF, Span::new(7, 7)),
        ]
    );
}

#[test]
fn test_empty_left_range_error() {
    let source = br#" ..a123 "#;
    let tokens = lex(source);

    assert!(matches!(
        tokens,
        Err(ParseError::IntegerParse(Span { start: 3, end: 3 }))
    ));
}

#[test]
fn test_complete_range() {
    let source = br#" 12..123 "#;
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(
                TokenType::Range {
                    start: Some(12),
                    end: Some(123)
                },
                Span::new(1, 8)
            ),
            Token::new(TokenType::EOF, Span::new(9, 9)),
        ]
    );
}

#[test]
fn test_empty_right_range() {
    let source = br#" 12.. "#;
    let tokens = lex(source).unwrap();

    assert_eq!(
        tokens,
        vec![
            Token::new(
                TokenType::Range {
                    start: Some(12),
                    end: None
                },
                Span::new(1, 5)
            ),
            Token::new(TokenType::EOF, Span::new(6, 6)),
        ]
    );
}

#[test]
fn test_range_parse_error() {
    let source = br#" b..a123 "#;
    let tokens = lex(source);

    assert!(matches!(
        tokens,
        Err(ParseError::IntegerParse(Span { start: 1, end: 1 }))
    ));
}
