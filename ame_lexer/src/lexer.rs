use crate::token::{Base, Keyword, LiteralKind, Span, Token, TokenKind};

const EOF: char = '\0';

#[derive(Debug)]
struct Lexer<'a> {
    chars: std::str::Chars<'a>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    #[inline]
    fn new(src: &'a str) -> Self {
        Self {
            chars: src.chars(),
            pos: 0,
        }
    }

    #[inline]
    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.pos += c.len_utf8();

        Some(c)
    }

    #[inline]
    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF)
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    #[inline]
    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.peek()) && !self.is_eof() {
            self.bump();
        }
    }

    fn next(&mut self) -> Result<Token, LexError> {
        let mut start_pos = self.pos;

        let first;
        loop {
            match self.bump() {
                Some(c) if c.is_whitespace() => {
                    start_pos += c.len_utf8();
                    continue;
                }
                Some('/') => match self.peek() {
                    '/' => {
                        self.eat_while(|c| c != '\n');
                        start_pos = self.pos; // self.pos was advanced when skipping comment

                        continue;
                    }
                    '*' => {
                        while let Some(c) = self.bump() {
                            if c == '*' && self.peek() == '/' {
                                self.bump();
                                break;
                            }
                        }

                        start_pos = self.pos; // self.pos was advanced when skipping comment
                    }
                    '=' => {
                        self.bump();

                        return Ok(Token::new(
                            TokenKind::SlashAssign,
                            Span::new(start_pos, self.pos),
                        ));
                    }
                    _ => return Ok(Token::new(TokenKind::Slash, Span::new(start_pos, self.pos))),
                },
                Some(c) => {
                    first = c;
                    break;
                }
                None => return Ok(Token::new(TokenKind::Eof, Span::new(self.pos, self.pos))),
            }
        }

        macro_rules! or_assign {
            ($assign:expr, $no_assign:expr) => {
                if self.peek() == '=' {
                    self.bump();
                    $assign
                } else {
                    $no_assign
                }
            };
        }

        let kind = match first {
            c if c == '_' || unicode_xid::UnicodeXID::is_xid_start(c) => {
                let mut ident = String::from(c);

                loop {
                    let next = self.peek();

                    if !unicode_xid::UnicodeXID::is_xid_continue(next) {
                        break;
                    }

                    self.bump();
                    ident.push(next);
                }

                if let Ok(keyword) = ident.parse::<Keyword>() {
                    TokenKind::Keyword(keyword)
                } else {
                    TokenKind::Ident(ident)
                }
            }
            c @ '0'..='9' => {
                let kind = self.number(c);

                TokenKind::Literal { kind }
            }
            '"' => {
                let (value, terminated) = self.string()?;
                let kind = LiteralKind::String { terminated, value };

                TokenKind::Literal { kind }
            }

            // TODO: add more operators later
            '+' => or_assign!(TokenKind::PlusAssign, TokenKind::Plus),
            '-' => or_assign!(TokenKind::MinusAssign, TokenKind::Minus),
            '*' => or_assign!(TokenKind::AsteriskAssign, TokenKind::Asterisk),
            '%' => or_assign!(TokenKind::PercentAssign, TokenKind::Percent),
            '&' => {
                if self.peek() == '=' {
                    self.bump();

                    TokenKind::AmpAssign
                } else if self.peek() == '&' {
                    self.bump();

                    TokenKind::And
                } else {
                    TokenKind::Amp
                }
            }
            '|' => {
                if self.peek() == '=' {
                    self.bump();

                    TokenKind::PipeAssign
                } else if self.peek() == '|' {
                    self.bump();

                    TokenKind::Or
                } else {
                    TokenKind::Pipe
                }
            }
            '^' => or_assign!(TokenKind::CaretAssign, TokenKind::Caret),

            '=' => or_assign!(TokenKind::Eq, TokenKind::Assign),
            '!' => or_assign!(TokenKind::Ne, TokenKind::Bang),
            '>' => or_assign!(TokenKind::Ge, TokenKind::Gt),
            '<' => or_assign!(TokenKind::Le, TokenKind::Lt),

            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,

            '(' => TokenKind::Lparen,
            ')' => TokenKind::Rparen,
            '{' => TokenKind::Lbrace,
            '}' => TokenKind::Rbrace,
            '[' => TokenKind::Lbracket,
            ']' => TokenKind::Rbracket,

            '.' => TokenKind::Dot,
            ',' => TokenKind::Comma,
            '#' => TokenKind::Pound,

            c => TokenKind::Unknown(c),
        };

        Ok(Token::new(kind, Span::new(start_pos, self.pos)))
    }

    fn number(&mut self, d: char) -> LiteralKind {
        let mut number = String::new();
        let mut base = Base::Decimal;

        if d == '0' {
            match self.peek() {
                'b' | 'B' => {
                    base = Base::Binary;
                    self.bump();
                    if !self.eat_based_digits(2, &mut number) {
                        return LiteralKind::Int {
                            base,
                            empty: true,
                            value: number,
                        };
                    }
                }
                'o' | 'O' => {
                    base = Base::Octal;
                    self.bump();
                    if !self.eat_based_digits(8, &mut number) {
                        return LiteralKind::Int {
                            base,
                            empty: true,
                            value: number,
                        };
                    }
                }
                'x' | 'X' => {
                    base = Base::Hexadecimal;
                    self.bump();
                    if !self.eat_based_digits(16, &mut number) {
                        return LiteralKind::Int {
                            base,
                            empty: true,
                            value: number,
                        };
                    }
                }
                '0'..='9' | '_' => {
                    number.push(d);

                    if !self.eat_based_digits(10, &mut number) {
                        return LiteralKind::Int {
                            base,
                            empty: true,
                            value: number,
                        };
                    }
                }
                '.' | 'e' | 'E' => {
                    number.push(d);
                } // not a base prefix, we're dealing with a float/double here
                _ => {
                    number.push(d);

                    return LiteralKind::Int {
                        base,
                        empty: false,
                        value: number,
                    };
                } // 0
            }
        } else {
            // no base prefix
            number.push(d);

            self.eat_based_digits(10, &mut number);
        }

        let mut empty_exp = false;
        match self.peek() {
            '.' => {
                number.push(self.bump().expect("we know it's not None"));

                if self.peek().is_ascii_digit() {
                    self.eat_based_digits(10, &mut number);

                    match self.peek() {
                        'e' | 'E' => {
                            number.push(self.bump().expect("we know it's not None"));
                            empty_exp = !self.eat_float_exponent(&mut number);
                        }
                        _ => {}
                    }
                }

                LiteralKind::Float {
                    base,
                    empty_exp,
                    value: number,
                }
            }
            'e' | 'E' => {
                number.push(self.bump().expect("we know it's not None"));
                empty_exp = !self.eat_float_exponent(&mut number);

                LiteralKind::Float {
                    base,
                    empty_exp,
                    value: number,
                }
            }
            _ => LiteralKind::Int {
                base,
                empty: false,
                value: number,
            },
        }
    }

    fn eat_based_digits(&mut self, base: u32, string: &mut String) -> bool {
        let mut has_digits = false;
        let predicate = |c: char| c.is_digit(base);

        loop {
            match self.peek() {
                c if predicate(c) => {
                    has_digits = true;
                    string.push(c);
                }
                '_' => {}
                _ => break,
            }
            self.bump();
        }

        has_digits
    }

    fn eat_float_exponent(&mut self, string: &mut String) -> bool {
        if self.peek() == '-' || self.peek() == '+' {
            string.push(self.bump().expect("we know it's not None"));
        }

        self.eat_based_digits(10, string)
    }

    fn string(&mut self) -> Result<(String, bool), LexError> {
        let mut value = String::new();

        // TODO: do some better escaping probably?
        while let Some(c) = self.bump() {
            match c {
                '"' => return Ok((value, true)),
                '\\' => match self.peek() {
                    '"' => {
                        self.bump();
                        value.push('"');
                    }
                    '\\' => {
                        self.bump();
                        value.push('\\');
                    }
                    'n' => {
                        self.bump();
                        value.push('\n');
                    }
                    'r' => {
                        self.bump();
                        value.push('\r');
                    }
                    't' => {
                        self.bump();
                        value.push('\t');
                    }
                    '0' => {
                        self.bump();
                        value.push('\0');
                    }
                    escape => return Err(LexError::InvalidEscape(escape)),
                },
                _ => value.push(c),
            }
        }

        Ok((value, false))
    }
}

pub fn tokenize(src: &str) -> impl Iterator<Item = Result<Token, LexError>> + '_ {
    use std::rc::Rc;

    let mut lexer = Lexer::new(src);
    let mut eof_encountered = Rc::new(false); // FIXME: idk what to do here tbh

    std::iter::from_fn(move || {
        let token = lexer.next();

        if *eof_encountered {
            None
        } else if matches!(&token, Ok(token) if token.kind != TokenKind::Eof) {
            Some(token)
        } else {
            let eof_encountered = Rc::get_mut(&mut eof_encountered).unwrap();
            *eof_encountered = true;

            Some(token)
        }
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnknownToken(char),
    InvalidEscape(char),
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownToken(token) => write!(f, "unknown token `{token}`"),
            Self::InvalidEscape(escape) => write!(f, "invalid escape sequence: `\\{escape}`"),
        }
    }
}

impl std::error::Error for LexError {}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test {
        ($name:ident: $input:tt -> #$length:expr $(, $comment:literal)?) => {
            #[test]
            fn $name() {
                let tokens = tokenize($input).collect::<Result<Vec<_>, _>>().unwrap();

                assert_eq!(tokens.len(), $length, $($comment)?);
            }
        };
        ($name:ident: $input:tt -> $output:pat $(, $comment:literal)?) => {
            #[test]
            fn $name() {
                let tokens = tokenize($input).map(|token| token.map(|token| token.kind)).collect::<Result<Vec<_>, _>>().unwrap();

                assert!(matches!(tokens.as_slice(), $output), $($comment)?);
            }
        };
        ($name:ident: $input:tt -> tokens $output:pat $(, $comment:literal)?) => {
            #[test]
            fn $name() {
                let tokens = tokenize($input).collect::<Result<Vec<_>, _>>().unwrap();

                assert!(matches!(tokens.as_slice(), $output), $($comment)?);
            }
        };
        ($name:ident: $input:tt -> expr $output:expr $(, $comment:literal)?) => {
            #[test]
            fn $name() {
                let tokens = tokenize($input).map(|token| token.map(|token| token.kind)).collect::<Result<Vec<_>, _>>().unwrap();

                assert_eq!(tokens.as_slice(), $output, $($comment)?);
            }
        };
    }

    test!(test_eof: "" -> &[TokenKind::Eof], "should always return EOF at the end");

    test!(test_comments: r#"// this comment should be ignored;
/* this multiline comment should
also be ignored
even when there's /*
another multiline comment
inside of it
*/"# -> #1, "comments should be ignored");

    test!(test_keywords: "let if else while fn return" -> &[
        TokenKind::Keyword(Keyword::Let),
        TokenKind::Keyword(Keyword::If),
        TokenKind::Keyword(Keyword::Else),
        TokenKind::Keyword(Keyword::While),
        TokenKind::Keyword(Keyword::Fn),
        TokenKind::Keyword(Keyword::Return),
        TokenKind::Eof,
    ]);

    #[test]
    fn test_int_literals() {
        let tokens = tokenize("01_231 0b0010101 0o1_251_274 0xFF_1B 0b 0o 0x")
            .map(|token| token.map(|token| token.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        let kinds = tokens
            .iter()
            .flat_map(|kind| match kind {
                TokenKind::Literal { kind } => match kind {
                    int @ LiteralKind::Int { .. } => Some(int),
                    other => panic!("there should be only int literals, got {other:?}"),
                },
                TokenKind::Eof => None,
                other => panic!("there should only be literals, got {other:?}"),
            })
            .collect::<Vec<_>>();

        #[rustfmt::skip]
        let result = &[
            &LiteralKind::Int { base: Base::Decimal, empty: false, value: "01231".into() },
            &LiteralKind::Int { base: Base::Binary, empty: false, value: "0010101".into() },
            &LiteralKind::Int { base: Base::Octal, empty: false, value: "1251274".into() },
            &LiteralKind::Int { base: Base::Hexadecimal, empty: false, value: "FF1B".into() },
            &LiteralKind::Int { base: Base::Binary, empty: true, value: "".into() },
            &LiteralKind::Int { base: Base::Octal, empty: true, value: "".into() },
            &LiteralKind::Int { base: Base::Hexadecimal, empty: true, value: "".into() },
        ];

        assert_eq!(kinds, result);
    }

    #[test]
    fn test_float_literals() {
        let tokens = tokenize("1.012 0xFF.421 0o42617e-11 0231e+5 2133. 999E")
            .map(|token| token.map(|token| token.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        let kinds = tokens
            .iter()
            .flat_map(|kind| match kind {
                TokenKind::Literal { kind } => match kind {
                    float @ LiteralKind::Float { .. } => Some(float),
                    other => panic!("there should be only float literals, got {other:?}"),
                },
                TokenKind::Eof => None,
                other => panic!("there should only be literals, got {other:?}"),
            })
            .collect::<Vec<_>>();

        #[rustfmt::skip]
        let result = &[
            &LiteralKind::Float { base: Base::Decimal, empty_exp: false, value: "1.012".into() },
            &LiteralKind::Float { base: Base::Hexadecimal, empty_exp: false, value: "FF.421".into() },
            &LiteralKind::Float { base: Base::Octal, empty_exp: false, value: "42617e-11".into() },
            &LiteralKind::Float { base: Base::Decimal, empty_exp: false, value: "0231e+5".into() },
            &LiteralKind::Float { base: Base::Decimal, empty_exp: false, value: "2133.".into() },
            &LiteralKind::Float { base: Base::Decimal, empty_exp: true, value: "999E".into() },
        ];

        assert_eq!(kinds, result);
    }

    test!(test_identifiers: "foo bar baz123 _underscore _ _123" -> expr &[
        TokenKind::Ident("foo".into()),
        TokenKind::Ident("bar".into()),
        TokenKind::Ident("baz123".into()),
        TokenKind::Ident("_underscore".into()),
        TokenKind::Ident("_".into()),
        TokenKind::Ident("_123".into()),

        TokenKind::Eof,
    ]);

    test!(test_single_char_operators: "+ - * / % = < > ! & | ^" -> &[
        TokenKind::Plus,
        TokenKind::Minus,
        TokenKind::Asterisk,
        TokenKind::Slash,
        TokenKind::Percent,
        TokenKind::Assign,
        TokenKind::Lt,
        TokenKind::Gt,
        TokenKind::Bang,
        TokenKind::Amp,
        TokenKind::Pipe,
        TokenKind::Caret,

        TokenKind::Eof,
    ]);

    test!(test_multi_char_operators: "== != <= >= && || += -= *= /= %= &= |=" -> &[
        TokenKind::Eq,
        TokenKind::Ne,
        TokenKind::Le,
        TokenKind::Ge,
        TokenKind::And,
        TokenKind::Or,
        TokenKind::PlusAssign,
        TokenKind::MinusAssign,
        TokenKind::AsteriskAssign,
        TokenKind::SlashAssign,
        TokenKind::PercentAssign,
        TokenKind::AmpAssign,
        TokenKind::PipeAssign,

        TokenKind::Eof,
    ]);

    test!(test_delimiters: "(){},;[]" -> &[
        TokenKind::Lparen,
        TokenKind::Rparen,
        TokenKind::Lbrace,
        TokenKind::Rbrace,
        TokenKind::Comma,
        TokenKind::Semicolon,
        TokenKind::Lbracket,
        TokenKind::Rbracket,

        TokenKind::Eof,
    ]);

    test!(test_string_literal: r#""hello\nworld" "goodbye world..." "unterminated string!"# -> expr &[
        TokenKind::Literal { kind: LiteralKind::String { terminated: true, value: "hello\nworld".into() } },
        TokenKind::Literal { kind: LiteralKind::String { terminated: true, value: "goodbye world...".into() } },
        TokenKind::Literal { kind: LiteralKind::String { terminated: false, value: "unterminated string!".into() } },

        TokenKind::Eof,
    ]);

    test!(test_escape_sequences: r#""\"\\\n\r\t\0""# -> expr &[
        TokenKind::Literal { kind: LiteralKind::String { terminated: true, value: "\"\\\n\r\t\0".into() } },

        TokenKind::Eof,
    ]);
}
