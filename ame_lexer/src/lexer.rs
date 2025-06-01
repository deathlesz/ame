use crate::token::{Base, Keyword, LiteralKind, NumberKind, Span, Token, TokenKind};

const EOF: char = '\0';

#[derive(Debug)]
struct Lexer<'a> {
    src: &'a str,
    chars: std::str::Chars<'a>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            chars: src.chars(),
            pos: 0,
        }
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.pos += c.len_utf8();

        Some(c)
    }

    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF)
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.peek()) && !self.is_eof() {
            self.bump();
        }
    }

    fn next(&mut self) -> Token {
        let start_pos = self.pos;

        let first;
        loop {
            match self.bump() {
                Some(c) if c.is_whitespace() => {
                    continue;
                }
                Some(c) => {
                    first = c;
                    break;
                }
                None => return Token::new(TokenKind::Eof, Span::new(self.pos, self.pos)),
            };
        }

        macro_rules! or_assign {
            ($assign:expr,$no_assign:expr) => {
                if (self.peek() == '=') {
                    self.bump();
                    $assign
                } else {
                    $no_assign
                }
            };
        }

        let kind = match first {
            '/' => match self.peek() {
                '/' => {
                    self.eat_while(|c| c != '\n');

                    TokenKind::LineComment
                }
                '*' => self.block_comment(),
                '=' => {
                    self.bump();

                    TokenKind::SlashAssign
                }
                _ => TokenKind::Slash,
            },
            c if unicode_xid::UnicodeXID::is_xid_start(c) => {
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
                let number_kind = self.number(c);
                let value = self.src[start_pos..self.pos].to_string();

                let kind = match number_kind {
                    NumberKind::Int { base, empty } => LiteralKind::Int { base, empty, value },
                    NumberKind::Float { base, empty_exp } => LiteralKind::Float {
                        base,
                        empty_exp,
                        value,
                    },
                };

                TokenKind::Literal { kind }
            }
            '"' => {
                self.bump();

                let (value, terminated) = self.string();
                let kind = LiteralKind::String { terminated, value };

                TokenKind::Literal { kind }
            }
            '+' => or_assign!(TokenKind::PlusAssign, TokenKind::Plus),
            '-' => or_assign!(TokenKind::MinusAssign, TokenKind::Minus),
            '*' => or_assign!(TokenKind::AsteriskAssign, TokenKind::Asterisk),
            '%' => or_assign!(TokenKind::PercentAssign, TokenKind::Percent),
            '&' => or_assign!(TokenKind::AndAssign, TokenKind::And),
            '|' => or_assign!(TokenKind::OrAssign, TokenKind::Or),
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

        let token = Token::new(kind, Span::new(start_pos, self.pos));

        token
    }

    fn block_comment(&mut self) -> TokenKind {
        while let Some(c) = self.bump() {
            if c == '*' && self.peek() == '/' {
                self.bump();
                return TokenKind::BlockComment { terminated: true };
            }
        }

        TokenKind::BlockComment { terminated: true }
    }

    fn number(&mut self, d: char) -> NumberKind {
        use {Base::*, NumberKind::*};

        let mut base = Decimal;

        if d == '0' {
            match self.peek() {
                'b' | 'B' => {
                    base = Binary;
                    self.bump();
                    if !self.eat_based_digits(2) {
                        return Int { base, empty: true };
                    }
                }
                'o' | 'O' => {
                    base = Octal;
                    self.bump();
                    if !self.eat_based_digits(8) {
                        return Int { base, empty: true };
                    }
                }
                'x' | 'X' => {
                    base = Hexadecimal;
                    self.bump();
                    if !self.eat_based_digits(16) {
                        return Int { base, empty: true };
                    }
                }
                '0'..='9' | '_' => {
                    self.eat_based_digits(10);
                }
                '.' | 'e' | 'E' => {} // not a base prefix, we're dealing with a float/double here
                _ => return Int { base, empty: false }, // 0
            }
        } else {
            // no base prefix
            self.eat_based_digits(10);
        }

        let mut empty_exp = false;
        match self.peek() {
            '.' => {
                self.bump();

                if self.peek().is_ascii_digit() {
                    self.eat_based_digits(10);

                    match self.peek() {
                        'e' | 'E' => {
                            self.bump();
                            empty_exp = !self.eat_float_exponent();
                        }
                        _ => {}
                    }
                }

                Float { base, empty_exp }
            }
            'e' | 'E' => {
                self.bump();
                empty_exp = !self.eat_float_exponent();
                Float { base, empty_exp }
            }
            _ => Int { base, empty: false },
        }
    }

    fn eat_based_digits(&mut self, base: u32) -> bool {
        let mut has_digits = false;
        let predicate = |c: char| c.is_digit(base);

        loop {
            match self.peek() {
                c if predicate(c) => has_digits = true,
                '_' => {}
                _ => break,
            }
            self.bump();
        }

        has_digits
    }

    fn eat_float_exponent(&mut self) -> bool {
        if self.peek() == '-' || self.peek() == '+' {
            self.bump();
        }

        self.eat_based_digits(10)
    }

    fn string(&mut self) -> (String, bool) {
        let mut value = String::new();

        // TODO: do some better escaping probably?
        while let Some(c) = self.bump() {
            match c {
                '"' => return (value, true),
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
                    _ => panic!("unrecognized escape"),
                },
                _ => value.push(c),
            }
        }

        (value, false)
    }
}

pub fn tokenize(src: &str) -> impl Iterator<Item = Token> + '_ {
    use std::rc::Rc;

    let mut lexer = Lexer::new(src);
    let mut eof_encountered = Rc::new(false); // FIXME: idk what to do here tbh

    std::iter::from_fn(move || {
        let token = lexer.next();

        if *eof_encountered {
            None
        } else if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            let eof_encountered = Rc::get_mut(&mut eof_encountered).unwrap();
            *eof_encountered = true;

            Some(token)
        }
    })
}
