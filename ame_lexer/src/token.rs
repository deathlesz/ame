#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[inline]
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Plus,
    PlusAssign,
    Minus,
    MinusAssign,
    Asterisk,
    AsteriskAssign,
    Slash,
    SlashAssign,
    Percent,
    PercentAssign,
    Amp,
    And,
    AmpAssign,
    Pipe,
    Or,
    PipeAssign,
    Caret,
    CaretAssign,
    Assign,

    Eq,
    Bang,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    Colon,
    Semicolon,

    Dot,
    Comma,
    Pound,

    Literal { kind: LiteralKind },

    Ident(String),
    Keyword(Keyword),

    Unknown(char),
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralKind {
    Int {
        base: Base,
        empty: bool,
        value: String,
    },
    Float {
        base: Base,
        empty_exp: bool,
        value: String,
    },
    String {
        terminated: bool,
        value: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Base {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Let,
    If,
    Else,
    While,
    Fn,
    Return,
}

impl std::str::FromStr for Keyword {
    type Err = ();

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "let" => Ok(Keyword::Let),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "while" => Ok(Keyword::While),
            "fn" => Ok(Keyword::Fn),
            "return" => Ok(Keyword::Return),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub start: usize,
    pub end: usize, // is exclusive
}

impl Span {
    #[inline]
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}
