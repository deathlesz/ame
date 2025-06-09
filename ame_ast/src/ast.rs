use ame_lexer::{LiteralKind, Token, TokenKind};
use ame_types::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtKind {
    VarDecl(VarDecl),
    If {
        branches: Vec<(Expr, Vec<Stmt>)>,
        else_body: Option<Vec<Stmt>>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
    ExprStmt(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Literal(LiteralKind),
    Variable(String),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Assign {
        op: AssignOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarDecl {
    pub name: String,
    pub ty: Type,
    pub init_expr: Option<Expr>,
}

// TODO: somehow don't repeat here?
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
}

impl TryFrom<&Token> for AssignOp {
    type Error = ();

    #[inline]
    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.kind {
            TokenKind::Assign => Ok(AssignOp::Assign),
            TokenKind::PlusAssign => Ok(AssignOp::Add),
            TokenKind::MinusAssign => Ok(AssignOp::Sub),
            TokenKind::AsteriskAssign => Ok(AssignOp::Mul),
            TokenKind::SlashAssign => Ok(AssignOp::Div),

            _ => todo!("add more Token -> AssignOp conversions"),
        }
    }
}

impl TryFrom<Token> for AssignOp {
    type Error = ();

    #[inline]
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        (&token).try_into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

impl TryFrom<&Token> for BinOp {
    type Error = ();

    #[inline]
    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.kind {
            TokenKind::Plus => Ok(BinOp::Add),
            TokenKind::Minus => Ok(BinOp::Sub),
            TokenKind::Asterisk => Ok(BinOp::Mul),
            TokenKind::Slash => Ok(BinOp::Div),
            TokenKind::Percent => Ok(BinOp::Rem),
            TokenKind::Eq => Ok(BinOp::Eq),
            TokenKind::Ne => Ok(BinOp::Ne),
            TokenKind::Le => Ok(BinOp::Le),
            TokenKind::Lt => Ok(BinOp::Lt),
            TokenKind::Ge => Ok(BinOp::Ge),
            TokenKind::Gt => Ok(BinOp::Gt),

            _ => todo!("add more Token -> BinOp conversions"),
        }
    }
}

impl TryFrom<Token> for BinOp {
    type Error = ();

    #[inline]
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        (&token).try_into()
    }
}

impl TryFrom<&AssignOp> for BinOp {
    type Error = ();

    #[inline]
    fn try_from(op: &AssignOp) -> Result<Self, Self::Error> {
        match op {
            AssignOp::Add => Ok(BinOp::Add),
            AssignOp::Sub => Ok(BinOp::Sub),
            AssignOp::Mul => Ok(BinOp::Mul),
            AssignOp::Div => Ok(BinOp::Div),

            _ => todo!("add more AssignOp -> BinOp conversions"),
        }
    }
}

impl TryFrom<AssignOp> for BinOp {
    type Error = ();

    #[inline]
    fn try_from(op: AssignOp) -> Result<Self, Self::Error> {
        (&op).try_into()
    }
}
