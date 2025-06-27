use ame_ast::{Expr, ExprKind, Stmt, StmtKind, VarDecl};
use ame_lexer::{Keyword, Span, Token, TokenKind};
use ame_types::Type;

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    // NOTE: probably not the best decision, but i don't want all functions to return Vecs
    // TODO: also, should probably use TinyVec or smth
    backlog: Vec<Stmt>,
    pos: usize,
}

impl<'a> Parser<'a> {
    #[inline]
    pub const fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            backlog: vec![],
            pos: 0,
        }
    }

    // `Token { kind: TokeKind::Eof, .. }` is always present at the end
    // so parsing can stop there without requiring these functions to return `Option`
    #[inline]
    const fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    #[inline]
    const fn next(&mut self) -> &Token {
        let token = &self.tokens[self.pos];
        self.pos += 1;

        token
    }

    #[inline]
    fn at(&self, kind: &TokenKind) -> bool {
        &self.peek().kind == kind
    }

    // fn eat(&mut self, kind: &TokenKind) -> bool {
    //     if self.at(kind) {
    //         self.next();
    //
    //         true
    //     } else {
    //         false
    //     }
    // }

    fn expect(&mut self, kind: &TokenKind) -> Result<&Token> {
        if self.at(kind) {
            Ok(self.next())
        } else {
            let got = self.peek();

            Err(ParseError::Unexpected {
                got: got.kind.clone(),
                expected: format!("{kind:?}"),
                span: got.span,
            })
        }
    }

    fn expect_ident(&mut self) -> Result<String> {
        let peek = self.peek();

        match peek.kind.clone() {
            TokenKind::Ident(name) => {
                self.next();

                Ok(name)
            }
            _ => Err(ParseError::Unexpected {
                got: peek.kind.clone(),
                expected: "ident".into(),
                span: peek.span,
            }),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while let Some(stmt) = self.parse_stmt()? {
            stmts.push(stmt);
        }

        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Option<Stmt>> {
        if let Some(stmt) = self.backlog.pop() {
            return Ok(Some(stmt));
        }

        let token = self.peek();
        match &token.kind {
            kw @ TokenKind::Keyword(keyword) => match keyword {
                Keyword::Let => {
                    let mut decls = self.parse_let()?;
                    let result = decls
                        .pop()
                        .expect("at least 1 declaration must've been parsed");

                    self.backlog.extend(decls);

                    Ok(Some(result))
                }
                Keyword::If => Ok(Some(self.parse_if()?)),
                Keyword::While => Ok(Some(self.parse_while()?)),
                Keyword::Else => Err(ParseError::Unexpected {
                    got: kw.clone(),
                    expected: "else to be preceeded by if statement".into(),
                    span: token.span,
                }),
                Keyword::Fn => Ok(Some(self.parse_fn()?)),
                Keyword::Return => {
                    self.next(); // `return` keyword
                    let expr = if !self.at(&TokenKind::Semicolon) {
                        Some(self.parse_expr(0)?)
                    } else {
                        None
                    };

                    self.expect(&TokenKind::Semicolon)?;

                    Ok(Some(Stmt {
                        kind: StmtKind::Return(expr),
                    }))
                }
            },
            TokenKind::Eof => Ok(None),
            TokenKind::Rbrace => Ok(None),
            _ => {
                let expr = self.parse_expr(0)?;
                self.expect(&TokenKind::Semicolon)?;

                Ok(Some(Stmt {
                    kind: StmtKind::ExprStmt(expr),
                }))
            }
        }
    }

    fn parse_let(&mut self) -> Result<Vec<Stmt>> {
        self.next(); // `let` keyword

        let mut decls = vec![];
        loop {
            let name = self.expect_ident()?;
            let ty = if self.expect(&TokenKind::Colon).is_ok() {
                Some(self.expect_ident()?)
            } else {
                None
            };

            let with_init = self.expect(&TokenKind::Assign).is_ok();
            let init_expr = if with_init {
                Some(self.parse_expr(0)?)
            } else {
                None
            };

            decls.push(Stmt {
                kind: StmtKind::VarDecl(VarDecl {
                    name,
                    ty: ty.map(|ty| ty.into()).unwrap_or(Type::Unknown),
                    init_expr,
                }),
            });

            if self.at(&TokenKind::Semicolon) {
                self.next();
                break;
            }

            self.expect(&TokenKind::Comma)?;
        }

        decls.reverse();
        Ok(decls)
    }

    fn parse_if(&mut self) -> Result<Stmt> {
        self.next(); // `if` keyword

        let cond = self.parse_expr(0)?;
        self.expect(&TokenKind::Lbrace)?;
        let body = self.parse()?;
        self.expect(&TokenKind::Rbrace)?;

        let mut branches = vec![];
        branches.push((cond, body));

        while self.expect(&TokenKind::Keyword(Keyword::Else)).is_ok() {
            if self.at(&TokenKind::Keyword(Keyword::If)) {
                self.next(); // `if` keyword

                let cond = self.parse_expr(0)?;
                self.expect(&TokenKind::Lbrace)?;
                let body = self.parse()?;
                self.expect(&TokenKind::Rbrace)?;

                branches.push((cond, body))
            } else {
                self.expect(&TokenKind::Lbrace)?;
                let else_body = self.parse()?;
                self.expect(&TokenKind::Rbrace)?;

                return Ok(Stmt {
                    kind: StmtKind::If {
                        branches,
                        else_body: Some(else_body),
                    },
                });
            }
        }

        Ok(Stmt {
            kind: StmtKind::If {
                branches,
                else_body: None,
            },
        })
    }

    fn parse_fn(&mut self) -> Result<Stmt> {
        self.next(); // `fn` keyword

        let name = self.expect_ident()?;

        let mut args = vec![];
        self.expect(&TokenKind::Lparen)?;
        if !self.at(&TokenKind::Rparen) {
            loop {
                let name = self.expect_ident()?;
                self.expect(&TokenKind::Colon)?;
                let ty = self.expect_ident()?.into();

                let with_init = self.expect(&TokenKind::Assign).is_ok();
                let init_expr = if with_init {
                    Some(self.parse_expr(0)?)
                } else {
                    None
                };

                // NOTE: maybe this is genius, maybe it's fucking awful
                // we'll see
                args.push(Stmt {
                    kind: StmtKind::VarDecl(VarDecl {
                        name,
                        ty,
                        init_expr,
                    }),
                });

                if self.at(&TokenKind::Rparen) {
                    break;
                }

                self.expect(&TokenKind::Comma)?;
            }
        }
        self.next(); // `)`

        let return_ty = if let Ok(ty) = self.expect_ident() {
            ty.into()
        } else {
            Type::None
        };

        self.expect(&TokenKind::Lbrace)?;
        let body = self.parse()?;
        self.expect(&TokenKind::Rbrace)?;

        Ok(Stmt {
            kind: StmtKind::FnDecl {
                name,
                body,
                args,
                return_ty,
            },
        })
    }

    fn parse_while(&mut self) -> Result<Stmt> {
        self.next(); // `while` keyword

        let cond = self.parse_expr(0)?;
        self.expect(&TokenKind::Lbrace)?;
        let body = self.parse()?;
        self.expect(&TokenKind::Rbrace)?;

        Ok(Stmt {
            kind: StmtKind::While { cond, body },
        })
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr> {
        let mut lhs = self.parse_atom()?;

        while let Some((l_bp, r_bp)) = infix_binding_power(self.peek()) {
            if l_bp < min_bp {
                break;
            }

            let op = self.next().clone();
            match op.kind {
                // TODO: add more operators
                TokenKind::Assign
                | TokenKind::PlusAssign
                | TokenKind::MinusAssign
                | TokenKind::AsteriskAssign
                | TokenKind::SlashAssign => {
                    if !matches!(lhs.kind, ExprKind::Variable(_)) {
                        return Err(ParseError::InvalidLValue);
                    }

                    let rhs = self.parse_expr(r_bp)?;

                    lhs = Expr {
                        kind: ExprKind::Assign {
                            op: op.try_into().unwrap(),
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        ty: Type::default(),
                    }
                }
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Asterisk
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Eq
                | TokenKind::Ne
                | TokenKind::Le
                | TokenKind::Lt
                | TokenKind::Ge
                | TokenKind::Gt => {
                    let rhs = self.parse_expr(r_bp)?;
                    lhs = Expr {
                        kind: ExprKind::Binary(
                            op.try_into().unwrap(),
                            Box::new(lhs),
                            Box::new(rhs),
                        ),
                        ty: Type::default(),
                    };
                }
                _ => todo!(),
            }
        }

        Ok(lhs)
    }

    fn parse_atom(&mut self) -> Result<Expr> {
        let token = self.next().clone();

        match &token.kind {
            TokenKind::Literal { kind } => Ok(Expr {
                kind: ExprKind::Literal(kind.clone()),
                ty: Type::default(),
            }),
            TokenKind::Lparen => {
                let expr = self.parse_expr(0);
                self.expect(&TokenKind::Rparen)?;

                expr
            }
            TokenKind::Ident(name) => {
                if self.at(&TokenKind::Lparen) {
                    // actually a function call
                    self.next();

                    let mut args = vec![];
                    while !self.at(&TokenKind::Rparen) {
                        args.push(self.parse_expr(0)?);
                        if self.at(&TokenKind::Comma) {
                            self.next();
                        }
                    }
                    self.next();

                    return Ok(Expr {
                        kind: ExprKind::FnCall(name.clone(), args),
                        ty: Type::default(),
                    });
                }

                Ok(Expr {
                    kind: ExprKind::Variable(name.clone()),
                    ty: Type::default(),
                })
            }
            got => Err(ParseError::Unexpected {
                got: got.clone(),
                expected: "an atom".into(),
                span: token.span,
            }),
        }
    }
}

#[inline]
const fn infix_binding_power(op: &Token) -> Option<(u8, u8)> {
    // TODO: add more operators
    match op.kind {
        TokenKind::Assign
        | TokenKind::PlusAssign
        | TokenKind::MinusAssign
        | TokenKind::AsteriskAssign
        | TokenKind::SlashAssign => Some((2, 1)), // right-associative so you can do `a = b += c`
        TokenKind::Eq
        | TokenKind::Ne
        | TokenKind::Le
        | TokenKind::Lt
        | TokenKind::Ge
        | TokenKind::Gt => Some((3, 4)),
        TokenKind::Plus | TokenKind::Minus => Some((5, 6)),
        TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => Some((7, 8)),
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    Unexpected {
        got: TokenKind,
        expected: String,
        span: Span,
    },
    InvalidLValue,
}

impl std::fmt::Display for ParseError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unexpected {
                got,
                expected,
                span: _span,
            } => {
                write!(f, "unexpected {got:?}, expected {expected}")?;
            }
            Self::InvalidLValue => {
                write!(f, "invalid l-value")?;
            }
        };

        Ok(())
    }
}

impl std::error::Error for ParseError {}
