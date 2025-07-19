use ame_ast::{Expr, ExprKind, Stmt, StmtKind};
use ame_lexer::{Keyword, Span, Token, TokenKind};

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
        let peek = self.peek().clone();

        match peek.kind {
            TokenKind::Ident(name) => {
                self.next();

                Ok(name)
            }
            _ => Err(ParseError::Unexpected {
                got: peek.kind,
                expected: "ident".into(),
                span: peek.span,
            }),
        }
    }

    fn try_parse_ty(&mut self) -> Result<Option<String>> {
        let is_ref = if self.at(&TokenKind::Amp) {
            self.next();

            true
        } else {
            false
        };

        if !matches!(self.peek().kind, TokenKind::Ident(_)) {
            if is_ref {
                let token = self.peek().clone();

                return Err(ParseError::Unexpected {
                    got: TokenKind::Amp,
                    expected: "a type".into(),
                    span: token.span,
                });
            } else {
                return Ok(None);
            }
        }

        let mut ident = self.expect_ident()?;
        if is_ref {
            ident.insert(0, '&');
        }

        Ok(Some(ident))
    }

    fn expect_ty(&mut self) -> Result<String> {
        let is_ref = if self.at(&TokenKind::Amp) {
            self.next();

            true
        } else {
            false
        };

        let mut ident = self.expect_ident()?;
        if is_ref {
            ident.insert(0, '&');
        }

        Ok(ident)
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
                Keyword::Extern => Err(ParseError::Unexpected {
                    got: kw.clone(),
                    expected: "fn(extern)".into(),
                    span: token.span,
                }),
                Keyword::For => Ok(Some(self.parse_for()?)),
                Keyword::Class => Ok(Some(self.parse_class_decl()?)),
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
                Some(self.expect_ty()?)
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
                kind: StmtKind::VarDecl {
                    name,
                    ty,
                    init_expr,
                },
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

        let is_extern = if self.at(&TokenKind::Lparen) {
            self.next();
            self.expect(&TokenKind::Keyword(Keyword::Extern))?;
            self.expect(&TokenKind::Rparen)?;

            true
        } else {
            false
        };

        let name = self.expect_ident()?;

        let mut args = vec![];
        let mut is_variadic = false;
        self.expect(&TokenKind::Lparen)?;
        if !self.at(&TokenKind::Rparen) {
            loop {
                if is_extern && self.at(&TokenKind::ThreeDot) {
                    self.next();
                    is_variadic = true;

                    break;
                }

                let name = self.expect_ident()?;
                self.expect(&TokenKind::Colon)?;
                let ty = self.expect_ty()?;

                let with_init = self.expect(&TokenKind::Assign).is_ok();
                let init_expr = if with_init {
                    Some(self.parse_expr(0)?)
                } else {
                    None
                };

                // NOTE: maybe this is genius, maybe it's fucking awful
                // we'll see
                args.push(Stmt {
                    kind: StmtKind::VarDecl {
                        name,
                        ty: Some(ty),
                        init_expr,
                    },
                });

                if self.at(&TokenKind::Rparen) {
                    break;
                }

                self.expect(&TokenKind::Comma)?;
            }
        }
        self.next(); // `)`

        // will break for, e.g. fn add(a: int32, b: int32) & { ... }
        let return_ty = self.try_parse_ty()?;

        let body = if self.at(&TokenKind::Lbrace) {
            self.next();
            let body = self.parse()?;
            self.expect(&TokenKind::Rbrace)?;

            Some(body)
        } else if is_extern {
            self.expect(&TokenKind::Semicolon)?;

            None
        } else {
            panic!("non-extern fns without body aren't allowed")
        };

        Ok(Stmt {
            kind: StmtKind::FnDecl {
                name,
                args,
                body,
                return_ty,
                is_extern,
                is_variadic,
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

    fn parse_for(&mut self) -> Result<Stmt> {
        self.next(); // `for` keyword

        let init = if self.at(&TokenKind::Keyword(Keyword::Let)) {
            Some(self.parse_let()?)
        } else if self.at(&TokenKind::Semicolon) {
            self.next();

            None
        } else {
            let expr = self.parse_expr(0)?;
            self.expect(&TokenKind::Semicolon)?;

            Some(vec![expr.into_stmt()])
        };

        let cond = if !self.at(&TokenKind::Semicolon) {
            Some(self.parse_expr(0)?)
        } else {
            None
        };
        self.expect(&TokenKind::Semicolon)?;

        let action = if !self.at(&TokenKind::Lbrace) {
            Some(self.parse_expr(0)?)
        } else {
            None
        };

        self.expect(&TokenKind::Lbrace)?;
        let body = self.parse()?;
        self.expect(&TokenKind::Rbrace)?;

        Ok(Stmt {
            kind: StmtKind::For {
                init: init.map(Box::new),
                cond,
                action,
                body,
            },
        })
    }

    fn parse_class_decl(&mut self) -> Result<Stmt> {
        self.next(); // `class` keyword

        let name = self.expect_ident()?;
        self.expect(&TokenKind::Lbrace)?;

        let mut fields = vec![];
        if !self.at(&TokenKind::Rbrace) {
            loop {
                let name = self.expect_ident()?;
                self.expect(&TokenKind::Colon)?;
                let ty = self.expect_ty()?;

                fields.push((name, ty));

                if self.at(&TokenKind::Rbrace) {
                    self.next();
                    break;
                }

                self.expect(&TokenKind::Comma)?;
            }
        }

        Ok(Stmt {
            kind: StmtKind::ClassDecl { name, fields },
        })
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr> {
        let mut lhs = self.parse_atom()?;

        while let Some((l_bp, r_bp)) = infix_binding_power(self.peek()) {
            if l_bp < min_bp {
                break;
            }

            let op = self.next().clone();
            match &op.kind {
                kind if kind.is_assign() => {
                    if !matches!(lhs.kind, ExprKind::Variable(_)) {
                        return Err(ParseError::InvalidLValue);
                    }

                    let rhs = self.parse_expr(r_bp)?;

                    lhs = Expr {
                        kind: ExprKind::Assign(
                            op.try_into().unwrap(),
                            Box::new(lhs),
                            Box::new(rhs),
                        ),
                    }
                }
                kind if kind.is_binary() => {
                    let rhs = self.parse_expr(r_bp)?;

                    lhs = Expr {
                        kind: ExprKind::Binary(
                            op.try_into().unwrap(),
                            Box::new(lhs),
                            Box::new(rhs),
                        ),
                    };
                }
                t => panic!("unhandled token in expr: {t:?}"),
            }
        }

        Ok(lhs)
    }

    fn parse_atom(&mut self) -> Result<Expr> {
        let token = self.next().clone();

        match &token.kind {
            TokenKind::Literal { kind } => Ok(Expr {
                kind: ExprKind::Literal(kind.clone()),
            }),
            TokenKind::Lparen => {
                if let Some(ty) = self.try_parse_ty()? {
                    self.expect(&TokenKind::Rparen)?;

                    let expr = self.parse_expr(0)?;
                    Ok(Expr {
                        kind: ExprKind::Cast(ty, Box::new(expr)),
                    })
                } else {
                    let expr = self.parse_expr(0);
                    self.expect(&TokenKind::Rparen)?;

                    expr
                }
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
                    });
                } else if self.at(&TokenKind::Lbrace) {
                    // a class instantiation
                    self.next();

                    let mut fields = vec![];
                    if !self.at(&TokenKind::Rbrace) {
                        loop {
                            let name = self.expect_ident()?;
                            self.expect(&TokenKind::Colon)?;
                            let value = self.parse_expr(0)?;

                            fields.push((name, value));

                            if self.at(&TokenKind::Rbrace) {
                                self.next();
                                break;
                            }

                            self.expect(&TokenKind::Comma)?;
                        }
                    }

                    return Ok(Expr {
                        kind: ExprKind::ClassInst(name.clone(), fields),
                    });
                }

                Ok(Expr {
                    kind: ExprKind::Variable(name.clone()),
                })
            }
            kind if kind.is_unary() => {
                let expr = self.parse_atom()?;

                Ok(Expr {
                    kind: ExprKind::Unary(token.try_into().unwrap(), Box::new(expr)),
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
pub const fn infix_binding_power(op: &Token) -> Option<(u8, u8)> {
    use TokenKind::*;

    Some(match op.kind {
        // assignment operators (right-associative)
        Assign | PlusAssign | MinusAssign | AsteriskAssign | SlashAssign | PercentAssign
        | AmpAssign | PipeAssign | CaretAssign | ShlAssign | ShrAssign => (1, 0),

        Or => (2, 3),
        And => (4, 5),

        Pipe => (6, 7),
        Caret => (8, 9),
        Amp => (10, 11),

        Eq | Ne | Lt | Le | Gt | Ge => (12, 13),

        Plus | Minus => (14, 15),

        Shl | Shr => (16, 17),

        Asterisk | Slash | Percent => (18, 19),

        _ => return None,
    })
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
