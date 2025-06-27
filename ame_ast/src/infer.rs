use ame_common::ScopeStack;
use ame_lexer::LiteralKind;
use ame_types::{unify, FloatKind, IntKind, Type, TypeCtx, TypeError};

use crate::{AssignOp, BinOp, Expr, ExprKind, Stmt, StmtKind, VarDecl};

type Result<T> = std::result::Result<T, TypeError>;

pub struct Inferrer<'a> {
    tcx: &'a mut TypeCtx,
    env: ScopeStack<String, Type>,
}

impl<'a> Inferrer<'a> {
    pub fn new(tcx: &'a mut TypeCtx) -> Self {
        Self {
            tcx,
            env: ScopeStack::new(),
        }
    }

    pub fn infer(&mut self, ast: &mut [Stmt]) -> Result<()> {
        for stmt in ast {
            self.infer_stmt(stmt)?;
        }

        Ok(())
    }

    pub fn resolve(&mut self, ast: &mut [Stmt]) {
        for stmt in ast {
            self.resolve_stmt(stmt);
        }
    }

    fn infer_stmt(&mut self, stmt: &mut Stmt) -> Result<()> {
        match &mut stmt.kind {
            StmtKind::VarDecl(decl) => {
                let inferred = if let Some(init) = &mut decl.init_expr {
                    self.infer_expr_type(init)?;
                    Some(init.ty.clone())
                } else {
                    None
                };

                if let Some(ty) = inferred {
                    if decl.ty == Type::Unknown {
                        decl.ty = ty.clone();
                    }

                    // FIXME: add handling for `let a: (u)int64 = 25`
                    // currently it errors out because can't unify (u)int64 and int32 (literals are
                    // int32 by default)
                    // ideally should work line in rust where integer type is inferred from use
                    unify(&decl.ty, &ty, self.tcx)?;
                    self.env.define(decl.name.clone(), ty.clone());
                }

                Ok(())
            }
            StmtKind::If {
                branches,
                else_body,
            } => {
                for (cond, body) in branches {
                    self.infer_expr_type(cond)?;
                    unify(&cond.ty, &Type::Bool, self.tcx)?;

                    self.env.enter();
                    for stmt in body {
                        self.infer_stmt(stmt)?;
                    }
                    self.env.exit();
                }

                if let Some(else_stmts) = else_body {
                    self.env.enter();
                    for stmt in else_stmts {
                        self.infer_stmt(stmt)?;
                    }
                    self.env.exit();
                }

                Ok(())
            }
            StmtKind::While { cond, body } => {
                self.infer_expr_type(cond)?;
                unify(&cond.ty, &Type::Bool, self.tcx)?;

                self.env.enter();
                for stmt in body {
                    self.infer_stmt(stmt)?;
                }
                self.env.exit();

                Ok(())
            }
            StmtKind::FnDecl {
                name,
                body,
                args,
                return_ty,
            } => {
                let mut arg_tys = vec![];

                self.env.enter();
                for arg in args {
                    self.infer_stmt(arg)?;

                    match &arg.kind {
                        StmtKind::VarDecl(VarDecl { name, ty, .. }) => {
                            arg_tys.push(ty.clone());
                            self.env.define(name.clone(), ty.clone());
                        }
                        _ => unreachable!("function arguments are always variable declarations"),
                    }
                }

                self.env.enter();

                let mut returns = vec![];
                for stmt in body {
                    self.infer_stmt(stmt)?;
                    if let StmtKind::Return(expr) = &stmt.kind {
                        returns.push(expr)
                    }
                }

                for expr in returns {
                    let ty = if let Some(expr) = expr {
                        expr.ty.resolve(self.tcx)
                    } else {
                        Type::None
                    };

                    unify(&ty, return_ty, self.tcx)?;
                }

                self.env.exit();
                self.env.exit();

                self.env
                    .define(name.clone(), Type::Fn(arg_tys, Box::new(return_ty.clone())));

                Ok(())
            }
            StmtKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.infer_expr_type(expr)?;
                }

                Ok(())
            }
            StmtKind::ExprStmt(expr) => {
                self.infer_expr_type(expr)?;

                Ok(())
            }
        }
    }

    fn infer_expr_type(&mut self, expr: &mut Expr) -> Result<Type> {
        let ty = match &mut expr.kind {
            ExprKind::Literal(lit) => Self::infer_literal_type(lit),
            ExprKind::Variable(name) => {
                if let Some(ty) = self.env.get(&*name) {
                    ty.clone()
                } else {
                    let ty = Type::Var(self.tcx.next_id());
                    self.env.define(name.clone(), ty.clone());

                    ty
                }
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let ltype = self.infer_expr_type(lhs)?;
                let rtype = self.infer_expr_type(rhs)?;

                match op {
                    BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Rem
                    | BinOp::BitAnd
                    | BinOp::BitXor
                    | BinOp::BitOr => {
                        unify(&ltype, &rtype, self.tcx)?;

                        ltype
                    }
                    BinOp::Shl | BinOp::Shr => {
                        // TODO: add error for this or modify `TypeError::CannotUnify`
                        match rtype {
                            Type::Int(_) => {}
                            _ => panic!("you can shl/shr only by integer values"),
                        }

                        ltype
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Ge | BinOp::Lt | BinOp::Le => {
                        unify(&ltype, &rtype, self.tcx)?;

                        Type::Bool
                    }
                    BinOp::And | BinOp::Or => {
                        unify(&ltype, &Type::Bool, self.tcx)?;
                        unify(&rtype, &Type::Bool, self.tcx)?;

                        Type::Bool
                    }
                }
            }
            ExprKind::Assign { op, lhs, rhs } => {
                let ltype = self.infer_expr_type(lhs)?;
                let rtype = self.infer_expr_type(rhs)?;

                match op {
                    AssignOp::Add
                    | AssignOp::Sub
                    | AssignOp::Mul
                    | AssignOp::Div
                    | AssignOp::Rem
                    | AssignOp::BitAnd
                    | AssignOp::BitXor
                    | AssignOp::BitOr => {
                        unify(&ltype, &rtype, self.tcx)?;

                        ltype
                    }
                    AssignOp::Shl | AssignOp::Shr => {
                        // TODO: add error for this or modify `TypeError::CannotUnify`
                        match rtype {
                            Type::Int(_) => {}
                            _ => panic!("you can shl/shr only by integer values"),
                        }

                        ltype
                    }
                    AssignOp::Assign => {
                        unify(&ltype, &rtype, self.tcx)?;

                        ltype
                    }
                }
            }
        };

        expr.ty = ty.resolve(self.tcx);
        Ok(ty)
    }

    fn infer_literal_type(lit: &LiteralKind) -> Type {
        match lit {
            LiteralKind::Int { .. } => Type::Int(IntKind::default()),
            // LiteralKind::Bool(_) => Type::Bool,
            LiteralKind::Float { .. } => Type::Float(FloatKind::default()),
            LiteralKind::String { .. } => Type::String,
        }
    }

    fn resolve_stmt(&self, stmt: &mut Stmt) {
        match &mut stmt.kind {
            StmtKind::VarDecl(var_decl) => {
                if let Some(expr) = &mut var_decl.init_expr {
                    self.resolve_expr(expr);
                }

                if var_decl.ty == Type::Unknown {
                    var_decl.ty = self
                        .env
                        .get(&var_decl.name)
                        .cloned()
                        .expect("by now all types must be known");
                }

                var_decl.ty.resolve(self.tcx);
            }
            StmtKind::ExprStmt(expr) => self.resolve_expr(expr),
            StmtKind::If {
                branches,
                else_body,
            } => {
                for (cond, body) in branches {
                    self.resolve_expr(cond);
                    for stmt in body {
                        self.resolve_stmt(stmt);
                    }
                }
                if let Some(body) = else_body {
                    for stmt in body {
                        self.resolve_stmt(stmt);
                    }
                }
            }
            StmtKind::While { cond, body } => {
                self.resolve_expr(cond);
                for stmt in body {
                    self.resolve_stmt(stmt);
                }
            }
            StmtKind::FnDecl {
                name,
                args,
                body,
                return_ty: _,
            } => {
                for arg in args {
                    self.resolve_stmt(arg);
                }

                for stmt in body {
                    self.resolve_stmt(stmt);
                }

                self.env
                    .get(name)
                    .expect("fn should be defined probably")
                    .resolve(self.tcx);
            }
            StmtKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr);
                }
            }
        }
    }

    fn resolve_expr(&self, expr: &mut Expr) {
        expr.ty = expr.ty.resolve(self.tcx);

        match &mut expr.kind {
            ExprKind::Binary(_, lhs, rhs) => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }
            ExprKind::Assign { lhs, rhs, .. } => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }
            _ => {}
        }
    }
}
