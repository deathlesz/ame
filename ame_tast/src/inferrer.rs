use ame_ast::{AssignOp, BinOp, Expr, ExprKind, Stmt, StmtKind};
use ame_common::ScopeStack;
use ame_lexer::LiteralKind;
use ame_types::{unify, Type, TypeCtx, TypeError};

use crate::{TypedExpr, TypedExprKind, TypedStmt, TypedStmtKind};

type Result<T> = std::result::Result<T, TypeError>;

pub struct Inferrer<'a> {
    tcx: &'a mut TypeCtx,
    env: ScopeStack<String, Type>,

    current_return_ty: Option<Type>,
}

impl<'a> Inferrer<'a> {
    pub fn new(tcx: &'a mut TypeCtx) -> Self {
        Self {
            tcx,
            env: ScopeStack::new(),

            current_return_ty: None,
        }
    }

    pub fn infer(&mut self, ast: &[Stmt]) -> Result<Vec<TypedStmt>> {
        self.define_functions(ast)?;

        let mut typed_ast = Vec::with_capacity(ast.len());
        for stmt in ast {
            typed_ast.push(self.infer_stmt(stmt)?)
        }

        Ok(typed_ast)
    }

    fn define_functions(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            if let StmtKind::FnDecl {
                name,
                args,
                return_ty,
                is_variadic,
                ..
            } = &stmt.kind
            {
                let mut arg_tys = Vec::with_capacity(args.len());
                for arg in args {
                    if let StmtKind::VarDecl { ty, .. } = &arg.kind {
                        arg_tys.push(
                            ty.as_ref()
                                .expect("type must be present in function argument")
                                .into(),
                        );
                    }
                }

                let fn_ty = Type::Fn {
                    args: arg_tys,
                    return_ty: Box::new(
                        return_ty.as_ref().map(|ty| ty.into()).unwrap_or(Type::None),
                    ),
                    is_variadic: *is_variadic,
                };

                self.env.define(name.clone(), fn_ty);
            }
        }

        Ok(())
    }

    fn infer_stmt(&mut self, stmt: &Stmt) -> Result<TypedStmt> {
        let kind = match &stmt.kind {
            StmtKind::VarDecl {
                name,
                ty,
                init_expr,
            } => {
                let typed_init_expr = init_expr
                    .as_ref()
                    .map(|expr| self.infer_expr(expr))
                    .transpose()?;

                let inferred_ty = if let Some(init) = &typed_init_expr {
                    init.ty.clone()
                } else {
                    Type::var(self.tcx)
                };

                let ty = ty
                    .as_ref()
                    .map(|ty| ty.into())
                    .unwrap_or_else(|| inferred_ty.clone());

                unify(&ty, &inferred_ty, self.tcx)?;
                self.env.define(name.clone(), ty.resolve(self.tcx));

                TypedStmtKind::VarDecl {
                    name: name.clone(),
                    ty,
                    init_expr: typed_init_expr,
                }
            }
            StmtKind::FnDecl {
                name,
                args,
                body,
                return_ty,
                is_extern,
                is_variadic,
            } => {
                self.env.enter();

                let mut typed_args = Vec::with_capacity(args.len());
                for arg in args {
                    if let StmtKind::VarDecl { name, ty, .. } = &arg.kind {
                        self.env.define(
                            name.clone(),
                            ty.as_ref()
                                .expect("type must be present in function argument")
                                .into(),
                        );
                        typed_args.push(self.infer_stmt(arg)?);
                    }
                }

                let return_ty = return_ty.as_ref().map(|ty| ty.into()).unwrap_or(Type::None);
                let typed_body = if let Some(stmts) = body {
                    let prev_return_ty = self.current_return_ty.replace(return_ty.clone());

                    let typed_stmts = self.infer(stmts)?;

                    self.current_return_ty = prev_return_ty;
                    Some(typed_stmts)
                } else {
                    None
                };

                self.env.exit();

                TypedStmtKind::FnDecl {
                    name: name.clone(),
                    args: typed_args,
                    body: typed_body,
                    return_ty,
                    is_extern: *is_extern,
                    is_variadic: *is_variadic,
                }
            }
            StmtKind::Return(expr) => {
                let typed_expr = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(expr))
                    .transpose()?;
                let return_ty = typed_expr
                    .as_ref()
                    .map(|expr| expr.ty.clone())
                    .unwrap_or(Type::None);

                if let Some(expected) = &self.current_return_ty {
                    unify(expected, &return_ty, self.tcx)?;
                }

                TypedStmtKind::Return(typed_expr)
            }
            StmtKind::If {
                branches,
                else_body,
            } => {
                let mut typed_branches = Vec::with_capacity(branches.len());

                for (cond, body) in branches {
                    let typed_cond = self.infer_expr(cond)?;
                    unify(&typed_cond.ty, &Type::Bool, self.tcx)?;

                    self.env.enter();
                    let typed_body = self.infer(body)?;
                    self.env.exit();

                    typed_branches.push((typed_cond, typed_body));
                }

                let typed_else_body = else_body
                    .as_ref()
                    .map(|stmts| {
                        self.env.enter();
                        let typed_body = self.infer(stmts);
                        self.env.exit();

                        typed_body
                    })
                    .transpose()?;

                TypedStmtKind::If {
                    branches: typed_branches,
                    else_body: typed_else_body,
                }
            }
            StmtKind::While { cond, body } => {
                let typed_cond = self.infer_expr(cond)?;

                self.env.enter();
                let typed_body = self.infer(body)?;
                self.env.exit();

                TypedStmtKind::While {
                    cond: typed_cond,
                    body: typed_body,
                }
            }
            StmtKind::ExprStmt(expr) => TypedStmtKind::ExprStmt(self.infer_expr(expr)?),
        };

        Ok(TypedStmt { kind })
    }

    fn infer_expr(&mut self, expr: &Expr) -> Result<TypedExpr> {
        let (kind, ty) = match &expr.kind {
            ExprKind::Literal(kind) => {
                let ty = match kind {
                    LiteralKind::Int { .. } => Type::var_int(self.tcx),
                    LiteralKind::Float { .. } => Type::var_float(self.tcx),
                    LiteralKind::String { .. } => Type::String,
                };

                (TypedExprKind::Literal(kind.clone()), ty)
            }
            ExprKind::Variable(name) => {
                if let Some(ty) = self.env.get(name) {
                    (TypedExprKind::Variable(name.clone()), ty.clone())
                } else {
                    panic!("undefined variable")
                }
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let typed_lhs = self.infer_expr(lhs)?;
                let typed_rhs = self.infer_expr(rhs)?;

                let result_ty = match op {
                    BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Rem
                    | BinOp::BitAnd
                    | BinOp::BitXor
                    | BinOp::BitOr => {
                        unify(&typed_lhs.ty, &typed_rhs.ty, self.tcx)?;

                        typed_lhs.ty.clone()
                    }
                    BinOp::Shl | BinOp::Shr => {
                        // TODO: add error for this or modify `TypeError::CannotUnify`
                        match &typed_lhs.ty {
                            Type::Int(_) => {}
                            _ => panic!("you can shl/shr only by integer values"),
                        }

                        typed_lhs.ty.clone()
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Ge | BinOp::Lt | BinOp::Le => {
                        unify(&typed_lhs.ty, &typed_rhs.ty, self.tcx)?;

                        Type::Bool
                    }
                    BinOp::And | BinOp::Or => {
                        unify(&typed_lhs.ty, &Type::Bool, self.tcx)?;
                        unify(&typed_rhs.ty, &Type::Bool, self.tcx)?;

                        Type::Bool
                    }
                };

                (
                    TypedExprKind::Binary(*op, Box::new(typed_lhs), Box::new(typed_rhs)),
                    result_ty,
                )
            }
            ExprKind::Assign(op, lhs, rhs) => {
                let typed_lhs = self.infer_expr(lhs)?;
                let typed_rhs = self.infer_expr(rhs)?;

                let result_ty = match op {
                    AssignOp::Add
                    | AssignOp::Sub
                    | AssignOp::Mul
                    | AssignOp::Div
                    | AssignOp::Rem
                    | AssignOp::BitAnd
                    | AssignOp::BitXor
                    | AssignOp::BitOr => {
                        unify(&typed_lhs.ty, &typed_rhs.ty, self.tcx)?;

                        typed_lhs.ty.clone()
                    }
                    AssignOp::Shl | AssignOp::Shr => {
                        // TODO: add error for this or modify `TypeError::CannotUnify`
                        match &typed_lhs.ty {
                            Type::Int(_) => {}
                            _ => panic!("you can shl/shr only by integer values"),
                        }

                        typed_lhs.ty.clone()
                    }
                    AssignOp::Assign => {
                        unify(&typed_lhs.ty, &typed_rhs.ty, self.tcx)?;

                        typed_lhs.ty.clone()
                    }
                };

                (
                    TypedExprKind::Assign(*op, Box::new(typed_lhs), Box::new(typed_rhs)),
                    result_ty,
                )
            }
            ExprKind::FnCall(name, args) => {
                if let Some(ty) = self.env.get(name).cloned() {
                    match ty {
                        Type::Fn {
                            args: arg_tys,
                            return_ty: ret_ty,
                            ..
                        } => {
                            let mut typed_args = Vec::with_capacity(args.len());

                            // required to still infer types of variadic arguments, but not unify them
                            for (arg, arg_ty) in args
                                .iter()
                                .zip(arg_tys.iter().map(Some).chain(std::iter::repeat(None)))
                            {
                                let typed_arg = self.infer_expr(arg)?;

                                if let Some(arg_ty) = arg_ty {
                                    unify(&typed_arg.ty, arg_ty, self.tcx)?;
                                }

                                typed_args.push(typed_arg);
                            }

                            (TypedExprKind::FnCall(name.clone(), typed_args), *ret_ty)
                        }
                        _ => unreachable!("function type is Fn, duh"),
                    }
                } else {
                    panic!("function `{name}` is not defined");
                }
            }
        };

        Ok(TypedExpr {
            kind,
            ty: ty.resolve(self.tcx),
        })
    }
}
