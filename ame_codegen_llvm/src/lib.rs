use std::{collections::HashMap, path::PathBuf};

pub use inkwell::context::Context;
use inkwell::{
    builder::Builder,
    module::{Linkage, Module},
    targets::{Target, TargetMachine, TargetTriple},
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue},
    AddressSpace,
};

use ame_common::ScopeStack;
use ame_lexer::LiteralKind;
use ame_tast::{
    AssignOp, BinOp, TypedAst, TypedExprId, TypedExprKind, TypedStmtId, TypedStmtKind, UnaryOp,
};
use ame_types::{Constraint, Type, TypeCtx};

mod options;
pub use options::*;
mod ty;

use crate::ty::{AsFloatPredicate, AsIntPredicate, AsLLVMType};

pub struct CodeGen<'a, 'ctx> {
    typed_ast: &'a TypedAst,
    nodes: &'a [TypedStmtId],
    tcx: TypeCtx,

    locals: ScopeStack<&'a String, BasicValueEnum<'ctx>>,
    fns: ScopeStack<&'a String, FunctionValue<'ctx>>,
    strings: HashMap<String, GlobalValue<'ctx>>,

    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    current_func: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    #[inline]
    pub fn new(
        typed_ast: &'a TypedAst,
        nodes: &'a [TypedStmtId],
        tcx: TypeCtx,

        context: &'ctx Context,
        module: Module<'ctx>,
        builder: Builder<'ctx>,
    ) -> Self {
        Self {
            typed_ast,
            nodes,
            tcx,

            locals: ScopeStack::new(),
            fns: ScopeStack::new(),
            strings: HashMap::new(),

            context,
            module,
            builder,

            current_func: None,
        }
    }

    pub fn generate(&mut self, options: CodeGenOptions) {
        self.generate_stmts(self.nodes);
        self.module.verify().expect("failed to verify");

        Target::initialize_all(&inkwell::targets::InitializationConfig::default());

        let target_triple = options
            .target
            .as_ref()
            .map(|target| TargetTriple::create(target))
            .unwrap_or_else(TargetMachine::get_default_triple);

        let cpu = options
            .target
            .unwrap_or_else(|| TargetMachine::get_host_cpu_name().to_string());

        let features = options
            .features
            .map(|features| {
                if features == "native" {
                    TargetMachine::get_host_cpu_features().to_string()
                } else {
                    features
                }
            })
            .unwrap_or_else(|| "+cmov,+cx8,+fxsr,+mmx,+sse,+sse2".into());

        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                &cpu,
                &features,
                if options.optimize {
                    inkwell::OptimizationLevel::Aggressive
                } else {
                    inkwell::OptimizationLevel::None
                },
                inkwell::targets::RelocMode::PIC, // NOTE: needs -fPIE if using ::Default
                inkwell::targets::CodeModel::Default,
            )
            .unwrap();

        self.module.set_triple(&target_triple);
        self.module
            .set_data_layout(&target_machine.get_target_data().get_data_layout());

        if options.optimize {
            self.module.run_passes("annotation2metadata,forceattrs,inferattrs,coro-early,function<eager-inv>(ee-instrument<>,lower-expect,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;no-switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-hoist-loads-stores-with-cond-faulting;no-sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,sroa<modify-cfg>,early-cse<>,callsite-splitting),openmp-opt,ipsccp,called-value-propagation,globalopt,function<eager-inv>(mem2reg,instcombine<max-iterations=1;no-verify-fixpoint>,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-hoist-loads-stores-with-cond-faulting;no-sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>),always-inline,require<globals-aa>,function(invalidate<aa>),require<profile-summary>,cgscc(devirt<4>(inline,function-attrs<skip-non-recursive-function-attrs>,argpromotion,openmp-opt-cgscc,function<eager-inv;no-rerun>(sroa<modify-cfg>,early-cse<memssa>,speculative-execution<only-if-divergent-target>,jump-threading,correlated-propagation,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-hoist-loads-stores-with-cond-faulting;no-sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,instcombine<max-iterations=1;no-verify-fixpoint>,aggressive-instcombine,libcalls-shrinkwrap,tailcallelim,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-hoist-loads-stores-with-cond-faulting;no-sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,reassociate,constraint-elimination,loop-mssa(loop-instsimplify,loop-simplifycfg,licm<no-allowspeculation>,loop-rotate<header-duplication;no-prepare-for-lto>,licm<allowspeculation>,simple-loop-unswitch<nontrivial;trivial>),simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-hoist-loads-stores-with-cond-faulting;no-sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,instcombine<max-iterations=1;no-verify-fixpoint>,loop(loop-idiom,indvars,extra-simple-loop-unswitch-passes,loop-deletion,loop-unroll-full),sroa<modify-cfg>,vector-combine,mldst-motion<no-split-footer-bb>,gvn<>,sccp,bdce,instcombine<max-iterations=1;no-verify-fixpoint>,jump-threading,correlated-propagation,adce,memcpyopt,dse,move-auto-init,loop-mssa(licm<allowspeculation>),coro-elide,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;hoist-common-insts;no-hoist-loads-stores-with-cond-faulting;sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,instcombine<max-iterations=1;no-verify-fixpoint>),function-attrs,function(require<should-not-run-function-passes>),coro-split,coro-annotation-elide)),deadargelim,coro-cleanup,globalopt,globaldce,elim-avail-extern,rpo-function-attrs,recompute-globalsaa,function<eager-inv>(float2int,lower-constant-intrinsics,chr,loop(loop-rotate<header-duplication;no-prepare-for-lto>,loop-deletion),loop-distribute,inject-tli-mappings,loop-vectorize<no-interleave-forced-only;no-vectorize-forced-only;>,infer-alignment,loop-load-elim,instcombine<max-iterations=1;no-verify-fixpoint>,simplifycfg<bonus-inst-threshold=1;forward-switch-cond;switch-range-to-icmp;switch-to-lookup;no-keep-loops;hoist-common-insts;no-hoist-loads-stores-with-cond-faulting;sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,slp-vectorizer,vector-combine,instcombine<max-iterations=1;no-verify-fixpoint>,loop-unroll<O3>,transform-warning,sroa<preserve-cfg>,infer-alignment,instcombine<max-iterations=1;no-verify-fixpoint>,loop-mssa(licm<allowspeculation>),alignment-from-assumptions,loop-sink,instsimplify,div-rem-pairs,tailcallelim,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;hoist-loads-stores-with-cond-faulting;no-sink-common-insts;speculate-blocks;simplify-cond-branch;speculate-unpredictables>),globaldce,constmerge,cg-profile,rel-lookup-table-converter,function(annotation-remarks),verify", &target_machine, inkwell::passes::PassBuilderOptions::create()).expect("failed to run passes");
        }

        let output = options.output.unwrap_or_else(|| {
            "out"
                .parse::<PathBuf>()
                .unwrap()
                .with_extension(options.output_kind.extension())
        });
        match options.output_kind {
            OutputKind::Object => {
                target_machine
                    .write_to_file(&self.module, inkwell::targets::FileType::Object, &output)
                    .unwrap();
            }
            OutputKind::LLVMIr => {
                std::fs::write(output, self.module.to_string()).unwrap();
            }
            OutputKind::Assembly => {
                target_machine
                    .write_to_file(&self.module, inkwell::targets::FileType::Assembly, &output)
                    .unwrap();
            }
        }
    }

    fn generate_stmts(&mut self, stmts: &'a [TypedStmtId]) -> bool {
        self.generate_fns(stmts);

        let mut returned = false;
        for stmt in stmts {
            let stmt = &self.typed_ast[*stmt];

            match stmt.kind() {
                TypedStmtKind::VarDecl {
                    name,
                    ty,
                    init_expr,
                } => {
                    if self.current_func.is_none() {
                        panic!("no current function")
                    }

                    let var = self
                        .builder
                        .build_alloca(
                            std::convert::TryInto::<BasicTypeEnum>::try_into(
                                ty.resolve(&self.tcx).as_llvm_type(self.context),
                            )
                            .unwrap(),
                            name,
                        )
                        .unwrap();

                    if let Some(init_expr) = init_expr {
                        let expr = self.generate_nonvoid_expr(*init_expr);

                        self.builder.build_store(var, expr).unwrap();
                    }

                    self.locals.define(name, var.as_basic_value_enum());
                }
                TypedStmtKind::If {
                    branches,
                    else_body,
                } => {
                    if let Some(func) = self.current_func {
                        self.generate_if(func, branches, else_body)
                    } else {
                        panic!("no current function")
                    }
                }
                TypedStmtKind::While { cond, body } => {
                    if let Some(func) = self.current_func {
                        let cond_generated = self.generate_nonvoid_expr(*cond);
                        let cmp = cond_generated.into_int_value();

                        let do_block = self.context.append_basic_block(func, "do");
                        let whilecont_block = self.context.append_basic_block(func, "whilecont");

                        self.builder
                            .build_conditional_branch(cmp, do_block, whilecont_block)
                            .unwrap();

                        self.builder.position_at_end(do_block);

                        self.locals.enter();
                        self.fns.enter();
                        let returned = self.generate_stmts(body);
                        self.fns.exit();
                        self.locals.exit();

                        if !returned {
                            let cond_generated = self.generate_nonvoid_expr(*cond);
                            let cmp = cond_generated.into_int_value();

                            self.builder
                                .build_conditional_branch(cmp, do_block, whilecont_block)
                                .unwrap();
                        }

                        self.builder.position_at_end(whilecont_block);
                    } else {
                        panic!("no current function")
                    }
                }
                TypedStmtKind::FnDecl {
                    name, args, body, ..
                } => {
                    if let Some(body) = body {
                        let arg_names =
                            args.iter().map(|stmt| match self.typed_ast[*stmt].kind() {
                                TypedStmtKind::VarDecl { name, .. } => name,
                                _ => {
                                    unreachable!(
                                        "function arguments are always variable declarations"
                                    )
                                }
                            });

                        // will never panic because it was pregenerated earlier
                        let func = *self.fns.get(name).unwrap();

                        let prev_func = self.current_func;
                        let prev_bb = self.builder.get_insert_block();
                        self.current_func = Some(func);

                        let bb = self.context.append_basic_block(func, name);
                        self.builder.position_at_end(bb);

                        self.locals.enter();
                        self.fns.enter();
                        for (arg, name) in func.get_params().into_iter().zip(arg_names) {
                            let ptr = self.builder.build_alloca(arg.get_type(), name).unwrap();
                            self.builder.build_store(ptr, arg).unwrap();

                            self.locals.define(name, ptr.as_basic_value_enum());
                        }

                        self.locals.enter();
                        self.fns.enter();
                        self.generate_stmts(body);

                        self.fns.exit();
                        self.locals.exit();
                        self.fns.exit();
                        self.locals.exit();

                        self.current_func = prev_func;
                        if let Some(prev_bb) = prev_bb {
                            self.builder.position_at_end(prev_bb);
                        }
                    }
                }
                TypedStmtKind::Return(expr) => {
                    if self.current_func.is_some() {
                        let value = expr.as_ref().and_then(|expr| self.generate_expr(*expr));

                        self.builder
                            .build_return(value.as_ref().map(|v| v as &dyn BasicValue<'ctx>))
                            .unwrap();

                        returned = true;
                    } else {
                        panic!("no current function")
                    }
                }
                TypedStmtKind::For {
                    init,
                    cond,
                    action,
                    body,
                } => {
                    if let Some(func) = self.current_func {
                        let forbody = self.context.append_basic_block(func, "forbody");
                        let forcont = self.context.append_basic_block(func, "forcont");

                        self.locals.enter();
                        self.fns.enter();

                        if let Some(stmts) = init {
                            self.generate_stmts(stmts);
                        }

                        if let Some(expr) = cond {
                            let cond = self.generate_nonvoid_expr(*expr).into_int_value();

                            self.builder
                                .build_conditional_branch(cond, forbody, forcont)
                                .unwrap();
                        } else {
                            self.builder.build_unconditional_branch(forbody).unwrap();
                        }

                        self.builder.position_at_end(forbody);
                        let returned = self.generate_stmts(body);

                        if !returned {
                            if let Some(expr) = action {
                                self.generate_expr(*expr);
                            }

                            if let Some(expr) = cond {
                                let cond = self.generate_nonvoid_expr(*expr).into_int_value();

                                self.builder
                                    .build_conditional_branch(cond, forbody, forcont)
                                    .unwrap();
                            } else {
                                self.builder.build_unconditional_branch(forbody).unwrap();
                            }
                        }

                        self.fns.exit();
                        self.locals.exit();

                        self.builder.position_at_end(forcont);
                    } else {
                        panic!("no current function")
                    }
                }
                TypedStmtKind::ExprStmt(expr) => {
                    _ = self.generate_expr(*expr); // we don't care if it's void
                }
            }
        }

        returned
    }

    fn generate_fns(&mut self, stmts: &'a [TypedStmtId]) {
        for stmt in stmts {
            let stmt = &self.typed_ast[*stmt];

            if let TypedStmtKind::FnDecl {
                name,
                args,
                return_ty,
                is_extern,
                is_variadic,
                ..
            } = stmt.kind()
            {
                // NOTE: this monstrosity is required because .unzip() requires Default
                // which is not implemented for BasicMetadataTypeEnum
                let (arg_names, arg_types) = args.iter().fold(
                    (
                        Vec::with_capacity(args.len()),
                        Vec::with_capacity(args.len()),
                    ),
                    |(mut names, mut types), stmt| match self.typed_ast[*stmt].kind() {
                        TypedStmtKind::VarDecl { name, ty, .. } => {
                            names.push(name);
                            types.push(
                                ty.resolve(&self.tcx)
                                    .as_llvm_type(self.context)
                                    .try_into()
                                    .unwrap(),
                            );

                            (names, types)
                        }
                        _ => {
                            unreachable!("function arguments are always variable declarations")
                        }
                    },
                );

                let ret_ty = return_ty.resolve(&self.tcx).as_llvm_type(self.context);
                let fn_type =
                    if let Ok(ty) = std::convert::TryInto::<BasicTypeEnum>::try_into(ret_ty) {
                        ty.fn_type(&arg_types, *is_variadic)
                    } else {
                        ret_ty.into_void_type().fn_type(&arg_types, *is_variadic)
                    };

                let func = self.module.add_function(
                    name,
                    fn_type,
                    Some(if *is_extern {
                        Linkage::External
                    } else {
                        Linkage::Private
                    }),
                );

                for (arg, name) in func.get_params().iter().zip(arg_names) {
                    arg.set_name(name);
                }

                self.fns.define(name, func);
            }
        }
    }

    fn generate_if(
        &mut self,
        func: FunctionValue<'ctx>,
        branches: &'a [(TypedExprId, Vec<TypedStmtId>)],
        else_body: &'a Option<Vec<TypedStmtId>>,
    ) {
        let Some(((cond, body), rest)) = branches.split_last() else {
            return;
        };

        let last_ifcont_block = self.context.append_basic_block(func, "ifcont");

        for (cond, body) in rest {
            let cond = self.generate_nonvoid_expr(*cond);
            let cmp = cond.into_int_value();

            let then_block = self.context.append_basic_block(func, "then");
            let ifcont_block = self.context.append_basic_block(func, "ifcont");

            self.builder
                .build_conditional_branch(cmp, then_block, ifcont_block)
                .unwrap();

            self.builder.position_at_end(then_block);

            self.locals.enter();
            self.fns.enter();
            let returned = self.generate_stmts(body);
            self.fns.exit();
            self.locals.exit();

            if !returned {
                self.builder
                    .build_unconditional_branch(last_ifcont_block)
                    .unwrap();
            }

            self.builder.position_at_end(ifcont_block);
        }

        let cond = self.generate_nonvoid_expr(*cond);
        let cmp = cond.into_int_value();

        let then_block = self.context.append_basic_block(func, "then");
        let else_block = if else_body.is_some() {
            Some(self.context.append_basic_block(func, "else"))
        } else {
            None
        };

        self.builder
            .build_conditional_branch(cmp, then_block, else_block.unwrap_or(last_ifcont_block))
            .unwrap();

        self.builder.position_at_end(then_block);

        self.locals.enter();
        self.fns.enter();
        let returned = self.generate_stmts(body);
        self.fns.exit();
        self.locals.exit();

        if !returned {
            self.builder
                .build_unconditional_branch(last_ifcont_block)
                .unwrap();
        }

        if let Some(else_body) = else_body {
            self.builder.position_at_end(else_block.unwrap());

            self.locals.enter();
            self.fns.enter();
            let returned = self.generate_stmts(else_body);
            self.fns.exit();
            self.locals.exit();

            if !returned {
                self.builder
                    .build_unconditional_branch(last_ifcont_block)
                    .unwrap();
            }
        }

        self.builder.position_at_end(last_ifcont_block);
    }

    fn generate_expr(&mut self, expr: TypedExprId) -> Option<BasicValueEnum<'ctx>> {
        let expr = &self.typed_ast[expr];

        let ty = expr.ty().resolve(&self.tcx);
        let llvm_ty = ty.as_llvm_type(self.context);

        match expr.kind() {
            TypedExprKind::Literal(kind) => match kind {
                LiteralKind::Int {
                    base,
                    empty: _,
                    value,
                } => match ty {
                    Type::Int(kind) if kind.unsigned() => Some(
                        llvm_ty
                            .into_int_type()
                            .const_int(
                                u64::from_str_radix(value.trim(), *base as u32).unwrap(),
                                false,
                            )
                            .into(),
                    ),
                    Type::Int(_) => Some(
                        llvm_ty
                            .into_int_type()
                            .const_int(
                                i64::from_str_radix(value.trim(), *base as u32).unwrap() as u64,
                                true,
                            )
                            .into(),
                    ),
                    _ => unreachable!("int's type is always int"),
                },
                LiteralKind::Float {
                    base: _,
                    empty_exp: _,
                    value,
                } => Some(
                    llvm_ty
                        .into_float_type()
                        .const_float(value.trim().parse().unwrap())
                        .into(),
                ),
                LiteralKind::String { value, .. } => Some({
                    if let Some(v) = self.strings.get(value) {
                        v.as_basic_value_enum()
                    } else {
                        let string = self.context.const_string(value.as_bytes(), true);

                        let global = self.module.add_global(
                            string.get_type(),
                            Some(AddressSpace::default()),
                            "str",
                        );

                        global.set_constant(true);
                        global.set_linkage(Linkage::Private);
                        global.set_initializer(&string);

                        self.strings.insert(value.clone(), global);

                        let i8_zero = self.context.i8_type().const_zero();
                        unsafe {
                            self.builder.build_in_bounds_gep(
                                string.get_type(),
                                global.as_pointer_value(),
                                &[i8_zero, i8_zero],
                                "load_str",
                            )
                        }
                        .unwrap()
                        .into()
                    }
                }),
            },
            TypedExprKind::Variable(name) => Some(
                self.builder
                    .build_load(
                        std::convert::TryInto::<BasicTypeEnum>::try_into(llvm_ty).unwrap(),
                        self.locals.get(name).unwrap().into_pointer_value(),
                        "load",
                    )
                    .unwrap(),
            ),
            TypedExprKind::Unary(op, val) => {
                let val = self.generate_nonvoid_expr(*val);

                Some(match op {
                    UnaryOp::Neg => match ty {
                        Type::Int(_)
                        | Type::Var(_, Constraint::Integer | Constraint::SignedInteger) => self
                            .builder
                            .build_int_neg(val.into_int_value(), "neg")
                            .unwrap()
                            .into(),
                        Type::Float(_) | Type::Var(_, Constraint::Float) => self
                            .builder
                            .build_float_neg(val.into_float_value(), "negf")
                            .unwrap()
                            .into(),
                        _ => unreachable!("type checking should rule out other types"),
                    },
                    UnaryOp::Not => self
                        .builder
                        .build_not(val.into_int_value(), "not")
                        .unwrap()
                        .into(),
                    UnaryOp::Ref => {
                        let ptr = self
                            .builder
                            .build_alloca(
                                TryInto::<BasicTypeEnum>::try_into(llvm_ty).unwrap(),
                                "alloc_ref",
                            )
                            .unwrap();

                        self.builder.build_store(ptr, val).unwrap();

                        ptr.into()
                    }
                    UnaryOp::Deref => self
                        .builder
                        .build_load(
                            TryInto::<BasicTypeEnum>::try_into(llvm_ty).unwrap(),
                            val.into_pointer_value(),
                            "deref",
                        )
                        .unwrap(),
                })
            }
            TypedExprKind::Binary(op, lhs, rhs) => {
                let (l, r) = (
                    self.generate_nonvoid_expr(*lhs),
                    self.generate_nonvoid_expr(*rhs),
                );

                Some(self.generate_binop(
                    ty,
                    *op,
                    l,
                    self.typed_ast[*lhs].ty().resolve(&self.tcx),
                    r,
                    self.typed_ast[*rhs].ty().resolve(&self.tcx),
                ))
            }
            TypedExprKind::Assign(op, lhs, rhs) => {
                let lhs = &self.typed_ast[*lhs];

                let ptr = match lhs.kind() {
                    TypedExprKind::Variable(name) => *self.locals.get(name).unwrap(),
                    _ => unreachable!("assignment lhs can only be variable"),
                }
                .into_pointer_value();

                let ty = lhs.ty().resolve(&self.tcx);

                match op {
                    AssignOp::Assign => {
                        let rhs = self.generate_nonvoid_expr(*rhs);

                        self.builder.build_store(ptr, rhs).unwrap();

                        Some(ptr.into())
                    }
                    _ => {
                        let l = self
                            .builder
                            .build_load(
                                std::convert::TryInto::<BasicTypeEnum>::try_into(llvm_ty).unwrap(),
                                ptr,
                                "load_lhs",
                            )
                            .unwrap();
                        let r = self.generate_nonvoid_expr(*rhs);

                        let result = self.generate_binop(
                            ty,
                            op.try_into()
                                .expect("assign case is handled above, otherwise can't panic"),
                            l,
                            lhs.ty().resolve(&self.tcx),
                            r,
                            self.typed_ast[*rhs].ty().resolve(&self.tcx),
                        );

                        self.builder.build_store(ptr, result).unwrap();
                        Some(ptr.into())
                    }
                }
            }
            TypedExprKind::FnCall(name, args) => {
                let func = *self.fns.get(name).expect("no func :(");

                let args = args
                    .iter()
                    .map(|arg| self.generate_nonvoid_expr(*arg).into())
                    .collect::<Vec<_>>();

                self.builder
                    .build_call(func, &args, name)
                    .unwrap()
                    .try_as_basic_value()
                    .left()
            }
            TypedExprKind::Cast(ty, expr) => {
                let generated_expr = self.generate_nonvoid_expr(*expr);
                let llvm_ty =
                    std::convert::TryInto::<BasicTypeEnum>::try_into(ty.as_llvm_type(self.context))
                        .unwrap();

                Some(match (self.typed_ast[*expr].ty(), ty) {
                    (Type::Int(src), Type::Int(dest)) => {
                        if src.width() < dest.width() {
                            if src.unsigned() {
                                self.builder
                                    .build_int_z_extend(
                                        generated_expr.into_int_value(),
                                        llvm_ty.into_int_type(),
                                        "upcast",
                                    )
                                    .unwrap()
                                    .into()
                            } else {
                                self.builder
                                    .build_int_s_extend(
                                        generated_expr.into_int_value(),
                                        llvm_ty.into_int_type(),
                                        "upcast",
                                    )
                                    .unwrap()
                                    .into()
                            }
                        } else if src.width() > dest.width() {
                            self.builder
                                .build_int_truncate(
                                    generated_expr.into_int_value(),
                                    llvm_ty.into_int_type(),
                                    "downcast",
                                )
                                .unwrap()
                                .into()
                        } else {
                            generated_expr
                        }
                    }
                    (Type::Float(src), Type::Float(dest)) => {
                        if src.width() < dest.width() {
                            self.builder
                                .build_float_ext(
                                    generated_expr.into_float_value(),
                                    llvm_ty.into_float_type(),
                                    "fupcast",
                                )
                                .unwrap()
                                .into()
                        } else if src.width() > dest.width() {
                            self.builder
                                .build_float_trunc(
                                    generated_expr.into_float_value(),
                                    llvm_ty.into_float_type(),
                                    "fdowncast",
                                )
                                .unwrap()
                                .into()
                        } else {
                            generated_expr
                        }
                    }
                    (Type::Float(_), Type::Int(dest)) => {
                        if dest.unsigned() {
                            self.builder
                                .build_float_to_unsigned_int(
                                    generated_expr.into_float_value(),
                                    llvm_ty.into_int_type(),
                                    "ftoui",
                                )
                                .unwrap()
                                .into()
                        } else {
                            self.builder
                                .build_float_to_signed_int(
                                    generated_expr.into_float_value(),
                                    llvm_ty.into_int_type(),
                                    "ftosi",
                                )
                                .unwrap()
                                .into()
                        }
                    }
                    (Type::Int(src), Type::Float(_)) => {
                        if src.unsigned() {
                            self.builder
                                .build_unsigned_int_to_float(
                                    generated_expr.into_int_value(),
                                    llvm_ty.into_float_type(),
                                    "uitof",
                                )
                                .unwrap()
                                .into()
                        } else {
                            self.builder
                                .build_signed_int_to_float(
                                    generated_expr.into_int_value(),
                                    llvm_ty.into_float_type(),
                                    "sitof",
                                )
                                .unwrap()
                                .into()
                        }
                    }
                    (Type::Ref(_), Type::Ref(_)) => self
                        .builder
                        .build_pointer_cast(
                            generated_expr.into_pointer_value(),
                            llvm_ty.into_pointer_type(),
                            "ptop",
                        )
                        .unwrap()
                        .into(),
                    (Type::Ref(_), Type::Int(_)) => self
                        .builder
                        .build_ptr_to_int(
                            generated_expr.into_pointer_value(),
                            llvm_ty.into_int_type(),
                            "ptoi",
                        )
                        .unwrap()
                        .into(),
                    (Type::Int(_), Type::Ref(_)) => self
                        .builder
                        .build_int_to_ptr(
                            generated_expr.into_int_value(),
                            llvm_ty.into_pointer_type(),
                            "itop",
                        )
                        .unwrap()
                        .into(),
                    (Type::Bool, Type::Int(dest)) => {
                        if dest.width() > 8 {
                            // NOTE: always use zero extend because otherwise true becomes -1 if
                            // casted to signed int
                            self.builder
                                .build_int_z_extend(
                                    generated_expr.into_int_value(),
                                    llvm_ty.into_int_type(),
                                    "upcast",
                                )
                                .unwrap()
                                .into()
                        } else {
                            generated_expr
                        }
                    }
                    _ => panic!("incorrect cast"),
                })
            }
        }
    }

    fn generate_nonvoid_expr(&mut self, expr: TypedExprId) -> BasicValueEnum<'ctx> {
        self.generate_expr(expr)
            .unwrap_or_else(|| panic!("void expr cannot be used"))
    }

    fn generate_binop(
        &mut self,
        ty: Type,
        op: BinOp,
        l: BasicValueEnum<'ctx>,
        l_ty: Type,
        r: BasicValueEnum<'ctx>,
        r_ty: Type,
    ) -> BasicValueEnum<'ctx> {
        match ty {
            Type::Int(kind) => {
                let (l, r) = (l.into_int_value(), r.into_int_value());

                match op {
                    // add uint handling for div/rem
                    BinOp::Add => self.builder.build_int_add(l, r, "add"),
                    BinOp::Sub => self.builder.build_int_sub(l, r, "sub"),
                    BinOp::Mul => self.builder.build_int_mul(l, r, "mul"),
                    BinOp::Div => match kind.unsigned() {
                        false => self.builder.build_int_signed_div(l, r, "div"),
                        true => self.builder.build_int_unsigned_div(l, r, "divu"),
                    },
                    BinOp::Rem => match kind.unsigned() {
                        false => self.builder.build_int_signed_rem(l, r, "rem"),
                        true => self.builder.build_int_unsigned_rem(l, r, "remu"),
                    },
                    BinOp::Shl => self.builder.build_left_shift(l, r, "shl"),
                    BinOp::Shr => self
                        .builder
                        .build_right_shift(l, r, !kind.unsigned(), "rhs"),
                    BinOp::BitOr => self.builder.build_or(l, r, "bitor"),
                    BinOp::BitAnd => self.builder.build_and(l, r, "bitor"),
                    BinOp::BitXor => self.builder.build_xor(l, r, "bitor"),
                    o => panic!("invalid binary operation for int: {o:?}"),
                }
            }
            .unwrap()
            .into(),
            Type::Float(_) => {
                let (l, r) = (l.into_float_value(), r.into_float_value());

                match op {
                    BinOp::Add => self.builder.build_float_add(l, r, "addf"),
                    BinOp::Sub => self.builder.build_float_sub(l, r, "subf"),
                    BinOp::Mul => self.builder.build_float_mul(l, r, "mulf"),
                    BinOp::Div => self.builder.build_float_div(l, r, "divf"),
                    BinOp::Rem => self.builder.build_float_rem(l, r, "remf"),
                    o => panic!("invalid binary operation for float: {o:?}"),
                }
            }
            .unwrap()
            .into(),
            Type::Bool => match op {
                BinOp::Eq | BinOp::Ne | BinOp::Le | BinOp::Lt | BinOp::Ge | BinOp::Gt => {
                    match (l_ty, r_ty) {
                        (Type::Int(k1), Type::Int(k2)) if k1 == k2 => {
                            self.builder.build_int_compare(
                                op.as_int_predicate(k1.unsigned()),
                                l.into_int_value(),
                                r.into_int_value(),
                                "cmp",
                            )
                        }
                        (Type::Float(_), Type::Float(_)) => self.builder.build_float_compare(
                            op.as_float_predicate(),
                            l.into_float_value(),
                            r.into_float_value(),
                            "cmpf",
                        ),
                        (a, b) => unreachable!("can't compare {a:?} and {b:?}"),
                    }
                }
                // TODO: make it short-curcuit (i.e. in a && b, if a is false, b is not evaluated), to do this add basic blocks and cmps
                BinOp::Or => self
                    .builder
                    .build_or(l.into_int_value(), r.into_int_value(), "or"),
                BinOp::And => self
                    .builder
                    .build_and(l.into_int_value(), r.into_int_value(), "and"),
                o => panic!("invalid binary operation for bool: {o:?}"),
            }
            .unwrap()
            .into(),
            _ => todo!("unsupported type for binary operation"),
        }
    }
}
