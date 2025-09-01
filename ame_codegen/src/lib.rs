use ame_common::{HashMap, ScopeStack};
use ame_tast::{TypedAst, TypedStmtId};
use ame_types::TypeCtx;

mod backend;

pub use crate::backend::Backend;

#[derive(Debug)]
pub struct CodeGenCtx<'a, B: Backend> {
    pub typed_ast: &'a TypedAst,
    pub nodes: &'a [TypedStmtId],
    pub tcx: &'a TypeCtx,

    pub locals: ScopeStack<&'a str, B::LocalValue>,
    pub fns: ScopeStack<&'a str, B::FunctionValue>,
    pub classes: ScopeStack<&'a str, B::ClassValue>,

    pub strings: HashMap<String, B::StringValue>,
}

impl<'a, B: Backend> CodeGenCtx<'a, B> {
    #[inline]
    pub fn new(typed_ast: &'a TypedAst, nodes: &'a [TypedStmtId], tcx: &'a TypeCtx) -> Self {
        Self {
            typed_ast,
            nodes,
            tcx,

            locals: ScopeStack::new(),
            fns: ScopeStack::new(),
            classes: ScopeStack::new(),

            strings: HashMap::default(),
        }
    }

    #[inline]
    pub fn enter_scope(&mut self) {
        self.locals.enter();
        self.fns.enter();
        self.classes.enter();
    }

    #[inline]
    pub fn exit_scope(&mut self) {
        self.locals.exit();
        self.fns.exit();
        self.classes.exit();
    }
}
