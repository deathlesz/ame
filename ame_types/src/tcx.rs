use ame_common::{HashMap, Interned, Interner};

use crate::{Constraint, Type, TypeError, VarId};

#[derive(Debug, Clone)]
pub struct TypeCtx {
    ty_interner: Interner<Type>,
    def_interner: Interner<DefKind>,

    name_to_def_id: HashMap<String, DefId>,
    var_id_to_ty: HashMap<VarId, Interned<Type>>,
    var_id: u32,
}

impl TypeCtx {
    #[inline]
    pub fn new() -> Self {
        Self {
            ty_interner: Interner::default(),
            def_interner: Interner::default(),

            name_to_def_id: HashMap::default(),
            var_id_to_ty: HashMap::default(),
            var_id: 0,
        }
    }

    #[inline]
    pub fn intern_type(&mut self, ty: Type) -> Interned<Type> {
        self.ty_interner.intern(ty)
    }

    pub fn define_fn(
        &mut self,
        name: String,
        args: Vec<Interned<Type>>,
        return_ty: Interned<Type>,
        is_variadic: bool,
    ) -> DefId {
        let def_kind = DefKind::Fn {
            args,
            return_ty,
            is_variadic,
        };
        let def_id = DefId(self.def_interner.intern(def_kind));

        let fn_ty = Type::Fn(def_id);
        self.ty_interner.intern(fn_ty);

        self.name_to_def_id.insert(name, def_id);

        def_id
    }

    #[inline]
    pub fn get_ty(&self, id: Interned<Type>) -> &Type {
        &self.ty_interner[id]
    }

    #[inline]
    pub fn get_def(&self, id: DefId) -> &DefKind {
        &self.def_interner[id.0]
    }

    #[inline]
    pub fn get_def_ty(&mut self, id: DefId) -> Interned<Type> {
        match self.def_interner.get(id.0) {
            DefKind::Fn { .. } => self.ty_interner.intern(Type::Fn(id)),
        }
    }

    #[inline]
    pub fn var(&mut self) -> Type {
        Type::Var(self.next_var_id(), Constraint::None)
    }

    #[inline]
    pub fn var_int(&mut self) -> Type {
        Type::Var(self.next_var_id(), Constraint::Integer)
    }

    #[inline]
    pub fn var_float(&mut self) -> Type {
        Type::Var(self.next_var_id(), Constraint::Float)
    }

    pub fn resolve(&self, ty: Interned<Type>) -> &Type {
        match self.get_ty(ty) {
            resolved_ty @ Type::Var(id, _) => {
                // dbg!(id, &self.var_id_to_ty);

                if let Some(ty) = self.var_id_to_ty.get(id) {
                    self.resolve(*ty)
                } else {
                    resolved_ty
                }
            }
            resolved_ty => resolved_ty,
        }
    }

    pub fn unify(&mut self, t1: Interned<Type>, t2: Interned<Type>) -> Result<(), TypeError> {
        let t1_resolved = self.resolve(t1);
        let t2_resolved = self.resolve(t2);

        match (&t1_resolved, &t2_resolved) {
            (Type::Int(k1), Type::Int(k2)) if k1 == k2 => Ok(()),
            (Type::Float(k1), Type::Float(k2)) if k1 == k2 => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::String, Type::String) => Ok(()),

            (Type::Var(id1, constraint1), Type::Var(id2, constraint2))
                if id1 == id2 && constraint1 == constraint2 =>
            {
                Ok(())
            }

            (Type::Var(id, constraint), ty) | (ty, Type::Var(id, constraint))
                if constraint.matches(ty) =>
            {
                let interned_ty = if ty == &t1_resolved { t1 } else { t2 };

                // prevents infinitely-recursive types
                if self.occurs_check(*id, interned_ty) {
                    Err(TypeError::Recursive(*id, (*ty).clone()))
                } else {
                    self.var_id_to_ty.insert(*id, interned_ty);

                    Ok(())
                }
            }
            (Type::Ref(ty1), Type::Ref(ty2)) => self.unify(*ty1, *ty2),
            (Type::None, Type::None) => Ok(()),
            (Type::Other(s1), Type::Other(s2)) if s1 == s2 => Ok(()),

            _ => Err(TypeError::CannotUnify(
                t1_resolved.clone(),
                t2_resolved.clone(),
            )),
        }
    }

    #[inline]
    fn next_var_id(&mut self) -> VarId {
        let result = VarId(self.var_id);
        self.var_id += 1;

        result
    }

    fn occurs_check(&self, id: VarId, ty: Interned<Type>) -> bool {
        match self.resolve(ty) {
            Type::Var(other, _) => id == *other,
            _ => false,
        }
    }
}

impl Default for TypeCtx {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId(Interned<DefKind>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefKind {
    Fn {
        args: Vec<Interned<Type>>,
        return_ty: Interned<Type>,
        is_variadic: bool,
    },
}
