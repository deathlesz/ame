use std::collections::HashMap;

type Result<T> = std::result::Result<T, TypeError>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Type {
    #[default]
    Unknown,
    Var(TypeId),
    Int(IntKind),
    Float(FloatKind),
    Bool,
    String,
    Other(String), // placeholder for more types later
}

impl Type {
    #[inline]
    pub fn var(ctx: &mut TypeCtx) -> Self {
        Self::Var(ctx.next_id())
    }

    pub fn resolve(&self, ctx: &TypeCtx) -> Self {
        match self {
            Type::Var(id) => {
                if let Some(ty) = ctx.get(id) {
                    ty.resolve(&ctx)
                } else {
                    self.clone()
                }
            }
            _ => self.clone(),
        }
    }
}

impl std::str::FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        if let Ok(kind) = s.parse::<IntKind>() {
            Ok(Self::Int(kind))
        } else if let Ok(kind) = s.parse::<FloatKind>() {
            Ok(Self::Float(kind))
        } else if s == "false" || s == "true" {
            Ok(Self::Bool)
        } else {
            Ok(Self::Other(s.into()))
        }
    }
}

impl From<String> for Type {
    fn from(s: String) -> Self {
        if let Ok(kind) = s.parse::<IntKind>() {
            Self::Int(kind)
        } else if let Ok(kind) = s.parse::<FloatKind>() {
            Self::Float(kind)
        } else if s == "false" || s == "true" {
            Self::Bool
        } else {
            Self::Other(s)
        }
    }
}

pub fn unify(t1: &Type, t2: &Type, ctx: &mut TypeCtx) -> Result<()> {
    let t1 = t1.resolve(ctx);
    let t2 = t2.resolve(ctx);

    match (&t1, &t2) {
        (Type::Int(k1), Type::Int(k2)) if k1 == k2 => Ok(()),
        (Type::Float(k1), Type::Float(k2)) if k1 == k2 => Ok(()),
        (Type::Bool, Type::Bool) => Ok(()),
        (Type::String, Type::String) => Ok(()),

        (Type::Var(id1), Type::Var(id2)) if id1 == id2 => Ok(()),

        (Type::Var(id), ty) | (ty, Type::Var(id)) => {
            // prevents infinitely-recursive types
            if occurs_check(&id, &ty, ctx) {
                Err(TypeError::Recursive(id.clone(), ty.clone()))
            } else {
                println!("hello???? {id:?} {ty:?}");
                ctx.set(id.clone(), ty.clone());
                Ok(())
            }
        }
        (Type::Other(s1), Type::Other(s2)) if s1 == s2 => Ok(()),

        _ => Err(TypeError::CannotUnify(t1, t2)),
    }
}

fn occurs_check(id: &TypeId, ty: &Type, ctx: &TypeCtx) -> bool {
    match ty.resolve(ctx) {
        Type::Var(ref other_id) => id == other_id,
        _ => false,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Clone)]
pub struct TypeCtx {
    last_id: usize,
    map: HashMap<TypeId, Type>,
}

impl TypeCtx {
    #[inline]
    pub fn new() -> Self {
        Self {
            last_id: 0,
            map: HashMap::new(),
        }
    }

    #[inline]
    pub fn next_id(&mut self) -> TypeId {
        let id = TypeId(self.last_id);

        self.last_id += 1;
        id
    }

    #[inline]
    pub fn get(&self, id: &TypeId) -> Option<&Type> {
        self.map.get(id)
    }

    #[inline]
    pub fn set(&mut self, id: TypeId, ty: Type) {
        self.map
            .entry(id)
            .and_modify(|old_ty| *old_ty = ty.clone())
            .or_insert(ty);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum IntKind {
    Int8,
    Int16,
    #[default]
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
}

impl std::str::FromStr for IntKind {
    type Err = ();

    #[inline]
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "int8" => Ok(Self::Int8),
            "int16" => Ok(Self::Int16),
            "int32" => Ok(Self::Int32),
            "int64" => Ok(Self::Int64),
            "uint8" => Ok(Self::Uint8),
            "uint16" => Ok(Self::Uint16),
            "uint32" => Ok(Self::Uint32),
            "uint64" => Ok(Self::Uint64),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum FloatKind {
    Float32,
    #[default]
    Float64,
}

impl std::str::FromStr for FloatKind {
    type Err = ();

    #[inline]
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "float32" => Ok(Self::Float32),
            "float64" => Ok(Self::Float64),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    Recursive(TypeId, Type),
    CannotUnify(Type, Type),
    Unbound(String),
}

impl std::fmt::Display for TypeError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Recursive(id, ty) => write!(f, "recursive type: {id:?} occurs in {ty:?}"),
            Self::CannotUnify(t1, t2) => write!(f, "cannot unify {t1:?} and {t2:?}"),
            Self::Unbound(name) => write!(f, "unbound variable: {name}"),
        }
    }
}

impl std::error::Error for TypeError {}
