use std::{collections::HashMap, convert::Infallible};

type Result<T> = std::result::Result<T, TypeError>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Var(TypeId, Constraint),
    Int(IntKind),
    Float(FloatKind),
    Bool,
    String,
    Fn {
        args: Vec<Type>,
        return_ty: Box<Type>,
        is_variadic: bool,
    },
    Ref(Box<Type>),
    None,
    Other(String), // placeholder for more types later
}

impl Type {
    #[inline]
    pub fn var(ctx: &mut TypeCtx) -> Self {
        Self::Var(ctx.next_id(), Constraint::None)
    }

    #[inline]
    pub fn var_int(ctx: &mut TypeCtx) -> Self {
        Self::Var(ctx.next_id(), Constraint::Integer)
    }

    #[inline]
    pub fn var_float(ctx: &mut TypeCtx) -> Self {
        Self::Var(ctx.next_id(), Constraint::Float)
    }

    pub fn resolve(&self, ctx: &TypeCtx) -> Self {
        match self {
            Type::Var(id, _) => {
                if let Some(ty) = ctx.get(id) {
                    ty.resolve(ctx)
                } else {
                    self.clone()
                }
            }
            _ => self.clone(),
        }
    }
}

impl std::str::FromStr for Type {
    type Err = Infallible;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        if let Ok(kind) = s.parse::<IntKind>() {
            Ok(Self::Int(kind))
        } else if let Ok(kind) = s.parse::<FloatKind>() {
            Ok(Self::Float(kind))
        } else if s == "string" {
            Ok(Self::String)
        } else if s == "bool" {
            Ok(Self::Bool)
        } else if s.starts_with('&') {
            Ok(Self::Ref(Box::new(s[1..].parse()?)))
        } else {
            Ok(Self::Other(s.into()))
        }
    }
}

impl From<String> for Type {
    fn from(s: String) -> Self {
        s.parse().unwrap()
    }
}

impl From<&String> for Type {
    fn from(s: &String) -> Self {
        s.parse().unwrap()
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

        (Type::Var(id1, constraint1), Type::Var(id2, constraint2))
            if id1 == id2 && constraint1 == constraint2 =>
        {
            Ok(())
        }

        (Type::Var(id, constraint), ty) | (ty, Type::Var(id, constraint))
            if constraint.matches(ty) =>
        {
            // prevents infinitely-recursive types
            if occurs_check(id, ty, ctx) {
                Err(TypeError::Recursive(id.clone(), ty.clone()))
            } else {
                ctx.set(id.clone(), ty.clone());

                Ok(())
            }
        }
        (Type::Ref(ty1), Type::Ref(ty2)) => unify(ty1, ty2, ctx),
        (Type::None, Type::None) => Ok(()),
        (Type::Other(s1), Type::Other(s2)) if s1 == s2 => Ok(()),

        _ => Err(TypeError::CannotUnify(t1, t2)),
    }
}

fn occurs_check(id: &TypeId, ty: &Type, ctx: &TypeCtx) -> bool {
    match ty.resolve(ctx) {
        Type::Var(ref other_id, _) => id == other_id,
        _ => false,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constraint {
    #[default]
    None,
    Integer,
    SignedInteger,
    Float,
}

impl Constraint {
    fn matches(&self, ty: &Type) -> bool {
        match self {
            Self::None => true,
            Self::Integer => matches!(ty, Type::Int(_) | Type::Var(_, Constraint::Integer)),
            Self::SignedInteger => matches!(ty, Type::Int(kind) if !kind.unsigned()),
            Self::Float => matches!(ty, Type::Float(_) | Type::Var(_, Constraint::Float)),
        }
    }
}

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

impl Default for TypeCtx {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
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

impl IntKind {
    #[inline]
    pub const fn unsigned(&self) -> bool {
        matches!(
            self,
            Self::Uint8 | Self::Uint16 | Self::Uint32 | Self::Uint64
        )
    }

    #[inline]
    pub const fn width(&self) -> u8 {
        use IntKind::*;

        match self {
            Int8 | Uint8 => 8,
            Int16 | Uint16 => 16,
            Int32 | Uint32 => 32,
            Int64 | Uint64 => 64,
        }
    }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum FloatKind {
    Float32,
    #[default]
    Float64,
}

impl FloatKind {
    #[inline]
    pub const fn width(&self) -> u8 {
        use FloatKind::*;

        match self {
            Float32 => 32,
            Float64 => 64,
        }
    }
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
