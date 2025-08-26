use ame_common::Interned;

mod tcx;
pub use tcx::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Var(VarId, Constraint),
    Int(IntKind),
    Float(FloatKind),
    Bool,
    String,
    Fn(DefId),
    Class(DefId),
    Ref(Interned<Type>),
    None,
    Other(String), // placeholder for more types later
}

impl Type {
    #[allow(
        clippy::manual_strip,
        reason = "need to somehow intern it `n` times to use"
    )]
    pub fn from_str(s: &str, tcx: &mut TypeCtx) -> Self {
        if let Ok(kind) = s.parse::<IntKind>() {
            Self::Int(kind)
        } else if let Ok(kind) = s.parse::<FloatKind>() {
            Self::Float(kind)
        } else if s == "string" {
            Self::String
        } else if s == "bool" {
            Self::Bool
        } else if s.starts_with('&') {
            let inner = Self::from_str(&s[1..], tcx);
            let interned = tcx.intern_type(inner);

            Self::Ref(interned)
        } else {
            Self::Other(s.into())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(u32);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constraint {
    #[default]
    None,
    Integer,
    SignedInteger,
    Float,
}

impl Constraint {
    #[inline]
    fn matches(&self, ty: &Type) -> bool {
        match self {
            Self::None => true,
            Self::Integer => matches!(ty, Type::Int(_) | Type::Var(_, Constraint::Integer)),
            Self::SignedInteger => matches!(ty, Type::Int(kind) if !kind.unsigned()),
            Self::Float => matches!(ty, Type::Float(_) | Type::Var(_, Constraint::Float)),
        }
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
    pub const fn width_in_bits(&self) -> u8 {
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
    pub const fn width_in_bits(&self) -> u8 {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeError {
    Recursive(VarId, Type),
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
