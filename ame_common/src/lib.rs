pub mod arena;
pub mod scopestack;

pub use arena::Id;
pub use scopestack::ScopeStack;

// expose to other compiler parts
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
pub type HashSet<V> = rustc_hash::FxHashSet<V>;
pub type Arena<T> = id_arena::Arena<T, arena::ArenaBehaviorU32<T>>;
