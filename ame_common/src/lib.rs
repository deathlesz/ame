pub mod arena;
pub mod interner;
pub mod scopestack;

pub use arena::{Id, IdArena as Arena};
pub use interner::Interned;
pub use scopestack::ScopeStack;

// expose to other compiler parts
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
pub type HashSet<V> = rustc_hash::FxHashSet<V>;
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, rustc_hash::FxBuildHasher>;
pub type IndexSet<K> = indexmap::IndexSet<K, rustc_hash::FxBuildHasher>;

pub type Interner<T> = interner::Interner<T, rustc_hash::FxBuildHasher>;
