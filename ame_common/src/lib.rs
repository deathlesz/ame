pub mod arena;
pub mod interner;
pub mod scopestack;

pub use arena::{Id, IdArena as Arena};
pub use interner::{Interned, Interner};
pub use scopestack::ScopeStack;

// expose to other compiler parts
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
pub type HashSet<V> = rustc_hash::FxHashSet<V>;
