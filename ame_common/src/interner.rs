use std::{
    collections::HashMap,
    hash::{BuildHasher, RandomState},
};

use crate::{Arena, Id};

#[derive(Clone)]
pub struct Interner<T, S> {
    // stores hash -> vec of possible ids to avoid storing the value itself
    dedup: HashMap<u64, Vec<Id<T>>, S>,
    arena: Arena<T>,
}

impl<T> Interner<T, RandomState> {
    #[inline]
    pub fn new() -> Self {
        Self {
            dedup: HashMap::default(),
            arena: Arena::new(),
        }
    }
}

impl<T: Eq + std::hash::Hash, S: BuildHasher> Interner<T, S> {
    pub fn intern(&mut self, value: T) -> Interned<T> {
        let hash = self.dedup.hasher().hash_one(&value);

        if let Some(ids) = self.dedup.get(&hash) {
            for &id in ids {
                if value == self.arena[id] {
                    return Interned(id);
                }
            }
        }

        let id = self.arena.alloc(value);
        self.dedup.entry(hash).or_default().push(id);

        Interned(id)
    }
}

impl<T: Eq + std::hash::Hash, S> Interner<T, S> {
    #[inline]
    pub fn get(&self, id: Interned<T>) -> &T {
        &self.arena[id.0]
    }

    #[inline]
    pub fn get_mut(&mut self, id: Interned<T>) -> &mut T {
        &mut self.arena[id.0]
    }
}

impl<T, S> Interner<T, S> {
    #[inline]
    pub const fn len(&self) -> u32 {
        self.arena.len()
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T, S: Default> Default for Interner<T, S> {
    #[inline]
    fn default() -> Self {
        Self {
            dedup: HashMap::default(),
            arena: Arena::default(),
        }
    }
}

impl<T: std::fmt::Debug, S> std::fmt::Debug for Interner<T, S> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Interner")
            .field("dedup", &self.dedup)
            .field("arena", &self.arena)
            .finish()
    }
}

impl<T: Clone + Eq + std::hash::Hash, S> std::ops::Index<Interned<T>> for Interner<T, S> {
    type Output = T;

    #[inline]
    fn index(&self, index: Interned<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T: Clone + Eq + std::hash::Hash, S> std::ops::IndexMut<Interned<T>> for Interner<T, S> {
    #[inline]
    fn index_mut(&mut self, index: Interned<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}

#[derive(Debug)]
pub struct Interned<T>(Id<T>);

impl<T> Copy for Interned<T> {}
impl<T> Clone for Interned<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for Interned<T> {
    #[inline]
    fn eq(&self, rhs: &Self) -> bool {
        self.0 == rhs.0
    }
}
impl<T> Eq for Interned<T> {}

impl<T> std::hash::Hash for Interned<T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.0.hash(h);
    }
}

impl<T> PartialOrd for Interned<T> {
    #[inline]
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}
impl<T> Ord for Interned<T> {
    #[inline]
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        self.0.cmp(&rhs.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_duplication() {
        let mut interner = Interner::new();

        let id1 = interner.intern(128);
        assert_eq!(interner.len(), 1);

        let id2 = interner.intern(128);
        assert_eq!(interner.len(), 1);
        assert_eq!(id1, id2);

        let id3 = interner.intern(256);
        assert_eq!(interner.len(), 2);
        assert_ne!(id1, id3);
        assert_ne!(id2, id3);

        let id4 = interner.intern(256);
        assert_eq!(interner.len(), 2);
        assert_eq!(id3, id4);
    }

    #[test]
    fn test_no_duplication_different_hasher() {
        use rustc_hash::FxBuildHasher;

        let mut interner = Interner::<_, FxBuildHasher>::default();

        let mut prev_id = interner.intern(1);
        for _ in 0..10_000 {
            let id = interner.intern(1);
            assert_eq!(interner.len(), 1);
            assert_eq!(prev_id, id);

            prev_id = id;
        }
    }
}
