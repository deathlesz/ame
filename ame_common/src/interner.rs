use crate::{Arena, HashMap, Id};

#[derive(Debug, Clone)]
pub struct Interner<T> {
    dedup: HashMap<T, Id<T>>,
    arena: Arena<T>,
}

impl<T> Interner<T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            dedup: HashMap::default(),
            arena: Arena::new(),
        }
    }
}

impl<T: Clone + Eq + std::hash::Hash> Interner<T> {
    pub fn intern(&mut self, value: T) -> Interned<T> {
        if let Some(id) = self.dedup.get(&value) {
            Interned(*id)
        } else {
            let id = self.arena.alloc(value.clone());
            self.dedup.insert(value, id);

            Interned(id)
        }
    }

    #[inline]
    pub fn get(&self, id: Interned<T>) -> &T {
        &self.arena[id.0]
    }

    #[inline]
    pub fn get_mut(&mut self, id: Interned<T>) -> &mut T {
        &mut self.arena[id.0]
    }
}

impl<T> Default for Interner<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone + Eq + std::hash::Hash> std::ops::Index<Interned<T>> for Interner<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: Interned<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T: Clone + Eq + std::hash::Hash> std::ops::IndexMut<Interned<T>> for Interner<T> {
    #[inline]
    fn index_mut(&mut self, index: Interned<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}

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
