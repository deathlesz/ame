use std::marker::PhantomData;

use id_arena::ArenaBehavior;

#[derive(Debug)]
pub struct Id<T> {
    id: u32,
    arena_id: u32,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Id<T> {
    #[inline]
    pub fn index(&self) -> u32 {
        self.id
    }
}

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for Id<T> {
    #[inline]
    fn eq(&self, rhs: &Self) -> bool {
        self.arena_id == rhs.arena_id && self.id == rhs.id
    }
}
impl<T> Eq for Id<T> {}

impl<T> std::hash::Hash for Id<T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.arena_id.hash(h);
        self.id.hash(h);
    }
}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}
impl<T> Ord for Id<T> {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        self.arena_id.cmp(&rhs.arena_id).then(self.id.cmp(&rhs.id))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArenaBehaviorU32<T> {
    _phantom: PhantomData<fn() -> T>,
}

impl<T> ArenaBehavior for ArenaBehaviorU32<T> {
    type Id = Id<T>;

    #[inline]
    fn new_id(arena_id: u32, id: usize) -> Self::Id {
        Id {
            id: id.try_into().expect("id must be a valid u32"),
            arena_id,
            _ty: PhantomData,
        }
    }

    #[inline]
    fn index(id: Self::Id) -> usize {
        id.id as usize
    }

    #[inline]
    fn arena_id(id: Self::Id) -> u32 {
        id.arena_id
    }
}
