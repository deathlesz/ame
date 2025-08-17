use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub struct IdArena<T> {
    inner: Vec<T>,
}

impl<T> IdArena<T> {
    #[inline]
    pub const fn new() -> Self {
        Self { inner: vec![] }
    }

    #[inline]
    pub fn with_capacity(capacity: u32) -> Self {
        Self {
            inner: Vec::with_capacity(capacity as usize),
        }
    }

    #[inline]
    pub fn alloc(&mut self, value: T) -> Id<T> {
        let id = Id {
            id: self.inner.len() as u32,
            _ty: PhantomData,
        };

        self.inner.push(value);

        id
    }

    // these don't return `Option<&T>` as `Id<T>` can only be obtained through the arena
    //
    // will panic if, for example, `Id<T>` was obtained through another arena and it's `id` is out
    // of bounds for this arena
    //
    // PERF: `.get_unchecked(id.id as usize)` seems to be ~4.6% slower in microbenchmark
    #[inline]
    pub fn get(&self, id: Id<T>) -> &T {
        self.inner
            .get(id.id as usize)
            .expect("supplied id is from a different arena")
    }

    #[inline]
    pub fn get_mut(&mut self, id: Id<T>) -> &mut T {
        self.inner
            .get_mut(id.id as usize)
            .expect("supplied id is from a different arena")
    }
}

impl<T> Default for IdArena<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T> std::ops::Index<Id<T>> for IdArena<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: Id<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T> std::ops::IndexMut<Id<T>> for IdArena<T> {
    #[inline]
    fn index_mut(&mut self, index: Id<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}

#[derive(Debug)]
pub struct Id<T> {
    id: u32,
    // covariance
    _ty: PhantomData<fn() -> T>,
}

impl<T> Id<T> {
    #[inline]
    pub const fn index(&self) -> u32 {
        self.id
    }
}

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for Id<T> {
    #[inline]
    fn eq(&self, rhs: &Self) -> bool {
        self.id == rhs.id
    }
}
impl<T> Eq for Id<T> {}

impl<T> std::hash::Hash for Id<T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.id.hash(h);
    }
}

impl<T> PartialOrd for Id<T> {
    #[inline]
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}
impl<T> Ord for Id<T> {
    #[inline]
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        self.id.cmp(&rhs.id)
    }
}
