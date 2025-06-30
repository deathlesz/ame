use std::{borrow::Borrow, collections::HashMap};

#[derive(Debug)]
pub struct ScopeStack<K, V> {
    scopes: Vec<HashMap<K, V>>,
}

impl<K, V> ScopeStack<K, V> {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit(&mut self) {
        self.scopes.pop().expect("tried to exit global scope");
    }
}

impl<K: Eq + std::hash::Hash, V> ScopeStack<K, V> {
    pub fn define(&mut self, name: K, value: V) {
        self.scopes
            .last_mut()
            .expect("there should always be global scope")
            .insert(name, value);
    }

    pub fn get(&self, name: impl Borrow<K>) -> Option<&V> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name.borrow()))
    }

    pub fn get_mut(&mut self, name: impl Borrow<K>) -> Option<&mut V> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(name.borrow()))
    }

    pub fn contains(&self, name: impl Borrow<K>) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.contains_key(name.borrow()))
    }
}

impl<K, V> Default for ScopeStack<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Iter<'a, K, V> {
    outer: std::iter::Rev<std::slice::Iter<'a, HashMap<K, V>>>,
    inner: Option<std::collections::hash_map::Iter<'a, K, V>>,
}

impl<'a, K, V> IntoIterator for &'a ScopeStack<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        let mut outer = self.scopes.iter().rev();
        let inner = outer.next().map(|map| map.iter());

        Iter { outer, inner }
    }
}

impl<'a, K, V> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match &mut self.inner {
                Some(iter) => {
                    if let Some(item) = iter.next() {
                        return Some(item);
                    }
                }
                None => return None,
            }

            self.inner = self.outer.next().map(|map| map.iter());
        }
    }
}
