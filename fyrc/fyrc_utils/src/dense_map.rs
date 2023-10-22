use std::{
    iter::{self, Enumerate},
    marker::PhantomData,
    ops::{self, Index, IndexMut},
    slice::{Iter, IterMut},
    vec,
};

use crate::EntityId;

#[derive(Clone)]
pub struct DenseMap<K, V> {
    data: Vec<V>,
    _marker: PhantomData<K>,
}

impl<K, V> DenseMap<K, V> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            _marker: PhantomData,
        }
    }

    pub fn with_prefilled(length: usize) -> Self
    where
        V: Default,
    {
        let mut data: Vec<V> = Vec::with_capacity(length);
        for _ in 0..length {
            data.push(V::default());
        }
        Self {
            data,
            _marker: PhantomData,
        }
    }
}

impl<K> DenseMap<K, K>
where
    K: EntityId,
{
    pub fn with_mirrored(length: usize) -> Self {
        let mut data: Vec<K> = Vec::with_capacity(length);
        for id in 0..length {
            data.push(K::with_id(id));
        }
        Self {
            data,
            _marker: PhantomData,
        }
    }
}

impl<K, V> Default for DenseMap<K, V> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> DenseMap<K, V>
where
    K: EntityId,
{
    pub fn insert(&mut self, value: V) -> K {
        let curr_len = self.data.len();
        self.data.push(value);
        K::with_id(curr_len)
    }

    #[inline]
    pub fn get(&self, idx: K) -> Option<&V> {
        self.data.get(idx.get_id())
    }

    #[inline]
    pub fn contains(&self, key: &K) -> bool {
        key.get_id() < self.data.len()
    }

    pub fn set(&mut self, key: K, val: V) -> bool {
        if self.contains(&key) {
            self.data[key.get_id()] = val;
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn get_mut(&mut self, idx: K) -> Option<&mut V> {
        self.data.get_mut(idx.get_id())
    }

    #[inline]
    pub fn iter(&self) -> EntityIter<'_, K, V> {
        EntityIter::new(self.data.iter())
    }

    #[inline]
    pub fn iter_mut(&mut self) -> EntityIterMut<'_, K, V> {
        EntityIterMut::new(self.data.iter_mut())
    }

    #[inline]
    pub fn keys(&self) -> Keys<K> {
        Keys::new(0..self.data.len())
    }

    #[inline]
    pub fn values(&self) -> Iter<'_, V> {
        self.data.iter()
    }

    #[inline]
    pub fn values_mut(&mut self) -> IterMut<'_, V> {
        self.data.iter_mut()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

impl<K, V> IntoIterator for DenseMap<K, V>
where
    K: EntityId,
{
    type Item = (K, V);
    type IntoIter = iter::Map<Enumerate<vec::IntoIter<V>>, fn((usize, V)) -> (K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.data
            .into_iter()
            .enumerate()
            .map(|(i, v)| (K::with_id(i), v))
    }
}

impl<K, V> Index<K> for DenseMap<K, V>
where
    K: EntityId,
{
    type Output = V;

    #[inline]
    fn index(&self, index: K) -> &Self::Output {
        &self.data[index.get_id()]
    }
}

impl<K, V> IndexMut<K> for DenseMap<K, V>
where
    K: EntityId,
{
    #[inline]
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.data[index.get_id()]
    }
}

pub struct Keys<K: EntityId> {
    inner: ops::Range<usize>,
    _marker: PhantomData<K>,
}

impl<K: EntityId> Keys<K> {
    fn new(inner: ops::Range<usize>) -> Self {
        Self {
            inner,
            _marker: PhantomData,
        }
    }
}

impl<K: EntityId> Iterator for Keys<K> {
    type Item = K;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(K::with_id)
    }
}

pub struct EntityIter<'a, K, V> {
    inner: Enumerate<Iter<'a, V>>,
    _marker: PhantomData<K>,
}

impl<'a, K, V> EntityIter<'a, K, V> {
    fn new(inner: Iter<'a, V>) -> Self {
        Self {
            inner: inner.enumerate(),
            _marker: PhantomData,
        }
    }
}

impl<'a, K, V> Iterator for EntityIter<'a, K, V>
where
    K: EntityId,
{
    type Item = (K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(id, val)| (K::with_id(id), val))
    }
}

pub struct EntityIterMut<'a, K, V> {
    inner: Enumerate<IterMut<'a, V>>,
    _marker: PhantomData<K>,
}

impl<'a, K, V> EntityIterMut<'a, K, V> {
    fn new(inner: IterMut<'a, V>) -> Self {
        Self {
            inner: inner.enumerate(),
            _marker: PhantomData,
        }
    }
}

impl<'a, K, V> Iterator for EntityIterMut<'a, K, V>
where
    K: EntityId,
{
    type Item = (K, &'a mut V);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(id, val)| (K::with_id(id), val))
    }
}

impl<K, V> FromIterator<V> for DenseMap<K, V>
where
    K: EntityId,
{
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Self {
            data: Vec::from_iter(iter),
            _marker: PhantomData,
        }
    }
}