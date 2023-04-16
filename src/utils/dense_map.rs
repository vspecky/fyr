use std::{
    iter::Enumerate,
    marker::PhantomData,
    ops::{Index, IndexMut},
    slice::{Iter, IterMut},
};

pub trait EntityId {
    fn get_id(&self) -> usize;
    fn with_id(idx: usize) -> Self;
}

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
    pub fn values(&self) -> Iter<'_, V> {
        self.data.iter()
    }

    #[inline]
    pub fn values_mut(&mut self) -> IterMut<'_, V> {
        self.data.iter_mut()
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
