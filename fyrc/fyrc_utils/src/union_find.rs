use rustc_hash::{FxHashSet, FxHashMap};

use crate::EntityId;

pub struct UnionFind<T> {
    pub parent: FxHashMap<T, T>,
    pub rank: FxHashMap<T, u16>,
}

impl<T> UnionFind<T> {
    pub fn new() -> Self {
        Self {
            parent: FxHashMap::default(),
            rank: FxHashMap::default(),
        }
    }
}

impl<T> UnionFind<T>
where
    T: EntityId + std::hash::Hash + Eq,
{
    fn find_inner(&mut self, v: T, insert: bool) -> T {
        let parent = if insert {
            *self
                .parent
                .entry(v)
                .or_insert(v)
        } else {
            self
                .parent
                .get(&v)
                .copied()
                .unwrap_or(v)
        };

        if v == parent {
            return v;
        }

        let grandparent = self.find_inner(parent, insert);
        self.parent.insert(parent, grandparent);

        grandparent
    }

    #[inline(always)]
    pub fn find(&mut self, v: T) -> T {
        self.find_inner(v, false)
    }

    pub fn union(&mut self, mut v1: T, mut v2: T) {
        v1 = self.find_inner(v1, true);
        v2 = self.find_inner(v2, true);

        if v1 == v2 {
            return;
        }

        let mut r1 = *self
            .rank
            .entry(v1)
            .or_default();

        let mut r2 = *self
            .rank
            .entry(v2)
            .or_default();

        if r1 < r2 {
            (v1, v2) = (v2, v1);
            (r1, r2) = (r2, r1);
        }

        self.parent.insert(v2, v1);

        if r1 == r2 {
            self.rank.insert(v1, r1 + 1);
        }
    }

    pub fn get_all_sets(&self) -> Vec<FxHashSet<T>>
    where
        T: std::hash::Hash + Eq,
    {
        let mut value_map = FxHashMap::<T, FxHashSet<T>>::default();

        for (child, parent) in self.parent.iter() {
            value_map
                .entry(*parent)
                .or_default()
                .insert(*child);
        }

        value_map.into_values().collect()
    }

    pub fn get_set(&self, v: T) -> FxHashSet<T>
    where
        T: std::hash::Hash + Eq,
    {
        let my_parent = self
            .parent
            .get(&v)
            .copied()
            .unwrap_or(v);

        if my_parent == v {
            return FxHashSet::from_iter([v]);
        }

        let mut set = FxHashSet::<T>::default();

        for (child, his_parent) in self.parent.iter() {
            if *his_parent == my_parent {
                set.insert(*child);
            }
        }

        set
    }
}
