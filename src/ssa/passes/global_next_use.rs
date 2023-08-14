use std::cmp::Ordering;

use rustc_hash::FxHashMap;

use crate::{
    ssa::{function::FunctionData, passes, Block, Value},
    utils::DenseMap,
};

#[derive(Debug, thiserror::Error)]
pub enum GlobalNextUseError {

}

#[derive(Debug, Clone, Copy)]
pub enum UseDistance {
    Finite(u64),
    Infinite,
}

impl PartialEq for UseDistance {
    fn eq(&self, other: &Self) -> bool {
        if let (UseDistance::Finite(l), UseDistance::Finite(r)) = (self, other) {
            l.eq(r)
        } else {
            false
        }
    }
}

impl Eq for UseDistance {}

impl PartialOrd for UseDistance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Finite(l), Self::Finite(r)) => l.partial_cmp(r),
            (Self::Finite(_), Self::Infinite) => Some(Ordering::Less),
            (Self::Infinite, Self::Finite(_)) => Some(Ordering::Greater),
            (Self::Infinite, Self::Infinite) => None,
        }
    }
}

struct GlobalNextUseBuilder<'a> {
    func: &'a FunctionData,
    dfs_tree: &'a passes::DfsTree,
    loop_forest: &'a passes::LoopNestingForest,
    use_in: DenseMap<Block, FxHashMap<Value, UseDistance>>,
    use_out: DenseMap<Block, FxHashMap<Value, UseDistance>>,
}

impl<'a> GlobalNextUseBuilder<'a> {
    fn new(
        func: &'a FunctionData,
        dfs_tree: &'a passes::DfsTree,
        loop_forest: &'a passes::LoopNestingForest,
    ) -> Self {
        Self {
            func,
            dfs_tree,
            loop_forest,
            use_in: DenseMap::with_prefilled(func.blocks.len()),
            use_out: DenseMap::with_prefilled(func.blocks.len()),
        }
    }
}
