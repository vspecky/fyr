use std::cmp::Ordering;

use error_stack::ResultExt;
use fyrc_ssa::{
    block::Block,
    value::Value
};
use fyrc_utils::DenseMap;
use rustc_hash::FxHashMap;

use crate::error::PassResult;

#[derive(Debug, thiserror::Error)]
pub enum GlobalNextUseError {
    #[error("failed a dependent pass of 'global_next_use'")]
    DependentPassFailure
}

#[derive(Debug, Clone, Copy)]
pub enum Distance {
    Finite(u64),
    Infinite,
}

impl PartialEq for Distance {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Finite(l), Self::Finite(r)) => l.eq(&r),
            (Self::Finite(_), Self::Infinite)
                | (Self::Infinite, Self::Finite(_))
                | (Self::Infinite, Self::Infinite) => false,
        }
    }
}

impl PartialOrd for Distance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Finite(l), Self::Finite(r)) => l.partial_cmp(&r),
            (Self::Finite(_), Self::Infinite) => Some(Ordering::Less),
            (Self::Infinite, Self::Finite(_)) => Some(Ordering::Greater),
            (Self::Infinite, Self::Infinite) => None,
        }
    }
}

pub struct ValueDistanceMap(FxHashMap<Value, Distance>);

impl ValueDistanceMap {
    fn new() -> Self {
        Self(FxHashMap::default())
    }

    fn insert(&mut self, val: Value, d: Distance) {
        self.0.insert(val, d);
    }

    pub fn get(&self, val: &Value) -> Distance {
        self.0.get(val).copied().unwrap_or(Distance::Infinite)
    }
}

impl Default for ValueDistanceMap {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

pub struct GlobalNextUse {
    head: DenseMap<Block, ValueDistanceMap>,
    tail: DenseMap<Block, ValueDistanceMap>,
}

impl crate::Pass for GlobalNextUse {
    type Error = GlobalNextUseError;

    fn name() -> String {
        "global_next_use".to_string()
    }

    fn dependencies(mut tracker: crate::PassDepTracker) -> PassResult<(), Self::Error> {
        tracker
            .dependency::<crate::LivenessAnalysis>()
            .change_context(GlobalNextUseError::DependentPassFailure)?;

        tracker
            .dependency::<crate::CssaTranslation>()
            .change_context(GlobalNextUseError::DependentPassFailure)?;

        Ok(())
    }

    fn execute(store: crate::PassStore) -> PassResult<Self, Self::Error> {
        let func = store.get_func();
        let liveness = store.get_pass::<crate::LivenessAnalysis>()
            .change_context(GlobalNextUseError::DependentPassFailure)?;

        let mut head = DenseMap::with_prefilled(func.blocks.len());
        let mut tail = DenseMap::with_prefilled(func.blocks.len());

        Ok(Self { head, tail })
    }
}
