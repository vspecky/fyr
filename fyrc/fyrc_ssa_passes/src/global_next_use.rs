use std::{cmp::Ordering, collections::VecDeque, ops::Add};

use error_stack::{report, ResultExt};
use fyrc_ssa::{block::Block, function::FunctionData, value::Value};
use fyrc_utils::DenseMap;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::error::PassResult;

const LOOP_EDGE_LENGTH: usize = 1_000_000;

#[derive(Debug, thiserror::Error)]
pub enum GlobalNextUseError {
    #[error("failed a dependent pass of 'global_next_use'")]
    DependentPassFailure,
    #[error("there was an error when interacting with the function data")]
    FunctionError,
    #[error("value distance map not found for a block")]
    DistanceMapNotFound,
    #[error("live out set was not found for a block")]
    LiveOutSetNotFound,
}

#[derive(Debug, Clone, Copy)]
pub enum Distance {
    Finite(usize),
    Infinite,
}

impl PartialEq for Distance {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Finite(l), Self::Finite(r)) => l.eq(&r),
            (Self::Finite(_), Self::Infinite) | (Self::Infinite, Self::Finite(_)) => false,
            (Self::Infinite, Self::Infinite) => true,
        }
    }
}

impl Eq for Distance {}

impl PartialOrd for Distance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Finite(l), Self::Finite(r)) => l.partial_cmp(&r),
            (Self::Finite(_), Self::Infinite) => Some(Ordering::Less),
            (Self::Infinite, Self::Finite(_)) => Some(Ordering::Greater),
            (Self::Infinite, Self::Infinite) => Some(Ordering::Equal),
        }
    }
}

impl Ord for Distance {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect("Distance cmp")
    }
}

impl Add for Distance {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Finite(l), Self::Finite(r)) => Self::Finite(l + r),
            (Self::Finite(_), Self::Infinite)
            | (Self::Infinite, Self::Finite(_))
            | (Self::Infinite, Self::Infinite) => Self::Infinite,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValueDistanceMap(FxHashMap<Value, Distance>);

impl ValueDistanceMap {
    fn new() -> Self {
        Self(FxHashMap::default())
    }

    #[inline]
    fn insert(&mut self, val: Value, d: Distance) {
        self.0.insert(val, d);
    }

    #[inline]
    fn remove(&mut self, val: &Value) -> Option<Distance> {
        self.0.remove(val)
    }

    fn merge(&mut self, val: Value, d: Distance) -> bool {
        if let Some(orig_distance) = self.0.get(&val).copied() {
            if d < orig_distance {
                self.0.insert(val, d);
                true
            } else {
                false
            }
        } else {
            self.0.insert(val, d);
            true
        }
    }

    pub fn get(&self, val: &Value) -> Distance {
        self.0.get(val).copied().unwrap_or(Distance::Infinite)
    }

    #[inline]
    fn iter(&self) -> impl Iterator<Item = (&Value, &Distance)> {
        self.0.iter()
    }
}

impl Default for ValueDistanceMap {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl FromIterator<Value> for ValueDistanceMap {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self(
            iter.into_iter()
                .map(|val| (val, Distance::Infinite))
                .collect(),
        )
    }
}

impl FromIterator<(Value, Distance)> for ValueDistanceMap {
    fn from_iter<T: IntoIterator<Item = (Value, Distance)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl IntoIterator for ValueDistanceMap {
    type Item = (Value, Distance);
    type IntoIter = <FxHashMap<Value, Distance> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

fn find_loop_out_edges(
    func: &FunctionData,
    loop_forest: &crate::LoopNestingForest,
) -> PassResult<FxHashSet<(Block, Block)>, GlobalNextUseError> {
    let mut loop_out_edges = FxHashSet::default();
    for (header, children) in loop_forest.forest.iter() {
        if children.is_empty() {
            continue;
        }

        let header_succs = func
            .get_block_succs(header)
            .change_context(GlobalNextUseError::FunctionError)?;

        for header_succ in header_succs {
            if !children.contains(&header_succ) {
                loop_out_edges.insert((header, header_succ));
            }
        }

        for &child in children {
            let child_succs = func
                .get_block_succs(child)
                .change_context(GlobalNextUseError::FunctionError)?;

            for child_succ in child_succs {
                if !children.contains(&child_succ) && child_succ != header {
                    loop_out_edges.insert((child, child_succ));
                }
            }
        }
    }

    Ok(loop_out_edges)
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
            .dependency::<crate::DfsTree>()
            .change_context(GlobalNextUseError::DependentPassFailure)?;

        tracker
            .dependency::<crate::LoopNestingForest>()
            .change_context(GlobalNextUseError::DependentPassFailure)?;

        tracker
            .dependency::<crate::CssaTranslation>()
            .change_context(GlobalNextUseError::DependentPassFailure)?;

        Ok(())
    }

    fn execute(store: crate::PassStore) -> PassResult<Self, Self::Error> {
        let func = store.get_func();
        let liveness = store
            .get_pass::<crate::LivenessAnalysis>()
            .change_context(GlobalNextUseError::DependentPassFailure)?;
        let dfs_tree = store
            .get_pass::<crate::DfsTree>()
            .change_context(GlobalNextUseError::DependentPassFailure)?;
        let loop_forest = store
            .get_pass::<crate::LoopNestingForest>()
            .change_context(GlobalNextUseError::DependentPassFailure)?;

        let loop_out_edges = find_loop_out_edges(&func, &loop_forest)?;

        let mut head = liveness
            .live_in
            .values()
            .map(|set| set.iter().copied().collect())
            .collect::<DenseMap<Block, ValueDistanceMap>>();
        let mut tail = liveness
            .live_out
            .values()
            .map(|set| set.iter().copied().collect())
            .collect::<DenseMap<Block, ValueDistanceMap>>();

        for block in func.blocks.keys() {
            let succ_phi_uses = func
                .get_succ_phi_uses(block)
                .change_context(GlobalNextUseError::FunctionError)?;

            let tail_set = tail
                .get_mut(block)
                .ok_or_else(|| report!(GlobalNextUseError::DistanceMapNotFound))?;

            for succ_use in succ_phi_uses {
                tail_set.merge(succ_use, Distance::Finite(0));
            }
        }

        let mut workset = FxHashSet::from_iter(dfs_tree.postorder.iter().copied());
        let mut worklist = VecDeque::from_iter(dfs_tree.postorder.iter().copied());

        while let Some(block) = worklist.pop_front() {
            workset.remove(&block);
            let mut mutated = false;

            let tail_set = tail
                .get_mut(block)
                .ok_or_else(|| report!(GlobalNextUseError::DistanceMapNotFound))?;

            let succ_head_sets = func
                .get_block_succs(block)
                .change_context(GlobalNextUseError::FunctionError)?
                .into_iter()
                .map(|succ| head.get(succ).map(|dmap| (succ, dmap)))
                .collect::<Option<Vec<_>>>()
                .ok_or_else(|| report!(GlobalNextUseError::DistanceMapNotFound))?;

            let live_out = liveness
                .get_live_out(block)
                .change_context(GlobalNextUseError::LiveOutSetNotFound)?;

            for &val in live_out {
                let incoming_distance = succ_head_sets
                    .iter()
                    .map(|(succ, dmap)| {
                        dmap.get(&val)
                            + loop_out_edges
                                .contains(&(block, *succ))
                                .then(|| Distance::Finite(LOOP_EDGE_LENGTH))
                                .unwrap_or(Distance::Finite(0))
                    })
                    .min()
                    .unwrap_or(Distance::Infinite);

                mutated |= tail_set.merge(val, incoming_distance);
            }

            let block_data = func
                .get_block(block)
                .change_context(GlobalNextUseError::FunctionError)?;

            let mut running_map = tail_set.clone();
            running_map
                .0
                .values_mut()
                .for_each(|d| *d = *d + Distance::Finite(block_data.instrs.len()));

            for (distance_from_start, instr) in block_data
                .instrs
                .iter()
                .copied()
                .enumerate()
                .map(|(d, i)| (d + 1, i))
                .rev()
            {
                let instr_uses = func
                    .get_instr_uses(instr)
                    .change_context(GlobalNextUseError::FunctionError)?;

                for instr_use in instr_uses {
                    running_map.insert(instr_use, Distance::Finite(distance_from_start));
                }

                if let Some(def) = func.get_instr_def(instr) {
                    running_map.remove(&def);
                }
            }

            let head_set = head
                .get_mut(block)
                .ok_or_else(|| report!(GlobalNextUseError::DistanceMapNotFound))?;

            for (val, distance) in running_map {
                mutated |= head_set.merge(val, distance);
            }

            if mutated {
                let preds = func
                    .get_block_preds(block)
                    .change_context(GlobalNextUseError::FunctionError)?;

                for pred in preds {
                    if !workset.contains(&pred) {
                        workset.insert(pred);
                        worklist.push_back(pred);
                    }
                }
            }
        }

        Ok(Self { head, tail })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_utils;

    #[test]
    fn test_single_loop() {
        let func_data = test_utils::get_function_with_single_loop();
        let (next_use, func_data) = test_utils::get_pass::<GlobalNextUse>(func_data);

        println!("FUNC:\n{}\n", func_data.viz().expect("function viz"));

        for ((block, head), (_, tail)) in next_use.head.into_iter().zip(next_use.tail.into_iter()) {
            println!("{block:?} | {head:?} | {tail:?}");
        }
    }

    #[test]
    fn test_nested_loop() {
        let func_data = test_utils::get_function_with_nested_loops();
        let (next_use, func_data) = test_utils::get_pass::<GlobalNextUse>(func_data);

        println!("FUNC:\n{}\n", func_data.viz().expect("function viz"));

        for ((block, head), (_, tail)) in next_use.head.into_iter().zip(next_use.tail.into_iter()) {
            println!("{block:?} | {head:?} | {tail:?}");
        }
    }
}
