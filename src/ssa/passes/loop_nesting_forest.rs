use std::collections::BTreeSet;

use error_stack::{IntoReport, ResultExt};
use rustc_hash::FxHashSet;

use crate::{
    ssa::{
        function::FunctionData,
        passes::{self, Pass, PassDepTracker, PassStore},
        Block,
    },
    utils::{DenseMap, UnionFind, UxoResult},
};

#[derive(Debug, thiserror::Error)]
pub enum LoopForestError {
    #[error("failed a dependency pass for 'loop_nesting_forest'")]
    DependentPassFailure,
    #[error("a block was not found in the forest")]
    ForestBlockNotFound,
    #[error("there was an error when interacting with the function data")]
    FunctionError,
}

struct LoopNestingForestBuilder<'a> {
    func: &'a FunctionData,
    dfs: &'a passes::DfsTree,
    union_find: UnionFind<Block>,
    forest: DenseMap<Block, Vec<Block>>,
    roots: FxHashSet<Block>,
}

impl<'a> LoopNestingForestBuilder<'a> {
    fn new(func: &'a FunctionData, dfs_tree: &'a passes::DfsTree) -> Self {
        Self {
            func,
            dfs: dfs_tree,
            union_find: UnionFind::new(),
            forest: DenseMap::with_prefilled(func.blocks.len()),
            roots: FxHashSet::default(),
        }
    }

    fn collapse(
        &mut self,
        header: Block,
        body: FxHashSet<Block>,
    ) -> UxoResult<(), LoopForestError> {
        let children_list = self
            .forest
            .get_mut(header)
            .ok_or(LoopForestError::ForestBlockNotFound)
            .into_report()?;

        for loop_block in body {
            children_list.push(loop_block);
            self.union_find.union(header, loop_block);
            self.roots.remove(&loop_block);
        }

        self.roots.insert(header);
        Ok(())
    }

    fn find_loop(&mut self, potential_header: Block) -> UxoResult<(), LoopForestError> {
        let mut loop_body: FxHashSet<Block> = FxHashSet::default();
        let mut worklist: BTreeSet<Block> = self
            .dfs
            .back_edges
            .iter()
            .filter(|(_, b)| *b == potential_header)
            .map(|(b, _)| self.union_find.find(*b))
            .collect();
        worklist.remove(&potential_header);

        while let Some(block) = worklist.pop_first() {
            loop_body.insert(block);

            let preds = self
                .func
                .get_block_preds(block)
                .change_context(LoopForestError::FunctionError)?;

            for pred in preds {
                if self.dfs.back_edges.contains(&(pred, block)) {
                    continue;
                }

                let found = self.union_find.find(pred);

                if found != potential_header
                    && !loop_body.contains(&found)
                    && !worklist.contains(&found)
                {
                    worklist.insert(found);
                }
            }
        }

        if !loop_body.is_empty() {
            self.collapse(potential_header, loop_body)?;
        }

        Ok(())
    }

    fn build(mut self) -> UxoResult<LoopNestingForest, LoopForestError> {
        for potential_header in self.dfs.postorder.iter().copied() {
            self.find_loop(potential_header)?;
        }

        Ok(LoopNestingForest {
            forest: self.forest,
            top_level: self.roots,
        })
    }
}

pub struct LoopNestingForest {
    pub forest: DenseMap<Block, Vec<Block>>,
    pub top_level: FxHashSet<Block>,
}

impl Pass for LoopNestingForest {
    type Error = LoopForestError;

    fn name() -> String {
        "loop_nesting_forest".to_string()
    }

    fn dependencies(mut tracker: PassDepTracker) -> crate::utils::UxoResult<(), Self::Error> {
        tracker
            .dependency::<passes::DfsTree>()
            .change_context(LoopForestError::DependentPassFailure)?;
        Ok(())
    }

    fn execute(store: PassStore) -> crate::utils::UxoResult<Self, Self::Error> {
        let dfs_tree = store
            .get_pass::<passes::DfsTree>()
            .change_context(LoopForestError::DependentPassFailure)?;
        let func = store.get_func();

        let dfs_tree = dfs_tree.borrow();
        let builder = LoopNestingForestBuilder::new(func, &dfs_tree);
        builder.build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ssa::passes::test_utils,
        utils::EntityId,
    };

    fn verify_loop_body(found: &FxHashSet<Block>, actual: &FxHashSet<Block>) {
        assert_eq!(
            found.len(),
            actual.len(),
            "invalid number of loop body nodes found",
        );

        let diff: FxHashSet<Block> = actual.symmetric_difference(found).copied().collect();

        assert!(
            diff.is_empty(),
            "difference between actual and found loop bodies"
        );
    }

    #[test]
    fn test_single_loop() {
        let mut func_data = test_utils::get_function_with_single_loop();
        let forest = test_utils::get_pass::<LoopNestingForest>(&mut func_data);
        let forest = forest.borrow();

        let block_1 = Block::with_id(1);
        let succs: FxHashSet<Block> = forest
            .forest
            .get(block_1)
            .expect("block_1 preds")
            .iter()
            .copied()
            .collect();

        let actual_succs = FxHashSet::from_iter([
            Block::with_id(3),
            Block::with_id(4),
            Block::with_id(5),
            Block::with_id(6),
        ]);

        verify_loop_body(&succs, &actual_succs);

        println!("FOREST NODES:\n");
        for (node, succs) in forest.forest.iter() {
            let block_strs: Vec<String> = succs.iter().map(|b| b.to_string()).collect();

            println!("{node} -> [{}]", block_strs.join(", "));
        }
    }

    #[test]
    fn test_nested_loop() {
        let mut func = test_utils::get_function_with_nested_loops();
        println!("NESTED LOOPS FUNC:\n{}", func.print().expect("print func"));
        let forest = test_utils::get_pass::<LoopNestingForest>(&mut func);
        let forest = forest.borrow();

        verify_loop_body(
            &forest
                .forest
                .get(Block::with_id(1))
                .expect("block_1 preds")
                .iter()
                .copied()
                .collect(),
            &FxHashSet::from_iter([Block::with_id(3), Block::with_id(4), Block::with_id(5)]),
        );

        verify_loop_body(
            &forest
                .forest
                .get(Block::with_id(4))
                .expect("block_1 preds")
                .iter()
                .copied()
                .collect(),
            &FxHashSet::from_iter([
                Block::with_id(6),
                Block::with_id(7),
                Block::with_id(8),
                Block::with_id(9),
            ]),
        );

        println!("ROOTS: {:?}", forest.top_level);
        println!("FOREST NODES:\n");
        for (node, succs) in forest.forest.iter() {
            let block_strs: Vec<String> = succs.iter().map(|b| b.to_string()).collect();

            println!("{node} -> [{}]", block_strs.join(", "));
        }
    }
}
