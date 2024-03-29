use std::cell::Ref;

use error_stack::{report, ResultExt};
use fxhash::{FxHashMap, FxHashSet};

use fyrc_ssa::{block::Block, function::FunctionData};
use fyrc_utils::{DenseMap, EntityId};

use crate::error::PassResult;

#[derive(Debug, thiserror::Error)]
pub enum DfsTreeError {
    #[error("dfs children of block not found")]
    ChildrenNotFound,
    #[error("a dependency pass failed for 'dfs_tree'")]
    DependentPassFailed,
    #[error("there was an error when interacting with the function data")]
    FunctionError,
    #[error("block not found in result map")]
    ResBlockNotFound,
    #[error("there was an inexplicable failure")]
    InexplicableFailure,
    #[error("could not find parent of block")]
    ParentNotFound,
}

struct BlockIndexVec {
    indices: FxHashMap<Block, usize>,
    block_vec: Vec<Block>,
}

impl BlockIndexVec {
    fn new() -> Self {
        Self {
            indices: FxHashMap::default(),
            block_vec: Vec::new(),
        }
    }

    fn push(&mut self, block: Block) -> bool {
        if self.indices.contains_key(&block) {
            return false;
        }

        let idx = self.block_vec.len();
        self.indices.insert(block, idx);
        self.block_vec.push(block);
        true
    }

    fn truncate_until(&mut self, block: Block) -> bool {
        if let Some(idx) = self.indices.get(&block).copied() {
            for further_block in self.block_vec.iter().skip(idx + 1) {
                self.indices.remove(further_block);
            }
            self.block_vec.truncate(idx + 1);
            true
        } else {
            false
        }
    }

    fn contains(&self, block: Block) -> bool {
        self.indices.contains_key(&block)
    }
}

fn postorder(
    tree: &DenseMap<Block, Vec<Block>>,
    order: &mut Vec<Block>,
    block: Block,
) -> PassResult<(), DfsTreeError> {
    let children = tree
        .get(block)
        .ok_or_else(|| report!(DfsTreeError::ChildrenNotFound))?;

    for &child in children {
        postorder(tree, order, child)?;
    }

    order.push(block);
    Ok(())
}

pub struct DfsTree {
    pub tree: DenseMap<Block, Vec<Block>>,
    pub back_edges: FxHashSet<(Block, Block)>,
    pub parents: DenseMap<Block, Block>,
    pub preorder: Vec<Block>,
    pub postorder: Vec<Block>,
}

impl DfsTree {
    fn from_function(func: Ref<FunctionData>) -> PassResult<Self, DfsTreeError> {
        let mut res_map: DenseMap<Block, Vec<Block>> = DenseMap::with_prefilled(func.blocks.len());

        let first_block = Block::with_id(0);
        let mut stack: Vec<(Block, Block)> = Vec::new();
        let mut visited: FxHashSet<Block> = FxHashSet::default();
        let mut parents: DenseMap<Block, Block> = DenseMap::with_prefilled(func.blocks.len());
        let mut idx_map = BlockIndexVec::new();
        let mut back_edges: FxHashSet<(Block, Block)> = FxHashSet::default();
        visited.insert(first_block);
        idx_map.push(first_block);

        for succ in func
            .get_block_succs(first_block)
            .change_context(DfsTreeError::FunctionError)
            .attach_printable("when getting first block successors")?
        {
            stack.push((first_block, succ));
        }

        while let Some((pred, next_block)) = stack.pop() {
            idx_map.truncate_until(pred);
            if visited.contains(&next_block) {
                if idx_map.contains(next_block) {
                    back_edges.insert((pred, next_block));
                }
                continue;
            }
            parents.set(next_block, pred);
            visited.insert(next_block);
            idx_map.push(next_block);

            res_map
                .get_mut(pred)
                .ok_or_else(|| report!(DfsTreeError::ResBlockNotFound))?
                .push(next_block);

            for succ in func
                .get_block_succs(next_block)
                .change_context(DfsTreeError::FunctionError)
                .attach_printable("when getting next_block successors")?
            {
                if !visited.contains(&succ) {
                    stack.push((next_block, succ));
                } else if idx_map.contains(succ) {
                    back_edges.insert((next_block, succ));
                }
            }
        }

        let mut preorder: Vec<Block> = Vec::with_capacity(res_map.len());
        let mut preorder_stack: Vec<Block> = vec![Block::with_id(0)];

        while let Some(block) = preorder_stack.pop() {
            preorder.push(block);
            let children = res_map
                .get(block)
                .ok_or_else(|| report!(DfsTreeError::ResBlockNotFound))?;

            for child in children.iter().rev().copied() {
                preorder_stack.push(child);
            }
        }

        let mut post_order = Vec::new();
        postorder(&res_map, &mut post_order, Block::START)?;

        Ok(Self {
            tree: res_map,
            parents,
            back_edges,
            preorder,
            postorder: post_order,
        })
    }

    #[inline]
    pub fn is_back_edge(&self, edge: (Block, Block)) -> bool {
        self.back_edges.contains(&edge)
    }

    pub fn get_parent(&self, block: Block) -> PassResult<Block, DfsTreeError> {
        self.parents
            .get(block)
            .copied()
            .ok_or_else(|| report!(DfsTreeError::ParentNotFound))
    }
}

impl crate::Pass for DfsTree {
    type Error = DfsTreeError;

    fn name() -> String {
        "dfs_tree".to_string()
    }

    fn dependencies(mut tracker: crate::PassDepTracker) -> PassResult<(), Self::Error> {
        tracker
            .dependency::<crate::CriticalEdgeRemoval>()
            .change_context(DfsTreeError::DependentPassFailed)?;

        Ok(())
    }

    fn execute(store: crate::PassStore) -> PassResult<Self, Self::Error> {
        DfsTree::from_function(store.get_func())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils;

    #[test]
    fn test_valid_dfs_tree() {
        let func_data = test_utils::get_function_with_single_loop();
        let (dfs_tree, func_data) = test_utils::get_pass::<DfsTree>(func_data);

        let postorder_set: FxHashSet<Block> = dfs_tree.postorder.iter().copied().collect();

        let func_blocks: FxHashSet<Block> = func_data.blocks.keys().collect();

        assert_eq!(
            postorder_set.len(),
            func_blocks.len(),
            "postorder set and total blocks have differing lengths",
        );

        let diff: FxHashSet<Block> = postorder_set
            .symmetric_difference(&func_blocks)
            .copied()
            .collect();

        assert_eq!(
            diff.len(),
            0,
            "difference between nodes of func blocks and postorder set",
        );

        let mut visited: FxHashSet<Block> = FxHashSet::default();
        let mut stack: Vec<(Block, Block)> = Vec::new();
        let first_block = Block::with_id(0);
        visited.insert(first_block);

        for succ in dfs_tree.tree.get(first_block).expect("first succs") {
            stack.push((first_block, *succ));
        }

        while let Some((pred, next_block)) = stack.pop() {
            assert!(
                !visited.contains(&next_block),
                "block visited twice, dfs tree is not a tree"
            );
            visited.insert(next_block);

            let cfg_preds = func_data.get_block_preds(next_block).expect("cfg preds");

            assert!(
                cfg_preds.contains(&pred),
                "edge in dfs tree doesn't exist in cfg",
            );

            for succ in dfs_tree.tree.get(next_block).expect("next succs") {
                stack.push((next_block, *succ));
            }
        }

        println!(
            "THE PROGRAM:\n{}",
            func_data.print().expect("print program")
        );

        println!("\nDFS Tree:\n");
        for (node, succs) in dfs_tree.tree.iter() {
            let block_strs: Vec<String> = succs.iter().map(|b| b.to_string()).collect();

            println!("{node} -> [{}]", block_strs.join(", "));
        }

        println!("\nBACKEDGES:\n");
        for (from, to) in dfs_tree.back_edges.iter() {
            println!("{from} -> {to}");
        }

        let postorder_str: Vec<String> = dfs_tree.postorder.iter().map(|b| b.to_string()).collect();

        println!("\nPOSTORDER: [{}]", postorder_str.join(", "));
    }
}
