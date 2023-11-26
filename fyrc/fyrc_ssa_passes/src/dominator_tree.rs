use error_stack::{report, ResultExt};
use fxhash::FxHashSet;
use fyrc_ssa::{block::Block, function::FunctionData};
use fyrc_utils::{BoolExt, DenseMap};

use crate::error::PassResult;

#[derive(Debug, thiserror::Error)]
pub enum DominatorTreeError {
    #[error("failed a dependent pass for 'dominator_tree'")]
    DependentPassFailure,
    #[error("could not find ancestor for node")]
    AncestorNotFound,
    #[error("could not find semi for node")]
    SemiNotFound,
    #[error("could not find label for node")]
    LabelNotFound,
    #[error("could not find block with the given number")]
    VertexNotFound,
    #[error("could not find bucket for the given block")]
    BucketNotFound,
    #[error("there was an error while interacting with the function data")]
    FunctionError,
    #[error("could not get parent for block")]
    ParentNotFound,
    #[error("could not get dominator for block")]
    DomNotFound,
    #[error("dominator block not found when constructing tree")]
    DominatorBlockNotFound,
}

pub struct DominatorTree {
    pub tree: DenseMap<Block, FxHashSet<Block>>,
    pub parent: DenseMap<Block, Block>,
}

struct DominatorTreeBuilder<'a> {
    dfs: &'a crate::DfsTree,
    func: &'a FunctionData,
    dom: DenseMap<Block, Block>,
    bucket: DenseMap<Block, FxHashSet<Block>>,
    semi: DenseMap<Block, usize>,
    ancestor: DenseMap<Block, Option<Block>>,
    label: DenseMap<Block, Block>,
}

impl<'a> DominatorTreeBuilder<'a> {
    fn new(
        dfs: &'a crate::DfsTree,
        func: &'a FunctionData,
    ) -> PassResult<Self, DominatorTreeError> {
        let mut semi = DenseMap::<Block, usize>::with_prefilled(func.blocks.len());

        for (idx, block) in dfs.preorder.iter().copied().enumerate() {
            semi.set(block, idx)
                .or_else_err(|| report!(DominatorTreeError::SemiNotFound))?;
        }

        Ok(Self {
            dfs,
            func,
            dom: DenseMap::with_mirrored(func.blocks.len()),
            bucket: DenseMap::with_prefilled(func.blocks.len()),
            ancestor: DenseMap::with_prefilled(func.blocks.len()),
            label: DenseMap::with_mirrored(func.blocks.len()),
            semi,
        })
    }

    fn get_bucket(&self, bucket: Block) -> PassResult<FxHashSet<Block>, DominatorTreeError> {
        self.bucket
            .get(bucket)
            .cloned()
            .ok_or_else(|| report!(DominatorTreeError::BucketNotFound))
    }

    fn remove_from_bucket(
        &mut self,
        bucket: Block,
        block: Block,
    ) -> PassResult<(), DominatorTreeError> {
        self.bucket
            .get_mut(bucket)
            .ok_or_else(|| report!(DominatorTreeError::BucketNotFound))?
            .remove(&block);

        Ok(())
    }

    fn add_to_bucket(&mut self, bucket: Block, block: Block) -> PassResult<(), DominatorTreeError> {
        self.bucket
            .get_mut(bucket)
            .ok_or_else(|| report!(DominatorTreeError::BucketNotFound))?
            .insert(block);

        Ok(())
    }

    fn get_ancestor(&self, block: Block) -> PassResult<Option<Block>, DominatorTreeError> {
        self.ancestor
            .get(block)
            .copied()
            .ok_or_else(|| report!(DominatorTreeError::AncestorNotFound))
    }

    fn set_ancestor(&mut self, block: Block, anc: Block) -> PassResult<(), DominatorTreeError> {
        self.ancestor
            .set(block, Some(anc))
            .or_else_err(|| report!(DominatorTreeError::AncestorNotFound))
    }

    fn get_label(&self, block: Block) -> PassResult<Block, DominatorTreeError> {
        self.label
            .get(block)
            .copied()
            .ok_or_else(|| report!(DominatorTreeError::LabelNotFound))
    }

    fn set_label(&mut self, block: Block, label: Block) -> PassResult<(), DominatorTreeError> {
        self.label
            .set(block, label)
            .or_else_err(|| report!(DominatorTreeError::LabelNotFound))
    }

    fn get_semi(&self, block: Block) -> PassResult<usize, DominatorTreeError> {
        self.semi
            .get(block)
            .copied()
            .ok_or_else(|| report!(DominatorTreeError::SemiNotFound))
    }

    fn set_semi(&mut self, b: Block, semi: usize) -> PassResult<(), DominatorTreeError> {
        self.semi
            .set(b, semi)
            .or_else_err(|| report!(DominatorTreeError::SemiNotFound))
    }

    fn get_dom(&self, of: Block) -> PassResult<Block, DominatorTreeError> {
        self.dom
            .get(of)
            .copied()
            .ok_or_else(|| report!(DominatorTreeError::DomNotFound))
    }

    fn set_dom(&mut self, block: Block, dom: Block) -> PassResult<(), DominatorTreeError> {
        self.dom
            .set(block, dom)
            .or_else_err(|| report!(DominatorTreeError::DomNotFound))
    }

    fn compress(&mut self, block: Block) -> PassResult<(), DominatorTreeError> {
        let maybe_anc = self.get_ancestor(block)?;
        let maybe_anc_anc = maybe_anc
            .map(|anc_block| self.get_ancestor(anc_block))
            .transpose()?
            .flatten();

        if let Some((anc, anc_anc)) = maybe_anc.zip(maybe_anc_anc) {
            self.compress(anc)?;
            let label = self.get_label(block)?;
            let label_anc = self.get_label(anc)?;
            let semi = self.get_semi(label)?;
            let semi_anc = self.get_semi(label_anc)?;

            if semi_anc < semi {
                self.set_label(block, label_anc)?;
            }

            self.set_ancestor(block, anc_anc)?;
        }
        Ok(())
    }

    #[inline]
    fn eval(&mut self, v: Block) -> PassResult<Block, DominatorTreeError> {
        if self.get_ancestor(v)?.is_some() {
            self.compress(v)?;
            self.get_label(v)
        } else {
            Ok(v)
        }
    }

    fn build(mut self) -> PassResult<DominatorTree, DominatorTreeError> {
        let mut tree: DenseMap<Block, FxHashSet<Block>> =
            DenseMap::with_prefilled(self.func.blocks.len());

        for w in self.dfs.preorder.iter().skip(1).rev().copied() {
            let preds = self
                .func
                .get_block_preds(w)
                .change_context(DominatorTreeError::FunctionError)?;

            for v in preds {
                let u = self.eval(v)?;
                let semi_w = self.get_semi(w)?;
                let semi_u = self.get_semi(u)?;
                if semi_u < semi_w {
                    self.set_semi(w, semi_u)?;
                }
            }

            let semi_w = self.get_semi(w)?;
            let vertex = self
                .dfs
                .preorder
                .get(semi_w)
                .copied()
                .ok_or_else(|| report!(DominatorTreeError::VertexNotFound))?;

            self.add_to_bucket(vertex, w)?;

            let parent_w = self
                .dfs
                .get_parent(w)
                .change_context(DominatorTreeError::ParentNotFound)?;

            self.set_ancestor(w, parent_w)?;

            for v in self.get_bucket(parent_w)? {
                self.remove_from_bucket(parent_w, v)?;
                let u = self.eval(v)?;
                let semi_u = self.get_semi(u)?;
                let semi_v = self.get_semi(v)?;
                self.set_dom(v, if semi_u < semi_v { u } else { parent_w })?;
            }
        }

        for w in self.dfs.preorder.iter().skip(1).copied() {
            let semi_w = self.get_semi(w)?;
            let dom_w = self.get_dom(w)?;
            let vertex = self
                .dfs
                .preorder
                .get(semi_w)
                .copied()
                .ok_or_else(|| report!(DominatorTreeError::VertexNotFound))?;

            if dom_w != vertex {
                self.set_dom(w, self.get_dom(dom_w)?)?;
            }
        }

        for (child, parent) in self.dom.iter() {
            tree.get_mut(*parent)
                .ok_or_else(|| report!(DominatorTreeError::DominatorBlockNotFound))?
                .insert(child);
        }

        for (parent, children) in tree.iter_mut() {
            children.remove(&parent);
        }

        Ok(DominatorTree {
            tree,
            parent: self.dom,
        })
    }
}

impl crate::Pass for DominatorTree {
    type Error = DominatorTreeError;

    fn name() -> String {
        "dominator_tree".to_string()
    }

    fn dependencies(mut tracker: crate::PassDepTracker) -> PassResult<(), Self::Error> {
        tracker
            .dependency::<crate::DfsTree>()
            .change_context(DominatorTreeError::DependentPassFailure)?;
        Ok(())
    }

    fn execute(store: crate::PassStore) -> PassResult<Self, Self::Error> {
        let dfs_tree = store
            .get_pass::<crate::DfsTree>()
            .change_context(DominatorTreeError::DependentPassFailure)?;

        let func = store.get_func();

        let builder = DominatorTreeBuilder::new(&dfs_tree, &func)?;
        builder.build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use fyrc_utils::EntityId;

    use crate::test_utils;

    macro_rules! dom_tree {
        ($size:expr; $($parent:literal -> ($($child:literal),+)),+) => {{
            let mut tree = DenseMap::<Block, FxHashSet<Block>>::with_prefilled($size);

            $(
                tree.set(
                    Block::with_id($parent),
                    FxHashSet::from_iter([
                        $(Block::with_id($child)),+
                    ]),
                );
            )+

            tree
        }};
    }

    fn check_tree(dom: &DominatorTree, actual: DenseMap<Block, FxHashSet<Block>>) {
        assert_eq!(
            dom.tree.len(),
            actual.len(),
            "actual and calculated dominator trees have different lengths"
        );

        for (parent, children) in dom.tree.iter() {
            let actual_children = actual.get(parent).expect("actual children");
            let mut diff = children.symmetric_difference(actual_children);
            assert!(diff.next().is_none(), "wrong dominator tree calculation");
        }
    }

    #[test]
    fn dom_test_single_loop() {
        let func_data = test_utils::get_function_with_single_loop();
        let (tree, func_data) = test_utils::get_pass::<DominatorTree>(func_data);

        println!("FUNC:\n{}", func_data.print().expect("print func"));
        println!();
        for (parent, children) in tree.tree.iter() {
            println!("{parent} -> {children:?}");
        }

        check_tree(
            &tree,
            dom_tree!(
                func_data.blocks.len();
                0 -> (1),
                1 -> (2, 3),
                3 -> (4, 5, 6)
            ),
        );
    }

    #[test]
    fn dom_test_double_loop() {
        let func_data = test_utils::get_function_with_nested_loops();
        let (tree, func_data) = test_utils::get_pass::<DominatorTree>(func_data);

        println!("FUNC:\n{}", func_data.print().expect("print func"));
        println!();
        for (parent, children) in tree.tree.iter() {
            println!("{parent} -> {children:?}");
        }

        check_tree(
            &tree,
            dom_tree!(
                func_data.blocks.len();
                0 -> (1),
                1 -> (2, 3),
                3 -> (4),
                4 -> (5, 6),
                6 -> (7, 8, 9)
            ),
        );
    }
}
