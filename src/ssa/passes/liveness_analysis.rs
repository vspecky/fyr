use error_stack::{IntoReport, ResultExt};
use rustc_hash::FxHashSet;

use crate::{
    ssa::{function::FunctionData, passes, Block, Value},
    utils::{BoolExt, DenseMap, UxoResult},
};

#[derive(Debug, thiserror::Error)]
pub enum LivenessAnalysisError {
    #[error("failed a dependency pass for 'liveness_analysis'")]
    DependentPassFailure,
    #[error("there was an error interacting with the function data")]
    FunctionError,
    #[error("live in set not found for block")]
    LiveInNotFound,
    #[error("live out set not found for block")]
    LiveOutNotFound,
    #[error("loop forest successors not found for block")]
    LoopSuccessorsNotFound,
}

struct LivenessAnalysisBuilder<'a> {
    func: &'a FunctionData,
    dfs_tree: &'a passes::DfsTree,
    loop_forest: &'a passes::LoopNestingForest,
    live_in: DenseMap<Block, FxHashSet<Value>>,
    live_out: DenseMap<Block, FxHashSet<Value>>,
}

impl<'a> LivenessAnalysisBuilder<'a> {
    fn new(
        func: &'a FunctionData,
        dfs_tree: &'a passes::DfsTree,
        loop_forest: &'a passes::LoopNestingForest,
    ) -> Self {
        Self {
            func,
            dfs_tree,
            loop_forest,
            live_in: DenseMap::with_prefilled(func.blocks.len()),
            live_out: DenseMap::with_prefilled(func.blocks.len()),
        }
    }

    fn propagate_partial_liveness_info(&mut self) -> UxoResult<(), LivenessAnalysisError> {
        for block in self.dfs_tree.postorder.iter().copied() {
            let phi_uses = self
                .func
                .get_succ_phi_uses(block)
                .change_context(LivenessAnalysisError::FunctionError)?;

            let mut live = FxHashSet::from_iter(phi_uses.into_iter());

            let succ_blocks = self
                .func
                .get_block_succs(block)
                .change_context(LivenessAnalysisError::FunctionError)?;

            for succ in succ_blocks {
                if self.dfs_tree.is_back_edge((block, succ)) {
                    continue;
                }

                let live_in_succ = self
                    .live_in
                    .get(succ)
                    .ok_or(LivenessAnalysisError::LiveInNotFound)
                    .into_report()?;

                let phi_defs: FxHashSet<Value> = self
                    .func
                    .get_phi_defs(succ)
                    .change_context(LivenessAnalysisError::FunctionError)?
                    .into_iter()
                    .collect();

                live = &live | &(live_in_succ - &phi_defs);
            }

            self.live_out
                .set(block, live.clone())
                .or_err(LivenessAnalysisError::LiveOutNotFound)
                .into_report()?;

            let block_data = self
                .func
                .get_block(block)
                .change_context(LivenessAnalysisError::FunctionError)?;

            for instr in block_data.instrs.iter().rev().copied() {
                if let Some(ref def) = self.func.get_instr_def(instr) {
                    live.remove(def);
                }

                let uses = self
                    .func
                    .get_instr_uses(instr)
                    .change_context(LivenessAnalysisError::FunctionError)?;

                for use_val in uses {
                    live.insert(use_val);
                }
            }

            let phi_defs = self
                .func
                .get_phi_defs(block)
                .change_context(LivenessAnalysisError::FunctionError)?;

            for def in phi_defs {
                live.insert(def);
            }

            self.live_in
                .set(block, live)
                .or_err(LivenessAnalysisError::LiveInNotFound)
                .into_report()?;
        }

        Ok(())
    }

    fn propagate_loop_liveness_info(&mut self) -> UxoResult<(), LivenessAnalysisError> {
        for root in self.loop_forest.top_level.iter().copied() {
            let mut stack: Vec<Block> = vec![root];

            while let Some(block) = stack.pop() {
                let loop_succs = self
                    .loop_forest
                    .forest
                    .get(block)
                    .ok_or(LivenessAnalysisError::LoopSuccessorsNotFound)
                    .into_report()?;

                if loop_succs.is_empty() {
                    continue;
                }

                let live_in = self
                    .live_in
                    .get(block)
                    .ok_or(LivenessAnalysisError::FunctionError)
                    .into_report()?;

                let phi_defs: FxHashSet<Value> = self
                    .func
                    .get_phi_defs(block)
                    .change_context(LivenessAnalysisError::FunctionError)?
                    .into_iter()
                    .collect();

                let live_loop = live_in - &phi_defs;

                for loop_succ in loop_succs.iter().copied() {
                    let live_in_succ = self
                        .live_in
                        .get(loop_succ)
                        .ok_or(LivenessAnalysisError::LiveInNotFound)
                        .into_report()?;

                    self.live_in
                        .set(loop_succ, live_in_succ | &live_loop)
                        .or_err(LivenessAnalysisError::LiveInNotFound)
                        .into_report()?;

                    let live_out_succ = self
                        .live_out
                        .get(loop_succ)
                        .ok_or(LivenessAnalysisError::LiveOutNotFound)
                        .into_report()?;

                    self.live_out
                        .set(loop_succ, live_out_succ | &live_loop)
                        .or_err(LivenessAnalysisError::LiveOutNotFound)
                        .into_report()?;

                    stack.push(loop_succ);
                }
            }
        }

        Ok(())
    }

    fn build(mut self) -> UxoResult<LivenessAnalysis, LivenessAnalysisError> {
        self.propagate_partial_liveness_info()?;
        self.propagate_loop_liveness_info()?;
        Ok(LivenessAnalysis {
            live_in: self.live_in,
            live_out: self.live_out,
        })
    }
}

pub struct LivenessAnalysis {
    pub live_in: DenseMap<Block, FxHashSet<Value>>,
    pub live_out: DenseMap<Block, FxHashSet<Value>>,
}

impl LivenessAnalysis {
    pub fn get_live_out(
        &self,
        block: Block,
    ) -> UxoResult<&FxHashSet<Value>, LivenessAnalysisError> {
        self.live_out
            .get(block)
            .ok_or(LivenessAnalysisError::LiveOutNotFound)
            .into_report()
    }
}

impl passes::Pass for LivenessAnalysis {
    type Error = LivenessAnalysisError;

    fn name() -> String {
        "liveness_analysis".to_string()
    }

    fn dependencies(mut tracker: passes::PassDepTracker) -> UxoResult<(), Self::Error> {
        tracker
            .dependency::<passes::DfsTree>()
            .change_context(LivenessAnalysisError::DependentPassFailure)?;
        tracker
            .dependency::<passes::LoopNestingForest>()
            .change_context(LivenessAnalysisError::DependentPassFailure)?;

        Ok(())
    }

    fn execute(store: passes::PassStore) -> UxoResult<Self, Self::Error> {
        let dfs_tree = store
            .get_pass::<passes::DfsTree>()
            .change_context(LivenessAnalysisError::DependentPassFailure)?;
        let dfs_tree = dfs_tree.borrow();

        let loop_forest = store
            .get_pass::<passes::LoopNestingForest>()
            .change_context(LivenessAnalysisError::DependentPassFailure)?;
        let loop_forest = loop_forest.borrow();

        let builder = LivenessAnalysisBuilder::new(store.get_func(), &dfs_tree, &loop_forest);

        builder.build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ssa::passes::test_utils;

    #[test]
    fn test_double_loop() {
        let mut func = test_utils::get_function_with_nested_loops();
        let liveness = test_utils::get_pass::<LivenessAnalysis>(&mut func);
        let liveness = liveness.borrow();

        println!("LiveIn:\n");
        for (block, set) in liveness.live_in.iter() {
            println!("{block} -> {:?}", set);
        }

        println!("\nLiveOut:\n");
        for (block, set) in liveness.live_out.iter() {
            println!("{block} -> {:?}", set);
        }
    }

    #[test]
    fn print_liveness() {
        let mut func = test_utils::get_function_with_single_loop();
        let liveness = test_utils::get_pass::<LivenessAnalysis>(&mut func);
        let liveness = liveness.borrow();
        println!("FUNC:\n{}", func.print().expect("print"));
        println!();
        for block in func.blocks.keys() {
            let livein = liveness.live_in.get(block).unwrap();
            let liveout = liveness.live_out.get(block).unwrap();
            println!("{block} -> {livein:?} | {liveout:?}");
        }
    }
}
