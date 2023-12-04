use error_stack::{report, ResultExt};
use fxhash::FxHashSet;

use fyrc_ssa::{block::Block, function::FunctionData, instr::Instr, value::Value};
use fyrc_utils::{BoolExt, DenseMap};

use crate::error::PassResult;

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
    #[error("subsequent use set not found for instruction")]
    SubsequentUseSetNotFound,
}

struct LivenessAnalysisBuilder<'a> {
    func: &'a FunctionData,
    dfs_tree: &'a crate::DfsTree,
    loop_forest: &'a crate::LoopNestingForest,
    live_in: DenseMap<Block, FxHashSet<Value>>,
    live_out: DenseMap<Block, FxHashSet<Value>>,
}

impl<'a> LivenessAnalysisBuilder<'a> {
    fn new(
        func: &'a FunctionData,
        dfs_tree: &'a crate::DfsTree,
        loop_forest: &'a crate::LoopNestingForest,
    ) -> Self {
        Self {
            func,
            dfs_tree,
            loop_forest,
            live_in: DenseMap::with_prefilled(func.blocks.len()),
            live_out: DenseMap::with_prefilled(func.blocks.len()),
        }
    }

    fn propagate_partial_liveness_info(&mut self) -> PassResult<(), LivenessAnalysisError> {
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
                    .ok_or_else(|| report!(LivenessAnalysisError::LiveInNotFound))?;

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
                .or_else_err(|| report!(LivenessAnalysisError::LiveOutNotFound))?;

            let block_data = self
                .func
                .get_block(block)
                .change_context(LivenessAnalysisError::FunctionError)?;

            for instr in block_data.iter_instr_rev() {
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
                .or_else_err(|| report!(LivenessAnalysisError::LiveInNotFound))?;
        }

        Ok(())
    }

    fn propagate_loop_liveness_info(&mut self) -> PassResult<(), LivenessAnalysisError> {
        for root in self.loop_forest.top_level.iter().copied() {
            let mut stack: Vec<Block> = vec![root];

            while let Some(block) = stack.pop() {
                let loop_succs = self
                    .loop_forest
                    .forest
                    .get(block)
                    .ok_or_else(|| report!(LivenessAnalysisError::LoopSuccessorsNotFound))?;

                if loop_succs.is_empty() {
                    continue;
                }

                let live_in = self
                    .live_in
                    .get(block)
                    .ok_or_else(|| report!(LivenessAnalysisError::FunctionError))?;

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
                        .ok_or_else(|| report!(LivenessAnalysisError::LiveInNotFound))?;

                    self.live_in
                        .set(loop_succ, live_in_succ | &live_loop)
                        .or_else_err(|| report!(LivenessAnalysisError::LiveInNotFound))?;

                    let live_out_succ = self
                        .live_out
                        .get(loop_succ)
                        .ok_or_else(|| report!(LivenessAnalysisError::LiveOutNotFound))?;

                    self.live_out
                        .set(loop_succ, live_out_succ | &live_loop)
                        .or_else_err(|| report!(LivenessAnalysisError::LiveOutNotFound))?;

                    stack.push(loop_succ);
                }
            }
        }

        Ok(())
    }

    fn build_subsequent_liveness_sets(
        &mut self,
    ) -> PassResult<DenseMap<Instr, Vec<Value>>, LivenessAnalysisError> {
        let mut subsequent_use_sets: DenseMap<Instr, Vec<Value>> =
            DenseMap::with_prefilled(self.func.instrs.len());

        for block in self.func.blocks.keys() {
            let block_data = self
                .func
                .get_block(block)
                .change_context(LivenessAnalysisError::FunctionError)?;

            let mut running_map = self
                .live_out
                .get(block)
                .ok_or_else(|| report!(LivenessAnalysisError::LiveOutNotFound))?
                .clone();

            for instr in block_data.iter_instr_rev() {
                let uses = self
                    .func
                    .get_instr_uses(instr)
                    .change_context(LivenessAnalysisError::FunctionError)?;

                let subsequent_uses = uses
                    .iter()
                    .copied()
                    .filter(|v| running_map.contains(v))
                    .collect::<Vec<_>>();

                subsequent_use_sets.set(instr, subsequent_uses);

                for instr_use in uses {
                    running_map.insert(instr_use);
                }

                if let Some(def) = self.func.get_instr_def(instr) {
                    running_map.remove(&def);
                }
            }
        }

        Ok(subsequent_use_sets)
    }

    fn build(mut self) -> PassResult<LivenessAnalysis, LivenessAnalysisError> {
        self.propagate_partial_liveness_info()?;
        self.propagate_loop_liveness_info()?;
        let subsequent_use_sets = self.build_subsequent_liveness_sets()?;
        Ok(LivenessAnalysis {
            live_in: self.live_in,
            live_out: self.live_out,
            subsequent_use_sets,
        })
    }
}

pub struct LivenessAnalysis {
    pub live_in: DenseMap<Block, FxHashSet<Value>>,
    pub live_out: DenseMap<Block, FxHashSet<Value>>,
    pub subsequent_use_sets: DenseMap<Instr, Vec<Value>>,
}

impl LivenessAnalysis {
    pub fn get_live_in(
        &self,
        block: Block,
    ) -> PassResult<&FxHashSet<Value>, LivenessAnalysisError> {
        self.live_in
            .get(block)
            .ok_or_else(|| report!(LivenessAnalysisError::LiveOutNotFound))
    }

    pub fn get_live_in_mut(
        &mut self,
        block: Block,
    ) -> PassResult<&mut FxHashSet<Value>, LivenessAnalysisError> {
        self.live_in
            .get_mut(block)
            .ok_or_else(|| report!(LivenessAnalysisError::LiveOutNotFound))
    }

    pub fn get_live_out(
        &self,
        block: Block,
    ) -> PassResult<&FxHashSet<Value>, LivenessAnalysisError> {
        self.live_out
            .get(block)
            .ok_or_else(|| report!(LivenessAnalysisError::LiveOutNotFound))
    }

    pub fn get_live_out_mut(
        &mut self,
        block: Block,
    ) -> PassResult<&mut FxHashSet<Value>, LivenessAnalysisError> {
        self.live_out
            .get_mut(block)
            .ok_or_else(|| report!(LivenessAnalysisError::LiveOutNotFound))
    }

    pub fn get_subsequent_use_set(
        &self,
        instr: Instr,
    ) -> PassResult<&Vec<Value>, LivenessAnalysisError> {
        self.subsequent_use_sets
            .get(instr)
            .ok_or_else(|| report!(LivenessAnalysisError::SubsequentUseSetNotFound))
    }
}

impl crate::Pass for LivenessAnalysis {
    type Error = LivenessAnalysisError;

    fn name() -> String {
        "liveness_analysis".to_string()
    }

    fn dependencies(mut tracker: crate::PassDepTracker) -> PassResult<(), Self::Error> {
        tracker
            .dependency::<crate::DfsTree>()
            .change_context(LivenessAnalysisError::DependentPassFailure)?;
        tracker
            .dependency::<crate::LoopNestingForest>()
            .change_context(LivenessAnalysisError::DependentPassFailure)?;

        Ok(())
    }

    fn execute(store: crate::PassStore) -> PassResult<Self, Self::Error> {
        let dfs_tree = store
            .get_pass::<crate::DfsTree>()
            .change_context(LivenessAnalysisError::DependentPassFailure)?;

        let loop_forest = store
            .get_pass::<crate::LoopNestingForest>()
            .change_context(LivenessAnalysisError::DependentPassFailure)?;

        let func_data = store.get_func();

        let builder = LivenessAnalysisBuilder::new(&func_data, &dfs_tree, &loop_forest);

        builder.build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils;

    #[test]
    fn test_double_loop() {
        let func = test_utils::get_function_with_nested_loops();
        let (liveness, _func) = test_utils::get_pass::<LivenessAnalysis>(func);

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
        let func = test_utils::get_function_with_single_loop();
        let (liveness, func) = test_utils::get_pass::<LivenessAnalysis>(func);

        println!("FUNC:\n{}", func.print().expect("print"));
        println!();
        for block in func.blocks.keys() {
            let livein = liveness.live_in.get(block).unwrap();
            let liveout = liveness.live_out.get(block).unwrap();
            println!("{block} -> {livein:?} | {liveout:?}");
        }
    }
}
