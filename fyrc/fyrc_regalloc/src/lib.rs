pub mod coloring;
pub mod spilling;

use error_stack::ResultExt;
use fyrc_ssa::function::InstructionSet;
use fyrc_ssa_passes::{self as passes, PassManager};

#[doc(hidden)]
pub mod __private {
    pub use fxhash::FxHashMap;
    pub use fyrc_ssa_passes as fsp;
}

#[derive(Debug, thiserror::Error)]
pub enum RegallocError {
    #[error("ssa pass not found")]
    PassNotFound,
    #[error("encountered error during spilling when performing regalloc")]
    SpillingFailure,
    #[error("recalculation of liveness analysis after spilling failed")]
    LivenessRecalcFailed,
    #[error("encountered an error during coloring when performing regalloc")]
    ColoringFailure,
}

pub type RegallocResult<T> = Result<T, error_stack::Report<RegallocError>>;

pub fn perform_regalloc(pass_manager: &PassManager) -> RegallocResult<coloring::RegallocOutput> {
    macro_rules! get_pass {
        ($pass_name:ty) => {
            pass_manager
                .get_pass::<$pass_name>()
                .change_context(RegallocError::PassNotFound)
        };
    }
    let mut func = pass_manager.get_func_mut();
    let gnu = get_pass!(passes::GlobalNextUse)?;
    let mut liveness = pass_manager
        .get_pass_mut::<passes::LivenessAnalysis>()
        .change_context(RegallocError::PassNotFound)?;
    let loop_forest = get_pass!(passes::LoopNestingForest)?;
    let def_use = get_pass!(passes::DefUse)?;
    let dfs_tree = get_pass!(passes::DfsTree)?;
    let dom = get_pass!(passes::DominatorTree)?;
    let max_regs = match func.signature.isa {
        InstructionSet::Thumb => 8,
    };

    let spill_ctx = spilling::SpillProcessCtx::new(
        &mut func,
        &gnu,
        &liveness,
        &loop_forest,
        &def_use,
        &dfs_tree,
        &dom,
        max_regs,
    );

    spilling::perform_spilling(spill_ctx).change_context(RegallocError::SpillingFailure)?;

    let liveness_builder =
        passes::liveness_analysis::LivenessAnalysisBuilder::new(&func, &dfs_tree, &loop_forest);
    *liveness = liveness_builder
        .build()
        .change_context(RegallocError::LivenessRecalcFailed)?;

    let regalloc_output = coloring::perform_coloring(&func, &dom, &liveness)
        .change_context(RegallocError::ColoringFailure)?;

    Ok(regalloc_output)
}
