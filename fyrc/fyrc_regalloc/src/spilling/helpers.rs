//! Helper functions for register spilling

use error_stack::{report, ResultExt};
use fxhash::{FxHashMap, FxHashSet};
use fyrc_ssa::{
    block::Block,
    function::FunctionData,
    instr,
    value::{self, Value},
};
use fyrc_ssa_passes::{GlobalNextUse, LivenessAnalysis, LoopNestingForest};

use crate::spilling::{SpillError, SpillProcessCtx, SpillResult};

pub(super) use loop_pressure::calculate_max_loop_pressure;
pub(super) use loop_uses::calculate_loop_use_sets;

mod loop_uses {
    use super::*;

    fn process_block(
        ctx: &mut SpillProcessCtx<'_>,
        parent_unused_set: &mut FxHashSet<Value>,
        block: Block,
    ) -> SpillResult<()> {
        let live_in = ctx
            .liveness
            .get_live_in(block)
            .change_context(SpillError::LiveInNotFound)?;

        let mut unused_set = live_in.clone();

        let block_data = ctx
            .func
            .get_block(block)
            .change_context(SpillError::FunctionError)?;

        for &phi in &block_data.phis {
            let phi_data = ctx
                .func
                .get_phi_data(phi)
                .change_context(SpillError::FunctionError)?;

            for arg in phi_data.args.values() {
                unused_set.remove(arg);
            }
        }

        for instr in block_data.iter_instr() {
            let instr_uses = ctx
                .func
                .get_instr_uses(instr)
                .change_context(SpillError::FunctionError)?;

            for instr_use in instr_uses {
                unused_set.remove(&instr_use);
            }
        }

        let loop_children = ctx
            .loop_forest
            .forest
            .get(block)
            .ok_or_else(|| report!(SpillError::LoopChildrenNotFound))?;

        let mut is_header = false;
        for child in loop_children {
            process_block(ctx, &mut unused_set, *child)?;

            is_header = true;
        }

        let mine_used = live_in - &unused_set;
        for used in &mine_used {
            parent_unused_set.remove(used);
        }

        if is_header {
            ctx.loop_use_sets.insert(block, mine_used);
        }

        Ok(())
    }

    pub(in crate::spilling) fn calculate_loop_use_sets(
        ctx: &mut SpillProcessCtx<'_>,
    ) -> SpillResult<()> {
        for &block in &ctx.loop_forest.top_level {
            process_block(ctx, &mut FxHashSet::default(), block)?;
        }

        Ok(())
    }
}

mod loop_pressure {
    use super::*;

    fn calculate_block_max_pressure(
        ctx: &mut SpillProcessCtx<'_>,
        block: Block,
    ) -> SpillResult<usize> {
        let mut max_pressure = ctx
            .gnu
            .get_block_reg_pressure(block)
            .ok_or_else(|| report!(SpillError::BlockRegPressureMissing))?;

        let loop_children = ctx
            .loop_forest
            .forest
            .get(block)
            .ok_or_else(|| report!(SpillError::LoopChildrenNotFound))?;

        for &child in loop_children {
            let child_pressure = calculate_block_max_pressure(ctx, child)?;

            max_pressure = std::cmp::max(max_pressure, child_pressure);
        }

        ctx.loop_max_pressures.insert(block, max_pressure);
        Ok(max_pressure)
    }

    pub(in crate::spilling) fn calculate_max_loop_pressure(
        ctx: &mut SpillProcessCtx<'_>,
    ) -> SpillResult<()> {
        for &block in &ctx.loop_forest.top_level {
            calculate_block_max_pressure(ctx, block)?;
        }

        Ok(())
    }
}

/// Creates a spill instruction in a function.
pub(super) fn make_spill_instr(func: &mut FunctionData, val: Value) -> SpillResult<instr::Instr> {
    let val_type = func
        .get_value(val)
        .change_context(SpillError::FunctionError)?
        .value_type;

    let the_instr = func
        .instrs
        .insert(instr::InstrData::SpillValue(instr::SpillValue { val }));

    let spill_result = func.values.insert(value::ValueData {
        value_type: val_type,
        value_kind: value::ValueKind::SpillRes(the_instr),
        is_mem: true,
    });

    func.results.insert(the_instr, spill_result);

    Ok(the_instr)
}

/// Creates a reload instruction in a function.
pub(super) fn make_reload_instr(func: &mut FunctionData, val: Value) -> SpillResult<instr::Instr> {
    let val_type = func
        .get_value(val)
        .change_context(SpillError::FunctionError)?
        .value_type;

    let the_instr = func
        .instrs
        .insert(instr::InstrData::ReloadValue(instr::ReloadValue { val }));

    let reload_result = func.values.insert(value::ValueData {
        value_type: val_type,
        value_kind: value::ValueKind::InstrRes(the_instr),
        is_mem: false,
    });

    func.results.insert(the_instr, reload_result);

    Ok(the_instr)
}
