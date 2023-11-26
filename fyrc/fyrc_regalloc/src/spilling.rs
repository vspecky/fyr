//! Register Spilling & Live-Range Splitting
//!
//! This module contains the logic to insert spills and reloads to the function to split
//! live ranges of values and make the interference graph K-colorable (where K = 8 for Thumb
//! functions and 16 for ARM functions). To decide what values to spill, We make use of a
//! heuristic that extends Belady's MIN Algorithm to work on Control Flow Graphs, put forward
//! by Braun & Hack
//! in [this paper](https://www.researchgate.net/publication/221302742_Register_Spilling_and_Live-Range_Splitting_for_SSA-Form_Programs).

mod helpers;

use error_stack::{report, ResultExt};
use fxhash::{FxHashMap, FxHashSet};
use fyrc_ssa::{
    function::FunctionData,
    instr,
    value::{ValueData, ValueKind},
    Block, Value,
};
use fyrc_ssa_passes::{
    global_next_use::Distance, DfsTree, GlobalNextUse, LivenessAnalysis, LoopNestingForest,
};
use fyrc_utils::DenseMap;

#[derive(Debug, thiserror::Error)]
pub enum SpillError {
    #[error("there was an error when interacting with the function data")]
    FunctionError,
    #[error("the block end register set is missing for a block")]
    BlockEndRegSetMissing,
    #[error("the block end spill set is missing for a block")]
    BlockEndSpillSetMissing,
    #[error("global nest use head set not found for block")]
    BlockGnuHeadMissing,
    #[error("def first use distance set not found for block")]
    BlockDFUseDSetMissing,
    #[error("max register pressure for block not found")]
    BlockRegPressureMissing,
    #[error("live-in set not found for block")]
    LiveInNotFound,
    #[error("loop children for block not found in loop nesting forest")]
    LoopChildrenNotFound,
    #[error("loop use set for block not found")]
    LoopUseSetNotFound,
    #[error("phi argument for a predecessor block not found in current function")]
    PredPhiArgNotFound,
    #[error("subsequent use set not found for instruction")]
    SubsequentUseSetNotFound,
}

type SpillResult<T> = Result<T, error_stack::Report<SpillError>>;

struct ForwardDistance {
    next_use_distance: Distance,
    distance_from_start: Distance,
}

struct ForwardDistanceMap(FxHashMap<Value, ForwardDistance>);

impl ForwardDistanceMap {
    fn new() -> Self {
        Self(FxHashMap::default())
    }

    fn insert(&mut self, val: Value, next_use_distance: Distance, distance_from_start: Distance) {
        self.0.insert(
            val,
            ForwardDistance {
                next_use_distance,
                distance_from_start,
            },
        );
    }

    fn remove(&mut self, val: &Value) -> Option<ForwardDistance> {
        self.0.remove(val)
    }

    #[inline]
    fn get_distance(&self, val: &Value, distance_from_start: Distance) -> Distance {
        if let Some(fd) = self.0.get(val) {
            fd.next_use_distance - (distance_from_start - fd.distance_from_start)
        } else {
            Distance::Infinite
        }
    }
}

pub(crate) struct SpillProcessCtx<'a> {
    func: &'a mut FunctionData,
    gnu: &'a GlobalNextUse,
    liveness: &'a LivenessAnalysis,
    loop_forest: &'a LoopNestingForest,
    dfs_tree: &'a DfsTree,
    max_regs: usize,
    start_reg_sets: FxHashMap<Block, FxHashSet<Value>>,
    start_spill_sets: FxHashMap<Block, FxHashSet<Value>>,
    end_reg_sets: FxHashMap<Block, FxHashSet<Value>>,
    end_spill_sets: FxHashMap<Block, FxHashSet<Value>>,
    loop_use_sets: FxHashMap<Block, FxHashSet<Value>>,
    loop_max_pressures: FxHashMap<Block, usize>,
}

impl<'a> SpillProcessCtx<'a> {
    fn new(
        func: &'a mut FunctionData,
        gnu: &'a GlobalNextUse,
        liveness: &'a LivenessAnalysis,
        loop_forest: &'a LoopNestingForest,
        dfs_tree: &'a DfsTree,
        max_regs: usize,
    ) -> Self {
        Self {
            func,
            gnu,
            liveness,
            loop_forest,
            dfs_tree,
            max_regs,
            start_reg_sets: FxHashMap::default(),
            start_spill_sets: FxHashMap::default(),
            end_reg_sets: FxHashMap::default(),
            end_spill_sets: FxHashMap::default(),
            loop_use_sets: FxHashMap::default(),
            loop_max_pressures: FxHashMap::default(),
        }
    }
}

/// Creates a spill instruction in a function.
fn make_spill_instr(func: &mut FunctionData, val: Value) -> instr::Instr {
    func.instrs
        .insert(instr::InstrData::SpillValue(instr::SpillValue { val }))
}

/// Creates a reload instruction in a function.
fn make_reload_instr(func: &mut FunctionData, val: Value) -> SpillResult<instr::Instr> {
    let val_type = func
        .get_value(val)
        .change_context(SpillError::FunctionError)?
        .value_type;

    let the_instr = func
        .instrs
        .insert(instr::InstrData::ReloadValue(instr::ReloadValue { val }));

    let reload_result = func.values.insert(ValueData {
        value_type: val_type,
        value_kind: ValueKind::InstrRes(the_instr),
    });

    func.results.insert(the_instr, reload_result);

    Ok(the_instr)
}

/// Takes the register set `reg_set`, a desired number of registers `max_regs` and evicts values from
/// the register set that have the farthest next use distance until the number of values in the set
/// equals `max_regs`. Does nothing if `len(reg_set) <= max_regs`.
fn limit(
    reg_set: &mut Vec<Value>,
    spill_set: &mut FxHashSet<Value>,
    running_map: &ForwardDistanceMap,
    max_regs: usize,
    distance_from_start: Distance,
) -> Vec<Value> {
    let mut to_spill = Vec::new();

    // Sort `W` in ascending order of the nearest next use distance.
    reg_set.sort_unstable_by(|a, b| {
        running_map
            .get_distance(a, distance_from_start)
            .cmp(&running_map.get_distance(b, distance_from_start))
    });

    for spill_val in reg_set.iter().skip(max_regs).copied() {
        // We want to add a spill for the value only if it is NOT already present in the spill set
        // `S` (if it is present in `S`, it's already spilled before) and its nearest next use distance
        // is not Infinity (The value is dead if the next use distance is Infinity)
        if !spill_set.contains(&spill_val)
            && !running_map
                .get_distance(&spill_val, distance_from_start)
                .is_infinite()
        {
            to_spill.push(spill_val);
        }

        spill_set.remove(&spill_val);
    }

    *reg_set = reg_set.iter().take(max_regs).copied().collect();

    to_spill
}

/// Takes the register and spill sets at the start of a block and applies Belady's MIN Algorithm on
/// the block, inserting spills and reloads as necessary to keep the total number of values in registers
/// less than or equal to `max_regs`.
fn block_belady_min(
    ctx: &mut SpillProcessCtx<'_>,
    block: Block,
    mut reg_set: Vec<Value>,
    mut spill_set: FxHashSet<Value>,
) -> SpillResult<(Vec<Value>, FxHashSet<Value>)> {
    let block_data = ctx
        .func
        .get_block(block)
        .change_context(SpillError::FunctionError)?;

    let block_instrs = block_data.instrs.clone();
    let phi_len = block_data.phis.len();
    let dfused_set = ctx
        .gnu
        .get_block_dfused_set(block)
        .ok_or_else(|| report!(SpillError::BlockDFUseDSetMissing))?;

    // the running_map keeps track of the nearest global next use distance of live values as we
    // iterate over the instructions of the block.
    let mut running_map = ForwardDistanceMap(
        ctx.gnu
            .get_block_head_set(block)
            .ok_or_else(|| report!(SpillError::BlockGnuHeadMissing))?
            .iter()
            .map(|(val, dist)| {
                (
                    *val,
                    ForwardDistance {
                        next_use_distance: *dist,
                        distance_from_start: Distance::Finite(0),
                    },
                )
            })
            .collect(),
    );

    let mut new_instrs = Vec::new();

    for (distance_from_start, instr) in
        block_instrs
            .into_iter()
            .enumerate()
            .map(|(distance_from_start, instr)| {
                (Distance::Finite(distance_from_start + phi_len), instr)
            })
    {
        let uses = ctx
            .func
            .get_instr_uses(instr)
            .change_context(SpillError::FunctionError)?;

        let mut instr_spill_values = Vec::new();
        // find all the values that are used in the instruction but are not present in a register
        // at this point
        let instr_reload_values = uses
            .iter()
            .copied()
            .filter(|u| !reg_set.contains(u))
            .collect::<Vec<_>>();

        // add every non-register used value to the register set. We will evict the excess values
        // from the register set down the line. Also add every non-register used value to the
        // spill set. We make the assumption that every non-register used value is spilled at
        // some earlier point.
        for suse in instr_reload_values.iter().copied() {
            reg_set.push(suse);
            spill_set.insert(suse);
        }

        // evict excess values from the register set that have the farthest next use distances
        let mut to_spill = limit(
            &mut reg_set,
            &mut spill_set,
            &running_map,
            ctx.max_regs,
            distance_from_start,
        );
        instr_spill_values.append(&mut to_spill);

        // remove the instruction used values from the running_map. We will add back still alive
        // values in with their nearest next use distance from behind the instruction later.
        for u in uses {
            running_map.remove(&u);
        }

        // add back in still alive instruction-used values to the running_map with their next use
        // distance from behind the current instruction.
        let subsequent_use_set = ctx
            .gnu
            .get_instr_subsequent_use_set(instr)
            .ok_or_else(|| report!(SpillError::SubsequentUseSetNotFound))?;

        for (used_val, dist) in subsequent_use_set {
            running_map.insert(*used_val, *dist, distance_from_start);
        }

        // if the current instruction defines a value, add it to the register set and evict excess
        // variables just as we did earlier. Also add the value to the running map with its nearest
        // next use distance.
        if let Some(defined_val) = ctx.func.get_instr_def(instr) {
            running_map.insert(
                defined_val,
                dfused_set.get(&defined_val),
                distance_from_start,
            );

            reg_set.push(defined_val);
            instr_spill_values.append(&mut limit(
                &mut reg_set,
                &mut spill_set,
                &running_map,
                ctx.max_regs,
                distance_from_start + 1,
            ));
        }

        // add all required spill instructions to the block.
        for spill_val in instr_spill_values {
            new_instrs.push(make_spill_instr(ctx.func, spill_val));
        }

        // add all required reload instructions to the block.
        for reload_val in instr_reload_values {
            new_instrs.push(make_reload_instr(ctx.func, reload_val)?);
        }

        new_instrs.push(instr);
    }

    let block_data = ctx
        .func
        .get_block_mut(block)
        .change_context(SpillError::FunctionError)?;

    block_data.instrs = new_instrs;

    Ok((reg_set, spill_set))
}

fn init_entry_spill_set(
    ctx: &SpillProcessCtx<'_>,
    block: Block,
    entry_reg_set: &FxHashSet<Value>,
) -> SpillResult<FxHashSet<Value>> {
    let mut total_spill_set = FxHashSet::default();

    let block_data = ctx
        .func
        .get_block(block)
        .change_context(SpillError::FunctionError)?;

    for &pred in &block_data.predecessors {
        let mut end_spill_set = if let Some(set) = ctx.end_spill_sets.get(&pred) {
            set.clone()
        } else {
            continue;
        };

        for phi in block_data.phis.iter().copied() {
            let phi_data = ctx
                .func
                .get_phi_data(phi)
                .change_context(SpillError::FunctionError)?;

            let pred_arg = phi_data
                .args
                .get(&pred)
                .copied()
                .ok_or_else(|| report!(SpillError::PredPhiArgNotFound))?;

            if end_spill_set.contains(&pred_arg) {
                end_spill_set.remove(&pred_arg);
                end_spill_set.insert(phi_data.value);
            }
        }

        total_spill_set = &total_spill_set | &end_spill_set;
    }

    Ok(&total_spill_set & entry_reg_set)
}

fn init_non_loop_header(
    ctx: &mut SpillProcessCtx<'_>,
    block: Block,
) -> SpillResult<(Vec<Value>, FxHashSet<Value>)> {
    let preds = ctx
        .func
        .get_block_preds(block)
        .change_context(SpillError::FunctionError)?;

    if let &[only_pred] = &preds[..] {
        let my_reg_set = ctx
            .end_reg_sets
            .get(&only_pred)
            .ok_or_else(|| report!(SpillError::BlockEndRegSetMissing))?
            .clone();

        let my_spill_set = ctx
            .end_spill_sets
            .get(&only_pred)
            .ok_or_else(|| report!(SpillError::BlockEndSpillSetMissing))?
            .clone();

        return Ok((my_reg_set.into_iter().collect(), my_spill_set));
    }

    let block_data = ctx
        .func
        .get_block(block)
        .change_context(SpillError::FunctionError)?;

    let head_next_use = ctx
        .gnu
        .get_block_head_set(block)
        .ok_or_else(|| report!(SpillError::BlockGnuHeadMissing))?;

    let mut all_reg_set = FxHashSet::default();
    let mut some_reg_set = FxHashSet::default();

    let mut first = true;
    for pred in preds {
        let mut end_reg_set = ctx
            .end_reg_sets
            .get(&block)
            .ok_or_else(|| report!(SpillError::BlockEndRegSetMissing))?
            .clone();

        for phi in block_data.phis.iter().copied() {
            let phi_data = ctx
                .func
                .get_phi_data(phi)
                .change_context(SpillError::FunctionError)?;

            let pred_arg = phi_data
                .args
                .get(&pred)
                .copied()
                .ok_or_else(|| report!(SpillError::PredPhiArgNotFound))?;

            end_reg_set.remove(&pred_arg);
            end_reg_set.insert(phi_data.value);
        }

        some_reg_set = &some_reg_set | &end_reg_set;

        if first {
            all_reg_set = &all_reg_set | &end_reg_set;
            first = false;
        } else {
            all_reg_set = &all_reg_set & &end_reg_set;
        }
    }

    let mut entry_reg_set = all_reg_set.clone();
    if entry_reg_set.len() < ctx.max_regs {
        let mut remaining_values = (&some_reg_set & &all_reg_set)
            .iter()
            .copied()
            .collect::<Vec<_>>();

        remaining_values.sort_unstable_by(|a, b| head_next_use.get(a).cmp(&head_next_use.get(b)));

        for &additional_value in remaining_values
            .iter()
            .take(ctx.max_regs - entry_reg_set.len())
        {
            entry_reg_set.insert(additional_value);
        }
    }

    let entry_spill_set = init_entry_spill_set(ctx, block, &entry_reg_set)?;

    Ok((entry_reg_set.into_iter().collect(), entry_spill_set))
}

fn init_loop_header(
    ctx: &mut SpillProcessCtx<'_>,
    block: Block,
) -> SpillResult<(Vec<Value>, FxHashSet<Value>)> {
    let used_hash_set = ctx
        .loop_use_sets
        .get(&block)
        .ok_or_else(|| report!(SpillError::LoopUseSetNotFound))?;

    let mut used_set = used_hash_set.iter().copied().collect::<Vec<_>>();

    let head_next_use = ctx
        .gnu
        .get_block_head_set(block)
        .ok_or_else(|| report!(SpillError::BlockGnuHeadMissing))?;

    used_set.sort_unstable_by(|a, b| head_next_use.get(a).cmp(&head_next_use.get(b)));

    let mut entry_reg_set = used_set.into_iter().take(ctx.max_regs).collect::<Vec<_>>();

    let max_pressure = ctx
        .loop_max_pressures
        .get(&block)
        .copied()
        .ok_or_else(|| report!(SpillError::BlockRegPressureMissing))?;

    if entry_reg_set.len() < ctx.max_regs && max_pressure < ctx.max_regs {
        let all_live_in = head_next_use.get_values::<FxHashSet<_>>();
        let mut live_outside_loop = (&all_live_in - used_hash_set)
            .iter()
            .copied()
            .collect::<Vec<_>>();

        live_outside_loop.sort_unstable_by(|a, b| head_next_use.get(a).cmp(&head_next_use.get(b)));
        for &additional_value in live_outside_loop
            .iter()
            .take(ctx.max_regs - entry_reg_set.len())
        {
            entry_reg_set.push(additional_value);
        }
    }

    let entry_spill_set =
        init_entry_spill_set(ctx, block, &entry_reg_set.iter().copied().collect())?;

    Ok((entry_reg_set, entry_spill_set))
}

fn init_block_reg_and_spill_sets(ctx: &mut SpillProcessCtx<'_>, block: Block) -> SpillResult<()> {
    let (entry_reg_set, entry_spill_set) = if block.is_start_block() {
        (
            ctx.func.arg_values.iter().take(3).copied().collect(),
            FxHashSet::default(),
        )
    } else if ctx.loop_forest.all_loop_headers.contains(&block) {
        init_loop_header(ctx, block)?
    } else {
        init_non_loop_header(ctx, block)?
    };

    ctx.start_reg_sets
        .insert(block, entry_reg_set.iter().copied().collect());
    ctx.start_spill_sets.insert(block, entry_spill_set.clone());

    let (end_reg_set, end_spill_set) =
        block_belady_min(ctx, block, entry_reg_set, entry_spill_set)?;

    ctx.end_reg_sets
        .insert(block, end_reg_set.into_iter().collect());
    ctx.end_spill_sets.insert(block, end_spill_set);

    Ok(())
}

pub(crate) fn perform_spilling(ctx: &mut SpillProcessCtx<'_>) -> SpillResult<()> {
    helpers::calculate_loop_use_sets(ctx)?;
    helpers::calculate_max_loop_pressure(ctx)?;

    for &block in ctx.dfs_tree.postorder.iter().rev() {
        init_block_reg_and_spill_sets(ctx, block)?;
    }
    Ok(())
}
