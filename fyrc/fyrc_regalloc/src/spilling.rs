//! Register Spilling & Live-Range Splitting
//!
//! This module contains the logic to insert spills and reloads to the function to split
//! live ranges of values and make the interference graph K-colorable (where K = 8 for Thumb
//! functions and 16 for ARM functions). To decide what values to spill, We make use of a
//! heuristic that extends Belady's MIN Algorithm to work on Control Flow Graphs, put forward
//! by Braun & Hack
//! in [this paper](https://www.researchgate.net/publication/221302742_Register_Spilling_and_Live-Range_Splitting_for_SSA-Form_Programs).

mod helpers;
mod ssafix;

use error_stack::{report, ResultExt};
use fxhash::{FxHashMap, FxHashSet};
use fyrc_ssa::{function::FunctionData, instr::Instr, Block, Value};
use fyrc_ssa_passes::{
    global_next_use::Distance, DefUse, DfsTree, DominatorTree, GlobalNextUse, LivenessAnalysis,
    LoopNestingForest,
};

#[derive(Debug, thiserror::Error)]
pub enum SpillError {
    #[error("there was an error when interacting with the function data")]
    FunctionError,
    #[error("the block end register set is missing for a block")]
    BlockEndRegSetMissing,
    #[error("the block end spill set is missing for a block")]
    BlockEndSpillSetMissing,
    #[error("the block entry regiter set is missing for a block")]
    BlockEntryRegSetMissing,
    #[error("the block entry spill set is missing for a block")]
    BlockEntrySpillSetMissing,
    #[error("global nest use head set not found for block")]
    BlockGnuHeadMissing,
    #[error("def first use distance set not found for block")]
    BlockDFUseDSetMissing,
    #[error("max register pressure for block not found")]
    BlockRegPressureMissing,
    #[error("def use not found for value")]
    DefUseNotFound,
    #[error("dj graph node data not found for block")]
    DJGraphNodeDataNotFound,
    #[error("dj graph successors not found")]
    DJGraphSuccessorsNotFound,
    #[error("dominator level not found")]
    DominatorLevelNotFound,
    #[error("dominator parent not found for block")]
    DominatorParentNotFound,
    #[error("live-in set not found for block")]
    LiveInNotFound,
    #[error("loop children for block not found in loop nesting forest")]
    LoopChildrenNotFound,
    #[error("loop use set for block not found")]
    LoopUseSetNotFound,
    #[error("phi use predecessor not found for value")]
    PhiPredNotFound,
    #[error("phi argument for a predecessor block not found in current function")]
    PredPhiArgNotFound,
    #[error("strict dominator set not found for block")]
    StrictDomSetNotFound,
    #[error("subsequent use set not found for instruction")]
    SubsequentUseSetNotFound,
    #[error("value def not found when going up the dominator tree")]
    ValueDefNotFound,
}

type SpillResult<T> = Result<T, error_stack::Report<SpillError>>;

struct ForwardDistance {
    next_use_distance: Distance,
    distance_from_start: Distance,
}

struct ForwardDistanceMap(FxHashMap<Value, ForwardDistance>);

impl ForwardDistanceMap {
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

pub struct SpillProcessCtx<'a, 'b> {
    pub func: &'b mut FunctionData,
    pub gnu: &'a GlobalNextUse,
    pub liveness: &'a LivenessAnalysis,
    pub loop_forest: &'a LoopNestingForest,
    pub def_use: &'a DefUse,
    pub dfs_tree: &'a DfsTree,
    pub dom: &'a DominatorTree,
    pub max_regs: usize,
    pub entry_reg_sets: FxHashMap<Block, FxHashSet<Value>>,
    pub entry_spill_sets: FxHashMap<Block, FxHashSet<Value>>,
    pub end_reg_sets: FxHashMap<Block, FxHashSet<Value>>,
    pub end_spill_sets: FxHashMap<Block, FxHashSet<Value>>,
    pub loop_use_sets: FxHashMap<Block, FxHashSet<Value>>,
    pub loop_max_pressures: FxHashMap<Block, usize>,
    pub inserted_instrs: FxHashMap<Value, Vec<(Instr, Block)>>,
}

impl<'a, 'b> SpillProcessCtx<'a, 'b> {
    pub fn new(
        func: &'b mut FunctionData,
        gnu: &'a GlobalNextUse,
        liveness: &'a LivenessAnalysis,
        loop_forest: &'a LoopNestingForest,
        def_use: &'a DefUse,
        dfs_tree: &'a DfsTree,
        dom: &'a DominatorTree,
        max_regs: usize,
    ) -> Self {
        Self {
            func,
            gnu,
            liveness,
            loop_forest,
            def_use,
            dfs_tree,
            dom,
            max_regs,
            entry_reg_sets: FxHashMap::default(),
            entry_spill_sets: FxHashMap::default(),
            end_reg_sets: FxHashMap::default(),
            end_spill_sets: FxHashMap::default(),
            loop_use_sets: FxHashMap::default(),
            loop_max_pressures: FxHashMap::default(),
            inserted_instrs: FxHashMap::default(),
        }
    }
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
    ctx: &mut SpillProcessCtx<'_, '_>,
    block: Block,
    mut reg_set: Vec<Value>,
    mut spill_set: FxHashSet<Value>,
) -> SpillResult<(Vec<Value>, FxHashSet<Value>)> {
    let block_data = ctx
        .func
        .get_block(block)
        .change_context(SpillError::FunctionError)?;

    let block_instrs = block_data.iter_instr().collect::<Vec<_>>();
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
            let instr = helpers::make_spill_instr(ctx.func, spill_val)?;
            ctx.inserted_instrs
                .entry(spill_val)
                .or_default()
                .push((instr, block));
            new_instrs.push(instr);
        }

        // add all required reload instructions to the block.
        for reload_val in instr_reload_values {
            let instr = helpers::make_reload_instr(ctx.func, reload_val)?;
            ctx.inserted_instrs
                .entry(reload_val)
                .or_default()
                .push((instr, block));
            new_instrs.push(instr);
        }

        new_instrs.push(instr);
    }

    let block_data = ctx
        .func
        .get_block_mut(block)
        .change_context(SpillError::FunctionError)?;

    block_data.set_instrs(new_instrs);

    Ok((reg_set, spill_set))
}

/// Initialize the entry spill set of a block. This is initialized as the union of all the exit
/// spill sets of all the predecessors of the block intersected with the entry register set of
/// the given block.
fn init_entry_spill_set(
    ctx: &SpillProcessCtx<'_, '_>,
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

/// initialize the entry register set and spill set of a block that's not a loop header
fn init_non_loop_header(
    ctx: &mut SpillProcessCtx<'_, '_>,
    block: Block,
) -> SpillResult<(Vec<Value>, FxHashSet<Value>)> {
    let preds = ctx
        .func
        .get_block_preds(block)
        .change_context(SpillError::FunctionError)?;

    // if the block has a single predecessor, the entry register and spill sets are the same as the
    // exit register and spill sets of the predecessor.
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

    // we create the `All` and `Some` sets. The All set is the set of all values that are present
    // in the end register set of every predecessor (intersection of all of them), and the `Some`
    // set is the union.
    let mut first = true;
    for pred in preds {
        let mut end_reg_set = ctx
            .end_reg_sets
            .get(&pred)
            .ok_or_else(|| report!(SpillError::BlockEndRegSetMissing))?
            .clone();

        // due to the way liveness is modeled in the IR, the arguments of a phi are live-out at the
        // end of the phi block's predecessors (thus potentially in their end register sets) and the
        // phi is live-in to its block. Hence to transfer the end register sets of a block's predecessors
        // to the block, we replace the phi arguments with the phi defined value for all phis in the
        // exit register sets of all predecessors.
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

    // the register set is basically the `All` set. If there is space left over in the registers,
    // we take values from `Some \ All` with the nearest next use distances and fill up the register
    // set with them.
    let mut entry_reg_set = all_reg_set.clone();
    if entry_reg_set.len() < ctx.max_regs {
        let mut remaining_values = (&some_reg_set - &all_reg_set)
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

/// Initializes the entry register and spill sets of a block that is the header of a loop.
fn init_loop_header(
    ctx: &mut SpillProcessCtx<'_, '_>,
    block: Block,
) -> SpillResult<(Vec<Value>, FxHashSet<Value>)> {
    // for loop headers, we ignore the exit register and spill sets of the predecessors and instead
    // get all the values that are used within the loop. These values are primary candidates for
    // the entry register set of the block so they can be available for loop uses without having
    // to be reloaded on every loop turn.
    let used_hash_set = ctx
        .loop_use_sets
        .get(&block)
        .ok_or_else(|| report!(SpillError::LoopUseSetNotFound))?;

    let mut used_set = used_hash_set.iter().copied().collect::<Vec<_>>();

    let head_next_use = ctx
        .gnu
        .get_block_head_set(block)
        .ok_or_else(|| report!(SpillError::BlockGnuHeadMissing))?;

    // for all values used in the loop, we sort them by their nearest next use distance and fill
    // the register set with them.
    used_set.sort_unstable_by(|a, b| head_next_use.get(a).cmp(&head_next_use.get(b)));

    let mut entry_reg_set = used_set.into_iter().take(ctx.max_regs).collect::<Vec<_>>();

    let max_pressure = ctx
        .loop_max_pressures
        .get(&block)
        .copied()
        .ok_or_else(|| report!(SpillError::BlockRegPressureMissing))?;

    // if there is space left in the register set, we first ensure that all the blocks in the entire
    // loop can hold more values based on the maximum loop register pressure, and if yes, we add
    // values to the register set that we are sure live throughout the loop (with the nearest)
    // next use distances
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

fn init_block_reg_and_spill_sets(
    ctx: &mut SpillProcessCtx<'_, '_>,
    block: Block,
) -> SpillResult<()> {
    let (entry_reg_set, entry_spill_set) = if block.is_start_block() {
        (
            ctx.func
                .arg_values
                .iter()
                .take(fyrc_utils::consts::ABI_ARGS_IN_REGS)
                .copied()
                .collect(),
            FxHashSet::default(),
        )
    } else if ctx.loop_forest.all_loop_headers.contains(&block) {
        init_loop_header(ctx, block)?
    } else {
        init_non_loop_header(ctx, block)?
    };

    let entry_reg_hashset = entry_reg_set.iter().copied().collect::<FxHashSet<_>>();
    let block_data = ctx
        .func
        .get_block(block)
        .change_context(SpillError::FunctionError)?;

    // if a phi defined value is not in the entry register set of this block, the phi is a mem phi
    // so mark it as such.
    for phi in block_data.phis.clone() {
        let phi_data = ctx
            .func
            .get_phi_data_mut(phi)
            .change_context(SpillError::FunctionError)?;
        let phi_value = phi_data.value;

        if !entry_reg_hashset.contains(&phi_value) {
            phi_data.is_mem = true;
        }

        let value_data = ctx
            .func
            .get_value_mut(phi_value)
            .change_context(SpillError::FunctionError)?;
        value_data.is_mem = !entry_reg_hashset.contains(&phi_value);
    }

    ctx.entry_reg_sets.insert(block, entry_reg_hashset);
    ctx.entry_spill_sets.insert(block, entry_spill_set.clone());

    let (end_reg_set, end_spill_set) =
        block_belady_min(ctx, block, entry_reg_set, entry_spill_set)?;

    ctx.end_reg_sets
        .insert(block, end_reg_set.into_iter().collect());
    ctx.end_spill_sets.insert(block, end_spill_set);

    Ok(())
}

/// Adds spill & reload instructions on block edges to couple the entry and exit register sets and
/// spill sets of blocks
fn connect_block_predecessors(ctx: &mut SpillProcessCtx<'_, '_>, block: Block) -> SpillResult<()> {
    let preds = ctx
        .func
        .get_block_preds(block)
        .change_context(SpillError::FunctionError)?;

    let mut live_in_except_phis = ctx
        .liveness
        .get_live_in(block)
        .change_context(SpillError::LiveInNotFound)?
        .clone();

    let phis = ctx
        .func
        .get_block_phis(block)
        .change_context(SpillError::FunctionError)?;

    // We do not couple phi instructions. We will let the SSA destruction phase take care of this
    // after coalescing
    for phi in phis {
        let phi_data = ctx
            .func
            .get_phi_data(phi)
            .change_context(SpillError::FunctionError)?;

        live_in_except_phis.remove(&phi_data.value);
    }

    let entry_reg_set = ctx
        .entry_reg_sets
        .get(&block)
        .ok_or_else(|| report!(SpillError::BlockEntryRegSetMissing))?;

    let entry_spill_set = ctx
        .entry_spill_sets
        .get(&block)
        .ok_or_else(|| report!(SpillError::BlockEntrySpillSetMissing))?;

    // Whether to add coupling instructions in the current block (or in the predecessor).
    // Since we have split critical edges, we have the assurance that each edge can be uniquely
    // attributed to one block such that addition of coupling instructions to the block does not
    // affect the control flow semantics of the program across other edges, so the count of
    // the predecessors of the block is enough information to decide where to add the coupling
    // instructions
    let add_in_me = preds.len() == 1;

    // add coupling code for all predecessors. We add the code such that all spill instructions
    // come before all reload instructions.
    for pred in preds {
        let pred_end_reg_set = ctx
            .end_reg_sets
            .get(&pred)
            .ok_or_else(|| report!(SpillError::BlockEndRegSetMissing))?;

        let pred_end_spill_set = ctx
            .end_spill_sets
            .get(&pred)
            .ok_or_else(|| report!(SpillError::BlockEndSpillSetMissing))?;

        let ins_block = if add_in_me { block } else { pred };
        let mut added_instrs = Vec::new();

        // spill all values (except phis) from the predecessor's exit register set that are live-in
        // to the block but not in the block's entry register set and not already spilled along the
        // path to the exit of this predecessor. Also spill a value even if it's present in the
        // block's entry register set but was never spilled along this path (i.e. it's not in the
        // exit spill set of the predecessor).
        for &val in pred_end_reg_set {
            // ignore phi defined values
            if !live_in_except_phis.contains(&val) {
                continue;
            }

            if (!entry_reg_set.contains(&val) && !pred_end_spill_set.contains(&val))
                || (entry_reg_set.contains(&val)
                    && !pred_end_spill_set.contains(&val)
                    && entry_spill_set.contains(&val))
            {
                let spill_instr = helpers::make_spill_instr(ctx.func, val)?;
                ctx.inserted_instrs
                    .entry(val)
                    .or_default()
                    .push((spill_instr, ins_block));
                added_instrs.push(spill_instr);
            }
        }

        // add reloads for (non-phi) values that are present in the entry register set of this block
        // but absent in the exit register set of the predecessor.
        for &val in entry_reg_set {
            // ignore phi-defined values
            if !live_in_except_phis.contains(&val) {
                continue;
            }

            if !pred_end_reg_set.contains(&val) {
                let reload_instr = helpers::make_reload_instr(ctx.func, val)?;
                ctx.inserted_instrs
                    .entry(val)
                    .or_default()
                    .push((reload_instr, ins_block));
                added_instrs.push(reload_instr);
            }
        }

        let block_data = ctx
            .func
            .get_block_mut(if add_in_me { block } else { pred })
            .change_context(SpillError::FunctionError)?;

        if add_in_me {
            added_instrs.extend(block_data.iter_instr());
            block_data.set_instrs(added_instrs);
        } else {
            for added_instr in added_instrs {
                block_data.append_instr(added_instr);
            }
        }
    }

    Ok(())
}

pub fn perform_spilling(mut ctx: SpillProcessCtx<'_, '_>) -> SpillResult<()> {
    helpers::calculate_loop_use_sets(&mut ctx)?;
    helpers::calculate_max_loop_pressure(&mut ctx)?;

    for &block in ctx.dfs_tree.postorder.iter().rev() {
        init_block_reg_and_spill_sets(&mut ctx, block)?;
    }

    for block in ctx.func.blocks.keys() {
        connect_block_predecessors(&mut ctx, block)?;
    }

    ssafix::fix_ssa(&mut ctx)?;

    Ok(())
}

pub mod test_utils {
    #[macro_export]
    macro_rules! build_spilling_ctx {
        ($func:ident, $ctx:ident, $max_regs:literal $(,$func_print:ident)?) => {
            let ((gnu, def_use, dom, liveness, loop_forest, dfs_tree), mut $func) =
                $crate::__private::fsp::test_utils::get_pass::<(
                    $crate::__private::fsp::GlobalNextUse,
                    $crate::__private::fsp::DefUse,
                    $crate::__private::fsp::DominatorTree,
                    $crate::__private::fsp::LivenessAnalysis,
                    $crate::__private::fsp::LoopNestingForest,
                    $crate::__private::fsp::DfsTree,
                )>($func);

            $(let $func_print = $func.viz().expect("func print");)?
            let $ctx = $crate::spilling::SpillProcessCtx {
                func: &mut $func,
                gnu: &gnu,
                liveness: &liveness,
                loop_forest: &loop_forest,
                def_use: &def_use,
                dfs_tree: &dfs_tree,
                dom: &dom,
                max_regs: $max_regs,
                entry_reg_sets: $crate::__private::FxHashMap::default(),
                entry_spill_sets: $crate::__private::FxHashMap::default(),
                end_reg_sets: $crate::__private::FxHashMap::default(),
                end_spill_sets: $crate::__private::FxHashMap::default(),
                loop_use_sets: $crate::__private::FxHashMap::default(),
                loop_max_pressures: $crate::__private::FxHashMap::default(),
                inserted_instrs: $crate::__private::FxHashMap::default(),
            };
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fyrc_ssa_builder::ssa_dsl;

    #[test]
    fn test_straight_line_spills() {
        let func = ssa_dsl! {(
            (let v0 35)
            (let v1 45)
            (let v2 60)
            (let v3 70)
            (let v4 (+ v2 v3))
            (let v5 (+ v0 v1))
            (ret v5)
        )};

        crate::build_spilling_ctx!(func, ctx, 3, func_print);
        println!("{}", func_print);
        perform_spilling(ctx).expect("spilling");
        println!("{}", func.viz().expect("func print"));
    }

    #[test]
    fn test_arg_slot_spills() {
        let func = ssa_dsl! {(
            (let v0 10)
            (let v1 20)
            (let v2 (+ v0 v1))
            (ret v2)
        )};

        crate::build_spilling_ctx!(func, ctx, 2, func_print);
        println!("{}", func_print);
        perform_spilling(ctx).expect("spilling");
        println!("{}", func.viz().expect("func print"));
    }

    #[test]
    fn test_diamond_top_spill_bot_reload() {
        let func = ssa_dsl! {(
            (let v0 10)
            (let v1 20)
            (if 1 2
              ((let v2 10)
               (let v3 10)
               (let v4 (+ v2 v3)))
              ())
            (let v5 (+ v0 v1))
            (ret v5)
        )};

        crate::build_spilling_ctx!(func, ctx, 3, func_print);
        println!("{}", func_print);
        perform_spilling(ctx).expect("spilling");
        println!("{}", func.viz().expect("func print"));
    }

    #[test]
    fn test_diamond_top_spill_left_reload() {
        let func = ssa_dsl! {(
            (let v0 10)
            (let v1 20)
            (if 1 2
              ((let v2 10)
               (let v4 (+ v1 v2)))
              ())
            (let v5 (+ v0 v1))
            (ret v5)
        )};

        crate::build_spilling_ctx!(func, ctx, 3, func_print);
        println!("{}", func_print);
        perform_spilling(ctx).expect("spilling");
        println!("{}", func.viz().expect("func print"));
    }

    #[test]
    fn test_loop_spill_front_reload_behind() {
        let func = ssa_dsl! {(
            (let v0 10)
            (let v1 20)
            (let v2 (+ v0 v1))
            (while v1 v2
              ((let v5 20)
               (let v4 (+ v1 v2))
               (mut v1 (+ v4 v5))))
            (let v3 (+ v0 10))
            (ret v3)
        )};

        crate::build_spilling_ctx!(func, ctx, 3, func_print);
        println!("{func_print}");
        perform_spilling(ctx).expect("spilling");
        println!("{}", func.viz().expect("func viz"));
    }

    #[test]
    fn test_do_something() {
        let func = ssa_dsl! {(
            (let v0 10)
            (let v1 20)
            (let v2 (+ v0 v1))
            (while v1 v2
              ((let v5 20)
               (let v4 (+ v1 v2))
               (mut v1 (+ v4 v5))
               (let v6 (+ v0 v5))))
            (let v3 (+ v0 10))
            (ret v3)
        )};

        println!("{:?}", func.values);

        crate::build_spilling_ctx!(func, ctx, 3, func_print);
        println!("{func_print}");
        perform_spilling(ctx).expect("spilling");
        println!("{}", func.viz().expect("func viz"));
    }
}
