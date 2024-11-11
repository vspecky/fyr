use std::{collections::BinaryHeap, ptr::null};

use error_stack::{report, ResultExt};
use fxhash::{FxHashMap, FxHashSet};
use fyrc_machinst::{
    block::{MachBlock, MachBlockData},
    constant::MachConst,
    func::{MachFunc, MachFuncData},
    hop::MachHop,
    instr::{thumb::ValueRefKind, Machinst, ThumbMachinstData},
    module::MachModule,
    value::MachGlobalValue,
};
use fyrc_machinst_builder::tasm;
use fyrc_ssa::{function::Function as SsaFunction, Block as SsaBlock};
use fyrc_ssa_passes::PassManager;
use fyrc_utils::{DenseMap, EntityId};

#[derive(Debug, thiserror::Error)]
pub enum LinkError {
    #[error("function was not found")]
    FunctionNotFound,
    #[error("block found with invalid branching instructions")]
    InvalidBranching,
    #[error("failed insertion of layout slot")]
    LayoutSlotInsertionFailed,
    #[error("layout slot not found")]
    LayoutSlotNotFound,
    #[error("there was an error interacting with the machinst function")]
    MachFuncError,
    #[error("required pass not found for function")]
    PassNotFound,
}

pub type LinkResult<T> = Result<T, error_stack::Report<LinkError>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LayoutSlot(usize);

impl EntityId for LayoutSlot {
    fn get_id(&self) -> usize {
        self.0
    }

    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Debug)]
pub struct LayoutSlotNeighbors {
    left: Option<LayoutSlot>,
    right: Option<LayoutSlot>,
}

#[derive(Debug)]
pub struct LayoutSlotInfo<T> {
    data: T,
    left: Option<LayoutSlot>,
    right: Option<LayoutSlot>,
    size: usize,
    offset: usize,
}

#[derive(Debug)]
pub struct Layout<T> {
    slots: DenseMap<LayoutSlot, LayoutSlotInfo<T>>,
    head: LayoutSlot,
    tail: LayoutSlot,
}

impl<T> Layout<T> {
    fn new(first_elem: T, first_elem_size: usize) -> Self {
        let mut slots = DenseMap::new();

        let slot = slots.insert(LayoutSlotInfo {
            data: first_elem,
            left: None,
            right: None,
            size: first_elem_size,
            offset: 0,
        });

        Self {
            slots,
            head: slot,
            tail: slot,
        }
    }

    fn get_size(&self) -> usize {
        let tail_slot = &self.slots[self.tail];
        tail_slot.offset + tail_slot.size
    }

    fn get_slot(&self, slot: LayoutSlot) -> Option<&LayoutSlotInfo<T>> {
        self.slots.get(slot)
    }

    fn get_predecessor(&self, slot: LayoutSlot) -> Option<LayoutSlot> {
        self.get_slot(slot).and_then(|info| info.left)
    }

    fn get_successor(&self, slot: LayoutSlot) -> Option<LayoutSlot> {
        self.get_slot(slot).and_then(|info| info.right)
    }

    fn resize(&mut self, slot: LayoutSlot, new_size: usize) -> bool {
        if !self.slots.contains(&slot) {
            return false;
        }

        self.slots[slot].size = new_size;
        let mut next_offset = self.slots[slot].offset + new_size;
        let mut maybe_right = self.slots[slot].right;
        while let Some(right) = maybe_right {
            self.slots[right].offset = next_offset;
            next_offset += self.slots[slot].size;
            maybe_right = self.slots[slot].right;
        }

        true
    }

    fn insert_after(&mut self, after: LayoutSlot, data: T, data_size: usize) -> Option<LayoutSlot> {
        if !self.slots.contains(&after) {
            return None;
        }

        let new_offset = self.slots[after].offset + self.slots[after].size;
        let maybe_original_right = self.slots[after].right;
        let new_slot = self.slots.insert(LayoutSlotInfo {
            data,
            left: Some(after),
            right: maybe_original_right,
            size: data_size,
            offset: new_offset,
        });
        self.slots[after].right = Some(new_slot);
        if let Some(original_right) = maybe_original_right {
            self.slots[original_right].left = Some(new_slot);

            let mut maybe_right = Some(original_right);
            while let Some(right) = maybe_right {
                self.slots[right].offset += data_size;
                maybe_right = self.slots[right].right;
            }
        } else {
            self.tail = new_slot;
        }

        Some(new_slot)
    }

    fn insert_before(
        &mut self,
        before: LayoutSlot,
        data: T,
        data_size: usize,
    ) -> Option<LayoutSlot> {
        if !self.slots.contains(&before) {
            return None;
        }

        let new_offset = self.slots[before].offset;
        let maybe_original_left = self.slots[before].left;
        let new_slot = self.slots.insert(LayoutSlotInfo {
            data,
            left: maybe_original_left,
            right: Some(before),
            size: data_size,
            offset: new_offset,
        });
        self.slots[before].left = Some(new_slot);
        if let Some(original_left) = maybe_original_left {
            self.slots[original_left].right = Some(new_slot);
        } else {
            self.head = new_slot;
        }

        let mut maybe_right = Some(before);
        while let Some(right) = maybe_right {
            self.slots[right].offset += data_size;
            maybe_right = self.slots[right].right;
        }

        Some(new_slot)
    }

    fn append(&mut self, data: T, data_size: usize) -> LayoutSlot {
        self.insert_after(self.tail, data, data_size)
            .expect("layout append")
    }

    fn prepend(&mut self, data: T, data_size: usize) -> LayoutSlot {
        self.insert_before(self.head, data, data_size)
            .expect("layout prepend")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FuncLayoutSlotData {
    Block(MachBlock),
    Const(MachConst),
    GlobalRef(MachGlobalValue),
    Hop(MachHop),
    NullBytes(usize),
}

impl From<ValueRefKind> for FuncLayoutSlotData {
    fn from(value: ValueRefKind) -> Self {
        match value {
            ValueRefKind::Const(constant) => Self::Const(constant),
            ValueRefKind::Global(global) => Self::GlobalRef(global),
        }
    }
}

type FuncLayout = Layout<FuncLayoutSlotData>;

struct FuncLinkCtx<'a> {
    func: &'a mut MachFuncData,
    layout: FuncLayout,
    block_branches: DenseMap<MachBlock, BlockBranchKind>,
    block_slots: FxHashMap<MachBlock, LayoutSlot>,
    load_slot_map: FxHashMap<Machinst, LayoutSlot>,
}

struct FuncLinkOutput {
    layout: FuncLayout,
    block_branches: DenseMap<MachBlock, BlockBranchKind>,
    load_slot_map: FxHashMap<Machinst, LayoutSlot>,
}

#[derive(Debug)]
enum CondBranchKind {
    Direct,
    UncondHop,
    ConstructAddress,
}

impl CondBranchKind {
    fn additional_size(&self) -> usize {
        match self {
            Self::Direct => 0,
            Self::UncondHop => 1 * 2,
            Self::ConstructAddress => 8 * 2,
        }
    }
}

#[derive(Debug)]
enum UncondBranchKind {
    Direct,
    ConstructAddress,
}

impl UncondBranchKind {
    fn additional_size(&self) -> usize {
        match self {
            Self::Direct => 0,
            // It's 7 additional instructions as opposed to conditional branches' 8 since we can
            // replace the unconditional branch instruction with one of the 8.
            Self::ConstructAddress => 7 * 2,
        }
    }
}

#[derive(Debug)]
struct ConditionalBranchData {
    cond_kind: CondBranchKind,
    uncond_kind: UncondBranchKind,
}

impl ConditionalBranchData {
    const WORST_CASE: Self = Self {
        cond_kind: CondBranchKind::ConstructAddress,
        uncond_kind: UncondBranchKind::ConstructAddress,
    };

    fn additional_size(&self) -> usize {
        self.cond_kind.additional_size() + self.uncond_kind.additional_size()
    }
}

#[derive(Debug)]
struct UnconditionalBranchData {
    kind: UncondBranchKind,
}

impl UnconditionalBranchData {
    const WORST_CASE: Self = Self {
        kind: UncondBranchKind::ConstructAddress,
    };

    fn additional_size(&self) -> usize {
        self.kind.additional_size()
    }
}

#[derive(Debug)]
enum BlockBranchKind {
    Cond(ConditionalBranchData),
    Uncond(UnconditionalBranchData),
    Ret,
}

impl BlockBranchKind {
    const WORST_CONDITIONAL: Self = Self::Cond(ConditionalBranchData::WORST_CASE);
    const WORST_UNCONDITIONAL: Self = Self::Uncond(UnconditionalBranchData::WORST_CASE);

    fn additional_size(&self) -> usize {
        match self {
            Self::Cond(cbd) => cbd.additional_size(),
            Self::Uncond(ubd) => ubd.additional_size(),
            Self::Ret => 0,
        }
    }
}

fn build_link_context<'a>(
    func: &'a mut MachFuncData,
    dfs_tree: &fyrc_ssa_passes::DfsTree,
    loop_forest: &fyrc_ssa_passes::LoopNestingForest,
) -> LinkResult<FuncLinkCtx<'a>> {
    let back_edges = dfs_tree
        .back_edges
        .iter()
        .map(|(pred, succ)| {
            (
                MachBlock::with_id(pred.get_id()),
                MachBlock::with_id(succ.get_id()),
            )
        })
        .collect::<FxHashSet<_>>();
    let start_size = func
        .get_block_len(MachBlock::START)
        .change_context(LinkError::MachFuncError)?;
    let mut layout = FuncLayout::new(FuncLayoutSlotData::Block(MachBlock::START), start_size);
    let mut block_preds: DenseMap<MachBlock, usize> = DenseMap::with_capacity(func.blocks.len());
    let mut block_slots = FxHashMap::default();

    for (block, block_data) in func.blocks.iter() {
        let mut total = 0;
        for &pred in &block_data.preds {
            if !back_edges.contains(&(pred, block)) {
                total += 1;
            }
        }
        block_preds.insert(total);
    }

    let mach_loop_depth: DenseMap<MachBlock, usize> =
        loop_forest.loop_depth.values().copied().collect();
    let mut stack = vec![(0, MachBlock::START)];

    while let Some((_, block)) = stack.pop() {
        if block != MachBlock::START {
            let block_size = func
                .get_block_len(block)
                .change_context(LinkError::MachFuncError)?;
            let slot = layout.append(FuncLayoutSlotData::Block(block), block_size);
            block_slots.insert(block, slot);
        }

        let succs = &func
            .get_block(block)
            .change_context(LinkError::MachFuncError)?
            .succs;

        let mut pushed = false;
        for &succ in succs {
            if back_edges.contains(&(block, succ)) {
                continue;
            }

            block_preds[succ] -= 1;
            if block_preds[succ] == 0 {
                stack.push((mach_loop_depth[succ] as i32, succ));
                pushed = true;
            }
        }

        if pushed {
            stack.sort_by(|a, b| a.0.cmp(&b.0));
        }
    }

    let mut block_branches = DenseMap::with_capacity(func.blocks.len());
    for block in func.blocks.keys() {
        let block_data = func
            .get_block(block)
            .change_context(LinkError::MachFuncError)?;

        let mut instr_iter = block_data.instrs.iter().rev().copied();
        let last_machinst = instr_iter
            .next()
            .ok_or_else(|| report!(LinkError::MachFuncError))
            .attach_printable("empty block encountered")?;

        let machinst_data = func
            .get_machinst(last_machinst)
            .change_context(LinkError::MachFuncError)?;

        if machinst_data.is_return() {
            block_branches.insert(BlockBranchKind::Ret);
        } else if machinst_data.is_uncond_branch() {
            if let Some(second_last_machinst) = instr_iter.next() {
                let sl_machinst_data = func
                    .get_machinst(second_last_machinst)
                    .change_context(LinkError::MachFuncError)?;

                if sl_machinst_data.is_cond_branch() {
                    block_branches.insert(BlockBranchKind::WORST_CONDITIONAL);
                } else {
                    block_branches.insert(BlockBranchKind::WORST_UNCONDITIONAL);
                }
            } else {
                block_branches.insert(BlockBranchKind::WORST_UNCONDITIONAL);
            }
        } else {
            return Err(report!(LinkError::InvalidBranching));
        }
    }

    Ok(FuncLinkCtx {
        func,
        layout,
        block_slots,
        block_branches,
        load_slot_map: FxHashMap::default(),
    })
}

/// Checks if a certain address is in the PC-relative load instruction range based on some current
/// location of the PC. The range is 255 words and more importantly, the address has to be word
/// aligned. So we :-
/// - Ensure that the current PC is less than the target data address
/// - Check if the address is within range of 256 words from the current word (PC is 1 word ahead
///   of the current instruction cuz of prefetch)
fn in_pc_load_range(me: usize, it: usize) -> bool {
    me < it && me / 4 + 1 + 255 >= it / 4
}

fn place_initial_block_data(
    ctx: &mut FuncLinkCtx<'_>,
    mut current_block: MachBlock,
    mut current_slot: LayoutSlot,
) -> LinkResult<()> {
    let mut load_instrs = Vec::new();
    let block_data = ctx
        .func
        .get_block(current_block)
        .change_context(LinkError::MachFuncError)?;

    let slot_data = ctx
        .layout
        .get_slot(current_slot)
        .ok_or_else(|| report!(LinkError::LayoutSlotNotFound))?;

    for (idx, machinst) in block_data.instrs.iter().copied().enumerate() {
        let machinst_data = ctx
            .func
            .get_machinst(machinst)
            .change_context(LinkError::MachFuncError)?;

        if let ThumbMachinstData::PcRelativeLoad(load) = machinst_data {
            load_instrs.push((machinst, load.imm.kind, slot_data.offset + idx * 2));
        }
    }

    let mut instr_iter = load_instrs.into_iter().rev().peekable();
    while let Some(_) = instr_iter.peek() {
        let slot_data = ctx
            .layout
            .get_slot(current_slot)
            .ok_or_else(|| report!(LinkError::LayoutSlotNotFound))?;
        let after_block_idx =
            slot_data.offset + slot_data.size + ctx.block_branches[current_block].additional_size();
        let data_slot_ptr = after_block_idx.next_multiple_of(4);
        let null_padding_size = data_slot_ptr - after_block_idx;
        let mut data: FxHashMap<ValueRefKind, LayoutSlot> = FxHashMap::default();
        let mut most_strained_reach: Option<usize> = None;
        let check_and_update_most_strained_reach = |reach: Option<usize>, new_distance: usize| {
            if reach.is_none() || reach.is_some_and(|old_distance| new_distance > old_distance) {
                Some(new_distance)
            } else {
                reach
            }
        };

        let data_adjacent_slot = if null_padding_size > 0 {
            ctx.layout
                .insert_after(
                    current_slot,
                    FuncLayoutSlotData::NullBytes(null_padding_size),
                    null_padding_size,
                )
                .ok_or_else(|| report!(LinkError::LayoutSlotInsertionFailed))
                .attach_printable("when inserting null bytes")?
        } else {
            current_slot
        };

        while let Some(&(machinst, load_kind, offset)) = instr_iter.peek() {
            if let Some(&existing) = data.get(&load_kind) {
                if in_pc_load_range(offset, ctx.layout.slots[existing].offset) {
                    let distance = (ctx.layout.slots[existing].offset - offset) / 4;
                    ctx.load_slot_map.insert(machinst, existing);
                    most_strained_reach =
                        check_and_update_most_strained_reach(most_strained_reach, distance);
                    continue;
                }
            }

            if most_strained_reach.is_some_and(|distance| distance + 1 > 256) {
                // Split block
                let pre_block = ctx.func.blocks.insert(MachBlockData::new());
                ctx.func
                    .redirect_predecessor_branches(current_block, pre_block)
                    .change_context(LinkError::MachFuncError)?;
                let block_data = ctx
                    .func
                    .get_block_mut(current_block)
                    .change_context(LinkError::MachFuncError)?;

                let mut pre_block_instrs = block_data
                    .instrs
                    .iter()
                    .copied()
                    .scan(false, |state, elem| {
                        if *state {
                            None
                        } else {
                            if elem == machinst {
                                *state = true;
                            }

                            Some(elem)
                        }
                    })
                    .collect::<Vec<_>>();

                let me_block_instrs = block_data
                    .instrs
                    .iter()
                    .copied()
                    .skip_while(|&elem| elem != machinst)
                    .skip(1)
                    .collect::<Vec<_>>();

                block_data.instrs = me_block_instrs;
                block_data.preds.push(pre_block);

                let jmp_machinst = ctx.func.instrs.insert(tasm!(B current_block).into());
                pre_block_instrs.push(jmp_machinst);
                ctx.func
                    .get_block_mut(pre_block)
                    .change_context(LinkError::MachFuncError)?
                    .instrs = pre_block_instrs;

                ctx.block_branches
                    .insert(BlockBranchKind::WORST_UNCONDITIONAL);

                let pre_block_len = ctx
                    .func
                    .get_block_len(pre_block)
                    .change_context(LinkError::MachFuncError)?
                    + ctx.block_branches[pre_block].additional_size();
                let my_block_len = ctx
                    .func
                    .get_block_len(current_block)
                    .change_context(LinkError::MachFuncError)?
                    + ctx.block_branches[current_block].additional_size();

                ctx.layout.resize(current_slot, my_block_len);
                current_slot = ctx
                    .layout
                    .insert_before(
                        current_slot,
                        FuncLayoutSlotData::Block(pre_block),
                        pre_block_len,
                    )
                    .ok_or_else(|| report!(LinkError::LayoutSlotInsertionFailed))?;
                current_block = pre_block;

                break;
            }

            // Add data to block tail
            let data_slot = ctx
                .layout
                .insert_after(data_adjacent_slot, load_kind.into(), 4)
                .ok_or_else(|| report!(LinkError::LayoutSlotInsertionFailed))?;
            ctx.load_slot_map.insert(machinst, data_slot);

            most_strained_reach = most_strained_reach.map(|distance| distance + 1);
            most_strained_reach = check_and_update_most_strained_reach(
                most_strained_reach,
                (data_slot_ptr - offset) / 4,
            );
            data.insert(load_kind, data_slot);
            instr_iter.next();
        }
    }

    Ok(())
}

fn place_func_local_data<'a>(
    func: &'a mut MachFuncData,
    dfs_tree: &fyrc_ssa_passes::DfsTree,
    loop_forest: &fyrc_ssa_passes::LoopNestingForest,
) -> LinkResult<FuncLinkOutput> {
    let mut ctx = build_link_context(func, dfs_tree, loop_forest)?;
    let initial_blocks = ctx
        .block_slots
        .iter()
        .map(|(&block, &slot)| (block, slot))
        .collect::<Vec<_>>();

    for (block, slot) in initial_blocks {
        place_initial_block_data(&mut ctx, block, slot)?;
    }

    Ok(FuncLinkOutput {
        layout: ctx.layout,
        block_branches: ctx.block_branches,
        load_slot_map: ctx.load_slot_map,
    })
}

#[derive(Debug)]
enum ProgramLayoutSlotData {
    Func(MachFunc),
    GlobalData(MachGlobalValue),
    NullBytes,
}

type ProgramLayout = Layout<ProgramLayoutSlotData>;

pub struct LinkingLayout {
    program: ProgramLayout,
    functions: DenseMap<MachFunc, FuncLinkOutput>,
    func2slot: DenseMap<MachFunc, LayoutSlot>,
}

fn decide_program_layout(
    module: &mut MachModule,
    passes: &DenseMap<SsaFunction, PassManager>,
) -> LinkResult<LinkingLayout> {
    let mut layout = ProgramLayout::new(ProgramLayoutSlotData::NullBytes, 0);
    let mut func2slot = DenseMap::new();
    let mut func_outputs: DenseMap<MachFunc, FuncLinkOutput> = DenseMap::new();
    for (mach_func, mach_func_data) in module.iter_funcs_mut() {
        let pass_manager = passes
            .get(SsaFunction::with_id(mach_func.get_id()))
            .ok_or_else(|| report!(LinkError::FunctionNotFound))?;

        let dfs_tree = pass_manager
            .get_pass::<fyrc_ssa_passes::DfsTree>()
            .change_context(LinkError::PassNotFound)?;

        let loop_forest = pass_manager
            .get_pass::<fyrc_ssa_passes::LoopNestingForest>()
            .change_context(LinkError::PassNotFound)?;

        let func_link_output = place_func_local_data(mach_func_data, &dfs_tree, &loop_forest)?;
        let func_size = func_link_output.layout.get_size();
        func_outputs.insert(func_link_output);

        let next_slot_start = layout.get_size();
        let word_aligned_start = next_slot_start.next_multiple_of(4);
        if word_aligned_start > next_slot_start {
            layout.append(
                ProgramLayoutSlotData::NullBytes,
                word_aligned_start - next_slot_start,
            );
        }

        let slot = layout.append(ProgramLayoutSlotData::Func(mach_func), func_size);
        func2slot.insert(slot);
    }

    Ok(LinkingLayout {
        program: layout,
        functions: func_outputs,
        func2slot,
    })
}
