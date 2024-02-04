use std::collections::BinaryHeap;

use error_stack::ResultExt;
use fxhash::FxHashSet;
use fyrc_machinst::{
    block::MachBlock, constant::MachConst, func::MachFuncData, hop::MachHop, value::MachGlobalValue,
};
use fyrc_ssa::Block as SsaBlock;
use fyrc_utils::{DenseMap, EntityId};

#[derive(Debug, thiserror::Error)]
pub enum LinkError {
    #[error("there was an error interacting with the machinst function")]
    MachFuncError,
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

struct FuncLinkCtx<'a> {
    layout: &'a mut FuncLayout,
    block_lengths: DenseMap<MachBlock, usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FuncLayoutSlotData {
    Block(MachBlock),
    Const(MachConst),
    Global(MachGlobalValue),
    Hop(MachHop),
    NullBytes(usize),
}

type FuncLayout = Layout<FuncLayoutSlotData>;

fn build_initial_function_layout(
    func: &MachFuncData,
    dfs_tree: &fyrc_ssa_passes::DfsTree,
    loop_forest: &fyrc_ssa_passes::LoopNestingForest,
) -> LinkResult<FuncLayout> {
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
            layout.append(FuncLayoutSlotData::Block(block), block_size);
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

    Ok(layout)
}
