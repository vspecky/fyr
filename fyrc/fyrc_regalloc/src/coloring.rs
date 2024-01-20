use std::collections::{BTreeSet, VecDeque};

use error_stack::{report, ResultExt};
use fxhash::FxHashSet;
use fyrc_machinst::types::Register;
use fyrc_ssa::{
    block::Block,
    function::{FunctionData, InstructionSet},
    instr::Instr,
    value::{Value, ValueData},
};
use fyrc_ssa_passes::{DominatorTree, LivenessAnalysis};
use fyrc_utils::{
    consts::{ABI_ARGS_IN_REGS, ABI_NUM_CALLEE_SAVED_REGS},
    DenseMap,
};

#[derive(Debug, thiserror::Error)]
pub enum ColoringError {
    #[error("tried to allocate an already allocated value (probably duplicate definitions)")]
    AllocAlreadyAllocdValue,
    #[error("dominator tree children not found for node")]
    DominatorChildrenNotFound,
    #[error("tried to free register that was not allocated")]
    FreedNonAllocatedRegister,
    #[error("tried to free a spill slot that was not allocated")]
    FreedNonAllocatedSpillSlot,
    #[error("tried to free a value that was not allocated")]
    FreedNonAllocatedValue,
    #[error("there was an error when interacting with the function data")]
    FunctionError,
    #[error("there was an error with the liveness analysis information")]
    LivenessAnalysisError,
    #[error("no register available for allocation")]
    NoRegisterAvailable,
    #[error("tried to set register that was already allocated")]
    SetAllocatedRegister,
    #[error("tried to set spill slot that was already allocated")]
    SetAllocatedSpillSlot,
    #[error("allocation not found for value")]
    ValueAllocationNotFound,
    #[error("value data not found for value")]
    ValueDataNotFound,
}

type ColoringResult<T> = Result<T, error_stack::Report<ColoringError>>;

/// Stores the free registers at a program point as a bitset
#[derive(Debug, Clone, Default, Copy, PartialEq, Eq)]
pub struct FreeRegisters(u16);

impl FreeRegisters {
    pub fn new(regs: impl IntoIterator<Item = Register>) -> Self {
        let mut free = 0u16;
        for reg in regs.into_iter() {
            free |= 0b1 << reg.0;
        }

        Self(free)
    }

    pub fn contains(&self, reg: Register) -> bool {
        self.0 & (0b1 << reg.thumb()) > 0
    }

    pub fn get_free_regs(&self) -> Vec<Register> {
        let mut free = self.0;
        let mut regs = Vec::new();
        for off in 0..16 {
            if free & 0b1 > 0 {
                regs.push(Register(off));
            }
            free >>= 1;
        }

        regs
    }

    pub fn mark_occupied(&mut self, reg: Register) {
        self.0 = self.0 & !(0b1 << reg.thumb());
    }
}

#[derive(Debug)]
struct RegisterFile {
    total_regs: u8,
    free_regs: BTreeSet<Register>,
    allocated_regs: FxHashSet<Register>,
}

impl RegisterFile {
    fn new(isa: InstructionSet) -> Self {
        let mut new_me = Self {
            total_regs: match isa {
                InstructionSet::Thumb => 8,
            },
            free_regs: BTreeSet::new(),
            allocated_regs: FxHashSet::default(),
        };
        new_me.reset();
        new_me
    }

    fn allocate(&mut self) -> ColoringResult<Register> {
        if let Some(free_reg) = self.free_regs.pop_first() {
            self.allocated_regs.insert(free_reg);
            Ok(free_reg)
        } else {
            Err(report!(ColoringError::NoRegisterAvailable))
        }
    }

    fn free(&mut self, reg: Register) -> ColoringResult<()> {
        if self.allocated_regs.remove(&reg) {
            self.free_regs.insert(reg);
            Ok(())
        } else {
            Err(report!(ColoringError::FreedNonAllocatedRegister))
        }
    }

    fn reset(&mut self) {
        self.free_regs = (0..self.total_regs).map(Register).collect();
        self.allocated_regs = FxHashSet::default();
    }

    fn set_allocated(&mut self, reg: Register) -> ColoringResult<()> {
        if self.free_regs.remove(&reg) {
            self.allocated_regs.insert(reg);
            Ok(())
        } else {
            Err(report!(ColoringError::SetAllocatedRegister))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SpillSlot(pub u16);

impl SpillSlot {
    pub fn slot_id(&self) -> u16 {
        self.0
    }

    pub fn offset(self, offset: u16) -> Self {
        Self(self.0 + offset)
    }
}

#[derive(Debug)]
struct SpillSlotFile {
    total_slots: u16,
    stack_arg_count: u16,
    free_slots: VecDeque<SpillSlot>,
    allocated_slots: FxHashSet<SpillSlot>,
}

impl SpillSlotFile {
    fn new(stack_arg_count: u16) -> Self {
        Self {
            // We set the total slots to 5 to account for r4-r7 (4) callee saved registers
            // and LR.
            total_slots: 5,
            stack_arg_count,
            free_slots: VecDeque::new(),
            allocated_slots: FxHashSet::from_iter((0..5).map(SpillSlot)),
        }
    }

    fn allocate(&mut self) -> SpillSlot {
        if let Some(slot) = self.free_slots.pop_back() {
            self.allocated_slots.insert(slot);
            slot
        } else {
            let next_slot = SpillSlot(self.total_slots);
            self.total_slots += 1;
            self.allocated_slots.insert(next_slot);
            next_slot
        }
    }

    fn free(&mut self, slot: SpillSlot) -> ColoringResult<()> {
        if self.allocated_slots.remove(&slot) {
            // here we prefer pushing to the front of the deque if the stack slot is one in which
            // the arguments of the function were passed in. This causes the allocation algorithm
            // to actively discourage allocating the arg stack slots to new memory variables.
            //
            // The reason we do this is cuz initially when new stack slots are added, they start
            // from 0 i.e. the closest to the stack pointer. So the initial stack slots allocated
            // to stack arguments are taken to be the closest to the stack pointer which is actually
            // not the case practically since the stack args are stack pushed by the caller before the
            // callee moves the stack pointer to make space for additional stack slots i.e. they are
            // actually the farthest from the stack pointer in practice. Hence we discourage picking
            // these slots as these are going to be re-Id'd to be the farthest slots anyways.
            if slot.slot_id() < self.stack_arg_count + ABI_NUM_CALLEE_SAVED_REGS as u16 {
                self.free_slots.push_front(slot);
            } else {
                self.free_slots.push_back(slot);
            }
            Ok(())
        } else {
            Err(report!(ColoringError::FreedNonAllocatedSpillSlot))
        }
    }

    fn reset(&mut self) {
        self.free_slots = (0..self.total_slots).map(SpillSlot).collect();
        self.allocated_slots = FxHashSet::default();
    }

    fn set_allocated(&mut self, slot: SpillSlot) -> ColoringResult<()> {
        if self.free_slots.contains(&slot) {
            self.free_slots.retain(|ss| ss != &slot);
            self.allocated_slots.insert(slot);
            Ok(())
        } else {
            Err(report!(ColoringError::SetAllocatedSpillSlot))
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum ValueAllocType {
    Register(Register),
    SpillSlot(SpillSlot),
    #[default]
    Unallocated,
}

impl ValueAllocType {
    #[inline]
    fn is_unallocated(&self) -> bool {
        matches!(self, Self::Unallocated)
    }

    pub fn get_register(&self) -> Option<Register> {
        match self {
            Self::Register(reg) => Some(*reg),
            Self::SpillSlot(_) | Self::Unallocated => None,
        }
    }

    pub fn get_spill_slot(&self) -> Option<SpillSlot> {
        match self {
            Self::SpillSlot(slot) => Some(*slot),
            Self::Register(_) | Self::Unallocated => None,
        }
    }
}

struct AllocFile<'a> {
    value_data: &'a DenseMap<Value, ValueData>,
    allocations: DenseMap<Value, ValueAllocType>,
    pre_instr_free_regs: DenseMap<Instr, FreeRegisters>,
    block_end_free_regs: DenseMap<Block, FreeRegisters>,
    reg_file: RegisterFile,
    spill_slot_file: SpillSlotFile,
}

impl<'a> AllocFile<'a> {
    fn new(func: &'a FunctionData) -> Self {
        Self {
            value_data: &func.values,
            allocations: DenseMap::with_prefilled(func.values.len()),
            pre_instr_free_regs: DenseMap::with_prefilled(func.instrs.len()),
            block_end_free_regs: DenseMap::with_prefilled(func.blocks.len()),
            reg_file: RegisterFile::new(func.signature.isa),
            spill_slot_file: SpillSlotFile::new(
                func.arg_values.len().saturating_sub(ABI_ARGS_IN_REGS) as u16,
            ),
        }
    }

    fn get_allocation(&self, val: Value) -> ColoringResult<ValueAllocType> {
        self.allocations
            .get(val)
            .copied()
            .ok_or_else(|| report!(ColoringError::ValueAllocationNotFound))
    }

    fn allocate(&mut self, val: Value) -> ColoringResult<()> {
        let allocation = self.get_allocation(val)?;

        if allocation.is_unallocated() {
            let is_mem = self
                .value_data
                .get(val)
                .ok_or_else(|| report!(ColoringError::ValueDataNotFound))?
                .is_mem;

            let allocation = if is_mem {
                ValueAllocType::SpillSlot(self.spill_slot_file.allocate())
            } else {
                ValueAllocType::Register(self.reg_file.allocate()?)
            };

            self.allocations.set(val, allocation);
            Ok(())
        } else {
            Err(report!(ColoringError::AllocAlreadyAllocdValue))
        }
    }

    fn free(&mut self, val: Value) -> ColoringResult<()> {
        let allocation = self.get_allocation(val)?;

        match allocation {
            ValueAllocType::Register(reg) => self.reg_file.free(reg)?,
            ValueAllocType::SpillSlot(ss) => self.spill_slot_file.free(ss)?,
            ValueAllocType::Unallocated => {
                return Err(report!(ColoringError::FreedNonAllocatedValue));
            }
        }

        Ok(())
    }

    fn reset(&mut self) {
        self.reg_file.reset();
        self.spill_slot_file.reset();
    }

    fn set_allocated(&mut self, to_set: impl Iterator<Item = Value>) -> ColoringResult<()> {
        for value in to_set {
            let allocation = self.get_allocation(value)?;

            match allocation {
                ValueAllocType::Register(reg) => self.reg_file.set_allocated(reg)?,
                ValueAllocType::SpillSlot(ss) => self.spill_slot_file.set_allocated(ss)?,
                ValueAllocType::Unallocated => {
                    return Err(report!(ColoringError::ValueAllocationNotFound));
                }
            }
        }

        Ok(())
    }

    fn set_instr_free_registers(&mut self, instr: Instr) {
        self.pre_instr_free_regs.set(
            instr,
            FreeRegisters::new(self.reg_file.free_regs.iter().copied()),
        );
    }

    fn set_block_end_free_registers(&mut self, block: Block) {
        self.block_end_free_regs.set(
            block,
            FreeRegisters::new(self.reg_file.free_regs.iter().copied()),
        );
    }

    /// This function rotates the stack slot IDs counter-clockwise by the number of arguments
    /// to the function that are passed on the stack so that the stack slots assigned to stack
    /// args are the farthest ones to the left (with the argument order on the stack being the
    /// same as the prototype). The reason we do this is explained in the stack slot file free()
    /// function above.
    fn shift_stack_slots(&mut self) {
        let stack_args_count = self.spill_slot_file.stack_arg_count as i16 + 5;
        let total_slots = self.spill_slot_file.total_slots as i16;

        if stack_args_count == 0 {
            return;
        }

        for alloc_type in self.allocations.values_mut() {
            if let ValueAllocType::SpillSlot(ss) = alloc_type {
                *ss = SpillSlot(((ss.slot_id() as i16 - stack_args_count) % total_slots) as u16);
            }
        }
    }

    fn done(mut self) -> RegallocOutput {
        self.shift_stack_slots();
        RegallocOutput {
            total_regs: self.reg_file.total_regs,
            pre_instr_free_regs: self.pre_instr_free_regs,
            block_end_free_regs: self.block_end_free_regs,
            total_spill_slots: self.spill_slot_file.total_slots,
            allocations: self.allocations,
        }
    }
}

pub struct RegallocOutput {
    pub total_regs: u8,
    pub total_spill_slots: u16,
    pub allocations: DenseMap<Value, ValueAllocType>,
    pub pre_instr_free_regs: DenseMap<Instr, FreeRegisters>,
    pub block_end_free_regs: DenseMap<Block, FreeRegisters>,
}

impl RegallocOutput {
    #[inline]
    pub fn get_alloc(&self, value: Value) -> ValueAllocType {
        self.allocations
            .get(value)
            .copied()
            .unwrap_or(ValueAllocType::Unallocated)
    }
}

pub fn perform_coloring(
    func: &FunctionData,
    dom: &DominatorTree,
    liveness: &LivenessAnalysis,
) -> ColoringResult<RegallocOutput> {
    let mut alloc_file = AllocFile::new(func);
    let mut stack = vec![Block::START];

    for &arg in &func.arg_values {
        alloc_file.allocate(arg)?;
    }

    while let Some(block) = stack.pop() {
        alloc_file.reset();
        let mut live_in = liveness
            .get_live_in(block)
            .change_context(ColoringError::LivenessAnalysisError)?
            .clone();
        let block_data = func
            .get_block(block)
            .change_context(ColoringError::FunctionError)?;

        let mut phi_values = Vec::with_capacity(block_data.phis.len());
        for &phi in &block_data.phis {
            let phi_data = func
                .get_phi_data(phi)
                .change_context(ColoringError::FunctionError)?;

            live_in.remove(&phi_data.value);
            phi_values.push(phi_data.value);
        }

        alloc_file.set_allocated(live_in.iter().copied())?;

        for phi_value in phi_values {
            alloc_file.allocate(phi_value)?;
        }

        for instr in block_data.iter_instr() {
            let instr_uses = func
                .get_instr_uses(instr)
                .change_context(ColoringError::FunctionError)?;
            let subsequent_uses = liveness
                .get_subsequent_use_set(instr)
                .change_context(ColoringError::LivenessAnalysisError)?;

            alloc_file.set_instr_free_registers(instr);
            for instr_use in instr_uses {
                if !subsequent_uses.contains(&instr_use) {
                    alloc_file.free(instr_use)?;
                }
            }

            if let Some(def) = func.get_instr_def(instr) {
                alloc_file.allocate(def)?;
            }
        }

        alloc_file.set_block_end_free_registers(block);

        let dom_children = dom
            .tree
            .get(block)
            .ok_or_else(|| report!(ColoringError::DominatorChildrenNotFound))?;

        stack.extend(dom_children);
    }

    Ok(alloc_file.done())
}
