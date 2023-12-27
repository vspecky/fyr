use std::collections::BTreeSet;

use error_stack::{report, ResultExt};
use fxhash::FxHashSet;
use fyrc_ssa::{
    block::Block,
    function::{FunctionData, InstructionSet},
    instr::Instr,
    value::{Value, ValueData},
};
use fyrc_ssa_passes::{DominatorTree, LivenessAnalysis};
use fyrc_utils::DenseMap;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Register(u8);

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
        if let Some(free_reg) = self.free_regs.pop_last() {
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
pub struct SpillSlot(u16);

#[derive(Debug)]
struct SpillSlotFile {
    total_slots: u16,
    free_slots: BTreeSet<SpillSlot>,
    allocated_slots: FxHashSet<SpillSlot>,
}

impl SpillSlotFile {
    fn new() -> Self {
        Self {
            total_slots: 0,
            free_slots: BTreeSet::new(),
            allocated_slots: FxHashSet::default(),
        }
    }

    fn allocate(&mut self) -> SpillSlot {
        if let Some(slot) = self.free_slots.pop_last() {
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
            self.free_slots.insert(slot);
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
        if self.free_slots.remove(&slot) {
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
}

struct AllocFile<'a> {
    value_data: &'a DenseMap<Value, ValueData>,
    allocations: DenseMap<Value, ValueAllocType>,
    free_regs: DenseMap<Instr, Option<Register>>,
    reg_file: RegisterFile,
    spill_slot_file: SpillSlotFile,
}

impl<'a> AllocFile<'a> {
    fn new(func: &'a FunctionData) -> Self {
        Self {
            value_data: &func.values,
            allocations: DenseMap::with_prefilled(func.values.len()),
            free_regs: DenseMap::with_prefilled(func.instrs.len()),
            reg_file: RegisterFile::new(func.signature.isa),
            spill_slot_file: SpillSlotFile::new(),
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

    fn set_instr_free_register(&mut self, instr: Instr) {
        self.free_regs
            .set(instr, self.reg_file.free_regs.first().copied());
    }

    fn done(self) -> RegallocResult {
        RegallocResult {
            total_regs: self.reg_file.total_regs,
            free_regs: self.free_regs,
            total_spill_slots: self.spill_slot_file.total_slots,
            allocations: self.allocations,
        }
    }
}

pub struct RegallocResult {
    pub total_regs: u8,
    pub total_spill_slots: u16,
    pub allocations: DenseMap<Value, ValueAllocType>,
    pub free_regs: DenseMap<Instr, Option<Register>>,
}

pub fn perform_coloring(
    func: &FunctionData,
    dom: &DominatorTree,
    liveness: &LivenessAnalysis,
) -> ColoringResult<RegallocResult> {
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

            alloc_file.set_instr_free_register(instr);
            for instr_use in instr_uses {
                if !subsequent_uses.contains(&instr_use) {
                    alloc_file.free(instr_use)?;
                }
            }

            if let Some(def) = func.get_instr_def(instr) {
                alloc_file.allocate(def)?;
            }
        }

        let dom_children = dom
            .tree
            .get(block)
            .ok_or_else(|| report!(ColoringError::DominatorChildrenNotFound))?;

        stack.extend(dom_children);
    }

    Ok(alloc_file.done())
}
