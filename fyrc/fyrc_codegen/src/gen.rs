use error_stack::{report, ResultExt};
use fyrc_machinst::{
    block::MachBlock,
    constant::{MachConst, MachConstData},
    func::MachFunc,
    instr::ThumbMachinstData,
    types::Register,
    value::MachGlobalValue,
};
use fyrc_machinst_builder::{builder::MachFuncBuilder, tasm};
use fyrc_regalloc::coloring::{FreeRegisters, RegallocOutput, SpillSlot, ValueAllocType};
use fyrc_ssa::{
    block::Block as SsaBlock,
    constant::ConstKind as SsaConstKind,
    function::FunctionData as SsaFunctionData,
    instr::{self as ssa_instr, Instr as SsaInstr, InstrData as SsaInstrData},
    value::{Value as SsaValue, ValueType},
};
use fyrc_utils::{consts::ABI_ARGS_IN_REGS, EntityId};

use crate::{
    error::{CodegenError, CodegenResult},
    ltg::{Location, LocationTransferGraph},
    utils::ValueTypeExt,
};

const SCRATCH_HI: Register = Register(0);
const WORD_SIZE: u8 = 4;

pub struct CodegenCtx<'a> {
    builder: MachFuncBuilder<'a>,
    ssa_func: SsaFunctionData,
    regalloc: RegallocOutput,
}

impl<'a> CodegenCtx<'a> {
    const MAX_SP_OFFSET_WORD_RANGE: u16 = 127;
    const MAX_SP_RELATIVE_READ_WORD_RANGE: u16 = 255;
    const CALLER_SAVED_REGS: [Register; 4] =
        [Register::R0, Register::R1, Register::R2, Register::R3];
    const CALLEE_SAVED_REGS: [Register; 4] =
        [Register::R4, Register::R5, Register::R6, Register::R7];

    pub fn new(
        builder: MachFuncBuilder<'a>,
        ssa_func: SsaFunctionData,
        regalloc: RegallocOutput,
    ) -> Self {
        Self {
            builder,
            ssa_func,
            regalloc,
        }
    }

    fn get_instr_def(&self, instr: SsaInstr) -> CodegenResult<SsaValue> {
        self.ssa_func
            .get_instr_def(instr)
            .ok_or_else(|| report!(CodegenError::InstrDefNotFound))
    }

    fn get_alloc_reg(&self, val: SsaValue) -> CodegenResult<Register> {
        self.regalloc
            .get_alloc(val)
            .get_register()
            .ok_or_else(|| report!(CodegenError::RegUnallocated))
    }

    fn get_alloc_spill_slot(&self, val: SsaValue) -> CodegenResult<SpillSlot> {
        self.regalloc
            .get_alloc(val)
            .get_spill_slot()
            .ok_or_else(|| report!(CodegenError::SpillSlotUnallocated))
    }

    fn emit_instr<I>(&mut self, instr: I) -> CodegenResult<()>
    where
        I: Into<ThumbMachinstData>,
    {
        self.builder
            .add_instr(instr)
            .change_context(CodegenError::InstrAddFailed)
    }

    fn emit_register_swap(&mut self, r1: Register, r2: Register) -> CodegenResult<()> {
        self.emit_instr(tasm!(MOV ^SCRATCH_HI, %r1))?;
        self.emit_instr(tasm!(LSL %r1, %r2, #0))?;
        self.emit_instr(tasm!(MOV %r2, ^SCRATCH_HI))?;
        Ok(())
    }

    #[inline]
    fn emit_mov(&mut self, dest: Register, src: Register) -> CodegenResult<()> {
        self.emit_instr(tasm!(LSL %dest, %src, #0))
    }

    fn emit_zero_ext(&mut self, rs: Register, val_type: ValueType) -> CodegenResult<()> {
        if let Some(offset) = val_type.get_ext_offset() {
            self.emit_instr(tasm!(LSL %rs, %rs, #offset))?;
            self.emit_instr(tasm!(LSR %rs, %rs, #offset))?;
        }
        Ok(())
    }

    fn emit_sign_ext(&mut self, rs: Register, val_type: ValueType) -> CodegenResult<()> {
        if let Some(offset) = val_type.get_ext_offset() {
            self.emit_instr(tasm!(LSL %rs, %rs, #offset))?;
            self.emit_instr(tasm!(ASR %rs, %rs, #offset))?;
        }
        Ok(())
    }

    fn emit_sp_forward(&mut self, mut distance: u16) -> CodegenResult<()> {
        while distance > Self::MAX_SP_OFFSET_WORD_RANGE {
            self.emit_sp_max_forward()?;
            distance -= Self::MAX_SP_OFFSET_WORD_RANGE;
        }

        if distance > 0 {
            self.emit_instr(tasm!(ADD SP, #distance as i16 * i16::from(WORD_SIZE)))?;
        }
        Ok(())
    }

    fn emit_sp_max_forward(&mut self) -> CodegenResult<()> {
        // +508 i.e. 127 words
        let offset_distance = 0b111111100i16;
        self.emit_instr(tasm!(ADD SP, #offset_distance))?;
        Ok(())
    }

    fn emit_sp_max_backward(&mut self) -> CodegenResult<()> {
        // -508 i.e. 127 words
        let offset_distance = -0b111111100i16;
        self.emit_instr(tasm!(ADD SP, #offset_distance))?;
        Ok(())
    }

    fn emit_load_stack_slot(&mut self, slot: SpillSlot, reg: Register) -> CodegenResult<()> {
        let mut spill_location_distance = slot.slot_id();
        let mut sp_forwards = 0u16;

        while spill_location_distance > Self::MAX_SP_RELATIVE_READ_WORD_RANGE {
            sp_forwards += 1;
            self.emit_sp_max_forward()?;
            spill_location_distance -= Self::MAX_SP_OFFSET_WORD_RANGE;
        }

        self.emit_instr(tasm!(LDR %reg, SP, #spill_location_distance * WORD_SIZE as u16))?;

        for _ in 0..sp_forwards {
            self.emit_sp_max_backward()?;
        }

        Ok(())
    }

    fn emit_store_stack_slot(&mut self, slot: SpillSlot, reg: Register) -> CodegenResult<()> {
        let mut store_location_distance = slot.slot_id();
        let mut sp_forwards = 0u16;

        while store_location_distance > Self::MAX_SP_RELATIVE_READ_WORD_RANGE {
            sp_forwards += 1;
            self.emit_sp_max_forward()?;
            store_location_distance -= Self::MAX_SP_OFFSET_WORD_RANGE;
        }

        self.emit_instr(tasm!(STR %reg, SP, #store_location_distance * WORD_SIZE as u16))?;

        for _ in 0..sp_forwards {
            self.emit_sp_max_backward()?;
        }

        Ok(())
    }

    fn emit_alu_binary_op(
        &mut self,
        instr: SsaInstr,
        data: &ssa_instr::AluBinaryOp,
    ) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let res_type = self
            .ssa_func
            .get_value_type(def_val)
            .change_context(CodegenError::SsaFunctionError)?;

        let rs = self.get_alloc_reg(data.rs)?;
        let rn = self.get_alloc_reg(data.rn)?;
        let rd = self.get_alloc_reg(def_val)?;

        macro_rules! comm {
            ($name:ident) => {{
                if rs == rd {
                    self.emit_instr(tasm!($name %rd, %rn))?;
                } else if rn == rd {
                    self.emit_instr(tasm!($name %rd, %rs))?;
                } else {
                    self.emit_mov(rd, rs)?;
                    self.emit_instr(tasm!($name %rd, %rn))?;
                }
            }};
        }

        macro_rules! non_comm {
            ($name:ident) => {{
                if rn == rd {
                    self.emit_register_swap(rs, rn)?;
                } else if rs != rd {
                    self.emit_mov(rd, rs)?;
                }

                self.emit_instr(tasm!($name %rd, %rn))?;
            }};
        }

        match data.op {
            ssa_instr::AluBinaryOpKind::Add => self.emit_instr(tasm!(ADD %rd, %rs, %rn))?,
            ssa_instr::AluBinaryOpKind::Sub => self.emit_instr(tasm!(SUB %rd, %rs, %rn))?,
            ssa_instr::AluBinaryOpKind::Mul => comm!(MUL),
            ssa_instr::AluBinaryOpKind::Lsl => non_comm!(LSL),
            ssa_instr::AluBinaryOpKind::Lsr => {
                self.emit_zero_ext(rs, res_type)?;
                non_comm!(LSR)
            }
            ssa_instr::AluBinaryOpKind::Asr => {
                self.emit_sign_ext(rs, res_type)?;
                non_comm!(ASR)
            }
            ssa_instr::AluBinaryOpKind::And => comm!(AND),
            ssa_instr::AluBinaryOpKind::Orr => comm!(ORR),
            ssa_instr::AluBinaryOpKind::Xor => comm!(EOR),
        }

        Ok(())
    }

    fn emit_alu_unary_op(
        &mut self,
        instr: SsaInstr,
        data: &ssa_instr::AluUnaryOp,
    ) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let res_type = self
            .ssa_func
            .get_value_type(def_val)
            .change_context(CodegenError::SsaFunctionError)?;

        let rs = self.get_alloc_reg(data.rs)?;
        let rd = self.get_alloc_reg(def_val)?;

        match data.op {
            ssa_instr::AluUnaryOpKind::Neg => {
                self.emit_sign_ext(rs, res_type)?;
                self.emit_instr(tasm!(NEG %rd, %rs))?;
            }

            ssa_instr::AluUnaryOpKind::Not => {
                self.emit_instr(tasm!(MVN %rd, %rs))?;
            }
        }

        Ok(())
    }

    fn emit_load(&mut self, instr: SsaInstr, data: &ssa_instr::Load) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let res_type = self
            .ssa_func
            .get_value_type(def_val)
            .change_context(CodegenError::SsaFunctionError)?;

        let rb = self.get_alloc_reg(data.base)?;
        let ro = self.get_alloc_reg(data.offset)?;
        let rd = self.get_alloc_reg(def_val)?;

        match res_type {
            ValueType::Int8 => self.emit_instr(tasm!(LDRB %rd, %rb, %ro))?,
            ValueType::Int16 => self.emit_instr(tasm!(LDRH %rd, %rb, %ro))?,
            ValueType::Int32 => self.emit_instr(tasm!(LDR %rd, %rb, %ro))?,
        }

        Ok(())
    }

    fn emit_store(&mut self, data: &ssa_instr::Store) -> CodegenResult<()> {
        let val_type = self
            .ssa_func
            .get_value_type(data.val)
            .change_context(CodegenError::SsaFunctionError)?;

        let rd = self.get_alloc_reg(data.val)?;
        let rb = self.get_alloc_reg(data.base)?;
        let ro = self.get_alloc_reg(data.offset)?;

        match val_type {
            ValueType::Int8 => self.emit_instr(tasm!(STRB %rd, %rb, %ro))?,
            ValueType::Int16 => self.emit_instr(tasm!(STRH %rd, %rb, %ro))?,
            ValueType::Int32 => self.emit_instr(tasm!(STR %rd, %rb, %ro))?,
        }
        Ok(())
    }

    fn emit_load_const(
        &mut self,
        instr: SsaInstr,
        data: &ssa_instr::LoadConst,
    ) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let mach_const = MachConst::with_id(data.const_ref.get_id());
        let rd = self.get_alloc_reg(def_val)?;

        self.emit_instr(tasm!(LDR %rd, PC, #mach_const))?;
        Ok(())
    }

    fn emit_load_global(
        &mut self,
        instr: SsaInstr,
        data: &ssa_instr::LoadGlobal,
    ) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let rd = self.get_alloc_reg(def_val)?;
        let global_val = MachGlobalValue::with_id(data.value.get_id());

        self.emit_instr(tasm!(LDR %rd, PC, #global_val))?;
        Ok(())
    }

    fn emit_call(&mut self, instr: SsaInstr, data: &ssa_instr::Call) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let rd = self.get_alloc_reg(def_val)?;
        let caller_saved_registers = Self::CALLER_SAVED_REGS
            .into_iter()
            .filter(|r| r != &rd)
            .collect::<Vec<_>>();

        self.emit_instr(tasm!(PUSH caller_saved_registers.clone()))?;
        let stack_args_offset = data.args.len().saturating_sub(ABI_ARGS_IN_REGS);
        self.emit_instr(tasm!(ADD SP, #-(stack_args_offset as i16 * WORD_SIZE as i16)))?;
        // stack args size + caller saved registers
        let slot_offset = stack_args_offset + caller_saved_registers.len();
        let mut ltg = LocationTransferGraph::new();

        for (dest, val) in data
            .args
            .iter()
            .take(ABI_ARGS_IN_REGS)
            .enumerate()
            .map(|(i, v)| (Register(i as u8), *v))
        {
            let alloc = self.regalloc.get_alloc(val);

            match alloc {
                ValueAllocType::Register(src) => {
                    ltg.add_edge(src, dest);
                }
                ValueAllocType::SpillSlot(ss) => {
                    ltg.add_edge(ss.offset(slot_offset as u16), dest);
                }
                ValueAllocType::Unallocated => {
                    return Err(report!(CodegenError::RegUnallocated));
                }
            }
        }

        for (dest, val) in data
            .args
            .iter()
            .skip(ABI_ARGS_IN_REGS)
            .enumerate()
            .map(|(i, v)| (SpillSlot(i as u16), *v))
        {
            let src_alloc = self.regalloc.get_alloc(val);

            match src_alloc {
                ValueAllocType::Register(src) => {
                    ltg.add_edge(src, dest);
                }

                ValueAllocType::SpillSlot(ss) => {
                    ltg.add_edge(ss.offset(slot_offset as u16), dest);
                }

                ValueAllocType::Unallocated => {
                    return Err(report!(CodegenError::RegUnallocated));
                }
            }
        }

        let ltg_applicator = LTGApplicator::new();
        let free_regs = self
            .regalloc
            .pre_instr_free_regs
            .get(instr)
            .copied()
            .ok_or_else(|| report!(CodegenError::FreeRegsNotFound))?;
        ltg_applicator.apply(self, ltg, free_regs)?;

        self.emit_instr(tasm!(BL MachFunc::with_id(data.func.get_id())))?;
        if stack_args_offset > 0 {
            self.emit_instr(tasm!(ADD SP, #stack_args_offset as i16 * WORD_SIZE as i16))?;
        }
        self.emit_mov(rd, Register::R0)?;
        self.emit_instr(tasm!(POP caller_saved_registers))?;

        Ok(())
    }

    fn emit_return(&mut self, data: &ssa_instr::Ret) -> CodegenResult<()> {
        if let Some(retval) = data.val {
            match self.regalloc.get_alloc(retval) {
                ValueAllocType::Register(rs) => {
                    if rs != Register::R0 {
                        self.emit_mov(Register::R0, rs)?;
                    }
                }

                ValueAllocType::SpillSlot(ss) => {
                    self.emit_load_stack_slot(ss, Register::R0)?;
                }

                ValueAllocType::Unallocated => {
                    return Err(report!(CodegenError::RegUnallocated));
                }
            }
        }

        let stack_args = self
            .ssa_func
            .arg_values
            .len()
            .saturating_sub(ABI_ARGS_IN_REGS);

        let to_move = self.regalloc.total_spill_slots - (stack_args + 5) as u16;
        self.emit_sp_forward(to_move)?;
        self.emit_instr(tasm!(POP Self::CALLEE_SAVED_REGS, PC))?;
        Ok(())
    }

    fn emit_icmp(&mut self, instr: SsaInstr, data: &ssa_instr::ICmp) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let rs_type = self
            .ssa_func
            .get_value_type(data.rs)
            .change_context(CodegenError::SsaFunctionError)?;
        let rn_type = self
            .ssa_func
            .get_value_type(data.rn)
            .change_context(CodegenError::SsaFunctionError)?;

        let rs = self.get_alloc_reg(data.rs)?;
        let rn = self.get_alloc_reg(data.rn)?;
        let rd = self.get_alloc_reg(def_val)?;

        match data.cond {
            ssa_instr::Cond::Eq
            | ssa_instr::Cond::Neq
            | ssa_instr::Cond::Ugt
            | ssa_instr::Cond::Ugte
            | ssa_instr::Cond::Ult
            | ssa_instr::Cond::Ulte => {
                self.emit_zero_ext(rs, rs_type)?;
                self.emit_zero_ext(rn, rn_type)?;
            }

            ssa_instr::Cond::Sgt
            | ssa_instr::Cond::Sgte
            | ssa_instr::Cond::Slt
            | ssa_instr::Cond::Slte => {
                self.emit_sign_ext(rs, rs_type)?;
                self.emit_sign_ext(rn, rn_type)?;
            }
        }

        if rd == rs {
            self.emit_instr(tasm!(MOV ^SCRATCH_HI, %rs))?;
        } else if rd == rn {
            self.emit_instr(tasm!(MOV ^SCRATCH_HI, %rn))?;
        }

        self.emit_instr(tasm!(MOV %rd, #1))?;

        if rd == rs {
            self.emit_instr(tasm!(CMP ^SCRATCH_HI, %rn))?;
        } else if rd == rn {
            self.emit_instr(tasm!(CMP %rs, ^SCRATCH_HI))?;
        }

        // since the PC is 4 bytes ahead of the current instruction owing to prefetch.
        let offset = -2;
        match data.cond {
            ssa_instr::Cond::Eq => self.emit_instr(tasm!(BEQ offset))?,
            ssa_instr::Cond::Neq => self.emit_instr(tasm!(BNE offset))?,
            ssa_instr::Cond::Sgt => self.emit_instr(tasm!(BGT offset))?,
            ssa_instr::Cond::Sgte => self.emit_instr(tasm!(BGE offset))?,
            ssa_instr::Cond::Slt => self.emit_instr(tasm!(BLT offset))?,
            ssa_instr::Cond::Slte => self.emit_instr(tasm!(BLE offset))?,
            ssa_instr::Cond::Ugt => self.emit_instr(tasm!(BHI offset))?,
            ssa_instr::Cond::Ugte => self.emit_instr(tasm!(BCS offset))?,
            ssa_instr::Cond::Ult => self.emit_instr(tasm!(BCC offset))?,
            ssa_instr::Cond::Ulte => self.emit_instr(tasm!(BLS offset))?,
        }

        self.emit_instr(tasm!(MOV %rd, #0))?;
        Ok(())
    }

    fn emit_branch(&mut self, data: &ssa_instr::Branch) -> CodegenResult<()> {
        let cr = self.get_alloc_reg(data.value)?;
        let then_block = MachBlock::with_id(data.then_block.get_id());
        let else_block = MachBlock::with_id(data.else_block.get_id());
        self.emit_instr(tasm!(CMP %cr, #1))?;
        self.emit_instr(tasm!(BEQ then_block))?;
        self.emit_instr(tasm!(B else_block))?;
        Ok(())
    }

    fn emit_jump(&mut self, data: &ssa_instr::Jump) -> CodegenResult<()> {
        let dest = MachBlock::with_id(data.dest.get_id());
        self.emit_instr(tasm!(B dest))?;
        Ok(())
    }

    fn emit_copy(&mut self, instr: SsaInstr, data: &ssa_instr::CopyV) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let rs = self.get_alloc_reg(data.val)?;
        let rd = self.get_alloc_reg(def_val)?;

        if rs != rd {
            self.emit_mov(rd, rs)?;
        }
        Ok(())
    }

    fn emit_spill(&mut self, instr: SsaInstr, data: &ssa_instr::SpillValue) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let rs = self.get_alloc_reg(data.val)?;
        let spill_slot = self.get_alloc_spill_slot(def_val)?;
        let mut spill_location_distance = spill_slot.slot_id();
        let mut sp_forwards = 0u16;

        while spill_location_distance > Self::MAX_SP_RELATIVE_READ_WORD_RANGE {
            sp_forwards += 1;
            self.emit_sp_max_forward()?;
            spill_location_distance -= Self::MAX_SP_OFFSET_WORD_RANGE;
        }

        self.emit_instr(tasm!(STR %rs, SP, #spill_location_distance * WORD_SIZE as u16))?;

        for _ in 0..sp_forwards {
            self.emit_sp_max_backward()?;
        }

        Ok(())
    }

    fn emit_reload(&mut self, instr: SsaInstr, data: &ssa_instr::ReloadValue) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let rd = self.get_alloc_reg(def_val)?;
        let spill_slot = self.get_alloc_spill_slot(data.val)?;
        self.emit_load_stack_slot(spill_slot, rd)?;
        Ok(())
    }

    fn process_block(&mut self, ssa_block: SsaBlock) -> CodegenResult<()> {
        let mach_block = MachBlock::with_id(ssa_block.get_id());
        self.builder
            .switch_to_block(mach_block)
            .change_context(CodegenError::BuilderError)?;

        let ssa_block_data = self
            .ssa_func
            .get_block(ssa_block)
            .change_context(CodegenError::SsaFunctionError)?;
        let ssa_block_instrs = ssa_block_data.iter_instr().collect::<Vec<_>>();
        let ssa_block_preds = ssa_block_data.predecessors.clone();
        let ssa_block_phis = ssa_block_data.phis.clone();

        if !ssa_block_data.phis.is_empty() {
            for pred in ssa_block_preds {
                let mut ltg = LocationTransferGraph::new();

                for &phi in &ssa_block_phis {
                    let phi_data = self
                        .ssa_func
                        .get_phi_data(phi)
                        .change_context(CodegenError::SsaFunctionError)?;

                    let def_alloc = self.regalloc.get_alloc(phi_data.value);
                    let &block_arg = phi_data
                        .args
                        .get(&pred)
                        .ok_or_else(|| report!(CodegenError::SsaFunctionError))
                        .attach_printable("couldn't find phi arg from predecessor block")?;

                    let arg_alloc = self.regalloc.get_alloc(block_arg);

                    match (def_alloc, arg_alloc) {
                        (ValueAllocType::Register(to), ValueAllocType::SpillSlot(from)) => {
                            ltg.add_edge(from, to);
                        }

                        (ValueAllocType::Register(to), ValueAllocType::Register(from)) => {
                            ltg.add_edge(from, to);
                        }

                        (ValueAllocType::SpillSlot(to), ValueAllocType::Register(from)) => {
                            ltg.add_edge(from, to);
                        }

                        (ValueAllocType::SpillSlot(to), ValueAllocType::SpillSlot(from)) => {
                            ltg.add_edge(from, to);
                        }

                        (ValueAllocType::Unallocated, _) | (_, ValueAllocType::Unallocated) => {
                            return Err(report!(CodegenError::RegUnallocated));
                        }
                    }
                }

                self.builder
                    .switch_to_block(MachBlock::with_id(pred.get_id()))
                    .change_context(CodegenError::BuilderError)?;

                let ltg_applicator = LTGApplicator::new();
                let &pred_end_free_registers = self
                    .regalloc
                    .block_end_free_regs
                    .get(pred)
                    .ok_or_else(|| report!(CodegenError::FreeRegsNotFound))?;
                ltg_applicator.apply(self, ltg, pred_end_free_registers)?;
            }

            self.builder
                .switch_to_block(mach_block)
                .change_context(CodegenError::BuilderError)?;
        }

        for instr in ssa_block_instrs {
            let ssa_instr_data = self
                .ssa_func
                .get_instr(instr)
                .change_context(CodegenError::SsaFunctionError)?
                .clone();

            match &ssa_instr_data {
                SsaInstrData::AluBinaryOp(binop) => self.emit_alu_binary_op(instr, binop),
                SsaInstrData::AluUnaryOp(unop) => self.emit_alu_unary_op(instr, unop),
                SsaInstrData::Call(call) => self.emit_call(instr, call),
                SsaInstrData::Load(load) => self.emit_load(instr, load),
                SsaInstrData::Store(store) => self.emit_store(store),
                SsaInstrData::LoadConst(ldconst) => self.emit_load_const(instr, ldconst),
                SsaInstrData::LoadGlobal(ldglobal) => self.emit_load_global(instr, ldglobal),
                SsaInstrData::Ret(ret) => self.emit_return(ret),
                SsaInstrData::ICmp(icmp) => self.emit_icmp(instr, icmp),
                SsaInstrData::Branch(branch) => self.emit_branch(branch),
                SsaInstrData::Jump(jmp) => self.emit_jump(jmp),
                SsaInstrData::CopyV(copyv) => self.emit_copy(instr, copyv),
                SsaInstrData::SpillValue(spill) => self.emit_spill(instr, spill),
                SsaInstrData::ReloadValue(reload) => self.emit_reload(instr, reload),
            }?
        }

        Ok(())
    }

    fn emit_prelude(&mut self) -> CodegenResult<()> {
        self.builder
            .switch_to_block(MachBlock::START)
            .change_context(CodegenError::BuilderError)?;

        self.emit_instr(tasm!(PUSH Self::CALLEE_SAVED_REGS, LR))?;
        Ok(())
    }

    pub fn build(mut self) -> CodegenResult<()> {
        for ssa_block_data in self.ssa_func.blocks.values() {
            let mach_block = self.builder.add_block();
            let mach_block_data = self
                .builder
                .get_func_data_mut()
                .get_block_mut(mach_block)
                .change_context(CodegenError::MachFuncError)?;

            mach_block_data.preds = ssa_block_data
                .predecessors
                .iter()
                .map(|pred| MachBlock::with_id(pred.get_id()))
                .collect();

            mach_block_data.succs = ssa_block_data
                .successors
                .iter()
                .map(|succ| MachBlock::with_id(succ.get_id()))
                .collect();
        }

        for const_kind in self.ssa_func.consts.values() {
            self.builder.define_const(match const_kind {
                SsaConstKind::Const8(c) => MachConstData::Int8(*c),
                SsaConstKind::Const16(c) => MachConstData::Int16(*c),
                SsaConstKind::Const32(c) => MachConstData::Int32(*c),
            });
        }

        self.emit_prelude()?;

        for block in self.ssa_func.blocks.keys() {
            self.process_block(block)?;
        }
        Ok(())
    }
}

struct LTGApplicator {
    preallocated_scratch_reg: Option<Register>,
    running_scratch_reg: Option<Register>,
    scratch_is_low: bool,
    preallocated_mem2mem_reg: Option<Register>,
}

impl LTGApplicator {
    fn new() -> Self {
        Self {
            preallocated_scratch_reg: None,
            preallocated_mem2mem_reg: None,
            running_scratch_reg: None,
            scratch_is_low: true,
        }
    }

    fn prealloc_scratch_regs(&mut self, ltg: &LocationTransferGraph, free_regs: &FreeRegisters) {
        let mut free_regs = free_regs.get_free_regs();
        let clobbered = ltg.get_clobbered_registers();
        free_regs.retain(|reg| !clobbered.contains(reg));

        if ltg.requires_scratch() {
            if let Some(free) = free_regs.pop() {
                self.preallocated_scratch_reg = Some(free);
            }
        }

        if ltg.has_mem2mem() {
            if let Some(free) = free_regs.pop() {
                self.preallocated_mem2mem_reg = Some(free);
            }
        }
    }

    fn with_mem2mem_reg<F>(&mut self, codegen: &mut CodegenCtx<'_>, func: F) -> CodegenResult<()>
    where
        F: Fn(&mut CodegenCtx<'_>, Register) -> CodegenResult<()>,
    {
        if let Some(preallocated) = self.preallocated_mem2mem_reg {
            return func(codegen, preallocated);
        }

        if let (Some(preallocated), None) =
            (self.preallocated_scratch_reg, self.running_scratch_reg)
        {
            return func(codegen, preallocated);
        }

        codegen.emit_instr(tasm!(MOV ^Register(2), %Register(0)))?;
        func(codegen, Register(0))?;
        codegen.emit_instr(tasm!(MOV %Register(0), ^Register(2)))?;
        Ok(())
    }

    fn allocate_running_scratch(&mut self) -> CodegenResult<Register> {
        if self.running_scratch_reg.is_some() {
            return Err(report!(CodegenError::RedundantScratchAssignment));
        }

        if let Some(preallocated) = self.preallocated_scratch_reg {
            self.running_scratch_reg = Some(preallocated);
            self.scratch_is_low = true;
            return Ok(preallocated);
        }

        self.running_scratch_reg = Some(Register(0));
        self.scratch_is_low = false;
        Ok(Register(0))
    }

    fn apply(
        mut self,
        codegen: &mut CodegenCtx<'_>,
        ltg: LocationTransferGraph,
        free_regs: FreeRegisters,
    ) -> CodegenResult<()> {
        self.prealloc_scratch_regs(&ltg, &free_regs);
        let transfers = ltg.solve();

        for transfer in transfers {
            match (transfer.to, transfer.from) {
                (Location::Reg(to), Location::Reg(from)) => {
                    codegen.emit_mov(to, from)?;
                }

                (Location::Reg(to), Location::Mem(from)) => {
                    codegen.emit_load_stack_slot(from, to)?;
                }

                (Location::Reg(to), Location::Scratch) => {
                    let scratch_reg = self
                        .running_scratch_reg
                        .ok_or_else(|| report!(CodegenError::NoScratchRegFound))?;
                    self.running_scratch_reg = None;

                    if self.scratch_is_low {
                        codegen.emit_mov(to, scratch_reg)?;
                    } else {
                        codegen.emit_instr(tasm!(MOV %to, ^scratch_reg))?;
                    }
                }

                (Location::Mem(to), Location::Reg(from)) => {
                    codegen.emit_store_stack_slot(to, from)?;
                }

                (Location::Mem(to), Location::Mem(from)) => {
                    self.with_mem2mem_reg(codegen, |codegen, reg| {
                        codegen.emit_load_stack_slot(from, reg)?;
                        codegen.emit_store_stack_slot(to, reg)
                    })?;
                }

                (Location::Mem(to), Location::Scratch) => {
                    let scratch_reg = self
                        .running_scratch_reg
                        .ok_or_else(|| report!(CodegenError::NoScratchRegFound))?;
                    self.running_scratch_reg = None;

                    if self.scratch_is_low {
                        codegen.emit_store_stack_slot(to, scratch_reg)?;
                    } else {
                        self.with_mem2mem_reg(codegen, |codegen, reg| {
                            codegen.emit_instr(tasm!(MOV %reg, ^scratch_reg))?;
                            codegen.emit_store_stack_slot(to, reg)
                        })?;
                    }
                }

                (Location::Scratch, Location::Reg(from)) => {
                    let scratch_reg = self.allocate_running_scratch()?;

                    if self.scratch_is_low {
                        codegen.emit_mov(scratch_reg, from)?;
                    } else {
                        codegen.emit_instr(tasm!(MOV ^scratch_reg, %from))?;
                    }
                }

                (Location::Scratch, Location::Mem(from)) => {
                    let scratch_reg = self.allocate_running_scratch()?;

                    if self.scratch_is_low {
                        codegen.emit_load_stack_slot(from, scratch_reg)?;
                    } else {
                        self.with_mem2mem_reg(codegen, |codegen, reg| {
                            codegen.emit_load_stack_slot(from, reg)?;
                            codegen.emit_instr(tasm!(MOV ^scratch_reg, %reg))
                        })?;
                    }
                }

                (Location::Scratch, Location::Scratch) => {
                    return Err(report!(CodegenError::Scratch2ScratchAssignment));
                }
            }
        }

        Ok(())
    }
}
