use error_stack::{report, ResultExt};
use fyrc_machinst::{
    block::MachBlock, constant::MachConst, instr::ThumbMachinstData, types::Register,
    value::MachGlobalValue,
};
use fyrc_machinst_builder::{builder::MachFuncBuilder, tasm};
use fyrc_regalloc::coloring::{RegallocOutput, SpillSlot};
use fyrc_ssa::{
    function::FunctionData as SsaFunctionData,
    instr::{self as ssa_instr, Instr as SsaInstr},
    value::{Value as SsaValue, ValueType},
};
use fyrc_utils::EntityId;

use crate::{
    error::{CodegenError, CodegenResult},
    utils::ValueTypeExt,
};

const SCRATCH_HI: Register = Register(0);
const WORD_SIZE: u16 = 4;

pub struct CodegenCtx<'a> {
    builder: MachFuncBuilder<'a>,
    ssa_func: SsaFunctionData,
    regalloc: RegallocOutput,
}

impl<'a> CodegenCtx<'a> {
    const MAX_SP_OFFSET_WORD_RANGE: u16 = 127;
    const MAX_SP_RELATIVE_READ_WORD_RANGE: u16 = 255;

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

    fn emit_return(&mut self, data: &ssa_instr::Ret) -> CodegenResult<()> {
        todo!()
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

        self.emit_instr(tasm!(STR %rs, SP, #spill_location_distance * 4))?;

        for _ in 0..sp_forwards {
            self.emit_sp_max_backward()?;
        }

        Ok(())
    }

    fn emit_reload(&mut self, instr: SsaInstr, data: &ssa_instr::ReloadValue) -> CodegenResult<()> {
        let def_val = self.get_instr_def(instr)?;
        let rd = self.get_alloc_reg(def_val)?;
        let spill_slot = self.get_alloc_spill_slot(data.val)?;
        let mut spill_location_distance = spill_slot.slot_id();
        let mut sp_forwards = 0u16;

        while spill_location_distance > Self::MAX_SP_RELATIVE_READ_WORD_RANGE {
            sp_forwards += 1;
            self.emit_sp_max_forward()?;
            spill_location_distance -= Self::MAX_SP_OFFSET_WORD_RANGE;
        }

        self.emit_instr(tasm!(LDR %rd, SP, #spill_location_distance * 4))?;

        for _ in 0..sp_forwards {
            self.emit_sp_max_backward()?;
        }

        Ok(())
    }
}
