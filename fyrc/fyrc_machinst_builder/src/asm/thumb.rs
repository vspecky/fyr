use fyrc_machinst::{instr::thumb, types::Register};

pub enum RegisterOr<T> {
    Register(Register),
    Or(T),
}

impl<T> From<Register> for RegisterOr<T> {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}

impl<T: From<u16>> From<u16> for RegisterOr<T> {
    fn from(value: u16) -> Self {
        Self::Or(T::from(value))
    }
}

impl<T: From<i16>> From<i16> for RegisterOr<T> {
    fn from(value: i16) -> Self {
        Self::Or(T::from(value))
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __tasm_internal {
    // MOV Rd, #Offset8
    // Move 8-bit immediate value into Rd.
    (MOV %$rd:expr, #$offset8:expr) => {
        MovCmpAddSubImm {
            op: MovCmpAddSubImmOp::Mov,
            rd: $rd,
            imm: Imm8::from($offset8),
        }
    };

    // MOV Rd, Hs
    // Move a value from a register in the range 8-15 to a register in the range 0-7.
    (MOV %$rd:expr, ^$hs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBxOp::Mov,
            h1: true,
            h2: false,
            rs_hs: $hs,
            rd_hd: $rd,
        }
    };

    // MOV Hd, Rs
    // Move a value from a register in the range 0-7 to a register in the range 8-15.
    (MOV ^$hd:expr, %$rs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBxOp::Mov,
            h1: false,
            h2: true,
            rs_hs: $rs,
            rd_hd: $hd,
        }
    };

    // MOV Hd, Hs
    // Move a value between two registers in the range 8-15.
    (MOV ^$hd:expr, ^$hs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBx::Mov,
            h1: true,
            h2: true,
            rs_hs: $hs,
            rd_hd: $hd,
        }
    };

    // BX Rs
    // Perform branch (plus optional state change) to address in a register in the range 0-7.
    (BX %$rs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBx::Bx,
            h1: false,
            h2: false,
            rs_hs: $rs,
            rd_hd: Register(0),
        }
    };

    // BX Hs,
    // Perform branch (plus optional state change) to address in a register in the range 8-15.
    (BX ^$hs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBx::Bx,
            h1: false,
            h2: true,
            rs_hs: $hs,
            rd_hd: Register(0),
        }
    };

    // CMP Rd, #Offset8
    // Compare contents of Rd with 8-bit immediate value.
    (CMP %$rd:expr, #$offset8:expr) => {
        MovCmpAddSubImm {
            op: MovCmpAddSubImmOp::Cmp,
            rd: $rd,
            imm: Imm8::from($offset8),
        }
    };

    // CMP Rd, Rs
    // Set condition codes on Rd - Rs.
    (CMP %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Cmp,
            rd: $rd,
            rs: $rs,
        }
    };

    // CMP Rd, Hs
    // Compare a register in the range 0-7 with a register in the range 8-15. Set the condition
    // code flags on the result.
    (CMP %$rd:expr, ^$hs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBxOp::Cmp,
            h1: true,
            h2: false,
            rs_hs: $hs,
            rd_hd: $rd,
        }
    };

    // CMP Hd, Rs
    // Compare a register in the range 8-15 with a register in the range 0-7. Set the condition
    // code flags on the result.
    (CMP ^$hd:expr, %$rs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBxOp::Cmp,
            h1: true,
            h2: false,
            rs_hs: $rs,
            rd_hd: $hd,
        }
    };

    // CMP Hd, Hs
    // Compare two registers in thr range 8-15. Set the condition code flags on the result.
    (CMP ^$hd:expr, ^$hs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBxOp::Cmp,
            h1: true,
            h2: true,
            rs_hs: $hs,
            rd_hd: $hd,
        }
    };

    // CMN Rd, Rs
    // Set condition codes on Rd + Rs.
    (CMN %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Cmn,
            rd: $rd,
            rs: $rs,
        }
    };

    // ADD Rd, PC, #Imm
    // Add #Imm to the current value of the program counter (PC) and load the result into Rd.
    (ADD %$rd:expr, PC, #$word8:expr) => {
        LdAddr {
            source: LdAddrSource::Pc,
            rd: $rd,
            imm: Word8::from($word8),
        }
    };

    // ADD Rd, SP, #Imm
    // Add #Imm to the current value of the stack pointer (SP) and load the result into Rd.
    (ADD %$rd:expr, SP, #$word8:expr) => {
        LdAddr {
            source: LdAddrSource::Sp,
            rd: $rd,
            imm: Word8::from($word8),
        }
    };

    // ADD Rd, Rs, Rn
    // Add contents of Rn to contents of Rs. Place result in Rd.
    (ADD %$rd:expr, %$rs:expr, %$rn:expr) => {
        AddSub {
            imm: AddSubImmFlag::RegisterOp($rn),
            op: AddSubOp::Add,
            rs: $rs,
            rd: $rd,
        }
    };

    // ADD Rd, Rs, #Offset3
    // Add 3-bit immediate value to contents of Rs. Place result in Rd.
    (ADD %$rd:expr, %$rs:expr, #$offset3:expr) => {
        AddSub {
            imm: AddSubImmFlag::ImmOp(Imm3::from($offset3)),
            op: AddSubOp::Add,
            rs: $rs,
            rd: $rd,
        }
    };

    // ADD SP, #Imm
    // ADD SP, #-Imm
    // Add immediate value to the stack pointer.
    (ADD SP, #$word7:expr) => {
        OffsetSP {
            sign_flag: if $word7 < 0 {
                OffsetSPSignFlag::Negative
            } else {
                OffsetSPSignFlag::Positive
            },
            imm: Word7::from($word7.abs() as u16),
        }
    };

    // ADD Rd, #Offset8
    // Add 8-bit immediate value to contents of Rd and place the result in Rd.
    (ADD %$rd:expr, #$offset8:expr) => {
        MovCmpAddSubImm {
            op: MovCmpAddSubImmOp::Add,
            rd: $rd,
            imm: Imm8::from($offset8),
        }
    };

    // ADD Rd, Hs
    // Add a register in the range 8-15 to a register in the range 0-7.
    (ADD %$rd:expr, ^$hs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBxOp::Add,
            h1: true,
            h2: false,
            rs_hs: $hs,
            rd_hd: $rd,
        }
    };

    // ADD Hd, Rs
    // Add a register in the range 0-7 to a register in the range 8-15.
    (ADD ^$hd:expr, %$rs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBxOp::Add,
            h1: false,
            h2: true,
            rs_hs: $rs,
            rd_hd: $hd,
        }
    };

    // ADD Hd, Hs
    // Add two registers in the range 8-15.
    (ADD ^$hd:expr, ^$hs:expr) => {
        HiRegOpsBx {
            op: HiRegOpsBxOp::Add,
            h1: true,
            h2: true,
            rs_hs: $hs,
            rd_hd: $hd,
        }
    };

    // SUB Rd, Rs, Rn
    // Subtract contents of Rn from contents of Rs. Place result in Rd.
    (SUB %$rd:expr, %$rs:expr, %$rn:expr) => {
        AddSub {
            imm: AddSubImmFlag::RegisterOp($rn),
            op: AddSubOp::Sub,
            rs: $rs,
            rd: $rd,
        }
    };

    // SUB Rd, Rs, #Offset3
    // Subtract 3-bit immediate value from contents of Rs. Place result in Rd.
    (SUB %$rd:expr, %$rs:expr, #$offset3:expr) => {
        AddSub {
            imm: AddSubImmFlag::ImmOp(Imm3::from($rn_or_offset)),
            op: AddSubOp::Sub,
            rs: $rs,
            rd: $rd,
        }
    };

    // SUB Rd, #Offset8
    // Subtract 8-bit immediate value from contents of Rd and place the result in Rd.
    (SUB %$rd:expr, %$offset8:expr) => {
        MovCmpAddSubImm {
            op: MovCmpAddSubImmOp::Sub,
            rd: $rd,
            imm: Imm8::from($offset8),
        }
    };

    // AND Rd, Rs
    // Rd := Rd AND Rs
    (AND %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::And,
            rs: $rs,
            rd: $rd,
        }
    };

    // EOR Rd, Rs
    // Rd := Rd EOR Rs
    (EOR %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Eor,
            rs: $rs,
            rd: $rd,
        }
    };

    // LSL Rd, Rs, #Offset5
    // Shift Rs left by a 5-bit immediate value and store the result in Rd.
    (LSL %$rd:expr, %$rs:expr, #$offset5:expr) => {
        MovShReg {
            op: MovShRegOp::Lsl,
            offset5: Imm5::new($offset5),
            rs: $rs,
            rd: $rd,
        }
    };

    // LSL Rd, Rs
    // Rd := Rd << Rs
    (LSL %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Lsl,
            rs: $rs,
            rd: $rd,
        }
    };

    // LSR Rd, Rs, #Offset5
    // Perform logical shift right on Rs by a 5-bit immediate value and store the result in Rd.
    (LSR %$rd:expr, %$rs:expr, #$offset5:expr) => {
        MovShReg {
            op: MovShRegOp::Lsr,
            offset5: Imm5::new($offset5),
            rs: $rs,
            rd: $rd,
        }
    };

    // LSR Rd, Rs
    // Rd := Rd >> Rs
    (LSR %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Lsr,
            rs: $rs,
            rd: $rd,
        }
    };

    // ASR Rd, Rs, #Offset5
    // Perform arithmetic shift right on Rs by a 5-bit immediate value and store the result in Rd.
    (ASR %$rd:expr, %$rs:expr, #$offset5:expr) => {
        MovShReg {
            op: MovShRegOp::Asr,
            offset5: Imm5::new($offset5),
            rs: $rs,
            rd: $rd,
        }
    };

    // ASR Rd, Rs
    // Rd := Rd ASR Rs
    (ASR %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Asr,
            rs: $rs,
            rd: $rd,
        }
    };

    // ADC Rd, Rs
    // Rd := Rd + Rs + Cbit
    (ADC %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Adc,
            rs: $rs,
            rd: $rd,
        }
    };

    // SBC Rd, Rs
    // Rd := Rd - Rs - NOT Cbit
    (SBC %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Sbc,
            rs: $rs,
            rd: $rd,
        }
    };

    // ROR Rd, Rs
    // Rd := Rd ROR Rs
    (ROR %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Ror,
            rs: $rs,
            rd: $rd,
        }
    };

    // TST Rd, Rs
    // Set condition codes on Rd AND Rs
    (TST %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Tst,
            rs: $rs,
            rd: $rd,
        }
    };

    // NEG Rd, Rs
    // Rd := -Rs
    (NEG %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Neg,
            rs: $rs,
            rd: $rd,
        }
    };

    // ORR Rd, Rs
    // Rd := Rd OR Rs
    (ORR %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Orr,
            rs: $rs,
            rd: $rd,
        }
    };

    // MUL Rd, Rs
    // Rd := Rs * Rd
    (MUL %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Mul,
            rs: $rs,
            rd: $rd,
        }
    };

    // BIC Rd, Rs
    // Rd := Rd AND NOT Rs
    (BIC %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Bic,
            rs: $rs,
            rd: $rd,
        }
    };

    // MVN Rd, Rs
    // Rd := NOT Rs
    (MVN %$rd:expr, %$rs:expr) => {
        Alu {
            op: AluOp::Mvn,
            rs: $rs,
            rd: $rd,
        }
    };

    // LDR Rd, PC, #Imm
    // Add unsigned offset (255 words, 1020 bytes) in Imm to the current value of the PC. Load the
    // word from the resulting address into Rd.
    (LDR %$rd:expr, PC, #$offset:expr) => {
        PcRelativeLoad {
            rd: $rd,
            imm: $offset.into(),
        }
    };

    // LDR Rd, SP, #Imm
    // Add unsigned offset (255 words, 1020 bytes) in Imm to the current value of the SP. Load the
    // word from the resulting address into Rd.
    (LDR %$rd:expr, SP, #$word8:expr) => {
        SpRelativeLdStr {
            ld_str_flag: LdStrFlag::Load,
            rd: $rd,
            imm: Word8::from($word8),
        }
    };

    // LDR Rd, Rb, Ro
    // Pre-indexed word load: Calculate the source address by adding together the value in Rb and
    // the value in Ro. Load the contents of the address into Rd.
    (LDR %$rd:expr, %$rb:expr, %$ro:expr) => {
        LdStrRoff {
            ld_str_flag: LdStrFlag::Load,
            byte_word_flag: LdStrRoffBWFlag::Word,
            ro: $ro,
            rb: $rb,
            rd: $rd,
        }
    };

    // LDR Rd, Rb, #Imm
    // Calculate the source address by adding together the value in Rb and Imm. Load Rd from the
    // address.
    (LDR %$rd:expr, %$rb:expr, #$word5:expr) => {
        LdStrImmOff {
            ld_str_flag: LdStrFlag::Load,
            byte_or_word: LdStrImmOff::Word(Word5::from($word5)),
            rb: $rb,
            rd: $rd,
        }
    };

    // LDRB Rd, Rb, Ro
    // Pre-indexed byte load: Calculate the source address by adding together the value in Rb and
    // the value in Ro. Load the byte value at the resulting address.
    (LDRB %$rd:expr, %$rb:expr, %$ro:expr) => {
        LdStrRoff {
            ld_str_flag: LdStrFlag::Load,
            byte_word_flag: LdStrRoffBWFlag::Byte,
            ro: $ro,
            rb: $rb,
            rd: $rd,
        }
    };

    // LDSB Rd, Rb, Ro
    // Load sign-extended byte: Add Ro to base address in Rb. Load bits 0-7 of Rd from the resulting
    // address, and set bits 8-31 of Rd to bit 7.
    (LDSB %$rd:expr, %$rb:expr, %$ro:expr) => {
        LdStSeBHw {
            h_flag: false,
            s_flag: true,
            ro: $ro,
            rb: $rb,
            rd: $rd,
        }
    };

    // LDRH Rd, Rb, Ro
    // Load halfword: Add Ro to base address in Rb. Load bits 0-15 of Rd from the resulting address,
    // and set bits 16-31 of Rd to 0.
    (LDRH %$rd:expr, %$rb:expr, %$ro:expr) => {
        LdStSeBHw {
            h_flag: true,
            s_flag: false,
            ro: $ro,
            rb: $rb,
            rd: $rd,
        }
    };

    // LDRH Rd, Rb, #Imm
    // Add #Imm to base address in Rb. Load bits 0-15 from the resulting address into Rd and set
    // bits 16-31 to zero.
    (LDRH %$rd:expr, %$rb:expr, #$halfword5:expr) => {
        LdStrHw {
            ld_str_flag: LdStrFlag::Load,
            offset5: Halfword5::from($halfword5),
            rb: $rb,
            rd: $rd,
        }
    };

    // LDRB Rd, Rb, #Imm
    // Calculate source address by adding together the value in Rb and Imm. Load the byte value
    // at the address into Rd.
    (LDRB %$rd:expr, %$rb:expr, #$imm5:expr) => {
        LdStrImmOff {
            ld_str_flag: LdStrFlag::Load,
            byte_or_word: LdStrImmOffBW::Byte(Imm5::from($imm5)),
            rb: $rb,
            rd: $rd,
        }
    };

    // LDSH Rd, Rb, Ro,
    // Load sign-extended halfword: Add Ro to base address in Rb. Load bits 0-15 of Rd from the
    // resulting address, and set bits 16-31 of Rd to bit 15.
    (LDSH %$rd:expr, %$rb:expr, %$ro:expr) => {
        LdStSeBHw {
            h_flag: true,
            s_flag: true,
            ro: $ro,
            rb: $rb,
            rd: $rd,
        }
    };

    // STR Rd, SP, #Imm
    // Add unsigned offset (255 words, 1020 bytes) in Imm to the current value of the SP. Store
    // the contents of Rd at the resulting address.
    (STR %$rd:expr, SP, #$word8:expr) => {
        SpRelativeLdStr {
            ld_str_flag: LdStrFlag::Store,
            rd: $rd,
            imm: Word8::from($word8),
        }
    };

    // STR Rd, Rb, Ro
    // Pre-indexed word store: Calculate the target address by adding together the value in Rb
    // and the value in Ro. Store the contents of Rd at the address.
    (STR %$rd:expr, %$rb:expr, %$ro:expr) => {
        LdStrRoff {
            ld_str_flag: LdStrFlag::Store,
            byte_word_flag: LdStrRoffBWFlag::Word,
            ro: $ro,
            rb: $rb,
            rd: $rd,
        }
    };

    // STR Rd, Rb, #Imm
    // Calculate the target address by adding together the value in Rb and Imm. Store the contents
    // of Rd at the address.
    (STR %$rd:expr, %$rb:expr, #$word5:expr) => {
        LdStrImmOff {
            ld_str_flag: LdStrFlag::Store,
            byte_or_word: LdStrImmOffBW::Word(Word5::from($word5)),
            rb: $rb,
            rd: $rd,
        }
    };

    // STRB Rd, Rb, Ro
    // Pre-indexed byte store: Calculate the target address by adding together the value in Rb and
    // the value in Ro. Store the byte value in Rd at the resulting address.
    (STRB %$rd:expr, %$rb:expr, %$ro:expr) => {
        LdStrRoff {
            ld_str_flag: LdStrFlag::Store,
            byte_word_flag: LdStrRoffBWFlag::Byte,
            ro: $ro,
            rb: $rb,
            rd: $rd,
        }
    };

    // STRB Rd, Rb, #Imm
    // Calculate the target address by adding together the value in Rb and Imm. Store the byte
    // value in Rd at the address.
    (STRB %$rd:expr, %$rb:expr, #$imm5:expr) => {
        LdStrImmOff {
            ld_str_flag: LdStrFlag::Store,
            byte_or_word: LdStrImmOffBW::Byte(Imm5::from($imm5)),
            rb: $rb,
            rd: $rd,
        }
    };

    // STRH Rd, Rb, Ro
    // Store halfword: Add Ro to base address in Rb. Store bits 0-15 of Rd at the resulting address.
    (STRH %$rd:expr, %$rb:expr, %$ro:expr) => {
        LdStSeBHw {
            h_flag: false,
            s_flag: false,
            ro: $ro,
            rb: $rb,
            rd: $rd,
        }
    };

    // STRH Rd, Rb, #Imm
    // Add #Imm to base address in Rb and store bits 0-15 of Rd at the resulting address.
    (STRH %$rd:expr, %$rb:expr, #$halfword5:expr) => {
        LdStrHw {
            ld_str_flag: LdStrFlag::Store,
            offset5: Halfword5::from($halfword5),
            rb: $rb,
            rd: $rd,
        }
    };

    // PUSH { Rlist, LR }
    // Push the Link Register and the registers specified by Rlist (if any) onto the stack. Update
    // the stack pointer.
    (PUSH $rlist:expr, LR) => {
        PushPopRegs {
            ld_str_flag: LdStrFlag::Store,
            pc_lr_bit: true,
            rlist: $rlist,
        }
    };

    // PUSH { Rlist }
    // Push the registers specified by Rlist onto the stack. Update the stack pointer.
    (PUSH $rlist:expr) => {
        PushPopRegs {
            ld_str_flag: LdStrFlag::Store,
            pc_lr_bit: false,
            rlist: $rlist,
        }
    };

    // POP { Rlist, PC }
    // Pop values off the stack and load into the registers specified by Rlist. Pop the PC off
    // the stack. Update the stack pointer.
    (POP $rlist:expr, PC) => {
        PushPopRegs {
            ld_str_flag: LdStrFlag::Load,
            pc_lr_bit: true,
            rlist: $rlist,
        }
    };

    // POP { Rlist }
    // Pop values off the stack into the registers specified by Rlist. Update the stack pointer.
    (POP $rlist:expr) => {
        PushPopRegs {
            ld_str_flag: LdStrFlag::Load,
            pc_lr_bit: false,
            rlist: $rlist,
        }
    };

    // STMIA Rb!, { Rlist }
    // Store the registers specified by Rlist, starting at the base address in Rb. Write back the
    // new base address.
    (STMIA %$rb:expr, $rlist:expr) => {
        MultipleLdStr {
            ld_str_flag: LdStrFlag::Store,
            rb: $rb,
            rlist: $rlist,
        }
    };

    // LDMIA Rb!, { Rlist }
    // Load the registers specified by Rlist, starting at the base address in Rb. Write back the
    // new base address.
    (LDMIA %$rb:expr, $rlist:expr) => {
        MultipleLdStr {
            ld_str_flag: LdStrFlag::Load,
            rb: $rb,
            rlist: $rlist,
        }
    };

    // BEQ label
    // Branch if Z set (equal)
    (BEQ $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Beq,
            soffset8: BranchDest::from($label),
        }
    };

    // BNE label
    // Branch if Z clear (not equal)
    (BNE $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bne,
            soffset8: BranchDest::from($label),
        }
    };

    // BCS label
    // Branch if C set (unsigned higher or same)
    (BCS $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bcs,
            soffset8: BranchDest::from($label),
        }
    };

    // BCC label
    // Branch if C clear (unsigned lower)
    (BCC $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bcc,
            soffset8: BranchDest::from($label),
        }
    };

    // BMI label
    // Branch if N set (negative)
    (BMI $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bmi,
            soffset8: BranchDest::from($label),
        }
    };

    // BPL label
    // Branch if N clear (positive or zero)
    (BPL $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bpl,
            soffset8: BranchDest::from($label),
        }
    };

    // BVS label
    // Branch if V set (overflow)
    (BVS $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bvs,
            soffset8: BranchDest::from($label),
        }
    };

    // BVC label
    // Branch if V clear (no overflow)
    (BVC $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bvc,
            soffset8: BranchDest::from($label),
        }
    };

    // BHI label
    // Branch if C set and Z clear (unsigned higher)
    (BHI $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bhi,
            soffset8: BranchDest::from($label),
        }
    };

    // BLS label
    // Branch if C clear or Z set (unsigned lower or same)
    (BLS $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bls,
            soffset8: BranchDest::from($label),
        }
    };

    // BGE label
    // Branch if N set and V set, or N clear and V clear (greater or equal)
    (BGE $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bge,
            soffset8: BranchDest::from($label),
        }
    };

    // BLT label
    // Branch if N set and V clear, or N clear and V set (less than)
    (BLT $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Blt,
            soffset8: BranchDest::from($label),
        }
    };

    // BGT label
    // Branch if Z clear, and either N set and V set or N clear and V clear (greater than)
    (BGT $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Bgt,
            soffset8: BranchDest::from($label),
        }
    };

    // BLE label
    // Branch if Z set, or N set and V clear, or N clear and V set (less than or equal)
    (BLE $label:expr) => {
        CondBranch {
            cond: CondBranchOp::Ble,
            soffset8: BranchDest::from($label),
        }
    };

    // SWI Value8
    // Perform Software Interrupt: Move the address of the next instruction into LR, move CPSR to
    // SPSR, load the SWI vector address (0x8) into the PC. Switch to ARM state and enter SVC mode.
    (SWI $value8:expr) => {
        Swi {
            value8: Imm8::from($value8),
        }
    };

    // B label
    // Branch PC relative +/- Offset11 << 1, where label is PC +/- 2048 bytes.
    (B $label:expr) => {
        UncondBranch {
            soffset11: BranchDest::from($label),
        }
    };

    // BL label
    // LR := PC + OffsetHigh << 12
    // temp := next instruction address
    // PC := LR + OffsetLow << 1
    // LR := temp | 1
    (BL $label:expr) => {
        BranchWithLink {
            offset: CallDest::from($label),
        }
    };
}

#[macro_export]
macro_rules! tasm {
    ($($tok:tt)+) => {{
        #[allow(unused)]
        use $crate::__private::fyrc_machinst::{
            block::MachBlock, constant::MachConst, instr::thumb::*, value::MachGlobalValue,
        };
        #[allow(unused)]
        use $crate::asm::thumb::RegisterOr;

        $crate::__tasm_internal!($($tok)+)
    }};
}

fn _test_tasm() {
    use fyrc_machinst::types::Register as R;

    let _a = tasm!(ADD %R(1), %R(2), #5);
    let _a = tasm!(ADD %R(1), %R(2), %R(3));
    let _a = tasm!(LSL %R(1), %R(2), #5);
    let _a = tasm!(LSR %R(1), %R(2), #5);
    let _a = tasm!(ASR %R(1), %R(2), #5);
    let _a = tasm!(PUSH vec![R(1)], LR);
}
