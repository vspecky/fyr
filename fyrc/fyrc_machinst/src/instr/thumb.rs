use std::fmt;

use fyrc_utils::EntityId;

use crate::{
    block::MachBlock,
    constant::MachConst,
    func::MachFunc,
    hop::MachHop,
    types::{make_thumb_imm, make_thumb_simm, MachineCode, Register},
    value::MachGlobalValue,
};

macro_rules! repr16 {
    (@component($res:ident, $pos:ident) ($num:literal: $bits:expr)) => {
        $res |= $bits << ($pos - $num);
        #[allow(unused)]
        {
            $pos -= $num;
        }
    };

    ($($comps:tt)+) => {{
        let mut res = 0u16;
        let mut pos = 16;

        $(repr16!(@component(res, pos) $comps);)+

        res
    }};
}

macro_rules! impl_from_instr_for_instr_data {
    ($($instr:ident),+) => {
        $(
            impl From<$instr> for ThumbMachinstData {
                fn from(ins: $instr) -> Self {
                    Self::$instr(ins)
                }
            }
        )+
    };
}

make_thumb_imm! {
    (Halfword5 5 1)
    (Imm3 3)
    (Imm5 5)
    (Imm8 8)
    (Offset11 11)
    (Word5 5 2)
    (Word7 7 2)
    (Word8 8 2)
}

make_thumb_simm! {
    (SHalfword8 8 1)
    (SHalfword11 11 1)
}

/// Common Load/Store flag
#[derive(Debug, Clone)]
pub enum LdStrFlag {
    Store,
    Load,
}

#[derive(Debug, Clone)]
pub enum BranchDestKind {
    Block(MachBlock),
    Hop(MachHop),
    Local,
}

impl fmt::Display for BranchDestKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "%{}",
            match self {
                Self::Block(b) => b.to_string(),
                Self::Hop(h) => h.to_string(),
                Self::Local => "%".to_string(),
            }
        )
    }
}

impl From<MachBlock> for BranchDestKind {
    fn from(value: MachBlock) -> Self {
        Self::Block(value)
    }
}

impl From<MachHop> for BranchDestKind {
    fn from(value: MachHop) -> Self {
        Self::Hop(value)
    }
}

#[derive(Debug, Clone)]
pub struct BranchDest<T> {
    pub kind: BranchDestKind,
    pub offset: Option<T>,
}

impl<T> From<MachBlock> for BranchDest<T> {
    fn from(value: MachBlock) -> Self {
        Self {
            kind: value.into(),
            offset: None,
        }
    }
}

impl<T> From<MachHop> for BranchDest<T> {
    fn from(value: MachHop) -> Self {
        Self {
            kind: value.into(),
            offset: None,
        }
    }
}

impl<T: From<i16>> From<i16> for BranchDest<T> {
    fn from(value: i16) -> Self {
        Self {
            kind: BranchDestKind::Local,
            offset: Some(T::from(value)),
        }
    }
}

impl<T> BranchDest<T> {
    pub fn set_offset(&mut self, offset: T) {
        self.offset = Some(offset);
    }
}

impl<T> fmt::Display for BranchDest<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.kind.to_string())?;

        if let Some(ref offset) = self.offset {
            f.write_str(&format!(" ({offset})"))?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ValueRefKind {
    Const(MachConst),
    Global(MachGlobalValue),
}

impl From<MachConst> for ValueRefKind {
    fn from(value: MachConst) -> Self {
        Self::Const(value)
    }
}

impl From<MachGlobalValue> for ValueRefKind {
    fn from(value: MachGlobalValue) -> Self {
        Self::Global(value)
    }
}

impl fmt::Display for ValueRefKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "&{}",
            match self {
                Self::Const(c) => c.to_string(),
                Self::Global(g) => g.to_string(),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct ValueRef<T> {
    pub kind: ValueRefKind,
    pub offset: Option<T>,
}

impl<T> fmt::Display for ValueRef<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.kind.to_string())?;
        if let Some(offset) = &self.offset {
            f.write_str(&format!("({})", offset))?;
        }

        Ok(())
    }
}

impl<T> From<MachConst> for ValueRef<T> {
    fn from(value: MachConst) -> Self {
        Self {
            kind: value.into(),
            offset: None,
        }
    }
}

impl<T> From<MachGlobalValue> for ValueRef<T> {
    fn from(value: MachGlobalValue) -> Self {
        Self {
            kind: value.into(),
            offset: None,
        }
    }
}

impl LdStrFlag {
    fn bits(&self) -> u16 {
        match self {
            Self::Store => 0b0,
            Self::Load => 0b1,
        }
    }
}

/// Opcodes for [`MovShReg`]
#[derive(Debug, Clone)]
pub enum MovShRegOp {
    Lsl,
    Lsr,
    Asr,
}

impl MovShRegOp {
    fn bits(&self) -> u16 {
        match self {
            Self::Lsl => 0b00,
            Self::Lsr => 0b01,
            Self::Asr => 0b10,
        }
    }
}

impl fmt::Display for MovShRegOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Lsl => "lsl",
                Self::Lsr => "lsr",
                Self::Asr => "asr",
            }
        )
    }
}

/// Format 1: move shifted register
///
/// These instructions move a shifted value between low registers.
#[derive(Debug, Clone)]
pub struct MovShReg {
    /// The shift opcode
    pub op: MovShRegOp,
    /// 5 bit immediate value that specifies the shift magnitude
    pub offset5: Imm5,
    /// Source register
    pub rs: Register,
    /// Destination register
    pub rd: Register,
}

impl MachineCode for MovShReg {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (3: 0b000)
            (2: self.op.bits())
            (5: self.offset5.bits())
            (3: self.rs.thumb())
            (3: self.rd.thumb())
        )])
    }
}

impl fmt::Display for MovShReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}, {}, {}", self.op, self.rd, self.rs, self.offset5)
    }
}

/// Immediate flag for [`AddSub`]
#[derive(Debug, Clone)]
pub enum AddSubImmFlag {
    RegisterOp(Register),
    ImmOp(Imm3),
}

impl From<Register> for AddSubImmFlag {
    fn from(value: Register) -> Self {
        Self::RegisterOp(value)
    }
}

impl From<Imm3> for AddSubImmFlag {
    fn from(value: Imm3) -> Self {
        Self::ImmOp(value)
    }
}

impl From<u16> for AddSubImmFlag {
    fn from(value: u16) -> Self {
        Self::ImmOp(Imm3::new(value))
    }
}

impl AddSubImmFlag {
    fn flag_bit(&self) -> u16 {
        match self {
            Self::RegisterOp(_) => 0b0,
            Self::ImmOp(_) => 0b1,
        }
    }

    fn bits(&self) -> u16 {
        match self {
            Self::RegisterOp(reg) => reg.thumb(),
            Self::ImmOp(imm) => imm.bits(),
        }
    }
}

/// Opcode for [`AddSub`]
#[derive(Debug, Clone)]
pub enum AddSubOp {
    Add,
    Sub,
}

impl AddSubOp {
    fn flag_bit(&self) -> u16 {
        match self {
            Self::Add => 0b0,
            Self::Sub => 0b1,
        }
    }
}

impl fmt::Display for AddSubOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "add",
                Self::Sub => "sub",
            }
        )
    }
}

/// Format 2: add/subtract
///
/// These instructions allow the contents of a low register or a 3-bit immediate value to be added
/// or subtracted from a low register.
#[derive(Debug, Clone)]
pub struct AddSub {
    /// Immediate flag (0 - Register Operand, 1 - Immediate Operand)
    /// and also Register(Rn)/Immediate Value
    pub imm: AddSubImmFlag,
    /// Opcode (0 - Add, 1 - Sub)
    pub op: AddSubOp,
    /// Source Register
    pub rs: Register,
    /// Destination Register
    pub rd: Register,
}

impl MachineCode for AddSub {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (5: 0b00011)
            (1: self.imm.flag_bit())
            (1: self.op.flag_bit())
            (3: self.imm.bits())
            (3: self.rs.thumb())
            (3: self.rd.thumb())
        )])
    }
}

impl fmt::Display for AddSub {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}, {}, {}",
            self.op,
            self.rd,
            self.rs,
            match self.imm {
                AddSubImmFlag::ImmOp(op) => op.to_string(),
                AddSubImmFlag::RegisterOp(reg) => reg.to_string(),
            }
        )
    }
}

/// Opcodes for [`MovCmpAddSubImm`]
#[derive(Debug, Clone)]
pub enum MovCmpAddSubImmOp {
    Mov,
    Cmp,
    Add,
    Sub,
}

impl MovCmpAddSubImmOp {
    fn bits(&self) -> u16 {
        match self {
            Self::Mov => 0b00,
            Self::Cmp => 0b01,
            Self::Add => 0b10,
            Self::Sub => 0b11,
        }
    }
}

impl fmt::Display for MovCmpAddSubImmOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Mov => "mov",
                Self::Cmp => "cmp",
                Self::Add => "add",
                Self::Sub => "sub",
            }
        )
    }
}

/// Format 3: move/compare/add/subtract immediate
///
/// The instructions in this group perform operations between a Lo register and an 8-bit immediate
/// value.
#[derive(Debug, Clone)]
pub struct MovCmpAddSubImm {
    /// Opcode (0 - Mov, 1 - Cmp, 2 - Add, 3 - Sub)
    pub op: MovCmpAddSubImmOp,
    /// Source/destination register
    pub rd: Register,
    /// Immediate value
    pub imm: Imm8,
}

impl MachineCode for MovCmpAddSubImm {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (3: 0b001)
            (2: self.op.bits())
            (3: self.rd.thumb())
            (8: self.imm.bits())
        )])
    }
}

impl fmt::Display for MovCmpAddSubImm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {}, {}", self.op, self.rd, self.imm)
    }
}

/// ALU operations opcodes
#[derive(Debug, Clone)]
pub enum AluOp {
    And,
    Eor,
    Lsl,
    Lsr,
    Asr,
    Adc,
    Sbc,
    Ror,
    Tst,
    Neg,
    Cmp,
    Cmn,
    Orr,
    Mul,
    Bic,
    Mvn,
}

impl fmt::Display for AluOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::And => "and",
                Self::Eor => "eor",
                Self::Lsl => "lsl",
                Self::Lsr => "lsr",
                Self::Asr => "asr",
                Self::Adc => "adc",
                Self::Sbc => "sbc",
                Self::Ror => "ror",
                Self::Tst => "tst",
                Self::Neg => "neg",
                Self::Cmp => "cmp",
                Self::Cmn => "cmn",
                Self::Orr => "orr",
                Self::Mul => "mul",
                Self::Bic => "bic",
                Self::Mvn => "mvn",
            }
        )
    }
}

impl AluOp {
    fn bits(&self) -> u16 {
        match self {
            Self::And => 0b0000,
            Self::Eor => 0b0001,
            Self::Lsl => 0b0010,
            Self::Lsr => 0b0011,
            Self::Asr => 0b0100,
            Self::Adc => 0b0101,
            Self::Sbc => 0b0110,
            Self::Ror => 0b0111,
            Self::Tst => 0b1000,
            Self::Neg => 0b1001,
            Self::Cmp => 0b1010,
            Self::Cmn => 0b1011,
            Self::Orr => 0b1100,
            Self::Mul => 0b1101,
            Self::Bic => 0b1110,
            Self::Mvn => 0b1111,
        }
    }
}

/// Format 4: ALU operations
///
/// These instructions perform ALU operations on a Lo register pair.
#[derive(Debug, Clone)]
pub struct Alu {
    /// Opcode
    pub op: AluOp,
    /// Source register 2
    pub rs: Register,
    /// Source/destination register
    pub rd: Register,
}

impl MachineCode for Alu {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (6: 0b010000)
            (4: self.op.bits())
            (3: self.rs.thumb())
            (3: self.rd.thumb())
        )])
    }
}

impl fmt::Display for Alu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}, {}", self.op, self.rd, self.rs)
    }
}

/// Opcodes for [`HiRegOpsBx`]
#[derive(Debug, Clone)]
pub enum HiRegOpsBxOp {
    Add,
    Cmp,
    Mov,
    Bx,
}

impl HiRegOpsBxOp {
    fn bits(&self) -> u16 {
        match self {
            Self::Add => 0b00,
            Self::Cmp => 0b01,
            Self::Mov => 0b10,
            Self::Bx => 0b11,
        }
    }
}

impl fmt::Display for HiRegOpsBxOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "add",
                Self::Cmp => "cmp",
                Self::Mov => "mov",
                Self::Bx => "bx",
            }
        )
    }
}

/// Format 5: Hi register operations/branch exchange
///
/// There are four sets of instructions in this. The first three allow ADD, CMP and MOV operations
/// to be performed between Lo and Hi registers, or a pair of Hi registers. The fourth, BX, allows
/// a Branch to be performed which may also be used to switch processor state.
#[derive(Debug, Clone)]
pub struct HiRegOpsBx {
    /// The opcode
    pub op: HiRegOpsBxOp,
    /// Whether the source register is a Hi register
    pub h1: bool,
    /// Whether the destination register is a Hi register
    pub h2: bool,
    /// Source register
    pub rs_hs: Register,
    /// Destination register
    pub rd_hd: Register,
}

impl MachineCode for HiRegOpsBx {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (6: 0b010001)
            (2: self.op.bits())
            (1: u16::from(self.h1))
            (1: u16::from(self.h2))
            (3: self.rs_hs.thumb())
            (3: self.rd_hd.thumb())
        )])
    }
}

impl fmt::Display for HiRegOpsBx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rs = if self.h1 {
            format!("h{}", self.rs_hs.thumb())
        } else {
            format!("r{}", self.rs_hs.thumb())
        };

        let rd = if self.h1 {
            format!("h{}", self.rd_hd.thumb())
        } else {
            format!("r{}", self.rd_hd.thumb())
        };

        write!(
            f,
            "{}",
            match self.op {
                HiRegOpsBxOp::Bx => {
                    format!("bx {rs}")
                }

                HiRegOpsBxOp::Add | HiRegOpsBxOp::Cmp | HiRegOpsBxOp::Mov => {
                    format!("{} {}, {}", self.op, rd, rs)
                }
            }
        )
    }
}

/// Format 6: PC-relative load
///
/// This instruction loads a word from an address specified as a 10-bit immediate offset from the PC.
///
/// #### Note 1
/// The value specified by `imm` is a full 10-bit address, but must always be word-aligned (i.e.
/// with bits 1:0 set to 0), so `imm >> 2` is placed in the `Word8`
///
/// #### Note 2
/// The value of the PC will be 4 bytes greater than the address of this instruction, but bit
/// 1 of the PC is forced to 0 to ensure it is word aligned
#[derive(Debug, Clone)]
pub struct PcRelativeLoad {
    /// Destination register
    pub rd: Register,
    /// Immediate value
    pub imm: ValueRef<Word8>,
}

impl MachineCode for PcRelativeLoad {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        self.imm.offset.map(|imm| {
            vec![repr16!(
                (5: 0b01001)
                (3: self.rd.thumb())
                (8: imm.bits())
            )]
        })
    }
}

impl fmt::Display for PcRelativeLoad {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ldr {}, pc, {}", self.rd, self.imm)
    }
}

/// Byte/Word flag for [`LdStrRoff`]
#[derive(Debug, Clone)]
pub enum LdStrRoffBWFlag {
    Word,
    Byte,
}

impl LdStrRoffBWFlag {
    fn bits(&self) -> u16 {
        match self {
            Self::Word => 0b0,
            Self::Byte => 0b1,
        }
    }
}

/// Format 7: load/store with register offset
///
/// These instructions transfer byte or word values between registers and memory.
/// Memory addresses are pre-indexed using an offset register in the range 0-7.
#[derive(Debug, Clone)]
pub struct LdStrRoff {
    /// Load/Store flag (0 - Store to memory, 1 - Load from memory)
    pub ld_str_flag: LdStrFlag,
    /// Byte/Word flag (0 - Transfer word quantity, 1 - Transfer byte quantity)
    pub byte_word_flag: LdStrRoffBWFlag,
    /// Offset register
    pub ro: Register,
    /// Base register
    pub rb: Register,
    /// Source/destination register
    pub rd: Register,
}

impl MachineCode for LdStrRoff {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (4: 0b0101)
            (1: self.ld_str_flag.bits())
            (1: self.byte_word_flag.bits())
            (1: 0b0)
            (3: self.ro.thumb())
            (3: self.rb.thumb())
            (3: self.rd.thumb())
        )])
    }
}

impl fmt::Display for LdStrRoff {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self.ld_str_flag {
            LdStrFlag::Load => "ldr",
            LdStrFlag::Store => "str",
        })?;

        if matches!(self.byte_word_flag, LdStrRoffBWFlag::Byte) {
            f.write_str("b")?;
        }

        f.write_str(&format!(" {}, {}, {}", self.rd, self.rb, self.ro))?;

        Ok(())
    }
}

/// Format 8: load/store sign-extended byte/halfword
///
/// These instructions load optionally sign-extended bytes or halfwords, and store halfwords.
#[derive(Debug, Clone)]
pub struct LdStSeBHw {
    /// H-flag
    pub h_flag: bool,
    /// Sign-extended flag (0 - Operand not sign-extended, 1 - Operand sign-extended)
    pub s_flag: bool,
    /// Offset register
    pub ro: Register,
    /// Base register
    pub rb: Register,
    /// Destination register
    pub rd: Register,
}

impl MachineCode for LdStSeBHw {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (4: 0b0101)
            (1: u16::from(self.h_flag))
            (1: u16::from(self.s_flag))
            (1: 0b1)
            (3: self.ro.thumb())
            (3: self.rb.thumb())
            (3: self.rd.thumb())
        )])
    }
}

impl fmt::Display for LdStSeBHw {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}, {}, {}",
            match (self.h_flag, self.s_flag) {
                (false, false) => "strh",
                (false, true) => "ldrh",
                (true, false) => "ldsb",
                (true, true) => "ldsh",
            },
            self.rd,
            self.rb,
            self.ro
        )
    }
}

/// Byte/Word flag and offset for [`LdStrImmOff`]
#[derive(Debug, Clone)]
pub enum LdStrImmOffBW {
    Word(Word5),
    Byte(Imm5),
}

impl LdStrImmOffBW {
    fn flag_bit(&self) -> u16 {
        match self {
            Self::Word(_) => 0b0,
            Self::Byte(_) => 0b1,
        }
    }

    fn bits(&self) -> u16 {
        match self {
            Self::Word(word) => word.bits(),
            Self::Byte(byte) => byte.bits(),
        }
    }
}

impl fmt::Display for LdStrImmOffBW {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Word(w) => w.to_string(),
                Self::Byte(b) => b.to_string(),
            }
        )
    }
}

/// Format 9: load/store with immediate offset
///
/// These instructions transfer byte or word values between registers and memory using an immediate
/// 5 or 7-bit offset.
///
/// #### Note
/// For word accesses, the value specified by the offset is a full 7-bit address, but must be
/// word-aligned (i.e. with bits 1:0 set to 0), so we place `offset >> 2` in the instr bits.
#[derive(Debug, Clone)]
pub struct LdStrImmOff {
    /// Load/Store flag (0 - Store to memory, 1 - Load from memory)
    pub ld_str_flag: LdStrFlag,
    /// Byte/Word flag (0 - Transfer word quantity, 1 - Transfer byte quantity)
    /// and also Offset value
    pub byte_or_word: LdStrImmOffBW,
    /// Base register
    pub rb: Register,
    /// Source/destination register
    pub rd: Register,
}

impl MachineCode for LdStrImmOff {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (3: 0b011)
            (1: self.byte_or_word.flag_bit())
            (1: self.ld_str_flag.bits())
            (5: self.byte_or_word.bits())
            (3: self.rb.thumb())
            (3: self.rd.thumb())
        )])
    }
}

impl fmt::Display for LdStrImmOff {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self.ld_str_flag {
            LdStrFlag::Load => "ldr",
            LdStrFlag::Store => "str",
        })?;

        if matches!(self.byte_or_word, LdStrImmOffBW::Byte(_)) {
            f.write_str("b")?;
        }

        f.write_str(&format!(" {}, {}, {}", self.rd, self.rb, self.byte_or_word))?;

        Ok(())
    }
}

/// Format 10: load/store halfword
///
/// These instructions transfer halfword values between a Lo register and memory.
/// Addresses are pre-indexed, using a 6-bit immediate value.
///
/// #### Note
/// `offset5` is actuallya full 6-bit address but must be halfword aligned (i.e. with bit 0 set to
/// 0), so we place `offset5 >> 1` into the final machinst bits.
#[derive(Debug, Clone)]
pub struct LdStrHw {
    /// Load/Store flag (0 - Store to memory, 1 - Load from memory)
    pub ld_str_flag: LdStrFlag,
    /// Immediate value
    pub offset5: Halfword5,
    /// Base register
    pub rb: Register,
    /// Source/destination register
    pub rd: Register,
}

impl MachineCode for LdStrHw {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (4: 0b1000)
            (1: self.ld_str_flag.bits())
            (5: self.offset5.bits())
            (3: self.rb.thumb())
            (3: self.rd.thumb())
        )])
    }
}

impl fmt::Display for LdStrHw {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}, {}, {}",
            match self.ld_str_flag {
                LdStrFlag::Load => "ldrh",
                LdStrFlag::Store => "strh",
            },
            self.rd,
            self.rb,
            self.offset5
        )
    }
}

/// Format 11: SP-relative load/store
///
/// The instructions in this group perform an SP-relative load or store.
///
/// #### Note
/// The offset supplied in `imm` is a full 10-bit address, but must always be word-aligned
/// (i.e. bits 1:0 set to 0), so we put `imm >> 2` in the final machinst bits.
#[derive(Debug, Clone)]
pub struct SpRelativeLdStr {
    /// Load/Store flag (0 - Store to memory, 1 - Load from memory)
    pub ld_str_flag: LdStrFlag,
    /// Destination register
    pub rd: Register,
    /// Immediate value
    pub imm: Word8,
}

impl MachineCode for SpRelativeLdStr {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (4: 0b1001)
            (1: self.ld_str_flag.bits())
            (3: self.rd.thumb())
            (8: self.imm.bits())
        )])
    }
}

impl fmt::Display for SpRelativeLdStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}, sp, {}",
            match self.ld_str_flag {
                LdStrFlag::Load => "ldr",
                LdStrFlag::Store => "str",
            },
            self.rd,
            self.imm
        )
    }
}

/// Load address source (PC/SP) for [`LdAddr`]
#[derive(Debug, Clone)]
pub enum LdAddrSource {
    Pc,
    Sp,
}

impl LdAddrSource {
    fn bits(&self) -> u16 {
        match self {
            Self::Pc => 0b0,
            Self::Sp => 0b1,
        }
    }
}

impl fmt::Display for LdAddrSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Pc => "pc",
                Self::Sp => "sp",
            }
        )
    }
}

/// Format 12: load address
///
/// These instructions calculate an address by adding a 10-bit constant to either the PC or the SP,
/// and load the resulting address into a register.
///
/// #### Note
/// The value specified by `imm` is a full 10-bit value, but this must be word-aligned (i.e. with
/// bits 1:0 set to 0), so we place `imm >> 2` in the final machinst bits.
///
/// Where the PC is used as the source register, bit 1 of the PC is always read as 0. The value of
/// the PC will be 4 bytes greater than the address of the instruction before bit 1 is forced to 0.
#[derive(Debug, Clone)]
pub struct LdAddr {
    pub source: LdAddrSource,
    pub rd: Register,
    pub imm: Word8,
}

impl MachineCode for LdAddr {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (4: 0b1010)
            (1: self.source.bits())
            (3: self.rd.thumb())
            (8: self.imm.bits())
        )])
    }
}

impl fmt::Display for LdAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "add {}, {}, {}", self.rd, self.source, self.imm)
    }
}

/// Sign flag for [`OffsetSP`]
#[derive(Debug, Clone)]
pub enum OffsetSPSignFlag {
    Positive,
    Negative,
}

impl OffsetSPSignFlag {
    fn bits(&self) -> u16 {
        match self {
            Self::Positive => 0b0,
            Self::Negative => 0b1,
        }
    }
}

impl fmt::Display for OffsetSPSignFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Positive => "+",
                Self::Negative => "-",
            }
        )
    }
}

/// Format 13: add offset to Stack Pointer
///
/// This instruction adds a 9-bit signed constant to the stack pointer.
///
/// #### Note
/// The offset specified by `imm` can be up to +/- 508, but must be word-aligned (i.e. with bits
/// 1:0 set to 0), so we put `imm >> 2` in the machinst bits.
#[derive(Debug, Clone)]
pub struct OffsetSP {
    /// Sign flag (0 - Offset is positive, 1 - Offset is negative)
    pub sign_flag: OffsetSPSignFlag,
    /// Offset
    pub imm: Word7,
}

impl MachineCode for OffsetSP {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (8: 0b10110000)
            (1: self.sign_flag.bits())
            (7: self.imm.bits())
        )])
    }
}

impl fmt::Display for OffsetSP {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "add sp, #{}{}", self.sign_flag, self.imm.mag())
    }
}

/// Format 14: push/pop registers
///
/// Teh instructions in this group allow registers 0-7 and optionally LR to be pushed onto the
/// stack, and registers 0-7 and optionally PC to be popped off the stack.
///
/// #### Note
/// The stack is always assumed to be Full Descending.
#[derive(Debug, Clone)]
pub struct PushPopRegs {
    /// Load/Store flag (0 - Store to memory, 1 - Load from memory)
    pub ld_str_flag: LdStrFlag,
    /// PC/LR bit (0 - Do not store LR/load PC, 1 - Store LR/Load PC)
    pub pc_lr_bit: bool,
    /// Register list
    pub rlist: Vec<Register>,
}

impl MachineCode for PushPopRegs {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        let mut reg_list = 0u16;
        for &reg in &self.rlist {
            reg_list |= 0b1 << reg.thumb();
        }

        Some(vec![repr16!(
            (4: 0b1011)
            (1: self.ld_str_flag.bits())
            (2: 0b10)
            (1: u16::from(self.pc_lr_bit))
            (8: reg_list)
        )])
    }
}

impl fmt::Display for PushPopRegs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut list = self
            .rlist
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();

        if self.pc_lr_bit {
            list.push(match self.ld_str_flag {
                LdStrFlag::Load => "pc".to_string(),
                LdStrFlag::Store => "lr".to_string(),
            });
        }

        write!(
            f,
            "{} {{{}}}",
            match self.ld_str_flag {
                LdStrFlag::Load => "pop",
                LdStrFlag::Store => "push",
            },
            list.join(", ")
        )
    }
}

/// Format 15: multiple load/store
///
/// These instructions allow multiple loading and storing of Lo registers.
#[derive(Debug, Clone)]
pub struct MultipleLdStr {
    /// Load/Store flag (0 - Store to memory, 1 - Load from memory)
    pub ld_str_flag: LdStrFlag,
    /// Base register
    pub rb: Register,
    /// Register list
    pub rlist: Vec<Register>,
}

impl MachineCode for MultipleLdStr {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        let mut reg_list = 0u16;
        for &reg in &self.rlist {
            reg_list |= 0b1 << reg.thumb();
        }

        Some(vec![repr16!(
            (4: 0b1100)
            (1: self.ld_str_flag.bits())
            (3: self.rb.thumb())
            (8: reg_list)
        )])
    }
}

impl fmt::Display for MultipleLdStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let list = self
            .rlist
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();

        write!(
            f,
            "{} {}!, {{{}}}",
            match self.ld_str_flag {
                LdStrFlag::Load => "ldmia",
                LdStrFlag::Store => "stmia",
            },
            self.rb,
            list.join(", ")
        )
    }
}

/// Condition codes for [`CondBranch`]
#[derive(Debug, Clone)]
pub enum CondBranchOp {
    /// Branch if Z set (equal)
    Beq,
    /// Branch if Z clear (not equal)
    Bne,
    /// Branch if C set (unsigned higher or same)
    Bcs,
    /// Branch if C clear (unsigned lower)
    Bcc,
    /// Branch if N set (negative)
    Bmi,
    /// Branch if N clear (positive or zero)
    Bpl,
    /// Branch if V set (overflow)
    Bvs,
    /// Branch if V clear (no overflow)
    Bvc,
    /// Branch if C set and Z clear (unsigned higher)
    Bhi,
    /// Branch if C clear or Z set (unsigned lower or same)
    Bls,
    /// Branch if N set and V set, or N clear and V clear (greater or equal)
    Bge,
    /// Branch if N set and V clear, or N clear and V set (less than)
    Blt,
    /// Branch if Z clear, and either N set and V set or N clear and V clear (greater than)
    Bgt,
    /// Branch if Z set, or N set and V clear, or N clear and V set (less than or equal)
    Ble,
}

impl CondBranchOp {
    fn bits(&self) -> u16 {
        match self {
            Self::Beq => 0b0000,
            Self::Bne => 0b0001,
            Self::Bcs => 0b0010,
            Self::Bcc => 0b0011,
            Self::Bmi => 0b0100,
            Self::Bpl => 0b0101,
            Self::Bvs => 0b0110,
            Self::Bvc => 0b0111,
            Self::Bhi => 0b1000,
            Self::Bls => 0b1001,
            Self::Bge => 0b1010,
            Self::Blt => 0b1011,
            Self::Bgt => 0b1100,
            Self::Ble => 0b1101,
        }
    }
}

impl fmt::Display for CondBranchOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Beq => "beq",
                Self::Bne => "bne",
                Self::Bcs => "bcs",
                Self::Bcc => "bcc",
                Self::Bmi => "bmi",
                Self::Bpl => "bpl",
                Self::Bvs => "bvs",
                Self::Bvc => "bvc",
                Self::Bhi => "bhi",
                Self::Bls => "bls",
                Self::Bge => "bge",
                Self::Blt => "blt",
                Self::Bgt => "bgt",
                Self::Ble => "ble",
            }
        )
    }
}

/// Format 16: conditional branch
///
/// The instructions in this group all perform a conditional branch depending on the state
/// of the CPSR condition codes. The branch offset must take account of the prefetch operation,
/// which causes the PC to be 1 word (4 bytes) ahead of the current instruction.
///
/// #### Note 1
/// While the offset specifies a full 9-bit two's complement address. This must always be halfword
/// aligned (i.e. with bit 0 set to 0) so we place `offset >> 1` in the final machinst bits.
///
/// #### Note 2
/// Cond = 1110 is undefined, and should not be used. Cond = 1111 creates the SWI instruction.
#[derive(Debug, Clone)]
pub struct CondBranch {
    /// The condition code
    pub cond: CondBranchOp,
    /// Branch offset
    pub soffset8: BranchDest<SHalfword8>,
}

impl MachineCode for CondBranch {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        self.soffset8.offset.map(|offset| {
            vec![repr16!(
                (4: 0b1101)
                (4: self.cond.bits())
                (8: offset.bits())
            )]
        })
    }
}

impl fmt::Display for CondBranch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.cond, self.soffset8)
    }
}

/// Format 17: software interrupt
///
/// The SWI instruction performs a software interrupt. On taking the SWI, the processor switches
/// into ARM state and enters Supervisor (SVC) mode.
///
/// #### Note
/// `value8` is used solely by the SWI handler: it is ignored by the processor.
#[derive(Debug, Clone)]
pub struct Swi {
    /// Comment field
    pub value8: Imm8,
}

impl MachineCode for Swi {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        Some(vec![repr16!(
            (8: 0b11011111)
            (8: self.value8.bits())
        )])
    }
}

impl fmt::Display for Swi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "swi {}", self.value8)
    }
}

/// Format 18: unconditional branch
///
/// This instruction performs a PC-relative Branch. The branch offset must take account of the
/// prefetch operation, which causes the PC to be 1 word (4 bytes) ahead of the current
/// instruction.
///
/// #### Note
/// The address specified by `soffset11` is a full 12-bit two's complement address, but must
/// always be halfword aligned (i.e. bit 0 set to 0), so we place `soffset11 >> 1` into the
/// machinst bits.
#[derive(Debug, Clone)]
pub struct UncondBranch {
    /// Immediate value
    pub soffset11: BranchDest<SHalfword11>,
}

impl MachineCode for UncondBranch {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        self.soffset11.offset.map(|offset| {
            vec![repr16!(
                (5: 0b11100)
                (11: offset.bits())
            )]
        })
    }
}

impl fmt::Display for UncondBranch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b {}", self.soffset11)
    }
}

#[derive(Debug, Clone)]
pub struct CallDest {
    func: MachFunc,
    offset: Option<i32>,
}

impl From<MachFunc> for CallDest {
    fn from(value: MachFunc) -> Self {
        Self {
            func: value,
            offset: None,
        }
    }
}

/// Format 19: long branch with link
///
/// This format specifies a long branch with link.
///
/// We must split the 23-bit two's complement half-word offset specified by the label into two
/// 11-bit halves, ignoring bit 0 (which must be 0 i.e. halfword aligned), and create two THUMB
/// instructions basically.
///
/// #### Instruction 1 (high = false)
/// In the first instruction the offset field contains the upper 11 bits of the target address.
/// This is shifted left by 12 bits and added to the current PC address. The resulting address
/// is placed in LR.
///
/// #### Instruction 2 (high = true)
/// In the second instruction the offset field contains an 11-bit representation lower half of
/// the target address. This is shifted left by 1 bit and added to LR. LR, which now contains
/// the full 23-bit address, is placed in PC, the address of the instruction following the BL
/// is placed in LR and bit 0 of LR is set.
///
/// The branch offset must take account of the prefetch operation, which causes the PC to be
/// 1 word (4 bytes) ahead of the current instruction.
#[derive(Debug, Clone)]
pub struct BranchWithLink {
    pub dest: CallDest,
}

impl MachineCode for BranchWithLink {
    type Output = u16;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>> {
        self.dest.offset.map(|offset| {
            let mut hi = (offset >> 11) & ((0b1 << 10) - 1);
            if offset < 0 {
                hi |= 0b1 << 10;
            }

            let lo = offset & ((0b1 << 11) - 1);

            vec![
                repr16!(
                    (5: 0b11111)
                    (11: hi as u16)
                ),
                repr16!(
                    (5: 0b11110)
                    (11: lo as u16)
                ),
            ]
        })
    }

    fn len(&self) -> usize {
        2
    }
}

impl fmt::Display for BranchWithLink {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bl @@{}", self.dest.func.get_id())
    }
}

impl_from_instr_for_instr_data! {
    MovShReg,
    AddSub,
    MovCmpAddSubImm,
    Alu,
    HiRegOpsBx,
    PcRelativeLoad,
    LdStrRoff,
    LdStSeBHw,
    LdStrImmOff,
    LdStrHw,
    SpRelativeLdStr,
    LdAddr,
    OffsetSP,
    PushPopRegs,
    MultipleLdStr,
    CondBranch,
    Swi,
    UncondBranch,
    BranchWithLink
}

#[derive(Debug, Clone)]
pub enum ThumbMachinstData {
    MovShReg(MovShReg),
    AddSub(AddSub),
    MovCmpAddSubImm(MovCmpAddSubImm),
    Alu(Alu),
    HiRegOpsBx(HiRegOpsBx),
    PcRelativeLoad(PcRelativeLoad),
    LdStrRoff(LdStrRoff),
    LdStSeBHw(LdStSeBHw),
    LdStrImmOff(LdStrImmOff),
    LdStrHw(LdStrHw),
    SpRelativeLdStr(SpRelativeLdStr),
    LdAddr(LdAddr),
    OffsetSP(OffsetSP),
    PushPopRegs(PushPopRegs),
    MultipleLdStr(MultipleLdStr),
    CondBranch(CondBranch),
    Swi(Swi),
    UncondBranch(UncondBranch),
    BranchWithLink(BranchWithLink),
}

impl fmt::Display for ThumbMachinstData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::MovShReg(i) => i.to_string(),
                Self::AddSub(i) => i.to_string(),
                Self::MovCmpAddSubImm(i) => i.to_string(),
                Self::Alu(i) => i.to_string(),
                Self::HiRegOpsBx(i) => i.to_string(),
                Self::PcRelativeLoad(i) => i.to_string(),
                Self::LdStrRoff(i) => i.to_string(),
                Self::LdStSeBHw(i) => i.to_string(),
                Self::LdStrImmOff(i) => i.to_string(),
                Self::LdStrHw(i) => i.to_string(),
                Self::SpRelativeLdStr(i) => i.to_string(),
                Self::LdAddr(i) => i.to_string(),
                Self::OffsetSP(i) => i.to_string(),
                Self::PushPopRegs(i) => i.to_string(),
                Self::MultipleLdStr(i) => i.to_string(),
                Self::CondBranch(i) => i.to_string(),
                Self::Swi(i) => i.to_string(),
                Self::UncondBranch(i) => i.to_string(),
                Self::BranchWithLink(i) => i.to_string(),
            }
        )
    }
}
