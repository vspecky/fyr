use std::fmt;

use fyrc_utils::EntityId;

use crate::{instr::Instr, phi::Phi};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(u32);

impl Value {
    pub fn rewrite(&mut self, from: Value, to: Value) {
        if *self == from {
            *self = to;
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl EntityId for Value {
    #[inline]
    fn get_id(&self) -> usize {
        self.0 as usize
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx as u32)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Int8,
    Int16,
    Int32,
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Int8 => "i8",
                Self::Int16 => "i16",
                Self::Int32 => "i32",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    InstrRes(Instr),
    SpillRes(Instr),
    FuncArg,
    Phi(Phi),
    MemPhi(Phi),
    Tombstone,
}

#[derive(Debug, Clone)]
pub struct ValueData {
    pub value_type: ValueType,
    pub value_kind: ValueKind,
    pub is_mem: bool,
}

impl ValueData {
    #[inline]
    pub fn entomb(&mut self) {
        self.value_kind = ValueKind::Tombstone;
    }

    #[inline]
    pub fn is_spill(&self) -> bool {
        matches!(self.value_kind, ValueKind::SpillRes(_))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GlobalValue(usize);

impl EntityId for GlobalValue {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Debug, Clone)]
pub enum GlobalConstKind {
    Int8(u8),
    Int16(u16),
    Int32(u32),
}

impl GlobalConstKind {
    pub fn get_type(&self) -> ValueType {
        match self {
            Self::Int8(_) => ValueType::Int8,
            Self::Int16(_) => ValueType::Int16,
            Self::Int32(_) => ValueType::Int32,
        }
    }
}

#[derive(Debug, Clone)]
pub enum GlobalValueData {
    Const(GlobalConstKind),
    Static {
        data: Vec<u8>,
        alignment: fyrc_utils::Alignment,
        mutable: bool,
    },
}

impl GlobalValueData {
    #[inline]
    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const(_))
    }

    #[inline]
    pub fn is_static(&self) -> bool {
        matches!(self, Self::Static { .. })
    }

    #[inline]
    pub fn is_mutable(&self) -> bool {
        matches!(self, Self::Static { mutable: true, .. })
    }

    pub fn get_type(&self) -> ValueType {
        match self {
            Self::Const(c) => c.get_type(),
            Self::Static { .. } => ValueType::Int32,
        }
    }
}
