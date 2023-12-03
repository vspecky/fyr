use std::fmt;

use fyrc_utils::EntityId;

use crate::{block::Block, instr::Instr, phi::Phi, variable::Variable};

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

#[derive(Clone)]
pub enum ValueKind {
    InstrRes(Instr),
    SpillRes(Instr),
    FuncArg,
    Phi(Phi),
    MemPhi(Phi),
    Tombstone,
}

#[derive(Clone)]
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
