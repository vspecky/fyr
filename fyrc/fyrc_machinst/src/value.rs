use std::fmt;

use fyrc_utils::EntityId;

#[derive(Debug, Clone, Copy)]
pub enum MachValueType {
    Int8,
    Int16,
    Int32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MachGlobalValue(usize);

impl EntityId for MachGlobalValue {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

impl fmt::Display for MachGlobalValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "global{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum MachGlobalConstKind {
    Int8(u8),
    Int16(u16),
    Int32(u32),
}

impl MachGlobalConstKind {
    pub fn get_type(&self) -> MachValueType {
        match self {
            Self::Int8(_) => MachValueType::Int8,
            Self::Int16(_) => MachValueType::Int16,
            Self::Int32(_) => MachValueType::Int32,
        }
    }
}

#[derive(Debug, Clone)]
pub enum MachGlobalValueData {
    Const(MachGlobalConstKind),
    Static {
        data: Vec<u8>,
        alignment: fyrc_utils::Alignment,
        mutable: bool,
    },
}

impl MachGlobalValueData {
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

    pub fn get_type(&self) -> MachValueType {
        match self {
            Self::Const(c) => c.get_type(),
            Self::Static { .. } => MachValueType::Int32,
        }
    }
}
