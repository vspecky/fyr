use std::fmt;

use fyrc_utils::EntityId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MachConst(usize);

impl EntityId for MachConst {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

impl fmt::Display for MachConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MachConstSlot(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MachConstData {
    Int8(u8),
    Int16(u16),
    Int32(u32),
}
