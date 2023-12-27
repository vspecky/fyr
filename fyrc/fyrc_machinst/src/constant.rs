use fyrc_utils::EntityId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MachConstSlot(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MachConstData {
    Int8(u8),
    Int16(u16),
    Int32(u32),
}
