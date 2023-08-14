use fyrc_utils::EntityId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Const(u32);

impl EntityId for Const {
    #[inline]
    fn get_id(&self) -> usize {
        self.0 as usize
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx as u32)
    }
}

impl Default for Const {
    fn default() -> Self {
        Self(0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstKind {
    Const8(u8),
    Const16(u16),
    Const32(u32),
}
