use fyrc_utils::EntityId;

use crate::instr::Machinst;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MachBlock(usize);

impl MachBlock {
    pub const START: Self = Self(0);
}

impl EntityId for MachBlock {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Debug)]
pub struct MachBlockData {
    pub instrs: Vec<Machinst>,
}
