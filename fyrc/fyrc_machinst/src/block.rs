use std::fmt;

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

impl fmt::Display for MachBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "block{}", self.0)
    }
}

#[derive(Debug)]
pub struct MachBlockData {
    pub instrs: Vec<Machinst>,
}

impl MachBlockData {
    pub fn new() -> Self {
        Self { instrs: Vec::new() }
    }
}
