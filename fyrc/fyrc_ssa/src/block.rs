use std::fmt;

use fyrc_utils::EntityId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Block(u32);

impl EntityId for Block {
    #[inline]
    fn get_id(&self) -> usize {
        self.0 as usize
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx as u32)
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::with_id(0)
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "block_{}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BlockFillKind {
    Empty,
    Partial,
    Filled,
}

#[derive(Debug, Clone, Copy)]
pub enum BlockSealStatus {
    Unsealed,
    Sealed,
}

#[derive(Clone)]
pub struct BlockData {
    pub predecessors: Vec<Block>,
    pub successors: Vec<Block>,
    pub phis: FxHashMap<Variable, Phi>,
    pub instrs: Vec<instr::Instr>,
    pub status: BlockFillKind,
    pub sealed: BlockSealStatus,
}
