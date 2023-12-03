use std::fmt;

use fxhash::{FxHashMap, FxHashSet};
use fyrc_utils::EntityId;

use crate::{instr::Instr, phi::Phi, variable::Variable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Block(u32);

impl Block {
    pub const START: Self = Self(0);

    pub fn is_start_block(&self) -> bool {
        self.0 == 0
    }
}

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
        write!(f, "B{}", self.0)
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

#[derive(Clone, Debug)]
pub struct BlockData {
    pub predecessors: Vec<Block>,
    pub successors: Vec<Block>,
    pub var_phi_map: FxHashMap<Variable, Phi>,
    pub phis: FxHashSet<Phi>,
    instrs: Vec<Instr>,
    pub status: BlockFillKind,
    pub sealed: BlockSealStatus,
    pub exit: Instr,
}

impl BlockData {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            predecessors: Vec::new(),
            successors: Vec::new(),
            var_phi_map: FxHashMap::default(),
            phis: FxHashSet::default(),
            instrs: Vec::new(),
            status: BlockFillKind::Empty,
            sealed: BlockSealStatus::Unsealed,
            exit: Instr::NOP,
        }
    }

    #[inline]
    pub fn is_sealed(&self) -> bool {
        matches!(self.sealed, BlockSealStatus::Sealed)
    }

    pub fn len(&self) -> usize {
        self.instrs.len().saturating_add(1)
    }

    pub fn iter_instr(&self) -> impl Iterator<Item = Instr> + '_ {
        self.instrs
            .iter()
            .copied()
            .chain(std::iter::once(self.exit))
    }

    pub fn iter_instr_rev(&self) -> impl Iterator<Item = Instr> + '_ {
        std::iter::once(self.exit).chain(self.instrs.iter().rev().copied())
    }

    pub fn append_instr(&mut self, ins: Instr) {
        self.instrs.push(ins);
    }

    pub fn prepend_instr(&mut self, ins: Instr) {
        self.instrs.insert(0, ins);
    }

    pub fn set_instrs(&mut self, mut instrs: Vec<Instr>) {
        if let Some(exit) = instrs.pop() {
            self.exit = exit;
        }
        self.instrs = instrs;
    }
}
