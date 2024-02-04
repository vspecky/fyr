use std::fmt;

use fyrc_utils::EntityId;

use crate::block::MachBlock;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MachHop(usize);

impl EntityId for MachHop {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

impl fmt::Display for MachHop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "hop{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum MachHopKind {
    ToBlock(MachBlock),
    Hop { hop: MachHop, to: MachBlock },
}
