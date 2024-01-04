use fyrc_utils::EntityId;

use crate::block::MachBlock;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub enum MachHopKind {
    ToBlock(MachBlock),
    Hop { hop: MachHop, to: MachBlock },
}
