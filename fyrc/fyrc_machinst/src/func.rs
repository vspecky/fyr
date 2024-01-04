use fxhash::FxHashMap;
use fyrc_utils::{DenseMap, EntityId};

use crate::{
    block::{MachBlock, MachBlockData},
    constant::{MachConst, MachConstData},
    hop::{MachHop, MachHopKind},
    instr::{Machinst, ThumbMachinstData},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MachFunc(usize);

impl EntityId for MachFunc {
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
pub enum MachLayoutSlot {
    Block(MachBlock),
    Const(MachConst),
    Hop(MachHop),
}

#[derive(Debug)]
pub struct MachFuncData {
    pub instrs: DenseMap<Machinst, ThumbMachinstData>,
    pub consts: DenseMap<MachConst, MachConstData>,
    pub consts_rev_lookup: FxHashMap<MachConstData, MachConst>,
    pub blocks: DenseMap<MachBlock, MachBlockData>,
    pub hops: DenseMap<MachHop, MachHopKind>,
    pub layout: Vec<MachLayoutSlot>,
}
