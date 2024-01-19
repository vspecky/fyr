use error_stack::{report, ResultExt};
use fxhash::FxHashMap;
use fyrc_utils::{DenseMap, EntityId};

use crate::{
    block::{MachBlock, MachBlockData},
    constant::{MachConst, MachConstData},
    error::{MachinstError, MachinstResult},
    hop::{MachHop, MachHopKind},
    instr::{Machinst, ThumbMachinstData},
    value::MachGlobalValue,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MachFunc(usize);

impl MachFunc {
    pub const MAIN: Self = Self(0);
}

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
pub enum MachFuncLayoutSlot {
    Block(MachBlock),
    Const(MachConst),
    Global(MachGlobalValue),
    Hop(MachHop),
}

#[derive(Debug)]
pub struct MachFuncData {
    pub instrs: DenseMap<Machinst, ThumbMachinstData>,
    pub consts: DenseMap<MachConst, MachConstData>,
    pub blocks: DenseMap<MachBlock, MachBlockData>,
    pub hops: DenseMap<MachHop, MachHopKind>,
    pub layout: Vec<MachFuncLayoutSlot>,
}

impl MachFuncData {
    pub fn new() -> Self {
        Self {
            instrs: DenseMap::new(),
            consts: DenseMap::new(),
            blocks: DenseMap::new(),
            hops: DenseMap::new(),
            layout: Vec::new(),
        }
    }

    pub fn get_block(&self, block: MachBlock) -> MachinstResult<&MachBlockData> {
        self.blocks
            .get(block)
            .ok_or_else(|| report!(MachinstError::BlockNotFound))
    }

    pub fn get_block_mut(&mut self, block: MachBlock) -> MachinstResult<&mut MachBlockData> {
        self.blocks
            .get_mut(block)
            .ok_or_else(|| report!(MachinstError::BlockNotFound))
    }

    pub fn get_const(&self, c: MachConst) -> MachinstResult<&MachConstData> {
        self.consts
            .get(c)
            .ok_or_else(|| report!(MachinstError::ConstNotFound))
    }

    pub fn get_machinst(&self, machinst: Machinst) -> MachinstResult<&ThumbMachinstData> {
        self.instrs
            .get(machinst)
            .ok_or_else(|| report!(MachinstError::InstrNotFound))
    }

    pub fn get_machinst_mut(
        &mut self,
        machinst: Machinst,
    ) -> MachinstResult<&mut ThumbMachinstData> {
        self.instrs
            .get_mut(machinst)
            .ok_or_else(|| report!(MachinstError::InstrNotFound))
    }
}
