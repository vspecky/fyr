use error_stack::{report, ResultExt};
use fyrc_utils::{DenseMap, EntityId};

use crate::{
    block::{MachBlock, MachBlockData},
    constant::{MachConst, MachConstData},
    error::{MachinstError, MachinstResult},
    hop::{MachHop, MachHopKind},
    instr::{thumb::BranchDestKind, Machinst, ThumbMachinstData},
    types::MachineCode,
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

#[derive(Debug)]
pub struct MachFuncData {
    pub instrs: DenseMap<Machinst, ThumbMachinstData>,
    pub consts: DenseMap<MachConst, MachConstData>,
    pub blocks: DenseMap<MachBlock, MachBlockData>,
    pub hops: DenseMap<MachHop, MachHopKind>,
}

impl MachFuncData {
    pub fn new() -> Self {
        Self {
            instrs: DenseMap::new(),
            consts: DenseMap::new(),
            blocks: DenseMap::new(),
            hops: DenseMap::new(),
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

    pub fn get_block_len(&self, block: MachBlock) -> MachinstResult<usize> {
        let block_data = self
            .get_block(block)
            .attach_printable("when getting block length")?;

        let mut length = 0usize;
        for &machinst in &block_data.instrs {
            let machinst_data = self
                .get_machinst(machinst)
                .attach_printable("when getting block length")?;

            length += machinst_data.len();
        }

        Ok(length)
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

    pub fn redirect_predecessor_branches(
        &mut self,
        block: MachBlock,
        redirect_to: MachBlock,
    ) -> MachinstResult<()> {
        let preds = self.get_block(block)?.preds.clone();
        for pred in preds {
            self.update_branch_destination(pred, block, redirect_to)?;
        }

        Ok(())
    }

    pub fn update_branch_destination(
        &mut self,
        forb: MachBlock,
        from: MachBlock,
        to: MachBlock,
    ) -> MachinstResult<()> {
        let block_data = self.get_block(forb)?;
        let branch_instrs = block_data
            .instrs
            .iter()
            .rev()
            .copied()
            .take(2)
            .collect::<Vec<_>>();

        if branch_instrs.is_empty() {
            return Err(report!(MachinstError::InstrNotFound))
                .attach_printable("expected branch instructions at the end of function");
        }

        let uncond_machinst = self.get_machinst_mut(branch_instrs[0])?;
        let mut updated = false;

        if let ThumbMachinstData::UncondBranch(branch) = uncond_machinst {
            if branch.soffset11.kind == BranchDestKind::Block(from) {
                branch.soffset11.kind = BranchDestKind::Block(to);
                updated = true;
            }

            let maybe_cond = branch_instrs
                .get(1)
                .map(|&machinst| self.get_machinst_mut(machinst))
                .transpose()?;

            if let Some(ThumbMachinstData::CondBranch(cbranch)) = maybe_cond {
                if cbranch.soffset8.kind == BranchDestKind::Block(from) {
                    cbranch.soffset8.kind = BranchDestKind::Block(to);
                    updated = true;
                }
            }
        }

        if updated {
            let block_data = self.get_block_mut(forb)?;
            block_data.succs.retain(|b| *b != from);
            block_data.succs.push(to);

            let from_block_data = self.get_block_mut(from)?;
            from_block_data.preds.retain(|b| *b != forb);

            let to_block_data = self.get_block_mut(to)?;
            to_block_data.preds.push(forb);
        }

        Ok(())
    }
}
