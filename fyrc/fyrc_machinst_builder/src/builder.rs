use error_stack::{report, ResultExt};
use fyrc_machinst::{
    block::{MachBlock, MachBlockData},
    constant::{MachConst, MachConstData},
    func::{MachFunc, MachFuncData},
    instr::ThumbMachinstData,
};
use fyrc_utils::BoolExt;

use crate::{
    error::{BuilderError, BuilderResult},
    module::MachModule,
};

pub struct MachFuncBuilder<'a> {
    pub(crate) module: &'a mut MachModule,
    pub(crate) func: MachFunc,
    pub(crate) func_data: MachFuncData,
    pub(crate) current_block: MachBlock,
}

impl<'a> MachFuncBuilder<'a> {
    #[inline]
    pub fn get_current_block(&self) -> MachBlock {
        self.current_block
    }

    #[inline]
    pub fn get_func(&self) -> MachFunc {
        self.func
    }

    #[inline]
    pub fn get_func_data(&self) -> &MachFuncData {
        &self.func_data
    }

    #[inline]
    pub fn get_func_data_mut(&mut self) -> &mut MachFuncData {
        &mut self.func_data
    }

    pub fn define_const(&mut self, constant: MachConstData) -> MachConst {
        self.func_data.consts.insert(constant)
    }

    pub fn add_block(&mut self) -> MachBlock {
        self.func_data.blocks.insert(MachBlockData::new())
    }

    pub fn add_instr<I>(&mut self, instr: I) -> BuilderResult<()>
    where
        I: Into<ThumbMachinstData>,
    {
        let machinst = self.func_data.instrs.insert(instr.into());
        let block_data = self
            .func_data
            .get_block_mut(self.current_block)
            .change_context(BuilderError::FunctionError)?;
        block_data.instrs.push(machinst);
        Ok(())
    }

    pub fn switch_to_block(&mut self, new_block: MachBlock) -> BuilderResult<()> {
        self.func_data
            .blocks
            .contains(&new_block)
            .or_else_err(|| report!(BuilderError::BlockNotFound))?;
        self.current_block = new_block;
        Ok(())
    }

    pub fn finalize(self) -> BuilderResult<()> {
        self.module.update_definition(self.func, self.func_data)
    }
}
