use error_stack::ResultExt;
use rustc_hash::FxHashMap;

use crate::{
    ssa::{function::FunctionData, instr::Instr, passes, Block, Value},
    utils::{EntityId, DenseMap, UxoResult},
};

#[derive(Debug, thiserror::Error)]
pub enum DefUseError {

}

pub enum DefLocation {
    Phi,
    Instr(Instr),
}

pub struct ValueDef {
    pub block: Block,
    pub loc: DefLocation,
}

#[derive(Default)]
pub struct ValueDefUse {
    pub def: Option<ValueDef>,
    pub uses: FxHashMap<Block, Instr>,
}

pub struct DefUse {
    pub def_use: DenseMap<Value, ValueDefUse>,
}

pub struct DefUseBuilder<'a> {
    func: &'a FunctionData,
    def_use: DenseMap<Value, ValueDefUse>,
}

impl<'a> DefUseBuilder<'a> {
    fn new(func: &'a FunctionData) -> Self {
        Self {
            func,
            def_use: DenseMap::with_prefilled(func.values.len()),
        }
    }

    fn build(&mut self) -> UxoResult<DefUse, DefUseError> {
        todo!()
    }
}
