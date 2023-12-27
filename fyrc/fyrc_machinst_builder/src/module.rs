use error_stack::report;
use fyrc_machinst::{
    block::MachBlock,
    func::{MachFunc, MachFuncData},
    value::{MachGlobalValue, MachGlobalValueData},
};
use fyrc_utils::DenseMap;

use crate::error::{BuilderError, BuilderResult};

#[derive(Debug)]
pub(crate) enum MachFuncState {
    Undefined,
    Defined(Box<MachFuncData>),
}

#[derive(Debug)]
pub struct MachModule {
    definitions: DenseMap<MachFunc, MachFuncState>,
    main: MachFunc,
    global_data: DenseMap<MachGlobalValue, MachGlobalValueData>,
}

impl MachModule {
    pub fn new() -> Self {
        let mut definitions = DenseMap::new();
        let main = definitions.insert(MachFuncState::Undefined);

        Self {
            definitions,
            main,
            global_data: DenseMap::new(),
        }
    }

    pub(crate) fn update_definition(
        &mut self,
        function: MachFunc,
        data: MachFuncData,
    ) -> BuilderResult<()> {
        *self
            .definitions
            .get_mut(function)
            .ok_or_else(|| report!(BuilderError::FunctionNotFound))? =
            MachFuncState::Defined(Box::new(data));

        Ok(())
    }

    #[inline]
    pub(crate) fn make_function(&mut self) -> MachFunc {
        self.definitions.insert(MachFuncState::Undefined)
    }
}
