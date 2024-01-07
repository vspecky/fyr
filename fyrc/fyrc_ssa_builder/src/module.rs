use error_stack::{report, ResultExt};
use fxhash::FxHashMap;
use fyrc_ssa::{
    block::Block,
    function::{Function, FunctionData, InstructionSet, Signature},
    value::{GlobalValue, GlobalValueData},
};
use fyrc_utils::{BoolExt, DenseMap};

use crate::{
    builder::FunctionBuilder,
    error::{BuilderError, BuilderResult},
};

pub(crate) enum FunctionState {
    Undefined,
    Defined(Box<FunctionData>),
}

pub struct SsaModule {
    signatures: DenseMap<Function, Signature>,
    definitions: DenseMap<Function, FunctionState>,
    main: Function,
    global_data: DenseMap<GlobalValue, GlobalValueData>,
}

impl SsaModule {
    pub fn construct(main_isa: InstructionSet) -> Self {
        let mut signatures = DenseMap::new();
        let mut definitions = DenseMap::new();
        let sig = Signature {
            name: "_main".to_string(),
            isa: main_isa,
            args: Vec::new(),
            ret_type: None,
        };
        let main = signatures.insert(sig);
        definitions.insert(FunctionState::Undefined);

        Self {
            signatures,
            definitions,
            main,
            global_data: DenseMap::new(),
        }
    }

    pub(crate) fn update_definition(
        &mut self,
        function: Function,
        data: FunctionData,
    ) -> BuilderResult<()> {
        *self
            .definitions
            .get_mut(function)
            .ok_or_else(|| report!(BuilderError::FunctionNotFound))
            .attach_printable("when trying to update function definition")? =
            FunctionState::Defined(Box::new(data));

        Ok(())
    }

    pub(crate) fn get_signature(&self, func: Function) -> BuilderResult<Signature> {
        self.signatures
            .get(func)
            .cloned()
            .ok_or_else(|| report!(BuilderError::FunctionNotFound))
    }

    #[inline]
    pub fn define_global(&mut self, data: GlobalValueData) -> GlobalValue {
        self.global_data.insert(data)
    }

    #[inline]
    pub fn get_global(&self, global: GlobalValue) -> Option<&GlobalValueData> {
        self.global_data.get(global)
    }

    pub fn declare_function(&mut self, sig: Signature) -> Function {
        let function = self.signatures.insert(sig);
        self.definitions.insert(FunctionState::Undefined);
        function
    }

    pub fn build_function(&mut self, func: Function) -> BuilderResult<FunctionBuilder<'_>> {
        let err = "when trying to get function builder";
        let def_status = self
            .definitions
            .get(func)
            .ok_or_else(|| report!(BuilderError::FunctionNotFound))
            .attach_printable(err)?;

        matches!(def_status, FunctionState::Undefined)
            .or_else_err(|| report!(BuilderError::FunctionAlreadyBuilt))
            .attach_printable(err)?;

        let sig = self
            .signatures
            .get(func)
            .ok_or_else(|| report!(BuilderError::FunctionNotFound))
            .attach_printable(err)?
            .clone();

        Ok(FunctionBuilder {
            module: self,
            function: func,
            func_data: FunctionData::new(sig),
            incomplete_phis: FxHashMap::default(),
            phis: DenseMap::new(),
            const_map: FxHashMap::default(),
            current_block: Block::START,
        })
    }

    pub fn get_main_function(&self) -> Function {
        self.main
    }

    pub fn get_main_function_data(&self) -> BuilderResult<&FunctionData> {
        let def = self
            .definitions
            .get(self.main)
            .ok_or_else(|| report!(BuilderError::FunctionNotFound))?;

        if let FunctionState::Defined(data) = def {
            Ok(data)
        } else {
            Err(report!(BuilderError::FunctionNotDefined))
        }
    }
}
