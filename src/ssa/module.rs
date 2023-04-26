use error_stack::{IntoReport, ResultExt};
use rustc_hash::FxHashMap;

use crate::{
    ssa::{
        error::SsaError,
        fbuilder::FunctionBuilder,
        function::{Function, FunctionData, InstructionSet, Signature},
        Block, SsaResult,
    },
    utils::{BoolExt, DenseMap},
};

pub(super) enum FunctionState {
    Undefined,
    Defined(FunctionData),
}

pub struct SsaModule {
    signatures: DenseMap<Function, Signature>,
    definitions: DenseMap<Function, FunctionState>,
    finalized: bool,
    main: Function,
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
            finalized: false,
            main,
        }
    }

    pub(super) fn update_definition(
        &mut self,
        function: Function,
        data: FunctionData,
    ) -> SsaResult<()> {
        *self
            .definitions
            .get_mut(function)
            .ok_or(SsaError::FunctionNotFound)
            .into_report()
            .attach_printable("when trying to update function definition")? =
            FunctionState::Defined(data);

        Ok(())
    }

    pub(super) fn get_signature(&self, func: Function) -> SsaResult<Signature> {
        self.signatures
            .get(func)
            .cloned()
            .ok_or(SsaError::FunctionNotFound)
            .into_report()
    }

    pub fn declare_function(&mut self, sig: Signature) -> Function {
        let function = self.signatures.insert(sig);
        self.definitions.insert(FunctionState::Undefined);
        function
    }

    pub fn build_function(&mut self, func: Function) -> SsaResult<FunctionBuilder<'_>> {
        let err = "when trying to get function builder";
        let def_status = self
            .definitions
            .get(func)
            .ok_or(SsaError::FunctionNotFound)
            .into_report()
            .attach_printable(err)?;

        matches!(def_status, FunctionState::Undefined)
            .or_err(SsaError::FunctionAlreadyBuilt)
            .into_report()
            .attach_printable(err)?;

        let sig = self
            .signatures
            .get(func)
            .ok_or(SsaError::FunctionNotFound)
            .into_report()
            .attach_printable(err)?
            .clone();

        Ok(FunctionBuilder {
            module: self,
            function: func,
            func_data: FunctionData::new(sig),
            incomplete_phis: FxHashMap::default(),
            const_map: FxHashMap::default(),
            current_block: Block(0),
        })
    }

    pub fn get_main_function(&self) -> Function {
        self.main
    }
}
