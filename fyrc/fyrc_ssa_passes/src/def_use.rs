use error_stack::{report, ResultExt};
use fyrc_ssa::{block::Block, instr::Instr, phi::Phi, value::Value};
use fyrc_utils::DenseMap;

use crate::error::PassResult;

#[derive(Debug, thiserror::Error)]
pub enum DefUseError {
    #[error("def use not found for value")]
    DefUseNotFound,
    #[error("failed a dependent pass for 'dominator_tree'")]
    DependentPassFailure,
    #[error("dominator children not found for node")]
    DomChildrenNotFound,
    #[error("there was an error while interacting with the function data")]
    FunctionError,
    #[error("found a value that was used before being defined")]
    ValueUsedBeforeDef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefUseType {
    Instr(Instr),
    Phi(Phi, Block),
}

#[derive(Debug, Clone)]
pub struct DefUseData {
    pub def_type: DefUseType,
    pub def_block: Block,
    pub uses: Vec<(DefUseType, Block)>,
}

pub struct DefUse {
    def_uses: DenseMap<Value, Option<DefUseData>>,
}

impl DefUse {
    pub fn get_def_use(&self, value: Value) -> Option<&DefUseData> {
        self.def_uses.get(value).map(|o| o.as_ref()).flatten()
    }

    pub fn get_def_use_mut(&mut self, value: Value) -> Option<&mut DefUseData> {
        self.def_uses.get_mut(value).map(|o| o.as_mut()).flatten()
    }
}

impl crate::Pass for DefUse {
    type Error = DefUseError;

    fn name() -> String {
        "def_use".to_string()
    }

    fn dependencies(mut tracker: crate::PassDepTracker) -> PassResult<(), Self::Error> {
        tracker
            .dependency::<crate::CssaTranslation>()
            .change_context(DefUseError::DependentPassFailure)?;

        tracker
            .dependency::<crate::DominatorTree>()
            .change_context(DefUseError::DependentPassFailure)?;

        Ok(())
    }

    fn execute(store: crate::PassStore) -> PassResult<Self, Self::Error> {
        let func = store.get_func();
        let dom = store
            .get_pass::<crate::DominatorTree>()
            .change_context(DefUseError::DependentPassFailure)?;

        let mut def_uses = DenseMap::with_prefilled(func.values.len());
        let mut stack = vec![Block::START];

        while let Some(block) = stack.pop() {
            let block_data = func
                .get_block(block)
                .change_context(DefUseError::FunctionError)?;

            for &phi in &block_data.phis {
                let phi_data = func
                    .get_phi_data(phi)
                    .change_context(DefUseError::FunctionError)?;

                def_uses.set(
                    phi_data.value,
                    Some(DefUseData {
                        def_type: DefUseType::Phi(phi, block),
                        def_block: block,
                        uses: Vec::new(),
                    }),
                );
            }

            for instr in block_data.iter_instr() {
                let uses = func
                    .get_instr_uses(instr)
                    .change_context(DefUseError::FunctionError)?;

                for vuse in uses {
                    let duse = def_uses
                        .get_mut(vuse)
                        .map(|o| o.as_mut())
                        .flatten()
                        .ok_or_else(|| report!(DefUseError::DefUseNotFound))?;

                    duse.uses.push((DefUseType::Instr(instr), block));
                }

                if let Some(def) = func.get_instr_def(instr) {
                    def_uses.set(
                        def,
                        Some(DefUseData {
                            def_type: DefUseType::Instr(instr),
                            def_block: block,
                            uses: Vec::new(),
                        }),
                    );
                }
            }

            let dom_succs = dom
                .tree
                .get(block)
                .ok_or_else(|| report!(DefUseError::DomChildrenNotFound))?;

            for &succ in dom_succs {
                stack.push(succ);
            }
        }

        for (block, block_data) in func.blocks.iter() {
            for &phi in &block_data.phis {
                let phi_data = func
                    .get_phi_data(phi)
                    .change_context(DefUseError::FunctionError)?;

                for (&pblock, &arg) in phi_data.args.iter() {
                    let duse = def_uses
                        .get_mut(arg)
                        .map(|o| o.as_mut())
                        .flatten()
                        .ok_or_else(|| report!(DefUseError::DefUseNotFound))?;

                    duse.uses.push((DefUseType::Phi(phi, pblock), block))
                }
            }
        }

        Ok(Self { def_uses })
    }
}
