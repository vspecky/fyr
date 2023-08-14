use error_stack::{IntoReport, ResultExt};
use rustc_hash::FxHashSet;

use crate::{
    ssa::{function::FunctionData, passes, Block, Phi, Value},
    utils::{BoolExt, DenseMap, UnionFind, UxoResult},
};

#[derive(Debug, thiserror::Error)]
pub enum CssaError {

}

pub struct CssaTranslation;

pub struct CssaTranslator<'a> {
    func: &'a mut FunctionData,
    live: &'a passes::LivenessAnalysis,
    ig: &'a passes::InterferenceGraph,
    congruence: UnionFind<Value>,
}

impl<'a> CssaTranslator<'a> {
    fn new(
        func: &'a mut FunctionData,
        live: &'a passes::LivenessAnalysis,
        ig: &'a passes::InterferenceGraph,
    ) -> Self {
        Self {
            func,
            live,
            ig,
            congruence: UnionFind::new(),
        }
    }

    fn translate(&mut self) -> UxoResult<(), CssaError> {
        Ok(())
    }
}
