pub mod critical_edge_removal;

use std::{
    any::{Any, TypeId},
    rc::Rc,
};

use error_stack::IntoReport;
use rustc_hash::FxHashMap;

use crate::{ssa::function::FunctionData, utils::UxoResult};

pub trait Pass: Sized {
    type Error: error_stack::Context;
    fn name() -> String;
    fn dependencies(manager: PassDepTracker) -> UxoResult<(), Self::Error>;
    fn execute(store: PassStore) -> UxoResult<Self, Self::Error>;
}

#[derive(Debug, thiserror::Error)]
pub enum PassManagerError {
    #[error("the pass '{0}' was not found, maybe it was not run")]
    PassNotFound(String),
    #[error("downcast for retrieval of pass '{0}' failed")]
    RetrievalDowncastFailure(String),
}

pub struct PassManager<'a> {
    function_data: &'a mut FunctionData,
    pass_results: FxHashMap<TypeId, Rc<dyn Any>>,
}

impl<'a> PassManager<'a> {
    pub fn ensure_pass<T: Pass + 'static>(&mut self) -> UxoResult<(), T::Error> {
        let type_id = TypeId::of::<T>();
        if self.pass_results.contains_key(&type_id) {
            return Ok(());
        }

        T::dependencies(PassDepTracker::new(self))?;
        let res = T::execute(PassStore::new(self))?;
        self.pass_results.insert(type_id, Rc::new(res));
        Ok(())
    }

    pub fn get_pass<T: Pass + 'static>(&self) -> UxoResult<Rc<T>, PassManagerError> {
        self.pass_results
            .get(&TypeId::of::<T>())
            .ok_or_else(|| PassManagerError::PassNotFound(T::name()))
            .into_report()?
            .clone()
            .downcast::<T>()
            .map_err(|_| PassManagerError::RetrievalDowncastFailure(T::name()))
            .into_report()
    }
}

pub struct PassDepTracker<'short, 'long> {
    manager: &'short mut PassManager<'long>,
}

impl<'short, 'long> PassDepTracker<'short, 'long> {
    pub fn new(manager: &'short mut PassManager<'long>) -> Self {
        Self { manager }
    }

    pub fn dependency<T: Pass + 'static>(&mut self) -> UxoResult<(), T::Error> {
        self.manager.ensure_pass::<T>()
    }
}

pub struct PassStore<'short, 'long> {
    manager: &'short mut PassManager<'long>,
}

impl<'short, 'long> PassStore<'short, 'long> {
    pub fn new(manager: &'short mut PassManager<'long>) -> Self {
        Self { manager }
    }

    pub fn get_pass<T: Pass + 'static>(&self) -> UxoResult<Rc<T>, PassManagerError> {
        self.manager.get_pass::<T>()
    }

    pub fn get_func(&mut self) -> &mut FunctionData {
        self.manager.function_data
    }
}
