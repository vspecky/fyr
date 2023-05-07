pub mod critical_edge_removal;
pub mod dfs_tree;
pub mod interference_graph;
pub mod liveness_analysis;
pub mod loop_nesting_forest;

pub use critical_edge_removal::CriticalEdgeRemoval;
pub use dfs_tree::DfsTree;
pub use liveness_analysis::LivenessAnalysis;
pub use loop_nesting_forest::LoopNestingForest;

use std::{
    any::{Any, TypeId},
    cell::RefCell,
    rc::Rc,
};

use error_stack::IntoReport;
use rustc_hash::FxHashMap;

use crate::{ssa::function::FunctionData, utils::UxoResult};

pub trait Pass: Sized {
    type Error: error_stack::Context;
    fn name() -> String;
    fn dependencies(tracker: PassDepTracker) -> UxoResult<(), Self::Error>;
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
    pub fn new(func: &'a mut FunctionData) -> Self {
        Self {
            function_data: func,
            pass_results: FxHashMap::default(),
        }
    }

    pub fn ensure_pass<T: Pass + 'static>(&mut self) -> UxoResult<(), T::Error> {
        let type_id = TypeId::of::<T>();
        if self.pass_results.contains_key(&type_id) {
            return Ok(());
        }

        T::dependencies(PassDepTracker::new(self))?;
        let res = T::execute(PassStore::new(self))?;
        self.pass_results
            .insert(type_id, Rc::new(RefCell::new(res)));
        Ok(())
    }

    pub fn get_pass<T: Pass + 'static>(&self) -> UxoResult<Rc<RefCell<T>>, PassManagerError> {
        self.pass_results
            .get(&TypeId::of::<T>())
            .ok_or_else(|| PassManagerError::PassNotFound(T::name()))
            .into_report()?
            .clone()
            .downcast::<RefCell<T>>()
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

    pub fn get_pass<T: Pass + 'static>(&self) -> UxoResult<Rc<RefCell<T>>, PassManagerError> {
        self.manager.get_pass::<T>()
    }

    pub fn get_func(&self) -> &FunctionData {
        self.manager.function_data
    }

    pub fn get_func_mut(&mut self) -> &mut FunctionData {
        self.manager.function_data
    }
}

#[cfg(test)]
mod test_utils {
    use super::*;

    pub fn get_pass<T: Pass + 'static>(func: &mut FunctionData) -> Rc<RefCell<T>> {
        let mut manager = PassManager::new(func);
        manager.ensure_pass::<T>().expect("ensure pass");
        manager.get_pass::<T>().expect("pass")
    }

    pub fn get_function_with_single_loop() -> FunctionData {
        crate::build_function! {
            // HEAD
            (#block while_header)
            (#block continuation)
            (#block inside_while)
            (#block inside_then)
            (#block inside_else)
            (#block while_end)
            (#vdecl var "test"::Int32)

            // start block
            (v0 = const32 34)
            (#vdef var v0)
            (jmp while_header)

            // while header
            (#switch while_header)
            (#vuse var v)
            (c67 = const32 67)
            (cmps v, c67)
            (brs LessThan, inside_while, continuation)

            (#seal continuation)
            (#seal inside_while)

            // inside while
            (#switch inside_while)
            (c10 = const32 10)
            (c11 = const32 11)
            (cmps c10, c11)
            (brs LessThan, inside_then, inside_else)

            (#seal inside_then)
            (#seal inside_else)

            // inside then
            (#switch inside_then)
            (#vuse var v)
            (c100 = const32 100)
            (add_res = add v, c100)
            (#vdef var add_res)
            (jmp while_end)

            // inside else
            (#switch inside_else)
            (c200 = const32 200)
            (c201 = const32 201)
            (sub c201, c200)
            (jmp while_end)

            (#seal while_end)

            // while end
            (#switch while_end)
            (jmp while_header)

            (#seal while_header)

            // continuation
            (#switch continuation)
            (#vuse var v)
            (c2 = const32 2)
            (res = mul v, c2)
            (ret Some(res))
        }
    }

    pub fn get_function_with_nested_loops() -> FunctionData {
        crate::build_function! {
            // HEAD & decls
            (#block while_header)
            (#block continuation)
            (#block while_inside)
            (#block n_while_header)
            (#block n_continuation)
            (#block n_inside_if_header)
            (#block n_inside_if_then)
            (#block n_inside_if_else)
            (#block n_inside_if_merge)
            (#vdecl var "test"::Int8)

            // start block
            (c = const8 0)
            (#vdef var c)
            (jmp while_header)

            // while header
            (#switch while_header)
            (#vuse var v)
            (c = const8 10)
            (cmps v, c)
            (brs LessThan, while_inside, continuation)

            (#seal while_inside)
            (#seal continuation)

            // while_inside
            (#switch while_inside)
            (c = const8 20)
            (#vdef var c)
            (jmp n_while_header)

            // nested while header
            (#switch n_while_header)
            (#vuse var v)
            (c = const8 30)
            (cmps c, v)
            (brs GreaterThan, n_inside_if_header, n_continuation)

            (#seal n_inside_if_header)
            (#seal n_continuation)

            // nested if header
            (#switch n_inside_if_header)
            (#vuse var v)
            (c = const8 40)
            (cmps c, v)
            (brs LessThan, n_inside_if_then, n_inside_if_else)

            (#seal n_inside_if_then)
            (#seal n_inside_if_else)

            // nested if then
            (#switch n_inside_if_then)
            (c = const8 50)
            (#vdef var c)
            (jmp n_inside_if_merge)

            // nested if else
            (#switch n_inside_if_else)
            (c = const8 60)
            (#vdef var c)
            (jmp n_inside_if_merge)

            (#seal n_inside_if_merge)

            // nested if merge
            (#switch n_inside_if_merge)
            (jmp n_while_header)

            (#seal n_while_header)

            // nested continuation
            (#switch n_continuation)
            (jmp while_header)

            (#seal while_header)

            // continuation
            (#switch continuation)
            (#vuse var v)
            (c = const8 70)
            (res = mul v, c)
            (ret Some(res))
        }
    }
}
