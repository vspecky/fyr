pub mod critical_edge_removal;
pub mod cssa_translation;
pub mod def_use;
pub mod dfs_tree;
pub mod dominator_tree;
pub mod error;
pub mod global_next_use;
pub mod interference_graph;
pub mod liveness_analysis;
pub mod loop_nesting_forest;
pub mod utils;

pub use critical_edge_removal::CriticalEdgeRemoval;
pub use cssa_translation::CssaTranslation;
pub use def_use::DefUse;
pub use dfs_tree::DfsTree;
pub use dominator_tree::DominatorTree;
pub use global_next_use::GlobalNextUse;
pub use interference_graph::InterferenceGraph;
pub use liveness_analysis::LivenessAnalysis;
pub use loop_nesting_forest::LoopNestingForest;

use std::{
    any::{Any, TypeId},
    cell::{Ref, RefCell, RefMut},
};

use error_stack::{report, ResultExt};
use fxhash::FxHashMap;
use fyrc_ssa::function::FunctionData;

use crate::{error::PassResult, utils::RefCellExt};

pub trait Pass: Sized {
    type Error: error_stack::Context;
    fn name() -> String;
    fn dependencies(tracker: PassDepTracker) -> PassResult<(), Self::Error>;
    fn execute(store: PassStore) -> PassResult<Self, Self::Error>;
}

#[derive(Debug, thiserror::Error)]
pub enum PassManagerError {
    #[error("the pass '{0}' was not found, maybe it was not run")]
    PassNotFound(String),
    #[error("downcast for retrieval of pass '{0}' failed")]
    RetrievalDowncastFailure(String),
    #[error("failed to ensure pass")]
    EnsureFailure,
    #[error("failed to pop pass")]
    PopFailure,
}

pub struct PassManager {
    function_data: RefCell<FunctionData>,
    pass_results: FxHashMap<TypeId, RefCell<Box<dyn Any>>>,
}

impl PassManager {
    pub fn new(func: FunctionData) -> Self {
        Self {
            function_data: RefCell::new(func),
            pass_results: FxHashMap::default(),
        }
    }

    pub fn ensure_pass<T: Pass + 'static>(&mut self) -> PassResult<(), T::Error> {
        let type_id = TypeId::of::<T>();
        if self.pass_results.contains_key(&type_id) {
            return Ok(());
        }

        T::dependencies(PassDepTracker::new(self))?;
        let res = T::execute(PassStore::new(self))?;
        self.pass_results
            .insert(type_id, RefCell::new(Box::new(res)));
        Ok(())
    }

    pub fn get_pass<T: Pass + 'static>(&self) -> PassResult<Ref<T>, PassManagerError> {
        self.pass_results
            .get(&TypeId::of::<T>())
            .ok_or_else(|| report!(PassManagerError::PassNotFound(T::name())))?
            .downcast()
            .ok_or_else(|| report!(PassManagerError::RetrievalDowncastFailure(T::name())))
    }

    pub fn get_pass_mut<T: Pass + 'static>(&self) -> PassResult<RefMut<T>, PassManagerError> {
        self.pass_results
            .get(&TypeId::of::<T>())
            .ok_or_else(|| report!(PassManagerError::PassNotFound(T::name())))?
            .downcast_mut()
            .ok_or_else(|| report!(PassManagerError::RetrievalDowncastFailure(T::name())))
    }

    pub fn pop_pass<T: Pass + 'static>(&mut self) -> PassResult<T, PassManagerError> {
        self.pass_results
            .remove(&TypeId::of::<T>())
            .ok_or_else(|| report!(PassManagerError::PassNotFound(T::name())))?
            .into_inner()
            .downcast::<T>()
            .map_err(|_| report!(PassManagerError::RetrievalDowncastFailure(T::name())))
            .map(|b| *b)
    }

    pub fn ensure_and_get_pass<T: Pass + 'static>(
        &mut self,
    ) -> PassResult<Ref<T>, PassManagerError> {
        self.ensure_pass::<T>()
            .change_context(PassManagerError::EnsureFailure)?;
        self.get_pass::<T>()
    }

    pub fn get_func(&self) -> Ref<FunctionData> {
        self.function_data.borrow()
    }
}

pub struct PassDepTracker<'a> {
    manager: &'a mut PassManager,
}

impl<'a> PassDepTracker<'a> {
    pub fn new(manager: &'a mut PassManager) -> Self {
        Self { manager }
    }

    pub fn dependency<T: Pass + 'static>(&mut self) -> PassResult<(), T::Error> {
        self.manager.ensure_pass::<T>()
    }
}

pub struct PassStore<'a> {
    manager: &'a mut PassManager,
}

impl<'a> PassStore<'a> {
    pub fn new(manager: &'a mut PassManager) -> Self {
        Self { manager }
    }

    pub fn get_pass<T: Pass + 'static>(&self) -> PassResult<Ref<T>, PassManagerError> {
        self.manager.get_pass::<T>()
    }

    pub fn get_pass_mut<T: Pass + 'static>(&self) -> PassResult<RefMut<T>, PassManagerError> {
        self.manager.get_pass_mut::<T>()
    }

    pub fn get_func(&self) -> Ref<FunctionData> {
        self.manager.function_data.borrow()
    }

    pub fn get_func_mut(&self) -> RefMut<FunctionData> {
        self.manager.function_data.borrow_mut()
    }
}

pub(crate) trait Dependency {
    fn ensure(manager: &mut PassManager);
}

pub mod test_utils {
    use super::*;

    pub trait MultiPass {
        fn ensure(manager: &mut PassManager);
        fn pop_pass(manager: &mut PassManager) -> Self;
    }

    impl<T: Pass + 'static> MultiPass for T {
        fn ensure(manager: &mut PassManager) {
            manager.ensure_pass::<T>().expect("ensure pass");
        }

        fn pop_pass(manager: &mut PassManager) -> Self {
            manager.pop_pass::<T>().expect("pop pass")
        }
    }

    macro_rules! derive_multi_pass {
        ($(($($gen:ident),+))+) => {
            $(
                impl<$($gen),+> MultiPass for ($($gen),+,)
                where
                    $($gen: Pass + 'static),+
                {
                    fn ensure(manager: &mut PassManager) {
                        $(
                            manager.ensure_pass::<$gen>().expect("ensure pass");
                        )+
                    }

                    fn pop_pass(manager: &mut PassManager) -> Self {
                        ($(manager.pop_pass::<$gen>().expect("pop pass")),+,)
                    }
                }
            )+
        };
    }

    derive_multi_pass! {
        (A)
        (A, B)
        (A, B, C)
        (A, B, C, D)
        (A, B, C, D, E)
        (A, B, C, D, E, F)
    }

    pub fn get_pass<T: MultiPass + 'static>(func: FunctionData) -> (T, FunctionData) {
        let mut manager = PassManager::new(func);
        T::ensure(&mut manager);
        let pass = T::pop_pass(&mut manager);
        (pass, manager.function_data.into_inner())
    }

    pub fn get_function_with_single_loop() -> FunctionData {
        fyrc_ssa_builder::build_function! {
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
        fyrc_ssa_builder::build_function! {
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
