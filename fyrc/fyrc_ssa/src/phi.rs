use crate::{Block, Value, Variable};

use fxhash::FxHashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Phi(usize);

impl fyrc_utils::EntityId for Phi {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Debug, Clone)]
pub struct PhiData {
    pub block: Block,
    pub var: Variable,
    pub value: Value,
    pub args: FxHashMap<Block, Value>,
    pub is_mem: bool,
}

impl PhiData {
    pub fn rewrite_arg(&mut self, from: Value, to: Value) -> bool {
        let mut rewritten: bool = false;
        for arg in self.args.values_mut() {
            if *arg == from {
                *arg = to;
                rewritten = true;
            }
        }
        rewritten
    }
}
