use crate::{Block, Variable, Value};

use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub struct Phi {
    pub var: Variable,
    pub value: Value,
    pub args: FxHashMap<Block, Value>,
}

impl Phi {
    pub fn rewrite_arg(&mut self, from: Value, to: Value) -> bool {
        let mut rewritten: bool = false;
        for (_, arg) in self.args.iter_mut() {
            if *arg == from {
                *arg = to;
                rewritten = true;
            }
        }
        rewritten
    }
}
