use error_stack::ResultExt;
use rustc_hash::FxHashSet;

use fyrc_ssa::{function::FunctionData, value::Value};
use fyrc_utils::EntityId;

use crate::error::PassResult;

#[derive(Debug, thiserror::Error)]
pub enum InterferenceGraphError {
    #[error("a dependency pass failed for 'interference_graph'")]
    DependentPassFailed,
    #[error("checked bit shift failed")]
    BitshiftFailure,
    #[error("bit indexing failed")]
    IndexingFailure,
    #[error("there was an error interacting with the function data")]
    FunctionError,
    #[error("there was an error interacting with the liveness analysis results")]
    LivenessError,
}

// address = low + (high * (high - 1) / 2)
// address_byte = address / 8
// address_offset = address % 8;
pub struct InterferenceGraph {
    val_size: usize,
    mat: Vec<u8>,
}

impl InterferenceGraph {
    fn new(size: usize) -> Self {
        let length = size * (size - 1) / 2;
        let extra_byte = if length % 8 > 0 { 1 } else { 0 };
        Self {
            val_size: size,
            mat: vec![0; length / 8 + extra_byte],
        }
    }

    fn construct_from_function(
        func: &FunctionData,
        liveness: &crate::LivenessAnalysis,
    ) -> PassResult<Self, InterferenceGraphError> {
        let mut graph = Self::new(func.values.len());

        for block in func.blocks.keys() {
            let mut live = liveness
                .get_live_out(block)
                .change_context(InterferenceGraphError::LivenessError)?
                .clone();

            for v1 in live.iter().copied() {
                for v2 in live.iter().copied() {
                    graph.mark(v1, v2);
                }
            }

            let block_data = func
                .get_block(block)
                .change_context(InterferenceGraphError::FunctionError)?;

            for instr in block_data.instrs.iter().rev().copied() {
                if let Some(ref def) = func.get_instr_def(instr) {
                    live.remove(def);
                }

                let uses = func
                    .get_instr_uses(instr)
                    .change_context(InterferenceGraphError::FunctionError)?;

                for use_val in uses {
                    live.insert(use_val);
                }

                for v1 in live.iter().copied() {
                    for v2 in live.iter().copied() {
                        graph.mark(v1, v2);
                    }
                }
            }
        }

        Ok(graph)
    }

    #[inline]
    fn address(v1: Value, v2: Value) -> (usize, usize) {
        let mut low = v1.get_id();
        let mut high = v2.get_id();
        (low, high) = if low < high { (low, high) } else { (high, low) };

        let address = low + high * (high - 1) / 2;
        (address / 8, address % 8)
    }

    #[cfg(test)]
    fn print(&self) {
        print!("[");
        let mut iter = self.mat.iter().copied();
        if let Some(first) = iter.next() {
            print!("{first:08b}");
        }
        for num in iter {
            print!(", {num:08b}");
        }
        println!("]");
    }

    pub fn size(&self) -> usize {
        self.val_size
    }

    pub fn interferes(&self, v1: Value, v2: Value) -> bool {
        if v1 == v2 {
            return true;
        }

        let (address, offset) = Self::address(v1, v2);
        (self.mat[address] & (0b10000000u8 >> offset)) > 0
    }

    pub fn get_interference_set(&self, v: Value) -> FxHashSet<Value> {
        let mut set = (0..self.val_size)
            .map(Value::with_id)
            .filter(|v2| self.interferes(v, *v2))
            .collect::<FxHashSet<Value>>();

        set.remove(&v);
        set
    }

    pub fn mark(&mut self, v1: Value, v2: Value) {
        if v1 == v2 {
            return;
        }

        let (address, offset) = Self::address(v1, v2);
        self.mat[address] |= 0b10000000u8 >> offset;
    }

    pub fn unmark(&mut self, v1: Value, v2: Value) {
        if v1 == v2 {
            return;
        }

        let (address, offset) = Self::address(v1, v2);
        self.mat[address] ^= 0b10000000u8 >> offset;
    }

    pub fn expand(&mut self, additional: usize) {
        let new_val_size = self.val_size + additional;
        let new_bitsize = new_val_size * (new_val_size - 1) / 2;
        let new_length = new_bitsize / 8 + if new_bitsize % 8 > 0 { 1 } else { 0 };
        let additional_length = new_length - self.mat.len();

        self.mat.reserve(new_length - self.mat.len());
        for _ in 0..additional_length {
            self.mat.push(0);
        }
    }
}

impl crate::Pass for InterferenceGraph {
    type Error = InterferenceGraphError;

    fn name() -> String {
        "interference_graph".to_string()
    }

    fn dependencies(mut tracker: crate::PassDepTracker) -> PassResult<(), Self::Error> {
        tracker
            .dependency::<crate::LivenessAnalysis>()
            .change_context(InterferenceGraphError::DependentPassFailed)?;
        Ok(())
    }

    fn execute(store: crate::PassStore) -> PassResult<Self, Self::Error> {
        let liveness = store
            .get_pass::<crate::LivenessAnalysis>()
            .change_context(InterferenceGraphError::DependentPassFailed)?;

        Self::construct_from_function(&store.get_func(), &liveness)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils;

    #[test]
    fn test_byte_array_length() {
        let graph = InterferenceGraph::new(8);
        graph.print();
        assert_eq!(graph.mat.len(), 4, "invalid byte array length");
    }

    #[test]
    fn test_correct_marking() {
        let mut graph = InterferenceGraph::new(8);
        let v1 = Value::with_id(3);
        let v2 = Value::with_id(5);
        graph.mark(v1, v2);
        graph.print();
        assert_eq!(graph.mat[1], 0b00000100);
        let v3 = Value::with_id(7);
        graph.mark(v1, v3);
        graph.print();
        assert_eq!(graph.mat[3], 0b10000000);
        graph.unmark(v1, v2);
        assert_eq!(graph.mat[1], 0);
        graph.unmark(v1, v3);
        assert_eq!(graph.mat[3], 0);
    }

    #[test]
    #[should_panic]
    fn test_out_of_bounds_marking() {
        let mut graph = InterferenceGraph::new(8);
        let v1 = Value::with_id(8);
        let v2 = Value::with_id(4);
        graph.mark(v1, v2);
    }

    #[test]
    fn test_expansion() {
        let mut graph = InterferenceGraph::new(3);
        graph.print();
        graph.expand(5);
        graph.print();
        assert_eq!(graph.mat.len(), 4);
    }

    #[test]
    fn test_interference_graph_calculation() {
        let func = test_utils::get_function_with_single_loop();
        println!("FUNC:\n{}", func.print().expect("print"));
        let (interf, func) = test_utils::get_pass::<InterferenceGraph>(func);

        println!();
        for value in func.values.keys() {
            let set = interf.get_interference_set(value);
            println!("{value} -> {set:?}");
        }

        interf.print();
    }
}
