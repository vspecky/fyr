use error_stack::{report, ResultExt};
use fyrc_ssa::{
    block::{BlockData, BlockSealStatus},
    function::FunctionData,
    instr,
};

use crate::{error::PassResult, Pass, PassDepTracker, PassStore};

#[derive(Debug, thiserror::Error)]
pub enum CriticalEdgeRemovalError {
    #[error("successors for block not found")]
    BlockSuccessorsNotFound,
    #[error("successors for block not found")]
    BlockPredecessorsNotFound,
    #[error("there was an error when interacting with the function data")]
    FunctionError,
    #[error("encountered empty block")]
    EmptyBlock,
}

pub struct CriticalEdgeRemoval;

fn remove_critical_edges(func: &mut FunctionData) -> PassResult<(), CriticalEdgeRemovalError> {
    for block in func.blocks.keys() {
        let succs = func
            .get_block_succs(block)
            .change_context(CriticalEdgeRemovalError::BlockSuccessorsNotFound)?;

        if succs.len() <= 1 {
            continue;
        }

        for succ in succs {
            let preds = func
                .get_block_preds(succ)
                .change_context(CriticalEdgeRemovalError::BlockPredecessorsNotFound)?;

            if preds.len() <= 1 {
                continue;
            }

            let mut new_block_data = BlockData::new();
            new_block_data.sealed = BlockSealStatus::Sealed;
            let jump_instr = func
                .instrs
                .insert(instr::InstrData::Jump(instr::Jump { dest: succ }));
            new_block_data.exit = jump_instr;
            new_block_data.predecessors.push(block);
            new_block_data.successors.push(succ);
            let new_block = func.blocks.insert(new_block_data);

            let pred_block_data = func
                .get_block_mut(block)
                .change_context(CriticalEdgeRemovalError::FunctionError)?;

            for succ_block in &mut pred_block_data.successors {
                if *succ_block == succ {
                    *succ_block = new_block;
                }
            }

            let last_instr = pred_block_data.exit;

            let instr_data = func
                .get_instr_mut(last_instr)
                .change_context(CriticalEdgeRemovalError::FunctionError)?;

            match instr_data {
                instr::InstrData::Jump(jmp) => {
                    if jmp.dest == succ {
                        jmp.dest = new_block;
                    }
                }

                instr::InstrData::Branch(br) => {
                    if br.then_block == succ {
                        br.then_block = new_block;
                    }
                    if br.else_block == succ {
                        br.else_block = new_block;
                    }
                }

                _ => {
                    return Err(report!(CriticalEdgeRemovalError::FunctionError)).attach_printable(
                        "tried to remove critical edges on block with no branches",
                    )
                }
            }

            let succ_block_phis = func
                .get_block_phis(succ)
                .change_context(CriticalEdgeRemovalError::FunctionError)?;

            for phi in succ_block_phis {
                let phi_data = func
                    .get_phi_data_mut(phi)
                    .change_context(CriticalEdgeRemovalError::FunctionError)?;
                if let Some(val) = phi_data.args.remove(&block) {
                    phi_data.args.insert(new_block, val);
                }
            }
        }
    }

    Ok(())
}

impl Pass for CriticalEdgeRemoval {
    type Error = CriticalEdgeRemovalError;

    fn name() -> String {
        "critical_edge_removal".to_string()
    }

    fn dependencies(_tracker: PassDepTracker) -> PassResult<(), Self::Error> {
        Ok(())
    }

    fn execute(store: PassStore) -> PassResult<Self, Self::Error> {
        remove_critical_edges(&mut store.get_func_mut())?;
        Ok(Self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fyrc_ssa::{function::InstructionSet, value::ValueType};
    use fyrc_ssa_builder::module::SsaModule;

    #[test]
    fn test_splitting_and_phis() {
        let mut module = SsaModule::construct(InstructionSet::Thumb);
        let mut b = module
            .build_function(module.get_main_function())
            .expect("get builder");

        let var = b
            .declare_variable("test".to_string(), ValueType::Int32)
            .expect("var decl");

        let v0 = b.ins().const32(34).expect("v0 def");
        b.define_variable(var, v0).expect("v0 var def");

        let then_block = b.make_block().expect("then_block make");
        let next_block = b.make_block().expect("next_block make");

        b.ins()
            .br(v0, then_block, next_block)
            .expect("jmp then, next");

        b.seal_block(then_block).expect("seal then_block");

        b.switch_to_block(then_block).expect("switch then_block");
        let v1 = b.ins().const32(78).expect("v1 def");
        b.define_variable(var, v1).expect("v1 var def");
        b.ins().jmp(next_block).expect("jmp next_block");

        b.seal_block(next_block).expect("seal next_block");
        b.switch_to_block(next_block).expect("switch next_block");
        let v_use = b.use_variable(var).expect("var use");
        b.ins().ret(Some(v_use)).expect("ret");

        b.finalize().expect("finalization");
        let mut func_data = module
            .get_main_function_data()
            .expect("main function retrieval")
            .clone();

        println!(
            "BEFORE SPLITTING:\n{}",
            func_data.print().expect("print before")
        );

        remove_critical_edges(&mut func_data).expect("splitting");

        println!(
            "\nAFTER SPLITTING:\n{}",
            func_data.print().expect("print before")
        );
    }
}
