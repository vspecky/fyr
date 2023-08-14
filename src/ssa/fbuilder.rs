use error_stack::{IntoReport, ResultExt};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    ssa::{
        error::SsaError,
        function::{Function, FunctionData},
        instr::{self, InstrData},
        module::SsaModule,
        Block, BlockData, BlockFillKind, BlockSealStatus, Const, ConstKind, Phi, SsaResult, Value,
        ValueData, ValueKind, ValueType, Variable, VariableData,
    },
    utils::BoolExt,
};

pub struct FunctionBuilder<'a> {
    pub(super) module: &'a mut SsaModule,
    pub(super) function: Function,
    pub(super) func_data: FunctionData,
    pub(super) incomplete_phis: FxHashMap<Block, FxHashMap<Variable, Phi>>,
    pub(super) const_map: FxHashMap<ConstKind, Const>,
    pub(super) current_block: Block,
}

impl<'a> FunctionBuilder<'a> {
    pub(super) fn print(&self) -> SsaResult<String> {
        self.func_data.print()
    }

    fn get_current_block(&self) -> SsaResult<&BlockData> {
        self.func_data
            .get_block(self.current_block)
            .attach_printable("when trying to get current block")
    }

    fn get_current_block_mut(&mut self) -> SsaResult<&mut BlockData> {
        let block_id = self.current_block;
        self.func_data
            .get_block_mut(block_id)
            .attach_printable("when trying to get mutable current block")
    }

    pub(crate) fn is_current_block_filled(&self) -> SsaResult<bool> {
        self.get_current_block()
            .map(|block| matches!(block.status, BlockFillKind::Filled))
    }

    fn is_current_block_empty(&self) -> SsaResult<bool> {
        self.get_current_block()
            .map(|block| matches!(block.status, BlockFillKind::Empty))
    }

    fn is_block_filled(&self, block: Block) -> SsaResult<bool> {
        self.func_data
            .get_block(block)
            .attach_printable("when trying to check if block is filled")
            .map(|data| matches!(data.status, BlockFillKind::Filled))
    }

    fn is_block_sealed(&self, block: Block) -> SsaResult<bool> {
        self.func_data
            .get_block(block)
            .attach_printable("when trying to check if block is sealed")
            .map(|data| matches!(data.sealed, BlockSealStatus::Sealed))
    }

    fn is_block_empty(&self, block: Block) -> SsaResult<bool> {
        self.func_data
            .get_block(block)
            .attach_printable("when trying to check if block is sealed")
            .map(|data| matches!(data.status, BlockFillKind::Empty))
    }

    fn get_block_preds(&self, block: Block) -> SsaResult<&[Block]> {
        self.func_data
            .get_block(block)
            .attach_printable("when trying to get block predecessors")
            .map(|data| &data.predecessors as &[Block])
    }

    fn set_current_block_partial(&mut self) -> SsaResult<()> {
        let block = self.get_current_block_mut()?;
        block.status = BlockFillKind::Partial;
        Ok(())
    }

    fn set_current_block_filled(&mut self) -> SsaResult<()> {
        let block = self.get_current_block_mut()?;
        block.status = BlockFillKind::Filled;
        Ok(())
    }

    fn set_block_sealed(&mut self, block: Block) -> SsaResult<()> {
        let block = self
            .func_data
            .get_block_mut(block)
            .attach_printable("when trying to set block as sealed")?;
        block.sealed = BlockSealStatus::Sealed;
        Ok(())
    }

    fn insert_block_phi(&mut self, block: Block, phi: Phi) -> SsaResult<()> {
        let block_data = self
            .func_data
            .get_block_mut(block)
            .attach_printable("when trying to insert phi in block")?;

        block_data.phis.insert(phi.var, phi);
        Ok(())
    }

    fn add_successor(&mut self, to: Block, succ: Block) -> SsaResult<()> {
        let succ_block = self.func_data.get_block_mut(succ)?;
        succ_block
            .is_sealed()
            .and_err(SsaError::NoSealedSuccessors)?;
        succ_block.predecessors.push(to);

        self.func_data.get_block_mut(to)?.successors.push(succ);

        Ok(())
    }

    fn get_variable_type(&self, var: Variable) -> SsaResult<ValueType> {
        self.func_data
            .variables
            .get(var)
            .ok_or(SsaError::VariableNotFound)
            .into_report()
            .map(|data| data.var_type)
    }

    fn get_value_type(&self, val: Value) -> SsaResult<ValueType> {
        self.func_data
            .values
            .get(val)
            .ok_or(SsaError::ValueNotFound)
            .into_report()
            .map(|val_data| val_data.value_type)
    }

    fn write_variable(&mut self, block: Block, var: Variable, val: Value) -> SsaResult<()> {
        self.func_data
            .var_defs
            .get_mut(&var)
            .ok_or(SsaError::VariableNotFound)
            .into_report()?
            .insert(block, val);

        Ok(())
    }

    pub fn make_block(&mut self) -> SsaResult<Block> {
        Ok(self.func_data.blocks.insert(BlockData::new()))
    }

    pub fn switch_to_block(&mut self, block: Block) -> SsaResult<()> {
        let err = "when trying to switch blocks";
        self.is_current_block_filled()
            .attach_printable(err)?
            .or_err(SsaError::BlockNotFilled)
            .into_report()
            .attach_printable("cannot move out of empty/partially-filled block")?;

        self.is_block_empty(block)
            .attach_printable(err)?
            .or_err(SsaError::BlockNotEmpty)
            .into_report()
            .attach_printable("cannot switch to a non-empty block")?;

        self.current_block = block;

        Ok(())
    }

    pub fn declare_variable(&mut self, name: String, var_type: ValueType) -> SsaResult<Variable> {
        let var = self
            .func_data
            .variables
            .insert(VariableData { name, var_type });
        self.func_data.var_defs.insert(var, FxHashMap::default());
        Ok(var)
    }

    pub fn define_variable(&mut self, var: Variable, value: Value) -> SsaResult<()> {
        let var_type = self
            .get_variable_type(var)
            .attach_printable("when trying to define variable")?;

        let val_type = self
            .func_data
            .values
            .get(value)
            .ok_or(SsaError::ValueNotFound)
            .into_report()
            .attach_printable("when trying to define variable")?
            .value_type;

        (var_type != val_type)
            .and_err(SsaError::InvalidValueType)
            .into_report()
            .attach_printable("when trying to define variable")?;

        self.write_variable(self.current_block, var, value)
            .attach_printable("when trying to update variable definition")?;

        Ok(())
    }

    fn read_variable(&mut self, var: Variable, block: Block) -> SsaResult<Value> {
        let err = "when trying to read variable";

        let local_value = self
            .func_data
            .var_defs
            .get(&var)
            .ok_or(SsaError::VariableNotFound)
            .into_report()
            .attach_printable(err)?
            .get(&block);

        if let Some(value) = local_value {
            return Ok(*value);
        }

        if !self.is_block_sealed(block).attach_printable(err)? {
            let var_type = self.get_variable_type(var).attach_printable(err)?;
            let phi_value = self.func_data.values.insert(ValueData {
                value_type: var_type,
                value_kind: ValueKind::Phi { var, block },
            });
            let phi = Phi {
                var,
                value: phi_value,
                args: FxHashMap::default(),
            };

            self.incomplete_phis
                .entry(block)
                .or_default()
                .insert(var, phi);

            self.write_variable(block, var, phi_value)
                .attach_printable(err)?;

            return Ok(phi_value);
        }

        let preds = self.get_block_preds(block).attach_printable(err)?.to_vec();

        Ok(if preds.len() == 1 {
            self.read_variable(var, preds[0])?
        } else {
            let var_type = self.get_variable_type(var).attach_printable(err)?;

            let phi_value = self.func_data.values.insert(ValueData {
                value_type: var_type,
                value_kind: ValueKind::Phi { var, block },
            });

            let mut phi = Phi {
                var,
                value: phi_value,
                args: FxHashMap::default(),
            };

            for pred in preds {
                let value = self.read_variable(var, pred)?;
                phi.args.insert(pred, value);
            }

            self.insert_block_phi(block, phi).attach_printable(err)?;
            self.write_variable(block, var, phi_value)
                .attach_printable(err)?;

            self.try_remove_trivial_phi(block, var)?
        })
    }

    pub fn use_variable(&mut self, var: Variable) -> SsaResult<Value> {
        self.read_variable(var, self.current_block)
    }

    fn try_remove_trivial_phi(&mut self, block: Block, var: Variable) -> SsaResult<Value> {
        let err = "when trying to remove trivial phi";
        let the_phi = self
            .func_data
            .get_block(block)
            .attach_printable(err)?
            .phis
            .get(&var)
            .ok_or(SsaError::PhiNotFound)
            .into_report()
            .attach_printable(err)?;

        let phi_value = the_phi.value;
        let mut value_set: FxHashSet<Value> = FxHashSet::from_iter(the_phi.args.values().copied());
        value_set.remove(&phi_value);

        Ok(if value_set.len() > 1 {
            phi_value
        } else {
            self.func_data
                .get_block_mut(block)
                .attach_printable(err)?
                .phis
                .remove(&var)
                .ok_or(SsaError::PhiNotFound)
                .into_report()
                .attach_printable(err)?;

            self.func_data
                .get_value_mut(phi_value)
                .attach_printable(err)?
                .entomb();

            let replace_to = value_set
                .iter()
                .next()
                .copied()
                .ok_or(SsaError::PhiEmpty)
                .into_report()
                .attach_printable(err)?;

            for instr_data in self.func_data.instrs.values_mut() {
                instr_data.rewrite_value(phi_value, replace_to);
            }

            let mut to_try_remove: Vec<Block> = Vec::new();

            for (block, block_data) in self.func_data.blocks.iter_mut() {
                if let Some(phi) = block_data.phis.get_mut(&var) {
                    if phi.rewrite_arg(phi_value, replace_to) {
                        to_try_remove.push(block);
                    }
                }
            }

            for block in to_try_remove {
                self.try_remove_trivial_phi(block, var)?;
            }

            replace_to
        })
    }

    pub fn seal_block(&mut self, block: Block) -> SsaResult<()> {
        let err = "when trying to seal block";
        self.is_block_sealed(block)
            .attach_printable(err)?
            .and_err(SsaError::SealingSealedBlock)?;

        let incomplete_phis = self.incomplete_phis.remove(&block).unwrap_or_default();

        let preds = self.get_block_preds(block).attach_printable(err)?.to_vec();

        for (var, mut phi) in incomplete_phis {
            for pred in &preds {
                let value = self.read_variable(var, *pred)?;
                phi.args.insert(*pred, value);
            }

            self.insert_block_phi(block, phi).attach_printable(err)?;
            self.try_remove_trivial_phi(block, var)
                .attach_printable(err)?;
        }

        self.set_block_sealed(block).attach_printable(err)?;

        Ok(())
    }

    pub fn ins<'short>(&'short mut self) -> FuncInstrBuilder<'short, 'a> {
        FuncInstrBuilder { builder: self }
    }

    pub fn finalize(self) -> SsaResult<()> {
        let err = "when finalizing function definition";
        for block in self.func_data.blocks.keys() {
            self.is_block_filled(block)?
                .or_err(SsaError::BlockNotFilled)
                .into_report()
                .attach_printable(err)?;

            self.is_block_sealed(block)?
                .or_err(SsaError::BlockNotSealed)
                .into_report()
                .attach_printable(err)?;
        }

        self.module
            .update_definition(self.function, self.func_data)
            .attach_printable(err)?;

        Ok(())
    }
}

pub struct FuncInstrBuilder<'short, 'long: 'short> {
    builder: &'short mut FunctionBuilder<'long>,
}

macro_rules! defbuild_3 {
    ($name:ident, $name_str:literal, $instr:ident) => {
        pub fn $name(&mut self, a: Value, b: Value) -> SsaResult<Value> {
            let err = format!("when trying to add '{}' instruction", $name_str);
            let value_type = self
                .builder
                .func_data
                .get_value(a)
                .attach_printable_lazy(|| err.clone())?
                .value_type;

            self.build(instr::$instr { rs: a, rn: b }, value_type)
                .attach_printable_lazy(|| err.clone())?
                .ok_or(SsaError::MissingResult)
                .into_report()
                .attach_printable_lazy(|| err.clone())
        }
    };
}

impl<'short, 'long> FuncInstrBuilder<'short, 'long> {
    fn build<I>(&mut self, data: I, res_type: ValueType) -> SsaResult<Option<Value>>
    where
        I: Into<InstrData>,
    {
        let data = data.into();
        let err = "when trying to build instruction";
        self.builder
            .is_current_block_filled()
            .attach_printable(err)?
            .and_err(SsaError::BlockFilled)
            .into_report()
            .attach_printable("cannot add instructions to filled block")?;

        let instr = self.builder.func_data.instrs.insert(data.clone());
        self.builder
            .get_current_block_mut()
            .attach_printable(err)?
            .instrs
            .push(instr);

        if self
            .builder
            .is_current_block_empty()
            .attach_printable(err)?
        {
            self.builder
                .set_current_block_partial()
                .attach_printable(err)?;
        }

        let current_block_id = self.builder.current_block;
        for block in data.get_referenced_blocks() {
            self.builder
                .add_successor(current_block_id, block)
                .attach_printable(err)?;
        }

        let result = if data.has_result() {
            let value = self.builder.func_data.values.insert(ValueData {
                value_type: res_type,
                value_kind: ValueKind::InstrRes(instr),
            });
            self.builder.func_data.results.insert(instr, value);
            Some(value)
        } else {
            None
        };

        if data.is_terminal() {
            self.builder
                .set_current_block_filled()
                .attach_printable(err)?;
        }

        Ok(result)
    }

    defbuild_3!(add, "add", Add);
    defbuild_3!(adc, "adc", Adc);
    defbuild_3!(sub, "sub", Sub);
    defbuild_3!(mul, "mul", Mul);
    defbuild_3!(lsl, "lsl", Lsl);
    defbuild_3!(lsr, "lsr", Lsr);
    defbuild_3!(asl, "asl", Asl);

    pub fn load(&mut self, base: Value, offset: Value, value_type: ValueType) -> SsaResult<Value> {
        let err = "when trying to add 'load' instruction";
        self.build(instr::Load { base, offset }, value_type)
            .attach_printable(err)?
            .ok_or(SsaError::MissingResult)
            .into_report()
            .attach_printable(err)
    }

    pub fn store(&mut self, base: Value, offset: Value, val: Value) -> SsaResult<()> {
        let err = "when trying to add 'store' instruction";
        let value_type = self
            .builder
            .func_data
            .get_value(val)
            .attach_printable(err)?
            .value_type;

        self.build(instr::Store { base, offset, val }, value_type)
            .attach_printable(err)?;

        Ok(())
    }

    pub fn ret(&mut self, val: Option<Value>) -> SsaResult<()> {
        let err = "when trying to add 'ret' instruction";
        let value_type = val
            .map(|val| -> SsaResult<ValueType> {
                Ok(self
                    .builder
                    .func_data
                    .get_value(val)
                    .attach_printable(err)?
                    .value_type)
            })
            .transpose()?;

        self.build(instr::Ret { val }, value_type.unwrap_or(ValueType::Int32))
            .attach_printable(err)?;

        Ok(())
    }

    pub fn cmps(&mut self, rs: Value, rn: Value) -> SsaResult<()> {
        self.build(
            instr::Cmp {
                rs,
                rn,
                result_cond: None,
            },
            ValueType::Int8,
        )
        .attach_printable("when trying to add 'cmp_status' instruction")?;

        Ok(())
    }

    pub fn cmpb(&mut self, rs: Value, rn: Value, cond: instr::Cond) -> SsaResult<Value> {
        let err = "when trying to add 'cmp_bool' instruction";
        self.build(
            instr::Cmp {
                rs,
                rn,
                result_cond: Some(cond),
            },
            ValueType::Int8,
        )
        .attach_printable(err)?
        .ok_or(SsaError::MissingResult)
        .into_report()
        .attach_printable(err)
    }

    pub fn brs(
        &mut self,
        cond: instr::Cond,
        then_block: Block,
        else_block: Block,
    ) -> SsaResult<()> {
        self.build(
            instr::Branch {
                kind: instr::BranchKind::Status(cond),
                then_block,
                else_block,
            },
            ValueType::Int8,
        )
        .attach_printable("when trying to add 'br_status' instruction")?;

        Ok(())
    }

    pub fn brz(&mut self, val: Value, then_block: Block, else_block: Block) -> SsaResult<()> {
        self.build(
            instr::Branch {
                kind: instr::BranchKind::Zero(val),
                then_block,
                else_block,
            },
            ValueType::Int8,
        )
        .attach_printable("when trying to add 'brz' instruction")?;

        Ok(())
    }

    pub fn brnz(&mut self, val: Value, then_block: Block, else_block: Block) -> SsaResult<()> {
        self.build(
            instr::Branch {
                kind: instr::BranchKind::NotZero(val),
                then_block,
                else_block,
            },
            ValueType::Int8,
        )
        .attach_printable("when trying to add 'brz' instruction")?;

        Ok(())
    }

    pub fn jmp(&mut self, dest: Block) -> SsaResult<()> {
        self.build(instr::Jump { dest }, ValueType::Int8)
            .attach_printable("when trying to add 'jmp' instruction")?;

        Ok(())
    }

    pub fn call(&mut self, func: Function, args: Vec<Value>) -> SsaResult<Option<Value>> {
        let err = "when trying to add 'call' instruction";
        let sig = self
            .builder
            .module
            .get_signature(func)
            .attach_printable(err)?;

        let arg_value_types: Vec<ValueType> = args
            .iter()
            .map(|val| self.builder.get_value_type(*val))
            .collect::<Result<_, _>>()
            .attach_printable(err)?;

        let sig_value_types: Vec<ValueType> = sig.args.iter().map(|vdata| vdata.var_type).collect();

        (arg_value_types.len() != sig_value_types.len())
            .and_err(SsaError::IncorrectFunctionArgs)
            .into_report()
            .attach_printable(err)?;

        for (arg_type, sig_type) in arg_value_types.into_iter().zip(sig_value_types.into_iter()) {
            (arg_type != sig_type)
                .and_err(SsaError::IncorrectFuncArgType)
                .into_report()
                .attach_printable(err)?;
        }

        let res = self
            .build(
                instr::Call {
                    func,
                    args,
                    ret_type: sig.ret_type,
                },
                sig.ret_type.unwrap_or(ValueType::Int8),
            )
            .attach_printable(err)?;

        Ok(res)
    }

    fn build_const(&mut self, kind: ConstKind) -> SsaResult<Value> {
        let const_ref = self
            .builder
            .const_map
            .get(&kind)
            .copied()
            .unwrap_or_else(|| {
                let const_ref = self.builder.func_data.consts.insert(kind);
                self.builder.const_map.insert(kind, const_ref);
                const_ref
            });

        self.build(
            instr::LoadConst { const_ref, kind },
            match kind {
                ConstKind::Const8(_) => ValueType::Int8,
                ConstKind::Const16(_) => ValueType::Int16,
                ConstKind::Const32(_) => ValueType::Int32,
            },
        )?
        .ok_or(SsaError::MissingResult)
        .into_report()
    }

    pub fn const8(&mut self, value: u8) -> SsaResult<Value> {
        self.build_const(ConstKind::Const8(value))
            .attach_printable("when trying to add 'const.8' instruction")
    }

    pub fn const16(&mut self, value: u16) -> SsaResult<Value> {
        self.build_const(ConstKind::Const16(value))
            .attach_printable("when trying to add 'const.16' instruction")
    }

    pub fn const32(&mut self, value: u32) -> SsaResult<Value> {
        self.build_const(ConstKind::Const32(value))
            .attach_printable("when trying to add 'const.32' instruction")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ssa::function::InstructionSet;

    #[test]
    fn test_basic_construction() {
        let mut module = SsaModule::construct(InstructionSet::Thumb);
        let mut builder = module
            .build_function(module.get_main_function())
            .expect("get builder");
        let mut ins = builder.ins();
        let x = ins.const32(45).expect("x");
        let y = ins.const16(64).expect("y");
        let added = ins.add(x, y).expect("add");
        ins.ret(Some(added)).expect("ret");

        let string = builder.print().expect("print");
        println!("{string}");
    }

    #[test]
    fn test_phi_construction1() {
        let mut module = SsaModule::construct(InstructionSet::Thumb);
        let mut b = module
            .build_function(module.get_main_function())
            .expect("get builder");
        let var = b
            .declare_variable("test".to_string(), ValueType::Int32)
            .expect("var decl");
        let mut ins = b.ins();
        let v0 = ins.const32(34).expect("v0_def");
        b.define_variable(var, v0).expect("v0 var def");

        let while_header = b.make_block().expect("def while_header");
        b.ins().jmp(while_header).expect("jmp while_header");
        b.switch_to_block(while_header)
            .expect("switch while_header");

        let continuation = b.make_block().expect("def continuation");
        let inside_while = b.make_block().expect("def inside_while");
        let v = b.use_variable(var).expect("use var while_header");
        let c67 = b.ins().const32(67).expect("c67");
        b.ins().cmps(v, c67).expect("while_header cmp");
        b.ins()
            .brs(instr::Cond::LessThan, inside_while, continuation)
            .expect("while_header brs");

        b.seal_block(continuation).expect("seal continuation");
        b.seal_block(inside_while).expect("seal inside_while");

        b.switch_to_block(inside_while)
            .expect("switch inside_while");
        let c10 = b.ins().const32(10).expect("def c10");
        let c11 = b.ins().const32(11).expect("def c11");
        b.ins().cmps(c10, c11).expect("cmps c10 c11");
        let inside_then = b.make_block().expect("make inside_then");
        let inside_else = b.make_block().expect("make inside_else");
        b.ins()
            .brs(instr::Cond::LessThan, inside_then, inside_else)
            .expect("brs inside_while");

        b.seal_block(inside_then).expect("seal inside_then");
        b.seal_block(inside_else).expect("seal inside_else");

        b.switch_to_block(inside_then).expect("switch inside_then");
        let v = b.use_variable(var).expect("use var inside_then");
        let c100 = b.ins().const32(100).expect("const32 c100");
        let add_res = b.ins().add(v, c100).expect("add v c100");
        b.define_variable(var, add_res)
            .expect("var def inside_then");
        b.ins()
            .jmp(while_header)
            .expect("inside_then -> while_header");

        b.switch_to_block(inside_else).expect("switch inside_else");
        let c200 = b.ins().const32(200).expect("c200");
        let c201 = b.ins().const32(201).expect("c201");
        b.ins().sub(c201, c200).expect("c201 - c200");
        b.ins()
            .jmp(while_header)
            .expect("inside_else -> while_header");

        b.seal_block(while_header).expect("seal while_header");

        b.switch_to_block(continuation)
            .expect("switch continuation");
        let v = b.use_variable(var).expect("use var continuation");
        let c2 = b.ins().const32(2).expect("c2");
        let res = b.ins().mul(v, c2).expect("mul v c2");
        b.ins().ret(Some(res)).expect("ret");

        println!("{}", b.print().expect("print"));
    }

    #[test]
    fn test_phi_construction2() {
        let mut module = SsaModule::construct(InstructionSet::Thumb);
        let mut b = module
            .build_function(module.get_main_function())
            .expect("get builder");
        let var = b
            .declare_variable("test".to_string(), ValueType::Int32)
            .expect("var decl");
        let mut ins = b.ins();
        let v0 = ins.const32(34).expect("v0_def");
        b.define_variable(var, v0).expect("v0 var def");

        let while_header = b.make_block().expect("def while_header");
        b.ins().jmp(while_header).expect("jmp while_header");
        b.switch_to_block(while_header)
            .expect("switch while_header");

        let continuation = b.make_block().expect("def continuation");
        let inside_while = b.make_block().expect("def inside_while");
        let v = b.use_variable(var).expect("use var while_header");
        let c67 = b.ins().const32(67).expect("c67");
        b.ins().cmps(v, c67).expect("while_header cmp");
        b.ins()
            .brs(instr::Cond::LessThan, inside_while, continuation)
            .expect("while_header brs");

        b.seal_block(continuation).expect("seal continuation");
        b.seal_block(inside_while).expect("seal inside_while");

        b.switch_to_block(inside_while)
            .expect("switch inside_while");
        let c10 = b.ins().const32(10).expect("def c10");
        let c11 = b.ins().const32(11).expect("def c11");
        b.ins().cmps(c10, c11).expect("cmps c10 c11");
        let inside_then = b.make_block().expect("make inside_then");
        let inside_else = b.make_block().expect("make inside_else");
        b.ins()
            .brs(instr::Cond::LessThan, inside_then, inside_else)
            .expect("brs inside_while");

        b.seal_block(inside_then).expect("seal inside_then");
        b.seal_block(inside_else).expect("seal inside_else");

        let while_end = b.make_block().expect("make while_end");

        b.switch_to_block(inside_then).expect("switch inside_then");
        let v = b.use_variable(var).expect("use var inside_then");
        let c100 = b.ins().const32(100).expect("const32 c100");
        let add_res = b.ins().add(v, c100).expect("add v c100");
        b.define_variable(var, add_res)
            .expect("var def inside_then");
        b.ins().jmp(while_end).expect("inside_then -> while_header");

        b.switch_to_block(inside_else).expect("switch inside_else");
        let c200 = b.ins().const32(200).expect("c200");
        let c201 = b.ins().const32(201).expect("c201");
        b.ins().sub(c201, c200).expect("c201 - c200");
        b.ins().jmp(while_end).expect("inside_else -> while_header");

        b.seal_block(while_end).expect("seal while_header");
        b.switch_to_block(while_end).expect("switch while_end");
        b.ins()
            .jmp(while_header)
            .expect("while_end -> while_header");

        b.seal_block(while_header).expect("seal while_header");

        b.switch_to_block(continuation)
            .expect("switch continuation");
        let v = b.use_variable(var).expect("use var continuation");
        let c2 = b.ins().const32(2).expect("c2");
        let res = b.ins().mul(v, c2).expect("mul v c2");
        b.ins().ret(Some(res)).expect("ret");

        println!("{}", b.print().expect("print"));
    }

    #[test]
    fn test_trivial_phi_removal() {
        let mut module = SsaModule::construct(InstructionSet::Thumb);
        let mut b = module
            .build_function(module.get_main_function())
            .expect("get builder");

        let sec = b.make_block().expect("def sec");
        let thi = b.make_block().expect("def thi");

        let var = b
            .declare_variable("test".to_string(), ValueType::Int16)
            .expect("vardef");
        let c10 = b.ins().const16(45).expect("c10");
        b.define_variable(var, c10).expect("define var");
        b.ins().jmp(sec).expect("jmp sec");

        b.switch_to_block(sec).expect("switch sec");
        let v = b.use_variable(var).expect("usevar sec");
        let c12 = b.ins().const16(12).expect("c12");
        b.ins().add(v, c12).expect("add v, c12");
        b.ins().jmp(thi).expect("jump thi");

        assert!(!b.incomplete_phis.is_empty(), "incomplete phis empty");
        println!("block_1 FILLED:\n{}", b.print().expect("print 1filled"));

        b.switch_to_block(thi).expect("switch thi");
        b.seal_block(thi).expect("seal thi");
        let v = b.use_variable(var).expect("use var thi");
        let c15 = b.ins().const16(15).expect("c15");
        let res = b.ins().mul(v, c15).expect("mul v, c15");
        b.ins().ret(Some(res)).expect("ret");

        assert!(!b.incomplete_phis.is_empty(), "incomplete phis empty");
        println!("\nblock_2 FILLED:\n{}", b.print().expect("print 2filled"));

        b.seal_block(sec).expect("seal sec");

        assert!(b.incomplete_phis.is_empty(), "incomplete phis empty");
        println!("\nblock_1 SEALED:\n{}", b.print().expect("print 1sealed"));

        for block_data in b.func_data.blocks.values() {
            assert!(block_data.phis.is_empty(), "expected empty block phi");
        }
    }
}
