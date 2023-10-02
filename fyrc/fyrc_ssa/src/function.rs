use std::fmt;

use error_stack::report;
use fyrc_utils::{DenseMap, EntityId};
use rustc_hash::FxHashMap;

use crate::{
    block::{Block, BlockData, BlockSealStatus},
    constant::{Const, ConstKind},
    error::{SsaError, SsaResult},
    instr::{Instr, InstrData},
    phi::{Phi, PhiData},
    value::{Value, ValueData, ValueKind, ValueType},
    variable::{Variable, VariableData},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function(usize);

impl EntityId for Function {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum InstructionSet {
    Thumb,
}

impl fmt::Display for InstructionSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Thumb => "thumb",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub name: String,
    pub isa: InstructionSet,
    pub args: Vec<VariableData>,
    pub ret_type: Option<ValueType>,
}

#[derive(Clone)]
pub struct FunctionData {
    pub signature: Signature,
    pub blocks: DenseMap<Block, BlockData>,
    pub consts: DenseMap<Const, ConstKind>,
    pub instrs: DenseMap<Instr, InstrData>,
    pub values: DenseMap<Value, ValueData>,
    pub phis: DenseMap<Phi, PhiData>,
    pub variables: DenseMap<Variable, VariableData>,
    pub results: FxHashMap<Instr, Value>,
    pub var_defs: FxHashMap<Variable, FxHashMap<Block, Value>>,
}

macro_rules! outwrite {
    ($out:ident, $fmt:literal $(,$($arg:expr),*)?) => {
        $out.push_str(&format!($fmt, $($($arg),*)?));
    };

    //($out:ident, $fmt:literal $(,$($arg:expr),*)?) => {
        //$out.write(format!($fmt, $($($arg),*)?).as_bytes())
            //.into_report()
            //.change_context(SsaError::PrintIO)?;
    //};
}

impl FunctionData {
    pub fn new(sig: Signature) -> Self {
        let mut values: DenseMap<Value, ValueData> = DenseMap::new();
        let mut variables: DenseMap<Variable, VariableData> = DenseMap::new();
        let mut var_defs: FxHashMap<Variable, FxHashMap<Block, Value>> = FxHashMap::default();
        let mut blocks: DenseMap<Block, BlockData> = DenseMap::new();

        let mut block_data = BlockData::new();
        block_data.sealed = BlockSealStatus::Sealed;
        let block = blocks.insert(block_data);
        for var_data in &sig.args {
            let var = variables.insert(var_data.clone());
            let val = values.insert(ValueData {
                value_type: var_data.var_type,
                value_kind: ValueKind::FuncArg,
            });

            var_defs.insert(var, FxHashMap::from_iter([(block, val)]));
        }

        Self {
            signature: sig,
            blocks,
            consts: DenseMap::new(),
            instrs: DenseMap::new(),
            values,
            phis: DenseMap::new(),
            variables,
            results: FxHashMap::default(),
            var_defs,
        }
    }

    pub fn get_block(&self, block: Block) -> SsaResult<&BlockData> {
        self.blocks
            .get(block)
            .ok_or_else(|| report!(SsaError::BlockNotFound))
    }

    pub fn get_block_mut(&mut self, block: Block) -> SsaResult<&mut BlockData> {
        self.blocks
            .get_mut(block)
            .ok_or_else(|| report!(SsaError::BlockNotFound))
    }

    pub fn get_block_preds(&self, block: Block) -> SsaResult<Vec<Block>> {
        Ok(self.get_block(block)?.predecessors.clone())
    }

    pub fn get_block_succs(&self, block: Block) -> SsaResult<Vec<Block>> {
        Ok(self.get_block(block)?.successors.clone())
    }

    pub fn get_block_phis(&self, block: Block) -> SsaResult<Vec<(Variable, Phi)>> {
        Ok(self
            .get_block(block)?
            .phis
            .iter()
            .map(|(&v, &p)| (v, p))
            .collect())
    }

    pub fn get_value(&self, value: Value) -> SsaResult<&ValueData> {
        self.values
            .get(value)
            .ok_or_else(|| report!(SsaError::ValueNotFound))
    }

    pub fn get_value_mut(&mut self, value: Value) -> SsaResult<&mut ValueData> {
        self.values
            .get_mut(value)
            .ok_or_else(|| report!(SsaError::ValueNotFound))
    }

    pub fn get_instr(&self, instr: Instr) -> SsaResult<&InstrData> {
        self.instrs
            .get(instr)
            .ok_or_else(|| report!(SsaError::InstrNotFound))
    }

    pub fn get_instr_mut(&mut self, instr: Instr) -> SsaResult<&mut InstrData> {
        self.instrs
            .get_mut(instr)
            .ok_or_else(|| report!(SsaError::InstrNotFound))
    }

    pub fn get_phi_data(&self, phi: Phi) -> SsaResult<&PhiData> {
        self.phis
            .get(phi)
            .ok_or_else(|| report!(SsaError::PhiNotFound))
    }

    pub fn get_phi_data_mut(&mut self, phi: Phi) -> SsaResult<&mut PhiData> {
        self.phis
            .get_mut(phi)
            .ok_or_else(|| report!(SsaError::PhiNotFound))
    }

    pub fn get_phi_by_block_var(&self, block: Block, var: Variable) -> SsaResult<Phi> {
        self.get_block(block)?
            .phis
            .get(&var)
            .copied()
            .ok_or_else(|| report!(SsaError::PhiNotFound))
    }

    pub fn get_var(&self, var: Variable) -> SsaResult<&VariableData> {
        self.variables
            .get(var)
            .ok_or_else(|| report!(SsaError::VariableNotFound))
    }

    pub fn get_var_mut(&mut self, var: Variable) -> SsaResult<&mut VariableData> {
        self.variables
            .get_mut(var)
            .ok_or_else(|| report!(SsaError::VariableNotFound))
    }

    pub fn get_var_def(&self, var: Variable) -> SsaResult<&FxHashMap<Block, Value>> {
        self.var_defs
            .get(&var)
            .ok_or_else(|| report!(SsaError::VariableDefsNotFound))
    }

    pub fn get_var_def_mut(&mut self, var: Variable) -> SsaResult<&mut FxHashMap<Block, Value>> {
        self.var_defs
            .get_mut(&var)
            .ok_or_else(|| report!(SsaError::VariableDefsNotFound))
    }

    pub fn get_succ_phi_uses(&self, block: Block) -> SsaResult<Vec<Value>> {
        Ok(self
            .get_block_succs(block)?
            .into_iter()
            .map(|succ| self.get_block(succ))
            .collect::<SsaResult<Vec<_>>>()?
            .into_iter()
            .map(|block_data| {
                Ok(block_data
                    .phis
                    .values()
                    .copied()
                    .map(|phi| self.get_phi_data(phi))
                    .collect::<SsaResult<Vec<_>>>()?
                    .into_iter()
                    .filter_map(|phi_data| phi_data.args.get(&block).copied()))
            })
            .collect::<SsaResult<Vec<_>>>()?
            .into_iter()
            .flatten()
            .collect())
    }

    pub fn get_instr_def(&self, instr: Instr) -> Option<Value> {
        self.results.get(&instr).copied()
    }

    pub fn get_instr_uses(&self, instr: Instr) -> SsaResult<Vec<Value>> {
        Ok(self.get_instr(instr)?.get_uses())
    }

    pub fn get_phi_defs(&self, block: Block) -> SsaResult<Vec<Value>> {
        let data = self.get_block(block)?;
        data.phis
            .values()
            .copied()
            .map(|phi| self.get_phi_data(phi).map(|p| p.value))
            .collect()
    }

    pub fn print(&self) -> SsaResult<String> {
        let mut out = String::new();
        outwrite!(
            out,
            "{} function {}(",
            self.signature.isa,
            self.signature.name
        );
        let mut first = true;
        for (idx, arg) in self.signature.args.iter().enumerate() {
            if !first {
                outwrite!(out, ", ");
            }
            first = false;

            outwrite!(out, "{}::{}", Value::with_id(idx), arg.var_type);
        }
        outwrite!(out, ") {{\n");

        let mut var_map: FxHashMap<Value, Variable> = FxHashMap::default();

        for (var, block_map) in self.var_defs.iter() {
            for (_, val) in block_map.iter() {
                var_map.insert(*val, *var);
            }
        }

        first = true;
        for (block, block_data) in self.blocks.iter() {
            if !first {
                outwrite!(out, "\n");
            }
            outwrite!(out, "  {block}:\n");
            first = false;

            for phi in block_data.phis.values() {
                let phi_data = self.get_phi_data(*phi)?;
                let value_data = self.get_value(phi_data.value)?;
                let var_data = self.get_var(phi_data.var)?;
                let args: Vec<String> = phi_data
                    .args
                    .iter()
                    .map(|(blk, val)| format!("{blk}:{val}"))
                    .collect();

                outwrite!(
                    out,
                    "    {}::{}[{}] = phi {}\n",
                    phi_data.value,
                    value_data.value_type,
                    var_data.name,
                    args.join(", ")
                );
            }

            for instr in block_data.instrs.iter().copied() {
                let result = self.results.get(&instr);
                outwrite!(out, "    ");

                if let Some(resval) = result {
                    let value_data = self.get_value(*resval)?;
                    outwrite!(out, "{}::{}", *resval, value_data.value_type);

                    if let Some(var_assign) = var_map.get(resval) {
                        let var_data = self.get_var(*var_assign)?;
                        outwrite!(out, "[{}]", var_data.name);
                    }

                    outwrite!(out, " = ");
                }

                let instr_data = self.get_instr(instr)?;
                let args: Vec<String> = instr_data
                    .get_args()
                    .iter()
                    .map(|kind| kind.to_string())
                    .collect();

                outwrite!(out, "{} {}\n", instr_data.get_name(), args.join(", "));
            }
        }

        outwrite!(out, "}}\n");
        Ok(out)
    }
}
