use std::fmt;

use error_stack::IntoReport;
use rustc_hash::FxHashMap;

use crate::{
    ssa::{
        error::SsaError,
        instr::{Instr, InstrData},
        Block, BlockData, BlockSealStatus, Const, ConstKind, SsaResult, Value, ValueData,
        ValueKind, ValueType, Variable, VariableData,
    },
    utils::{DenseMap, EntityId},
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
    pub(super) signature: Signature,
    pub(super) blocks: DenseMap<Block, BlockData>,
    pub(super) consts: DenseMap<Const, ConstKind>,
    pub(super) instrs: DenseMap<Instr, InstrData>,
    pub(super) values: DenseMap<Value, ValueData>,
    pub(super) variables: DenseMap<Variable, VariableData>,
    pub(super) results: FxHashMap<Instr, Value>,
    pub(super) var_defs: FxHashMap<Variable, FxHashMap<Block, Value>>,
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
    pub(super) fn new(sig: Signature) -> Self {
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
            variables,
            results: FxHashMap::default(),
            var_defs,
        }
    }

    pub(super) fn get_block(&self, block: Block) -> SsaResult<&BlockData> {
        self.blocks
            .get(block)
            .ok_or(SsaError::BlockNotFound)
            .into_report()
    }

    pub(super) fn get_block_mut(&mut self, block: Block) -> SsaResult<&mut BlockData> {
        self.blocks
            .get_mut(block)
            .ok_or(SsaError::BlockNotFound)
            .into_report()
    }

    pub(super) fn get_block_preds(&self, block: Block) -> SsaResult<Vec<Block>> {
        Ok(self.get_block(block)?.predecessors.clone())
    }

    pub(super) fn get_block_succs(&self, block: Block) -> SsaResult<Vec<Block>> {
        Ok(self.get_block(block)?.successors.clone())
    }

    pub(super) fn get_value(&self, value: Value) -> SsaResult<&ValueData> {
        self.values
            .get(value)
            .ok_or(SsaError::ValueNotFound)
            .into_report()
    }

    pub(super) fn get_value_mut(&mut self, value: Value) -> SsaResult<&mut ValueData> {
        self.values
            .get_mut(value)
            .ok_or(SsaError::ValueNotFound)
            .into_report()
    }

    pub(super) fn get_instr(&self, instr: Instr) -> SsaResult<&InstrData> {
        self.instrs
            .get(instr)
            .ok_or(SsaError::InstrNotFound)
            .into_report()
    }

    pub(super) fn get_instr_mut(&mut self, instr: Instr) -> SsaResult<&mut InstrData> {
        self.instrs
            .get_mut(instr)
            .ok_or(SsaError::InstrNotFound)
            .into_report()
    }

    pub(super) fn get_var(&self, var: Variable) -> SsaResult<&VariableData> {
        self.variables
            .get(var)
            .ok_or(SsaError::VariableNotFound)
            .into_report()
    }

    pub(super) fn get_succ_phi_uses(&self, block: Block) -> SsaResult<Vec<Value>> {
        Ok(self
            .get_block_succs(block)?
            .into_iter()
            .map(|succ| self.get_block(succ))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flat_map(|data| data.get_args_from_block(block))
            .collect())
    }

    pub(super) fn get_instr_def(&self, instr: Instr) -> Option<Value> {
        self.results.get(&instr).copied()
    }

    pub(super) fn get_instr_uses(&self, instr: Instr) -> SsaResult<Vec<Value>> {
        Ok(self.get_instr(instr)?.get_uses())
    }

    pub(super) fn get_phi_defs(&self, block: Block) -> SsaResult<Vec<Value>> {
        let data = self.get_block(block)?;
        Ok(data.phis.values().map(|phi| phi.value).collect())
    }

    pub(super) fn print(&self) -> SsaResult<String> {
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

            outwrite!(out, "{}::{}", Value(idx), arg.var_type);
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
                let value_data = self.get_value(phi.value)?;
                let var_data = self.get_var(phi.var)?;
                let args: Vec<String> = phi
                    .args
                    .iter()
                    .map(|(blk, val)| format!("{blk}:{val}"))
                    .collect();

                outwrite!(
                    out,
                    "    {}::{}[{}] = phi {}\n",
                    phi.value,
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
