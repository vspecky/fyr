pub mod error;
pub mod fbuilder;
pub mod function;
pub mod instr;
pub mod macros;
pub mod module;
pub mod passes;

use std::fmt;

use rustc_hash::FxHashMap;

use crate::utils::{EntityId, UxoResult};

pub type SsaResult<R> = UxoResult<R, error::SsaError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(usize);

impl Value {
    fn rewrite(&mut self, from: Value, to: Value) {
        if *self == from {
            *self = to;
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Int8,
    Int16,
    Int32,
}

impl EntityId for Value {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Int8 => "i8",
                Self::Int16 => "i16",
                Self::Int32 => "i32",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Phi {
    pub var: Variable,
    pub value: Value,
    pub args: FxHashMap<Block, Value>,
}

impl Phi {
    fn rewrite_arg(&mut self, from: Value, to: Value) -> bool {
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

#[derive(Clone)]
pub struct ValueData {
    pub value_type: ValueType,
    pub value_kind: ValueKind,
}

#[derive(Clone)]
pub enum ValueKind {
    InstrRes(instr::Instr),
    FuncArg,
    Phi { var: Variable, block: Block },
    Tombstone,
}

impl ValueData {
    pub fn entomb(&mut self) {
        self.value_kind = ValueKind::Tombstone;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Const(usize);

impl EntityId for Const {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstKind {
    Const8(u8),
    Const16(u16),
    Const32(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(usize);

impl EntityId for Variable {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct VariableData {
    pub name: String,
    pub var_type: ValueType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Block(usize);

impl EntityId for Block {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "block_{}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BlockFillKind {
    Empty,
    Partial,
    Filled,
}

#[derive(Debug, Clone, Copy)]
pub enum BlockSealStatus {
    Unsealed,
    Sealed,
}

#[derive(Clone)]
pub struct BlockData {
    pub predecessors: Vec<Block>,
    pub successors: Vec<Block>,
    pub phis: FxHashMap<Variable, Phi>,
    pub instrs: Vec<instr::Instr>,
    pub status: BlockFillKind,
    pub sealed: BlockSealStatus,
}

impl BlockData {
    pub fn new() -> Self {
        Self {
            predecessors: Vec::new(),
            successors: Vec::new(),
            phis: FxHashMap::default(),
            instrs: Vec::new(),
            status: BlockFillKind::Empty,
            sealed: BlockSealStatus::Unsealed,
        }
    }

    fn is_sealed(&self) -> bool {
        matches!(self.sealed, BlockSealStatus::Sealed)
    }

    fn get_args_from_block(&self, block: Block) -> Vec<Value> {
        self.phis
            .values()
            .filter_map(|phi| phi.args.get(&block).copied())
            .collect()
    }
}

impl Default for BlockData {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}
