use std::{
    fmt,
    ops::{Deref, DerefMut},
};

use crate::{
    block::Block,
    constant::{Const, ConstKind},
    function::Function,
    value::{GlobalValue, Value, ValueType},
};

use fyrc_utils::EntityId;

#[derive(Debug, Clone)]
pub enum ArgKind {
    Str(String),
    Value(Value),
    NamedValue(String, Value),
    Block(Block),
    NamedBlock(String, Block),
}

impl fmt::Display for ArgKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&match self {
            Self::Str(s) => s.clone(),
            Self::Value(val) => val.to_string(),
            Self::NamedValue(name, val) => format!("{name}:{val}"),
            Self::Block(block) => format!("%{}", block),
            Self::NamedBlock(name, block) => format!("{name}:%{block}"),
        })
    }
}

pub struct InstrArgs(Vec<ArgKind>);

impl FromIterator<ArgKind> for InstrArgs {
    fn from_iter<T: IntoIterator<Item = ArgKind>>(iter: T) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl Deref for InstrArgs {
    type Target = [ArgKind];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub trait SsaInstr {
    fn rewrite_value(&mut self, from: Value, to: Value);

    fn has_result(&self) -> bool {
        true
    }

    fn is_terminal(&self) -> bool {
        false
    }

    fn get_referenced_blocks(&self) -> Vec<Block> {
        Vec::new()
    }

    fn get_name(&self) -> String;

    fn get_args(&self) -> InstrArgs;

    fn get_uses(&self) -> Vec<Value>;
}

macro_rules! impl_from_instr_data {
    ($($name:ident),+) => {
        $(
            impl From<$name> for InstrData {
                fn from(instr: $name) -> InstrData {
                    InstrData::$name(instr)
                }
            }
        )+
    };
}

#[derive(Debug, Clone, Copy)]
pub enum AluBinaryOpKind {
    Add,
    Sub,
    Mul,
    Lsl,
    Lsr,
    Asr,
    And,
    Orr,
    Xor,
}

impl ToString for AluBinaryOpKind {
    fn to_string(&self) -> String {
        let op = match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Lsl => "lsl",
            Self::Lsr => "lsr",
            Self::Asr => "asr",
            Self::And => "and",
            Self::Orr => "orr",
            Self::Xor => "xor",
        };

        format!("alu.{op}")
    }
}

#[derive(Debug, Clone)]
pub struct AluBinaryOp {
    pub op: AluBinaryOpKind,
    pub rs: Value,
    pub rn: Value,
}

impl SsaInstr for AluBinaryOp {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        self.rs.rewrite(from, to);
        self.rn.rewrite(from, to);
    }

    fn get_name(&self) -> String {
        self.op.to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs::from_iter([
            ArgKind::NamedValue("rs".to_string(), self.rs),
            ArgKind::NamedValue("rn".to_string(), self.rn),
        ])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![self.rs, self.rn]
    }
}

#[derive(Debug, Clone)]
pub enum AluUnaryOpKind {
    Neg,
    Not,
}

impl ToString for AluUnaryOpKind {
    fn to_string(&self) -> String {
        let op = match self {
            Self::Neg => "neg",
            Self::Not => "not",
        };

        format!("alu.{op}")
    }
}

#[derive(Debug, Clone)]
pub struct AluUnaryOp {
    pub op: AluUnaryOpKind,
    pub rs: Value,
}

impl SsaInstr for AluUnaryOp {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        self.rs.rewrite(from, to);
    }

    fn get_name(&self) -> String {
        self.op.to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs::from_iter([ArgKind::NamedValue("rs".to_string(), self.rs)])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![self.rs]
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub func: Function,
    pub args: Vec<Value>,
    pub ret_type: Option<ValueType>,
}

impl SsaInstr for Call {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        for arg in &mut self.args {
            arg.rewrite(from, to);
        }
    }

    fn has_result(&self) -> bool {
        self.ret_type.is_some()
    }

    fn get_name(&self) -> String {
        "call".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs(self.args.iter().copied().map(ArgKind::Value).collect())
    }

    fn get_uses(&self) -> Vec<Value> {
        self.args.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Load {
    pub base: Value,
    pub offset: Value,
}

impl SsaInstr for Load {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        self.base.rewrite(from, to);
        self.offset.rewrite(from, to);
    }

    fn get_name(&self) -> String {
        "load".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs::from_iter([
            ArgKind::NamedValue("base".to_string(), self.base),
            ArgKind::NamedValue("offset".to_string(), self.offset),
        ])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![self.base, self.offset]
    }
}

#[derive(Debug, Clone)]
pub struct Store {
    pub val: Value,
    pub base: Value,
    pub offset: Value,
}

impl SsaInstr for Store {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        self.val.rewrite(from, to);
        self.base.rewrite(from, to);
        self.offset.rewrite(from, to);
    }

    fn has_result(&self) -> bool {
        false
    }

    fn get_name(&self) -> String {
        "store".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs::from_iter([
            ArgKind::NamedValue("val".to_string(), self.val),
            ArgKind::NamedValue("base".to_string(), self.base),
            ArgKind::NamedValue("offset".to_string(), self.offset),
        ])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![self.val, self.base, self.offset]
    }
}

#[derive(Debug, Clone)]
pub struct LoadConst {
    pub const_ref: Const,
    pub kind: ConstKind,
}

impl SsaInstr for LoadConst {
    fn rewrite_value(&mut self, _: Value, _: Value) {}

    fn get_name(&self) -> String {
        match self.kind {
            ConstKind::Const8(_) => "const.i8",
            ConstKind::Const16(_) => "const.i16",
            ConstKind::Const32(_) => "const.i32",
        }
        .to_string()
    }

    fn get_args(&self) -> InstrArgs {
        let value = match self.kind {
            ConstKind::Const8(val) => val.to_string(),
            ConstKind::Const16(val) => val.to_string(),
            ConstKind::Const32(val) => val.to_string(),
        };

        InstrArgs::from_iter([ArgKind::Str(format!("#{value}"))])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![]
    }
}

#[derive(Debug, Clone)]
pub struct LoadGlobal {
    pub value: GlobalValue,
}

impl SsaInstr for LoadGlobal {
    fn rewrite_value(&mut self, _: Value, _: Value) {}

    fn get_name(&self) -> String {
        "load_const".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs::from_iter([ArgKind::Str(format!("G({})", self.value.get_id()))])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![]
    }
}

#[derive(Debug, Clone)]
pub struct Ret {
    pub val: Option<Value>,
}

impl SsaInstr for Ret {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        if let Some(val) = &mut self.val {
            val.rewrite(from, to)
        }
    }

    fn has_result(&self) -> bool {
        false
    }

    fn is_terminal(&self) -> bool {
        true
    }

    fn get_name(&self) -> String {
        "ret".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs(
            self.val
                .map(|val| vec![ArgKind::Value(val)])
                .unwrap_or_default(),
        )
    }

    fn get_uses(&self) -> Vec<Value> {
        self.val.map(|v| vec![v]).unwrap_or_default()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Cond {
    Eq,
    Neq,
    Sgt,
    Sgte,
    Slt,
    Slte,
    Ugt,
    Ugte,
    Ult,
    Ulte,
}

impl fmt::Display for Cond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Eq => "eq",
            Self::Neq => "neq",
            Self::Sgt => "sgt",
            Self::Sgte => "sgte",
            Self::Slt => "slt",
            Self::Slte => "slte",
            Self::Ugt => "ugt",
            Self::Ugte => "ugte",
            Self::Ult => "ult",
            Self::Ulte => "ulte",
        })
    }
}

#[derive(Debug, Clone)]
pub struct ICmp {
    pub rs: Value,
    pub rn: Value,
    pub cond: Cond,
}

impl SsaInstr for ICmp {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        self.rs.rewrite(from, to);
        self.rn.rewrite(from, to);
    }

    fn has_result(&self) -> bool {
        true
    }

    fn get_name(&self) -> String {
        "icmp".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        let mut args = Vec::<ArgKind>::with_capacity(3);
        args.push(ArgKind::Str(self.cond.to_string()));
        args.push(ArgKind::Value(self.rs));
        args.push(ArgKind::Value(self.rn));

        InstrArgs(args)
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![self.rs, self.rn]
    }
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub value: Value,
    pub then_block: Block,
    pub else_block: Block,
}

impl SsaInstr for Branch {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        self.value.rewrite(from, to);
    }

    fn has_result(&self) -> bool {
        false
    }

    fn is_terminal(&self) -> bool {
        true
    }

    fn get_referenced_blocks(&self) -> Vec<Block> {
        vec![self.then_block, self.else_block]
    }

    fn get_name(&self) -> String {
        "br".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs::from_iter([
            ArgKind::NamedBlock("then".to_string(), self.then_block),
            ArgKind::NamedBlock("else".to_string(), self.else_block),
        ])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![self.value]
    }
}

#[derive(Debug, Clone)]
pub struct Jump {
    pub dest: Block,
}

impl SsaInstr for Jump {
    fn rewrite_value(&mut self, _: Value, _: Value) {}

    fn has_result(&self) -> bool {
        false
    }

    fn is_terminal(&self) -> bool {
        true
    }

    fn get_referenced_blocks(&self) -> Vec<Block> {
        vec![self.dest]
    }

    fn get_name(&self) -> String {
        "jmp".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs(vec![ArgKind::Block(self.dest)])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![]
    }
}

#[derive(Debug, Clone)]
pub struct CopyV {
    pub val: Value,
}

impl SsaInstr for CopyV {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        self.val.rewrite(from, to);
    }

    fn has_result(&self) -> bool {
        true
    }

    fn is_terminal(&self) -> bool {
        false
    }

    fn get_name(&self) -> String {
        "copy".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs(vec![ArgKind::Value(self.val)])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![self.val]
    }
}

#[derive(Debug, Clone)]
pub struct SpillValue {
    pub val: Value,
}

impl SsaInstr for SpillValue {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        self.val.rewrite(from, to);
    }

    fn has_result(&self) -> bool {
        false
    }

    fn is_terminal(&self) -> bool {
        false
    }

    fn get_name(&self) -> String {
        "spill".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs(vec![ArgKind::Value(self.val)])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![self.val]
    }
}

#[derive(Debug, Clone)]
pub struct ReloadValue {
    pub val: Value,
}

impl SsaInstr for ReloadValue {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        self.val.rewrite(from, to);
    }

    fn has_result(&self) -> bool {
        false
    }

    fn is_terminal(&self) -> bool {
        false
    }

    fn get_name(&self) -> String {
        "reload".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs(vec![ArgKind::Value(self.val)])
    }

    fn get_uses(&self) -> Vec<Value> {
        vec![self.val]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Instr(usize);

impl Instr {
    pub const NOP: Self = Self(usize::MAX);
}

impl EntityId for Instr {
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
pub enum InstrData {
    AluBinaryOp(AluBinaryOp),
    AluUnaryOp(AluUnaryOp),
    Call(Call),
    Load(Load),
    Store(Store),
    LoadConst(LoadConst),
    LoadGlobal(LoadGlobal),
    Ret(Ret),
    ICmp(ICmp),
    Branch(Branch),
    Jump(Jump),
    CopyV(CopyV),
    SpillValue(SpillValue),
    ReloadValue(ReloadValue),
}

impl_from_instr_data! {
    AluBinaryOp,
    AluUnaryOp,
    Call,
    Load,
    Store,
    LoadConst,
    LoadGlobal,
    Ret,
    ICmp,
    Branch,
    Jump,
    CopyV,
    SpillValue,
    ReloadValue
}

impl Deref for InstrData {
    type Target = dyn SsaInstr;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::AluBinaryOp(abo) => abo,
            Self::AluUnaryOp(auo) => auo,
            Self::Call(call) => call,
            Self::Load(load) => load,
            Self::Store(store) => store,
            Self::LoadConst(lc) => lc,
            Self::LoadGlobal(lg) => lg,
            Self::Ret(ret) => ret,
            Self::ICmp(cmp) => cmp,
            Self::Branch(branch) => branch,
            Self::Jump(jump) => jump,
            Self::CopyV(copy_v) => copy_v,
            Self::SpillValue(spill_v) => spill_v,
            Self::ReloadValue(reload_v) => reload_v,
        }
    }
}

impl DerefMut for InstrData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::AluBinaryOp(abo) => abo,
            Self::AluUnaryOp(auo) => auo,
            Self::Call(call) => call,
            Self::Load(load) => load,
            Self::Store(store) => store,
            Self::LoadConst(lc) => lc,
            Self::LoadGlobal(lg) => lg,
            Self::Ret(ret) => ret,
            Self::ICmp(cmp) => cmp,
            Self::Branch(branch) => branch,
            Self::Jump(jump) => jump,
            Self::CopyV(copy_v) => copy_v,
            Self::SpillValue(spill_v) => spill_v,
            Self::ReloadValue(reload_v) => reload_v,
        }
    }
}
