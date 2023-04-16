use std::{
    fmt,
    ops::{Deref, DerefMut},
};

use crate::{
    ssa::{function::FuncRef, Block, Const, ConstKind, Value},
    utils::EntityId,
};

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
}

macro_rules! instr_3 {
    (#[$doc:meta] $name:ident, $name_str:literal) => {
        #[$doc]
        #[derive(Debug, Clone)]
        pub struct $name {
            pub rs: Value,
            pub rd: Value,
        }

        impl SsaInstr for $name {
            fn rewrite_value(&mut self, from: Value, to: Value) {
                self.rs.rewrite(from, to);
                self.rd.rewrite(from, to);
            }

            fn get_name(&self) -> String {
                $name_str.to_string()
            }

            fn get_args(&self) -> InstrArgs {
                InstrArgs::from_iter([
                    ArgKind::NamedValue("rs".to_string(), self.rs),
                    ArgKind::NamedValue("rd".to_string(), self.rd),
                ])
            }
        }
    };
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

instr_3!(
    #[doc = "Add Rd to Rs and store in Rn"]
    Add,
    "add"
);

instr_3!(
    #[doc = "Add with carry"]
    Adc,
    "adc"
);

instr_3!(
    #[doc = "Subtract Rd from Rs and store in Rn"]
    Sub,
    "sub"
);

instr_3!(
    #[doc = "Multiply Rd with Rs and store in Rn"]
    Mul,
    "mul"
);

instr_3!(
    #[doc = "Logical shift left Rd by Rs and store in Rn"]
    Lsl,
    "lsl"
);

instr_3!(
    #[doc = "Logical shift right Rd by Rs and store in Rn"]
    Lsr,
    "lsr"
);

instr_3!(
    #[doc = "Arithmetic shift right Rd by Rs and store in Rn"]
    Asl,
    "asl"
);

#[derive(Debug, Clone)]
pub struct Call {
    pub func: FuncRef,
    pub args: Vec<Value>,
}

impl SsaInstr for Call {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        for arg in &mut self.args {
            arg.rewrite(from, to);
        }
    }

    fn get_name(&self) -> String {
        "call".to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs(self.args.iter().copied().map(ArgKind::Value).collect())
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
}

#[derive(Debug, Clone, Copy)]
pub enum Cond {
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanEq,
    LessThanEq,
}

impl fmt::Display for Cond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Equal => "eq",
            Self::NotEqual => "neq",
            Self::GreaterThan => "gt",
            Self::LessThan => "lt",
            Self::GreaterThanEq => "gte",
            Self::LessThanEq => "lte",
        })
    }
}

#[derive(Debug, Clone)]
pub struct Cmp {
    pub rs: Value,
    pub rn: Value,
    pub result_cond: Option<Cond>,
}

impl SsaInstr for Cmp {
    fn rewrite_value(&mut self, from: Value, to: Value) {
        self.rs.rewrite(from, to);
        self.rn.rewrite(from, to);
    }

    fn has_result(&self) -> bool {
        self.result_cond.is_some()
    }

    fn get_name(&self) -> String {
        if self.result_cond.is_some() {
            "cmpb"
        } else {
            "cmps"
        }
        .to_string()
    }

    fn get_args(&self) -> InstrArgs {
        let mut args = Vec::<ArgKind>::new();
        if let Some(ref cond) = self.result_cond {
            args.push(ArgKind::Str(cond.to_string()));
        }

        args.push(ArgKind::Value(self.rs));
        args.push(ArgKind::Value(self.rn));

        InstrArgs(args)
    }
}

#[derive(Debug, Clone)]
pub enum BranchKind {
    Status(Cond),
    Zero(Value),
    NotZero(Value),
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub kind: BranchKind,
    pub then_block: Block,
    pub else_block: Block,
}

impl SsaInstr for Branch {
    fn rewrite_value(&mut self, _: Value, _: Value) {}

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
        match &self.kind {
            BranchKind::Status(_) => "brs",
            BranchKind::Zero(_) => "brz",
            BranchKind::NotZero(_) => "brnz",
        }
        .to_string()
    }

    fn get_args(&self) -> InstrArgs {
        InstrArgs::from_iter([
            match self.kind {
                BranchKind::Status(cond) => ArgKind::Str(cond.to_string()),
                BranchKind::Zero(val) => ArgKind::Value(val),
                BranchKind::NotZero(val) => ArgKind::Value(val),
            },
            ArgKind::NamedBlock("then".to_string(), self.then_block),
            ArgKind::NamedBlock("else".to_string(), self.else_block),
        ])
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
}

impl_from_instr_data!(
    Add, Adc, Sub, Mul, Lsl, Lsr, Asl, Call, Load, Store, LoadConst, Ret, Cmp, Branch, Jump
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Instr(usize);

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
    Add(Add),
    Adc(Adc),
    Sub(Sub),
    Mul(Mul),
    Lsl(Lsl),
    Lsr(Lsr),
    Asl(Asl),
    Call(Call),
    Load(Load),
    Store(Store),
    LoadConst(LoadConst),
    Ret(Ret),
    Cmp(Cmp),
    Branch(Branch),
    Jump(Jump),
}

impl Deref for InstrData {
    type Target = dyn SsaInstr;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Add(add) => add,
            Self::Adc(adc) => adc,
            Self::Sub(sub) => sub,
            Self::Mul(mul) => mul,
            Self::Lsl(lsl) => lsl,
            Self::Lsr(lsr) => lsr,
            Self::Asl(asl) => asl,
            Self::Call(call) => call,
            Self::Load(load) => load,
            Self::Store(store) => store,
            Self::LoadConst(lc) => lc,
            Self::Ret(ret) => ret,
            Self::Cmp(cmp) => cmp,
            Self::Branch(branch) => branch,
            Self::Jump(jump) => jump,
        }
    }
}

impl DerefMut for InstrData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Add(add) => add,
            Self::Adc(adc) => adc,
            Self::Sub(sub) => sub,
            Self::Mul(mul) => mul,
            Self::Lsl(lsl) => lsl,
            Self::Lsr(lsr) => lsr,
            Self::Asl(asl) => asl,
            Self::Call(call) => call,
            Self::Load(load) => load,
            Self::Store(store) => store,
            Self::LoadConst(lc) => lc,
            Self::Ret(ret) => ret,
            Self::Cmp(cmp) => cmp,
            Self::Branch(branch) => branch,
            Self::Jump(jump) => jump,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_size() {
        let size = std::mem::size_of::<InstrData>();
        println!("Size is {size}");
    }
}
