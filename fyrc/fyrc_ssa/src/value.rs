use std::fmt;

use fyrc_utils::EntityId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(u32);

impl Value {
    pub fn rewrite(&mut self, from: Value, to: Value) {
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
        self.0 as usize
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx as u32)
    }
}

impl Default for Value {
    fn default() -> Self {
        Self(0)
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
