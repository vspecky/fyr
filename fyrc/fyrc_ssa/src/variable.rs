use std::fmt;

use crate::value::ValueType;

use fyrc_utils::EntityId;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(u32);

impl Variable {
    pub const NONE: Self = Self(u32::MAX);
}

impl EntityId for Variable {
    #[inline]
    fn get_id(&self) -> usize {
        self.0 as usize
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx as u32)
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
