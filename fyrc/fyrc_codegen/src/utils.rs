use fyrc_ssa::value::ValueType;

/// Extension trait for [`ValueType`]
pub trait ValueTypeExt {
    /// Returns how many bits a value of this type needs to be LSL and LSR'd after an arithmetic
    /// operation to zero out any excess bits out of the 32.
    fn get_ext_offset(&self) -> Option<u16>;
}

impl ValueTypeExt for ValueType {
    fn get_ext_offset(&self) -> Option<u16> {
        match self {
            ValueType::Int8 => Some(24),
            ValueType::Int16 => Some(16),
            ValueType::Int32 => None,
        }
    }
}
