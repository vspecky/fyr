pub type CodegenResult<T> = Result<T, error_stack::Report<CodegenError>>;

#[derive(Debug, thiserror::Error)]
pub enum CodegenError {
    #[error("error interacting with the SSA function")]
    SsaFunctionError,
    #[error("the function was not found")]
    FunctionNotFound,
    #[error("failed to add machinst instruction to machinst IR")]
    InstrAddFailed,
    #[error("the value defined by an instruction was not present in the SSA function")]
    InstrDefNotFound,
    #[error("instruction value was not allocated a register")]
    RegUnallocated,
    #[error("spill slot unallocated to value when one was expected to be")]
    SpillSlotUnallocated,
}
