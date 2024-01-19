pub type CodegenResult<T> = Result<T, error_stack::Report<CodegenError>>;

#[derive(Debug, thiserror::Error)]
pub enum CodegenError {
    #[error("there was an error with the machinst function builder")]
    BuilderError,
    #[error("error interacting with the SSA function")]
    SsaFunctionError,
    #[error("free registers not found for instruction")]
    FreeRegsNotFound,
    #[error("the function was not found")]
    FunctionNotFound,
    #[error("failed to add machinst instruction to machinst IR")]
    InstrAddFailed,
    #[error("the value defined by an instruction was not present in the SSA function")]
    InstrDefNotFound,
    #[error("No scratch register found when one was expected")]
    NoScratchRegFound,
    #[error("Assignment to scratch register appeared twice in LTG")]
    RedundantScratchAssignment,
    #[error("instruction value was not allocated a register")]
    RegUnallocated,
    #[error("tried to assigned scratch register to scratch register")]
    Scratch2ScratchAssignment,
    #[error("spill slot unallocated to value when one was expected to be")]
    SpillSlotUnallocated,
}
