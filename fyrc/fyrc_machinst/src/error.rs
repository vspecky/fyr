pub type MachinstResult<T> = Result<T, error_stack::Report<MachinstError>>;

#[derive(Debug, thiserror::Error)]
pub enum MachinstError {
    #[error("block not found in the function")]
    BlockNotFound,
    #[error("const not found in the function")]
    ConstNotFound,
    #[error("instruction not found in the function")]
    InstrNotFound,
}
