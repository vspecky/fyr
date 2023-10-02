use thiserror::Error;

pub type SsaResult<T> = Result<T, error_stack::Report<SsaError>>;

#[derive(Debug, Error)]
pub enum SsaError {
    #[error("referenced instruction not found")]
    InstrNotFound,
    #[error("referenced block not found")]
    BlockNotFound,
    #[error("referenced value not found in function")]
    ValueNotFound,
    #[error("the phi for the given variable was not found in the given block")]
    PhiNotFound,
    #[error("the given variable was not found")]
    VariableNotFound,
    #[error("defs for the given variable were not found")]
    VariableDefsNotFound,
    #[error("There was an IO error while printing the function")]
    PrintIO,
}
