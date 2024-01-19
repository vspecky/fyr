pub type BuilderResult<T> = Result<T, error_stack::Report<BuilderError>>;

#[derive(Debug, thiserror::Error)]
pub enum BuilderError {
    #[error("block not found in function")]
    BlockNotFound,
    #[error("the function was not found")]
    FunctionNotFound,
    #[error("error interacting with the mach function")]
    FunctionError,
}
