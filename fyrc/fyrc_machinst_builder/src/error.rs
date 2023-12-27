pub type BuilderResult<T> = Result<T, error_stack::Report<BuilderError>>;

#[derive(Debug, thiserror::Error)]
pub enum BuilderError {
    #[error("the function was not found")]
    FunctionNotFound,
}
