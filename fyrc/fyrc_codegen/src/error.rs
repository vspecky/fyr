pub type CodegenResult<T> = Result<T, error_stack::Report<CodegenError>>;

#[derive(Debug, thiserror::Error)]
pub enum CodegenError {
    #[error("the function was not found")]
    FunctionNotFound,
}
