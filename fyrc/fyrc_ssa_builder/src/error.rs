pub type BuilderResult<T> = Result<T, error_stack::Report<BuilderError>>;

#[derive(Debug, thiserror::Error)]
pub enum BuilderError {
    #[error("encountered an unexpectedly filled block")]
    BlockFilled,
    #[error("block was not filled when it was expected to be")]
    BlockNotFilled,
    #[error("block was not empty when it was expected to be")]
    BlockNotEmpty,
    #[error("block was not sealed when it was expected to be")]
    BlockNotSealed,
    #[error("function has already been built")]
    FunctionAlreadyBuilt,
    #[error("tried to retrieve an undefined function")]
    FunctionNotDefined,
    #[error("referenced function not found")]
    FunctionNotFound,
    #[error("a function resource was not found")]
    FunctionResourceMissing,
    #[error("a given global value was not found")]
    GlobalValueMissing,
    #[error("incorrect number of arguments received for function")]
    IncorrectFunctionArgs,
    #[error("argument of invalid type received for function")]
    IncorrectFuncArgType,
    #[error("a value with an invalid type was encountered")]
    InvalidValueType,
    #[error("instruction did not return a result even though expected to")]
    MissingResult,
    #[error("cannot declare a sealed block as successor to any other block")]
    NoSealedSuccessors,
    #[error("encountered an invalid empty phi")]
    PhiEmpty,
    #[error("block is already sealed")]
    SealingSealedBlock,
    #[error("cannot mutate block with a branching instruction")]
    NoBranchInMutation,
}
