use thiserror::Error;

#[derive(Debug, Error)]
pub enum SsaError {
    #[error("referenced instruction not found")]
    InstrNotFound,
    #[error("encountered an unexpectedly filled block")]
    BlockFilled,
    #[error("block was not filled when it was expected to be")]
    BlockNotFilled,
    #[error("block was not empty when it was expected to be")]
    BlockNotEmpty,
    #[error("referenced block not found")]
    BlockNotFound,
    #[error("cannot declare a sealed block as successor to any other block")]
    NoSealedSuccessors,
    #[error("block is already sealed")]
    SealingSealedBlock,
    #[error("referenced value not found in function")]
    ValueNotFound,
    #[error("the phi for the given variable was not found in the given block")]
    PhiNotFound,
    #[error("encountered an invalid empty phi")]
    PhiEmpty,
    #[error("a value with an invalid type was encountered")]
    InvalidValueType,
    #[error("The given variable was not found")]
    VariableNotFound,
    #[error("instruction did not return a result even though expected to")]
    MissingResult,
    #[error("There was an IO error while printing the function")]
    PrintIO,
}
