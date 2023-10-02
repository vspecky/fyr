pub type PassResult<T, E> = Result<T, error_stack::Report<E>>;
