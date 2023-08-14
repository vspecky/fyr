pub mod dense_map;
pub mod union_find;

pub use dense_map::DenseMap;
pub use union_find::UnionFind;

pub type UxoResult<R, E> = error_stack::Result<R, E>;

pub trait EntityId: Copy {
    fn get_id(&self) -> usize;
    fn with_id(idx: usize) -> Self;
}

pub trait BoolExt<E> {
    fn and_err(&self, err: E) -> Result<(), E>;
    fn and_then_err<F>(&self, f: F) -> Result<(), E>
    where
        F: Fn() -> E;
    fn or_err(&self, err: E) -> Result<(), E>;
    fn or_else_err<F>(&self, f: F) -> Result<(), E>
    where
        F: Fn() -> E;
}

impl<E> BoolExt<E> for bool {
    fn and_err(&self, err: E) -> Result<(), E> {
        if *self {
            Err(err)
        } else {
            Ok(())
        }
    }

    fn and_then_err<F>(&self, f: F) -> Result<(), E>
    where
        F: Fn() -> E,
    {
        if *self {
            Err(f())
        } else {
            Ok(())
        }
    }

    fn or_err(&self, err: E) -> Result<(), E> {
        if !*self {
            Err(err)
        } else {
            Ok(())
        }
    }

    fn or_else_err<F>(&self, f: F) -> Result<(), E>
    where
        F: Fn() -> E,
    {
        if !*self {
            Err(f())
        } else {
            Ok(())
        }
    }
}
