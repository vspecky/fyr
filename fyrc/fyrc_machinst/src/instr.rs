pub mod thumb;

use fyrc_utils::EntityId;
pub use thumb::ThumbMachinstData;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Machinst(usize);

impl EntityId for Machinst {
    #[inline]
    fn get_id(&self) -> usize {
        self.0
    }

    #[inline]
    fn with_id(idx: usize) -> Self {
        Self(idx)
    }
}
