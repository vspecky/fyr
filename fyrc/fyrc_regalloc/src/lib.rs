pub mod coloring;
pub mod spilling;

#[doc(hidden)]
pub mod __private {
    pub use fxhash::FxHashMap;
    pub use fyrc_ssa_passes as fsp;
}
