pub mod block;
pub mod constant;
pub mod error;
pub mod function;
pub mod instr;
pub mod phi;
pub mod value;
pub mod variable;
#[cfg(feature = "viz")]
pub mod viz;

pub use block::Block;
pub use value::Value;
pub use variable::Variable;
