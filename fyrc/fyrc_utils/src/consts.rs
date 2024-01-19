/// The number of function arguments that reside in registers when calling a function according to
/// the ARM ABI
pub const ABI_ARGS_IN_REGS: usize = 4;

/// Number of caller saved registers
pub const ABI_NUM_CALLER_SAVED_REGS: usize = 4;
/// Number of callee saved registers including the LR register
pub const ABI_NUM_CALLEE_SAVED_REGS: usize = 4 + 1;
