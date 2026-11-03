//! The tests of `sys_module`, one file per subject.

// Each named by path, as `sys_module.rs` names this file: a module included so finds its children beside it rather than in `tests/`.
#[path = "tests/exposure.rs"]
mod exposure;

#[path = "tests/lowering.rs"]
mod lowering;

#[path = "tests/roster.rs"]
mod roster;
