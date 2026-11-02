//! The tests of `sys_module`, one file per subject.

// Each named by path, as `sys_module.rs` names this file: a module included so finds its children beside it rather than in `tests/`.
#[path = "tests/exposure_tests.rs"]
mod exposure_tests;

#[path = "tests/lowering_tests.rs"]
mod lowering_tests;

#[path = "tests/roster_tests.rs"]
mod roster_tests;
