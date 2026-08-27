//! Structural acceptance fixtures: each compiles a small `.crs` fixture to raw pre-Binaryen wasm and asserts a property of the module.

mod closure_tests;
mod fusion_tests;
mod layout_tests;
mod loop_tests;
mod measurement_tests;
mod test_support;

// The shapes the sibling codegen suites also assert through.
pub(super) use test_support::{compile_raw, functions, user_allocations, wat};
