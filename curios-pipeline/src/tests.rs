//! The pipeline's test suite, one file per subject.
//!
//! Every case here compiles a program end to end and reads the answer off the fold's own report, which is what [`test_support`] wraps.

pub(crate) mod test_support;

mod baseline_tests;
mod diagnostic_tests;
mod erasure_tests;
mod foreign_tests;
mod goal_tests;
mod implicit_tests;
mod incremental_tests;
mod inductive_tests;
mod inference_tests;
mod tuple_tests;
mod unit_tests;
