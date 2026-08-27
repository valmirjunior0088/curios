//! The pipeline's test suite, one file per subject.
//!
//! Every case here compiles a program end to end and reads the answer off the fold's own report, which is what [`test_support`] wraps.

mod test_support;

mod diagnostic_tests;
mod erasure_tests;
mod foreign_tests;
mod goal_tests;
mod implicit_tests;
mod inductive_tests;
mod inference_tests;
mod tuple_tests;
mod unit_tests;
