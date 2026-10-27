//! The pipeline's test suite, one file per subject.
//!
//! Every case here compiles a program end to end and reads the answer off the fold's own report, which is what [`test_support`] wraps.

pub(crate) mod test_support;

mod baseline_tests;
mod budget_tests;
mod candidate_tests;
mod certification_tests;
mod diagnostic_tests;
mod erasure_tests;
mod foreign_tests;
mod goal_tests;
mod identity_tests;
mod implicit_tests;
mod incremental_agreement_tests;
mod incremental_dependency_tests;
mod incremental_edit_tests;
mod incremental_placement_tests;
mod inductive_tests;
mod inference_tests;
mod lint_tests;
mod order_tests;
mod paste_back_module_tests;
mod paste_back_type_tests;
mod paste_back_witness_tests;
mod placement_tests;
mod resolution_tests;
mod spelling_tests;
mod suggestion_tests;
mod translation_tests;
mod tuple_tests;
mod unit_tests;
