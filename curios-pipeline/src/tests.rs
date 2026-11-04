//! The pipeline's test suite, one file per subject.
//!
//! Every case here compiles a program end to end and reads the answer off the fold's own report, which is what [`test_support`] wraps.

pub(crate) mod test_support;

mod baseline;
mod budget;
mod candidate;
mod certification;
mod diagnostic;
mod erasure;
mod foreign;
mod goal;
mod identity;
mod implicit;
mod incremental_agreement;
mod incremental_dependency;
mod incremental_edit;
mod incremental_placement;
mod inductive;
mod inference;
mod lint;
mod order;
mod paste_back_module;
mod paste_back_type;
mod paste_back_witness;
mod placement;
mod read;
mod resolution;
mod spelling;
mod suggestion;
mod translation;
mod tuple;
mod unit;
