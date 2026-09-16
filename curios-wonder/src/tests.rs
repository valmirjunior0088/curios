//! The engine's test suite, one file per subject.
//!
//! Every case here puts a question to the engine the way a transport does and reads the answer off the records it hands back. The scope of packages more than one suite asks about is [`test_support`]'s.

mod test_support;

mod ask_tests;
mod record_tests;
mod stage_tests;
mod store_tests;
