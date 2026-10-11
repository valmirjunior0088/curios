//! The tests that read what the profiling instrumentation records.
//!
//! Each reads something `curios-profile` records only under its `enabled` feature, reached through this crate's `profile` feature: the per-pass spans of the Cont fixpoint, the stream a traced compilation and run file. So the module is the one thing gated on the feature, and no build that ships compiles it. `cargo xtask instrumented lint` is the step that does; nothing in the gate runs what is here, since that would compile the whole suite a second time with every feature on, so each test is taken by hand, by the command recorded beside it.

mod coordination;
mod fixpoint;
mod host_boundary;
mod test_support;
