//! The fixed prelude's build: `/sys`'s roster and `/std` as authored, and the work that lowers, elaborates, certifies and erases the two into the certified units `curios-prelude` embeds.
//!
//! `curios-prelude`'s build script takes this crate as a build-dependency and runs [`compile()`] into its `OUT_DIR`, keyed by [`key`] and filed in its checkout's [`Images`]; `profile`, this crate's program, runs the same work under a recorder; and the suites that read the library or the roster ask [`std_directory`], [`std_source`] and [`sys_module()`]. Why the build is a crate of its own, what `/sys` holds and how `/std` is laid out are `README.md`'s decisions.

mod compile;
pub use compile::compile;

mod images;
pub use images::{Images, Settled, key};

mod std_module;
pub use std_module::{std_directory, std_source};

mod sys_module;
pub use sys_module::{sys_module, sys_source};
