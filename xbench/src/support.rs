//! What the benchmark bench is written in and run by: what a reading is, what it is taken under, the contest that takes one, and the report that computes every figure the readings amount to. The readings themselves are the binary's, one a module beside `main.rs`, as `xboard`'s board is that crate's binary's.
//!
//! It reads in the order a sitting uses it:
//!
//! - [`held`] holds every tool to the pin the file that owns it states, before anything is built.
//! - [`capture`] asks the host where the reading is being taken.
//! - [`collect`] builds the contestants, holds them to the answers their workloads are known to have, times them interleaved, and prints a [`Reading`] as the module that records it.
//! - [`report()`] computes every median, span, ratio and refusal from the readings, since none of them is stored.

mod date;
pub(crate) use date::*;

mod platform;
pub(crate) use platform::*;

mod reading;
pub(crate) use reading::*;

mod pins;
pub(crate) use pins::*;

mod contest;
pub(crate) use contest::*;

mod report;
pub(crate) use report::*;
