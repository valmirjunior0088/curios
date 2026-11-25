//! The tests that read what the profiling instrumentation records.
//!
//! Each reads what only a recorder or an asked count takes: the per-pass spans of the CPS fixpoint and the stream a traced compilation and run file, the allocations counted while a trace listens or a measurement has asked, and from `curios-core`, the looks its walks count. Each is a measurement the suite compiles and ignores — one that names its figures through `xbench`'s harness taken by `cargo xbench measure <name>`, and the rest by hand, alone in its process, by the command recorded beside it — but `looks`'s, which every run holds: that a reading of the look count takes nothing from another.

mod allowance;
mod coordination;
mod fixpoint;
mod growth;
mod host_boundary;
mod looks;
mod runaways;
mod test_support;
mod towers;

use curios_profile::CountingAllocator;

// A program installs the counting allocator, and the suite these measurements run in is one: what they read of memory is what it counts.
#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;
