//! Programmatic profiling for the Curios workspace: the recorder of what `curios-common`'s `profile!`, `sample!` and `note!` instrument.
//!
//! Every build carries the instrumentation, and nothing records unless a caller installs a recorder: a measurement point no recorder listens to costs one relaxed load. This crate names `tracing` for the subscriber it implements, and is what reads its stream back:
//!
//! - [`trace()`] and [`install`] are the two scopings of the subscriber that writes one row per span and event as it happens. `trace` runs one closure under it and leaves the process-global default alone; `install` *is* that default, for a binary whose scope is the whole invocation because it has no closure to wrap. Both take the destination from their caller, and they are the only producers.
//! - [`fold()`] recomputes timings, allocation figures and distributions from those rows. A question the summaries do not answer is asked of the file.
//! - The counting allocator, [`CountingAllocator`], adds the memory half of a report: a program that reads memory installs it as its `#[global_allocator]`, and it counts only while a recorder listens — `install`'s for the rest of the process, `trace`'s for its closure — or a measurement has asked with [`count_allocations`], so a boundary row carries what the process took and returned while counting, and no run pays for a count nobody reads.
//! - [`Instructions`] counts what a measurement's thread and the threads it starts retire, which neither a look nor a byte sees of a walk that allocates nothing or of code that is not Rust.
//!
//! Why profiling is configured in code and never from the environment, why every build carries it, why a third instrument measures magnitude beside time and bytes and a fourth counts instructions, why the allocator counts process-wide and a program installs it, and why the library emits records rather than summaries are `README.md`'s decisions.

mod count;
use count::*;
pub use count::{CountingAllocator, allocated_bytes, allocation_count, count_allocations};

mod instructions;
pub use instructions::Instructions;

mod fold;
pub use fold::{OpenSpan, Report, SampleSummary, SpanInstance, Summary, Within, fold, fold_at};

mod trace;
use trace::*;
pub use trace::{Destination, install, trace};
