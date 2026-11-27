//! Programmatic profiling for the Curios workspace: the recorder of what `curios-tracing`'s `profile!`, `sample!` and `note!` instrument, written as Perfetto's trace.
//!
//! Every build carries the instrumentation, and nothing records unless a caller installs a recorder: a measurement point no recorder listens to costs one relaxed load. This crate implements `tracing`'s subscriber, reached through `curios-tracing` as the macros are, and is what reads its stream back:
//!
//! - [`trace()`] and [`install`] are the two scopings of the subscriber that writes one packet per span boundary and event as it happens, a packet sequence and a track for each thread that records. `trace` runs one closure under it and leaves the process-global default alone; `install` *is* that default, for a binary whose scope is the whole invocation because it has no closure to wrap. Both take the destination from their caller, and the [`Tally`]s, the counts their program keeps for each thread, which ride each slice beside the allocator's; they are the only producers.
//! - [`fold()`] recomputes timings, allocation figures and distributions from those packets into a [`Profile`], which draws `curios profile`'s answer, one block summarizing the run. A question the summaries do not answer is asked of the file, by `trace_processor` or Perfetto's UI.
//! - The counting allocator, [`CountingAllocator`], adds the memory half of a report: a program that reads memory installs it as its `#[global_allocator]`, and it counts only while a recorder listens — `install`'s for the rest of the process, `trace`'s for its closure — or a measurement has asked with [`count_allocations`], so a slice carries what its thread took and what the process held while counting, and no run pays for a count nobody reads.
//!
//! Why profiling is configured in code and never from the environment, why every build carries it, why a third instrument measures magnitude beside time and bytes, why each thread counts what it takes and a program installs the allocator, why a program hands the recorder the counts it keeps, and why the library emits records rather than summaries are `README.md`'s decisions.

mod count;
use count::*;
pub use count::{CountingAllocator, allocated_bytes, allocation_count, count_allocations};

mod perfetto;

mod fold;
pub use fold::{Memory, OpenSpan, Profile, SampleSummary, Summary, Tallied, fold, fold_at};

mod trace;
use trace::*;
pub use trace::{Destination, Tally, install, trace};
