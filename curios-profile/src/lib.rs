//! Programmatic profiling for the Curios workspace.
//!
//! Every crate that wants a measurement point depends on this crate, and every build carries what it instruments: nothing records unless a caller installs a recorder, and a measurement point no recorder listens to costs one relaxed load. A measurement point is one statement or one wrapped expression, and this crate is the only place in the workspace that names `tracing` at all. Three macros ask three questions — how long, how big, and why:
//!
//! - [`profile!`] as the first statement of a function times the whole function. It takes fields after the name — a `group` field is what makes a per-item span report *which item* rather than an average over all of them — and an `=>` form times one expression rather than the enclosing function.
//! - [`note!`] states *why* a decision went the way it did, for a refusal that has neither a duration nor a size. It is what keeps `tracing` named in this crate alone.
//! - [`sample!`] records a *magnitude* — how many, how wide, how deep. It is for a number that varies; a site that would always record the same number is a call counter, and a span already counts its calls.
//! - [`trace()`] and [`install`] are the two scopings of the subscriber that writes one row per span and event as it happens. `trace` runs one closure under it and leaves the process-global default alone; `install` *is* that default, for a binary whose scope is the whole invocation because it has no closure to wrap. Both take the destination from their caller, and they are the only producers.
//! - [`fold()`] recomputes timings, allocation figures and distributions from those rows. A question the summaries do not answer is asked of the file.
//! - The counting allocator adds the memory half of a report: this crate installs it as the `#[global_allocator]` of every binary it is linked into, and it counts only while a recorder listens — `install`'s for the rest of the process, `trace`'s for its closure — or a measurement has asked with [`count_allocations`], so a boundary row carries what the process took and returned while counting, no binary has to opt in, and none pays for a count nobody reads.
//! - [`Instructions`] counts what a measurement's thread and the threads it starts retire, which neither a look nor a byte sees of a walk that allocates nothing or of code that is not Rust.
//!
//! Why the dependency is named here and nowhere else, why profiling is configured in code and never from the environment, why every build carries it, why a third instrument measures magnitude beside time and bytes and a fourth counts instructions, why the allocator counts process-wide and is installed by this crate, why the library emits records rather than summaries, and why a span is this crate's macro rather than `tracing::instrument` are `README.md`'s decisions.
//!
//! Each macro above is a token template over `tracing`, whose callsite asks one relaxed load whether anything listens before it builds a span or evaluates a field.

pub use tracing;

/// Time a span: as a function's first statement it times the whole function, and with `=>` it times one expression and gives back its value.
///
/// Write the statement form named after the function, so a report reads as a call profile.
///
/// ```text
/// pub fn check_definition(…) -> Result<(), Error> {
///     curios_profile::profile!("check_definition");
///     …
/// }
/// ```
///
/// The `=>` form is for the per-step breakdown of a loop, where timing the stepped function would aggregate every call into one row:
///
/// ```text
/// let changed = curios_profile::profile!("inline_known_calls" => inline_known_calls(module))
///     | curios_profile::profile!("contify_calls" => contify_calls(module));
/// ```
///
/// Fields follow the name after a comma, in `tracing`'s own syntax. A `group` field is the one a fold reads: it aggregates each distinct value as its own row, which is how a per-item span answers *which item* rather than *how long in total*.
///
/// ```text
/// curios_profile::profile!("declaration", group = %item.describe());
/// ```
///
/// **One row per distinct group value**, so a group is for a bounded set — top-level declarations, stages, passes — and never for a span that runs per node. The value is also formatted at *every* span creation rather than once per distinct value, so a group on a span that runs per node would pay a formatting per node.
///
/// Any other field is kept with the call it describes: a fold keeps a row's costliest calls with the fields each was created with (`Summary::slowest`), which is how a span answers *which call* was slow and *what it was handed*. That is an investigation's question, and a field formatted at every call is an investigation's price, so such a span is added to isolate one question and removed once it is answered.
///
/// ```text
/// curios_profile::profile!("entails", assumed = assumed.len(), lower = ?lower, upper = ?upper);
/// ```
#[macro_export]
macro_rules! profile {
    ($name:literal => $expr:expr) => {{
        let __profile_guard = $crate::tracing::trace_span!($name).entered();
        $expr
    }};
    ($name:literal) => {
        let __profile_guard = $crate::tracing::trace_span!($name).entered();
    };
    ($name:literal, $($fields:tt)*) => {
        let __profile_guard = $crate::tracing::trace_span!($name, $($fields)*).entered();
    };
}

/// Record one observation of a magnitude under `name` — the sibling of [`profile!`] for *how big* rather than *how long*.
///
/// The expression is evaluated only while a recorder listens, so a measurement that is itself expensive to compute costs a run nobody records nothing.
///
/// ```text
/// curios_profile::sample!("universe::substitute_positions", positions.len());
/// ```
///
/// The name is any const `&'static str`, so a wrapper macro may derive one — `concat!($name, "::nodes")` — and still land on one row per distinct name.
///
/// **A magnitude, never a tally.** A site whose value is always the same number is a call counter wearing this instrument's clothes: it costs one row per call to say what the enclosing span's own row already says, and says nothing about the inputs. Reach for a span on the operation instead, which counts its calls and times them.
#[macro_export]
macro_rules! sample {
    ($name:expr, $value:expr) => {
        $crate::tracing::event!(
            name: $name,
            target: module_path!(),
            $crate::tracing::Level::TRACE,
            value = ($value) as u64,
        );
    };
}

/// Note why a decision went the way it did — the instrument for a refusal, which has neither a duration nor a size.
///
/// Takes `tracing`'s own event syntax, so a call site states the note and nothing else, and what it formats is formatted only while a recorder listens:
///
/// ```text
/// curios_profile::note!(target: "curios_elab::solve", meta = id.0, "failed: occurs check");
/// ```
///
/// **For a path taken rarely.** A note costs a row every time it fires and says nothing a fold aggregates, so it belongs where a solver gives up, falls back or contradicts itself — never on a path taken per call. The question it answers is *why this one*, and a note that fires on everything answers it about nothing.
#[macro_export]
macro_rules! note {
    ($($arguments:tt)*) => {
        $crate::tracing::debug!($($arguments)*);
    };
}

mod count;
use count::*;
pub use count::{allocated_bytes, allocation_count, count_allocations};

mod instructions;
pub use instructions::Instructions;

mod fold;
pub use fold::{OpenSpan, Report, SampleSummary, SpanInstance, Summary, Within, fold, fold_at};

mod trace;
use trace::*;
pub use trace::{Destination, install, trace};
