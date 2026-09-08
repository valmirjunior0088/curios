//! Programmatic profiling for the Curios workspace.
//!
//! Every crate that wants a measurement point depends on this crate unconditionally — it is close to empty until its `enabled` feature is on — and declares its own `profile` feature as `profile = ["curios-profile/enabled", …]`. A measurement point is then one statement or one wrapped expression, and this crate is the only place in the workspace that names `tracing` at all. Three macros ask three questions — how long, how big, and why:
//!
//! - [`profile!`] as the first statement of a function times the whole function, the successor of the retired `#[cfg_attr(feature = "profile", tracing::instrument(…))]` attribute — which could not survive re-export, because its expansion requires a crate literally named `tracing` in the invoking crate's extern prelude. It takes fields after the name — a `group` field is what makes a per-item span report *which item* rather than an average over all of them — and an `=>` form times one expression rather than the enclosing function.
//! - [`note!`] states *why* a decision went the way it did, for a refusal that has neither a duration nor a size. It carries the `profile` gate itself, and it is what keeps `tracing` named in this crate alone.
//! - [`sample!`] records a *magnitude* — how many, how wide, how deep. It is for a number that varies; a site that would always record the same number is a call counter, and a span already counts its calls.
//! - `trace` and `install` — present under `enabled`, so they carry no link here — are the two scopings of the subscriber that writes one row per span and event as it happens. `trace` runs one closure under it and leaves the process-global default alone; `install` *is* that default, for a binary whose scope is the whole invocation because it has no closure to wrap. Both take the destination from their caller: this crate names no path, so nothing here decides where a stream lands. They are the only producers, and profiling is configured where it is used, in code.
//! - `fold` — beside it — recomputes timings, allocation figures and distributions from those rows. Aggregation is a consumer of the stream rather than what a capture returns, so a run that never terminates still leaves everything it did on disk, and a question the summaries do not answer is asked of the file.
//! - `capture_host_records` — also under `enabled` — is the same scoping for what a *host library* says through the `log` facade rather than through spans: the engine announces each collection at trace level, and the bridge raises `log` only for the duration of one closure, so everything outside a capture pays one relaxed atomic load per suppressed record at most.
//! - `CountingAllocator`, under `enabled` beside it and unlinked for the same reason, adds the memory half of a report. A binary installs it as its `#[global_allocator]` under its own `profile` feature and every boundary row carries what the process held and had taken; a binary that installs nothing still gets its timings, with the memory columns reading zero.
//!
//! Why the dependency is named here and nowhere else, why profiling is configured in code and never from the environment, why a third instrument measures magnitude beside time and bytes, why the allocator counts process-wide, and why the library emits records rather than summaries are `README.md`'s decisions.
//!
//! Each macro above is a token template gated on the *invoking* crate's `profile` feature, so a disabled build strips the guard and pays nothing. Stage entrypoints and optimizer passes carry permanent spans; a span added to isolate one investigation is temporary instrumentation, removed once the question is answered, never left as a metrics API.

#[cfg(feature = "enabled")]
pub use tracing;

/// Time a span: as a function's first statement it times the whole function, and with `=>` it times one expression and gives back its value.
///
/// Write the statement form named after the function, so a report reads as a call profile.
///
/// ```text
/// pub fn check_definition(…) -> Result<(), KernelError> {
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
/// **One row per distinct group value**, so a group is for a bounded set — top-level declarations, stages, passes — and never for a span that runs per node. The value is also formatted at *every* span creation rather than once per distinct value, which is the same depth discipline [`sample!`] carries and for the same reason.
#[macro_export]
macro_rules! profile {
    ($name:literal => $expr:expr) => {{
        #[cfg(feature = "profile")]
        let __profile_guard = $crate::tracing::trace_span!($name).entered();
        $expr
    }};
    ($name:literal) => {
        #[cfg(feature = "profile")]
        let __profile_guard = $crate::tracing::trace_span!($name).entered();
    };
    ($name:literal, $($fields:tt)*) => {
        #[cfg(feature = "profile")]
        let __profile_guard = $crate::tracing::trace_span!($name, $($fields)*).entered();
    };
}

/// Record one observation of a magnitude under `name` — the sibling of [`profile!`] for *how big* rather than *how long*.
///
/// The expression is evaluated only in a `profile` build, so a measurement that is itself expensive to compute costs a disabled build nothing.
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
        #[cfg(feature = "profile")]
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
/// Takes `tracing`'s own event syntax, and carries the `profile` gate itself, so a call site states the note and nothing else:
///
/// ```text
/// curios_profile::note!(target: "curios_elab::solve", meta = id.0, "failed: occurs check");
/// ```
///
/// **For a path taken rarely.** A note costs a row every time it fires and says nothing a fold aggregates, so it belongs where a solver gives up, falls back or contradicts itself — never on a path taken per call. The question it answers is *why this one*, and a note that fires on everything answers it about nothing.
#[macro_export]
macro_rules! note {
    ($($arguments:tt)*) => {
        #[cfg(feature = "profile")]
        $crate::tracing::debug!($($arguments)*);
    };
}

#[cfg(feature = "enabled")]
mod count;
#[cfg(feature = "enabled")]
pub use count::*;
#[cfg(feature = "enabled")]
mod fold;
#[cfg(feature = "enabled")]
pub use fold::*;
#[cfg(feature = "enabled")]
mod host;
#[cfg(feature = "enabled")]
pub use host::*;
#[cfg(feature = "enabled")]
mod trace;
#[cfg(feature = "enabled")]
pub use trace::*;
