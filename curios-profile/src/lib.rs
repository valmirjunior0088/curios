//! Programmatic profiling for the Curios workspace.
//!
//! Every crate that wants a measurement point depends on this crate unconditionally — it is close to empty until its `enabled` feature is on — and declares its own `profile` feature as `profile = ["curios-profile/enabled", …]`. A measurement point is then one statement or one wrapped expression, and this crate is the only place in the workspace that names `tracing` at all:
//!
//! - [`profile!`] as the first statement of a function times the whole function, the successor of the retired `#[cfg_attr(feature = "profile", tracing::instrument(…))]` attribute — which could not survive re-export, because its expansion requires a crate literally named `tracing` in the invoking crate's extern prelude.
//! - [`profile_span!`] wraps one expression, for the per-step breakdown of a loop where timing the stepped function would aggregate every call.
//! - [`capture`] runs a closure under a thread-local subscriber and returns aggregate per-span timings, without touching the process-global subscriber. It is the only consumer; profiling is configured where it is used, in code — there is deliberately no environment-variable switch, because a measurement is already specified at its call sites and a second, out-of-band specification could only disagree with the first.
//! - [`sample!`] records a *magnitude* — how many, how wide, how deep — and the report returns that site's count, total, min, max, and mean.
//! - [`CountingAllocator`] adds the memory half of that report. A binary installs it as its `#[global_allocator]` under its own `profile` feature and every span gains what it retained and what it allocated; a binary that installs nothing still gets its timings, with the memory columns reading zero.
//!
//! The three answer different questions, and the third exists because the first two cannot settle the one that matters most about a slow pass: *whether the operation is wasteful, or whether it is being handed inputs it should never have seen*. A duration and a byte count are equally consistent with both, and optimizing the wrong one buys a constant factor against something structural. Reach for [`sample!`] on the input sizes — elements walked, entries rewritten, candidates considered — before optimizing a hot span, and let the distribution choose the fix.
//!
//! Both macros are token templates gated on the *invoking* crate's `profile` feature, so a disabled build strips the guard and pays nothing. Stage entrypoints and optimizer passes carry permanent spans; a span added to isolate one investigation is temporary instrumentation, removed once the question is answered, never left as a metrics API.

#[cfg(feature = "enabled")]
pub use tracing;

/// Time an entire function: expands to a span guard held to the end of the enclosing block. Write it as the function's first statement, named after the function, so the report reads as a call profile.
///
/// ```text
/// pub fn check_definition(…) -> Result<(), KernelError> {
///     curios_profile::profile!("check_definition");
///     …
/// }
/// ```
#[macro_export]
macro_rules! profile {
    ($name:literal) => {
        #[cfg(feature = "profile")]
        let __profile_guard = $crate::tracing::trace_span!($name).entered();
    };
}

/// Evaluate an expression inside a named span — the per-step sibling of [`profile!`], for breaking down one call site of a loop.
///
/// ```text
/// let changed = curios_profile::profile_span!("inline_known_calls", inline_known_calls(module))
///     | curios_profile::profile_span!("contify_calls", contify_calls(module));
/// ```
#[macro_export]
macro_rules! profile_span {
    ($name:literal, $expr:expr) => {{
        #[cfg(feature = "profile")]
        let __profile_span_guard = $crate::tracing::trace_span!($name).entered();
        $expr
    }};
}

/// Record one observation of a magnitude under `name` — the sibling of [`profile!`] for *how big* rather than *how long*.
///
/// The expression is evaluated only in a `profile` build, so a measurement that is itself expensive to compute costs a disabled build nothing.
///
/// ```text
/// curios_profile::sample!("universe::substitute_positions", positions.len());
/// ```
#[macro_export]
macro_rules! sample {
    ($name:literal, $value:expr) => {
        #[cfg(feature = "profile")]
        $crate::tracing::event!(
            name: $name,
            target: module_path!(),
            $crate::tracing::Level::TRACE,
            value = ($value) as u64,
        );
    };
}

#[cfg(feature = "enabled")]
mod collect;
#[cfg(feature = "enabled")]
pub use collect::*;
#[cfg(feature = "enabled")]
mod count;
#[cfg(feature = "enabled")]
pub use count::*;
