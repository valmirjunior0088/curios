//! The macros a site instruments with, each a token template over `tracing`, whose callsite asks one relaxed load whether anything listens before it builds a span or evaluates a field. Three macros ask three questions — how long, how big, and why:
//!
//! - [`profile!`](macro@crate::profile) as the first statement of a function times the whole function. It takes fields after the name — a `group` field is what makes a per-item span report *which item* rather than an average over all of them — and an `=>` form times one expression rather than the enclosing function.
//! - [`sample!`](macro@crate::sample) records a *magnitude* — how many, how wide, how deep. It is for a number that varies; a site that would always record the same number is a call counter, and a span already counts its calls.
//! - [`note!`](macro@crate::note) states *why* a decision went the way it did, for a refusal that has neither a duration nor a size.
//!
//! Nothing here records: `curios-profile` is the recorder a program installs, and its README states why every build carries these sites and what each instrument answers. Why a span is a macro of this crate rather than `tracing::instrument` is `README.md`'s decision.

// The path the macros expand through, so a site that instruments names no `tracing` of its own.
pub use tracing;

/// Time a span: as a function's first statement it times the whole function, and with `=>` it times one expression and gives back its value.
///
/// Write the statement form named after the function, so a report reads as a call profile.
///
/// ```text
/// pub fn check_definition(…) -> Result<(), Error> {
///     curios_tracing::profile!("check_definition");
///     …
/// }
/// ```
///
/// The `=>` form is for the per-step breakdown of a loop, where timing the stepped function would aggregate every call into one row:
///
/// ```text
/// let changed = curios_tracing::profile!("inline_known_calls" => inline_known_calls(module))
///     | curios_tracing::profile!("contify_calls" => contify_calls(module));
/// ```
///
/// Fields follow the name after a comma, in `tracing`'s own syntax. A `group` field is the one a fold reads: it aggregates each distinct value as its own row, which is how a per-item span answers *which item* rather than *how long in total*.
///
/// ```text
/// curios_tracing::profile!("declaration", group = %item.describe());
/// ```
///
/// **One row per distinct group value**, so a group is for a bounded set — top-level declarations, stages, passes — and never for a span that runs per node. The value is also formatted at *every* span creation rather than once per distinct value, so a group on a span that runs per node would pay a formatting per node.
///
/// Any other field is written with the call it describes, an argument of its slice, which is how a span answers *which call* was slow and *what it was handed*: a query of the trace, in Perfetto's UI or by `trace_processor`, orders a span's slices by duration beside their arguments. That is an investigation's question, and a field formatted at every call is an investigation's price, so such a span is added to isolate one question and removed once it is answered.
///
/// ```text
/// curios_tracing::profile!("entails", assumed = assumed.len(), lower = ?lower, upper = ?upper);
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

/// Record one observation of a magnitude under `name` — the sibling of [`profile!`](macro@crate::profile) for *how big* rather than *how long*.
///
/// The expression is evaluated only while a recorder listens, so a measurement that is itself expensive to compute costs a run nobody records nothing. What it writes is the one reading a recorder holds a magnitude by: an event of this name carrying a `value` field.
///
/// ```text
/// curios_tracing::sample!("universe::substitute_positions", positions.len());
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
/// curios_tracing::note!(target: "curios_elab::solve", meta = id.0, "failed: occurs check");
/// ```
///
/// **For a path taken rarely.** A note costs a row every time it fires and says nothing a fold aggregates, so it belongs where a solver gives up, falls back or contradicts itself — never on a path taken per call. The question it answers is *why this one*, and a note that fires on everything answers it about nothing.
#[macro_export]
macro_rules! note {
    ($($arguments:tt)*) => {
        $crate::tracing::debug!($($arguments)*);
    };
}
