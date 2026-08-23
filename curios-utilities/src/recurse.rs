//! The native-stack reserve every recursive walk over user data runs inside.
//!
//! A compiler stage recurses over two very different depths. One is *authored* — how deeply someone nested a lambda, a module, a match — and the default thread stack tolerates it because a human wrote every level. The other is *data-shaped*: the scan-state chain a string literal lowers to, the UTF-8 derivation of a `Str`, a spine built by a loop. That depth is a function of the input, so no constant bounds it. A reduction budget does not *prevent* it either — this bracket grows rather than aborting, which is the point — but it is no longer blind to it: the reduction entry points on both sides charge a level the native frame it takes, measured, so what a runaway walk costs in stack is now part of what the budget decides. See `documentation/design/toolchain/a-reduction-step-costs-what-it-builds.md`. The walks that are *not* reduction — the kernel's typing descent above all — are still bounded by this bracket alone.
//!
//! The alternative is to defunctionalize: turn the recursion into an explicit frame stack so native depth stays O(1) per link. That works, and it costs the thing a checker can least afford to lose. Reduction and conversion decide which programs typecheck, and this workspace deliberately implements each of them *twice* — once in `curios-elab` and once in `curios-cert` — so that a bug in one is caught by disagreement with the other. That bet only pays if a person can read the two side by side, and two hand-rolled state machines are far harder to diff than two recursive strategies. Defunctionalization also re-implements, by hand, what the call stack already did: scope entry and exit, sibling ordering, unwinding on the error path. Each of those becomes a comment arguing that the machine still does what recursion did for free.
//!
//! So the recursion stays and the stack grows to fit it. [`recurse`] is that bracket, and it is deliberately the *only* place the figures are written: three call sites once carried their own pair of constants, which is three chances for them to drift apart and no way to tell which one was right.
//!
//! # Telling a machine from a loop
//!
//! Twelve walks were restored to recursion behind this bracket, and classifying them correctly took two corrections worth keeping. The question is never whether a `Vec` is iterated — a machine can iterate a flat input, and a loop can accumulate — but what the accumulator *holds*.
//!
//! A **loop** accumulates results, and folding them afterwards carries no effects: the elaborator's `let` gathers `(label, type, body)` triples and folds them into a term. A **machine** accumulates frames, and shows three tells — an element mirroring a function's locals, a reverse phase carrying effects (scope exit, unwinding), and a comment arguing that ordering is preserved. The lowerer's `PendingLet` held six fields that were `build_let`'s local variables verbatim, twelve lines away in the same file.
//!
//! The element mirroring locals is the decisive one. A reverse-with-effects phase alone is not, because an **undo record** has one and is legitimate — and an undo record is right exactly when it stores what was *destroyed* (a refinement some scope overwrote, a node's distance before relaxation moved it) rather than what can be *derived* from state the walk never mutated. `documentation/design/toolchain/depth-is-bought-with-stack-not-with-hand-rolled-frames.md` carries the whole argument.

/// Native-stack headroom to keep in reserve before growing.
///
/// The *trigger*, not a cap: when less than this remains, the next [`recurse`] allocates a fresh segment. It must exceed the deepest single frame any guarded walk can push, since the check happens between frames and not inside one — measured at 21.5KiB per level for the kernel's typing walk in a debug build, so this clears the worst known frame by two orders of magnitude.
const RED_ZONE: usize = 4 * 1024 * 1024;

/// How much stack to take when the reserve runs low.
///
/// The *granularity*, not a cap either: nothing here bounds total depth, and a genuinely non-terminating walk is stopped by the reduction budget rather than by running out of stack. A larger segment means fewer allocations on a deep walk and more untouched address space on a shallow one; at this size a walk that never goes deep pays nothing, because the reserve is never taken.
const STACK_GROWTH: usize = 32 * 1024 * 1024;

/// Run `walk` with room to recurse over data-shaped depth.
///
/// Cheap enough to sit at the head of a hot recursive function: the common case is one comparison against the remaining stack. Guard the *entry point* of a recursive walk rather than each internal step — one check per level is the intent, and the segment is sized so that levels are rarely what triggers it.
///
/// That intent holds only where the thread has the reserve to begin with, which is what [`grown`] is for: on a thread smaller than `RED_ZONE` — every Rust test thread, at its default two mebibytes — the common case is not one comparison but a fresh segment mapped and unmapped around *every* outermost call, since the reserve can never be met on the thread itself. Re-erasing the whole standard library measured 2.5 s that way against 629 ms on a thread that had the reserve, with not one deep walk between them.
pub fn recurse<T>(walk: impl FnOnce() -> T) -> T {
    stacker::maybe_grow(RED_ZONE, STACK_GROWTH, walk)
}

/// Run a whole walk on a segment of its own, taken unconditionally — the bracket for the entry point of a stage, beneath which every [`recurse`] sees the reserve it expects whatever thread the caller happened to be on.
///
/// One mapping per entry rather than one per guarded call: a stage entered on a two-mebibyte test thread paid the latter, and the figure that found it is beside [`recurse`]. A walk that genuinely outgrows the segment still grows further through `recurse`, so nothing here bounds depth; it only decides where the first segment is taken.
pub fn grown<T>(walk: impl FnOnce() -> T) -> T {
    stacker::grow(STACK_GROWTH, walk)
}
