//! The native-stack reserve every recursive walk over user data runs inside.
//!
//! A compiler stage recurses over two very different depths. One is *authored* — how deeply someone nested a lambda, a module, a match — and the default thread stack tolerates it because a human wrote every level. The other is *data-shaped*: the scan-state chain a string literal lowers to, the UTF-8 derivation of a `Str`, a spine built by a loop. That depth is a function of the input, so no constant bounds it. A reduction budget does not *prevent* it either — this bracket grows rather than aborting, which is the point — but it is not blind to it: the reduction entry points on both sides charge a level the native frame it takes, measured, so what a runaway walk costs in stack is part of what the budget decides. See `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md`. The walks that are *not* reduction — the kernel's typing descent above all — are bounded by this bracket alone.
//!
//! So the recursion stays and the stack grows to fit it: [`recurse`] is that bracket, and the only place the figures are written. Why recursion rather than an explicit frame stack, and why the figures live once, are `README.md`'s decisions; how to tell a frame machine, which belongs behind this bracket as recursion, from a loop, which does not — a machine's elements mirror a function's locals — is `documentation/design/compilation/depth-is-bought-with-stack-not-with-hand-rolled-frames.md`'s.

/// Native-stack headroom to keep in reserve before growing.
///
/// The *trigger*, not a cap: when less than this remains, the next [`recurse`] allocates a fresh segment. It must exceed the deepest single frame any guarded walk can push, since the check happens between frames and not inside one. The deepest level measured is a guarded reduction level in a debug build, whose protocol and figure are `curios-core`'s `FRAME_UNITS`; this clears it about forty times over.
const RED_ZONE: usize = 4 * 1024 * 1024;

/// How much stack to take when the reserve runs low.
///
/// The *granularity*, not a cap either: nothing here bounds total depth, and a genuinely non-terminating walk is stopped by the reduction budget rather than by running out of stack. A larger segment means fewer allocations on a deep walk and more untouched address space on a shallow one; at this size a walk that never goes deep pays nothing, because the reserve is never taken.
const STACK_GROWTH: usize = 32 * 1024 * 1024;

/// Run `walk` with room to recurse over data-shaped depth.
///
/// Cheap enough to sit at the head of a hot recursive function: the common case is one comparison against the remaining stack. Guard the *entry point* of a recursive walk rather than each internal step — one check per level is the intent, and the segment is sized so that levels are rarely what triggers it.
///
/// That intent holds only where the thread has the reserve to begin with, which is what [`grown`] is for: on a thread smaller than `RED_ZONE` — every Rust test thread, at its default two mebibytes — the common case is not one comparison but a fresh segment mapped and unmapped around *every* outermost call, since the reserve can never be met on the thread itself, which costs re-erasing the standard library more than the walk does (`documentation/design/compilation/depth-is-bought-with-stack-not-with-hand-rolled-frames.md`).
pub fn recurse<T>(walk: impl FnOnce() -> T) -> T {
    stacker::maybe_grow(RED_ZONE, STACK_GROWTH, walk)
}

/// Run a whole walk on a segment of its own, taken unconditionally — the bracket for the entry point of a stage, beneath which every [`recurse`] sees the reserve it expects whatever thread the caller happened to be on.
///
/// One mapping per entry rather than one per guarded call, which is what a stage entered on a two-mebibyte test thread would otherwise pay (see [`recurse`]). A walk that genuinely outgrows the segment still grows further through `recurse`, so nothing here bounds depth; it only decides where the first segment is taken.
pub fn grown<T>(walk: impl FnOnce() -> T) -> T {
    stacker::grow(STACK_GROWTH, walk)
}
