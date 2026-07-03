//! The suffix-view algebra, shared by the two passes that re-base aggregate
//! access through a slice onto the underlying buffer.
//!
//! A `Bin`/`Arr` slice is semantically a copy, but it is only ever *read* through
//! three primitives, and each reads through to the base in `O(1)`:
//!
//! ```text
//! len(  slice(b, s, e)    )   ⟶   e − s
//! get(  slice(b, s, e), j )   ⟶   get(b, s + j)
//! slice(slice(b, s, e), p, q) ⟶   slice(b, s + p, s + q)
//! ```
//!
//! These are the canonical re-base laws. A consumer that is *not* one of the three
//! — a closure call, a host write, `concat`/`append`/`eql`, a bare return
//! — makes the slice *escape*, so the copy stays. Soundness rides on aggregate
//! immutability: `b[s + j] == slice(b, s, e)[j]` for every in-bounds access.
//!
//! Two passes encode this question on *different* IRs, so this module shares only
//! the small invariant core — the [`Carrier`] discriminant and the [`SuffixRead`]
//! classification — while each pass keeps its own recognizer, escape policy, and
//! rewrite mechanics (a decided "minimal share": no index-algebra trait, because the
//! `cont` side mints fresh SSA bindings for `s + j` while the `ersd` side builds
//! inline `Term` arithmetic, and that divergence is heavier than the ~6-line law):
//!
//! - `worker_wrapper`'s `SliceCursor` (on `ersd` `Term`s,
//!   indices inline `nat_add`/`nat_sub` trees) applies the laws *across recursion*
//!   with a whole-function, all-or-nothing escape gate;
//! - `slice_forwarding` (on `cont` SSA, indices `ValueName`s)
//!   applies them to *local* consumers, an unmatched one simply keeping the slice
//!   for the dead-code sweep.

/// The buffer carrier a suffix view threads over: `Bin` or `Arr`. Selects which
/// `len`/`get`/`slice` primitives are recognised and emitted; a consumer only
/// forwards through a slice of its own carrier, though well-typed code never mixes
/// them.
#[derive(Clone, Copy, PartialEq)]
pub enum Carrier {
    Bin,
    Arr,
}

/// A read of a (possibly-sliced) carrier buffer, generic over the index
/// representation `I` each IR supplies (`ersd` a `Term`, `cont` a `ValueName`):
/// the three reads the suffix-view laws re-base, and nothing else.
pub enum SuffixRead<I> {
    /// `len(buf)` — re-bases to `end − start`.
    Len,
    /// `get(buf, index)` — re-bases the index to `start + index`.
    Get(I),
    /// `slice(buf, start, end)` — re-bases the window to `[start + p, start + q)`.
    Slice(I, I),
}
