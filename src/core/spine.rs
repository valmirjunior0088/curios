//! The free-monoid peel shared by inversion (`invert`) and conversion
//! (`convert`). A primitive whose values are a literal run of generators over a
//! symbolic tail — a `Nat` count today, a `Bin` byte run or `Arr` element run
//! later — reduces two values by stripping their longest common literal head;
//! the residual tails go back to the caller's own recursion. `Bln`/`Int` are
//! the degenerate, zero-generator spines. The point of the seam: a new instance
//! is one `peel_prim` arm and nothing else — the drivers, the `Peel`
//! vocabulary, and the termination argument are shared.

use {
    super::{Nat, Prim, Term},
    num_traits::Zero,
    std::cmp::Ordering,
};

/// One step of peeling two free-monoid values. Each caller maps it into its own
/// vocabulary: `invert` to `Step::{Ok, Clash, Refuse}`, `convert` to a `bool`
/// with the residual enqueued.
pub enum Peel {
    /// Both sides consumed to the identity — definitionally equal.
    Equal,
    /// A common head peeled off; compare these residual tails next.
    Continue(Term, Term),
    /// Literal heads differ, or a positive head meets the identity — unequal.
    Clash,
    /// Undecidable by peeling (a symbolic-length head); the caller falls back.
    /// Unreachable for `Nat` — it is the seam the harder primitives plug into.
    Stuck,
}

/// Classify a reduced primitive pair. `None` means the pair is not a matched
/// spine-primitive, so the caller keeps its own handling; `Some` is the peel
/// outcome.
pub fn peel_prim(left: &Prim, right: &Prim) -> Option<Peel> {
    match (left, right) {
        (Prim::Nat(actual), Prim::Nat(target)) => Some(peel_nat(actual, target)),
        // Finite scalars are the degenerate (zero-generator) spines: no tail.
        (Prim::Bln(actual), Prim::Bln(target)) => Some(decide(actual == target)),
        (Prim::Int(actual), Prim::Int(target)) => Some(decide(actual == target)),
        _ => None,
    }
}

fn decide(equal: bool) -> Peel {
    match equal {
        true => Peel::Equal,
        false => Peel::Clash,
    }
}

/// `Nat` is the free monoid on one generator: `k + a ~ k' + t` peels the shared
/// successor spine and the leftover rides on the longer side — `2 ~ ?n + 1`
/// becomes `1 ~ ?n`. A leftover positive spine against zero is a definite clash.
/// The `is_zero` guards mirror the inverter's defence against a non-canonical
/// `Succ(0, _)` (which `Nat::new` normalisation never actually produces).
pub fn peel_nat(actual: &Nat, target: &Nat) -> Peel {
    let zero = || Term::prim(Prim::Nat(Nat::Zero));
    let succ = |spine, rest: &Term| Term::prim(Prim::Nat(Nat::Succ(spine, rest.clone())));

    match (actual, target) {
        (Nat::Zero, Nat::Zero) => Peel::Equal,
        (Nat::Zero, Nat::Succ(spine, rest)) => match spine.is_zero() {
            true => Peel::Continue(zero(), rest.clone()),
            false => Peel::Clash,
        },
        (Nat::Succ(spine, rest), Nat::Zero) => match spine.is_zero() {
            true => Peel::Continue(rest.clone(), zero()),
            false => Peel::Clash,
        },
        (Nat::Succ(ka, ra), Nat::Succ(kt, rt)) => match ka.cmp(kt) {
            Ordering::Equal => Peel::Continue(ra.clone(), rt.clone()),
            Ordering::Greater => Peel::Continue(succ(ka - kt, ra), rt.clone()),
            Ordering::Less => Peel::Continue(ra.clone(), succ(kt - ka, rt)),
        },
    }
}
