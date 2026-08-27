//! Deciding a `Nat` comparison, including under symbols.
//!
//! [`compare_nat`] cancels what the two sides share before it looks at what is left, so `x + a < x + b` decides on `a` and `b` rather than stalling on the whole spine. [`Comparison`] is the three-way verdict that carries an undecided answer back rather than guessing one.

use {
    super::*,
    crate::{Nat, ReduceError, Reducer, Subterm, Term},
    std::cmp::Ordering,
};

/// The structural outcome of comparing two `Nat`s. The whole comparison family (`eql`/`neq`/`lt`/`le`/`gt`/`ge`) reads this one result; each op differs only in how it maps the outcome to a `bool`. `Le`/`Ge` record a *non-strict* bound the operands force without pinning equality (e.g. `succ x ≥ 1`), letting `lt`/`ge` decide where `eql` still cannot; `Stuck` is undecidable, and the op's neutral term is rebuilt.
#[derive(Debug, PartialEq)]
pub(super) enum Comparison {
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Stuck,
}

pub(super) fn from_ordering(ordering: Ordering) -> Comparison {
    match ordering {
        Ordering::Less => Comparison::Lt,
        Ordering::Equal => Comparison::Eq,
        Ordering::Greater => Comparison::Gt,
    }
}

/// The `Nat` eliminator's structural comparison, specialized to the flat `Natural` successor spine: the floors stand in for peeling successors, so no recursion is needed and two literals decide in one `Natural` compare (the literal fold folds into the shared-inner shortcut). It decides ONLY where the answer is forced and is `Stuck` otherwise — a sound partial decision procedure, the shared body of the whole comparison family. (The `lt` partner of the `Unary` eliminator's successor peel; for `Bin`/`List` the same `Comparison` shape would recurse via `uncons`.)
///
/// Returns the operands with their shared successor floor peeled off, so an *undecided* comparison still rebuilds a normalized neutral: `cmp(x + m, y + m)` and `cmp(x, y)` reduce to the same term, which conversion needs (e.g. `Lt(a, succ b) ≡ Lt(succ a, succ(succ b))`).
pub(super) fn compare_nat(
    reducer: &mut impl Reducer,
    left: Term,
    right: Term,
) -> Result<(Comparison, Term, Term), ReduceError> {
    // The comparison cancels like terms across both sides, so a stuck product on either is distributed first, by name.
    let left = Nat::normalize(reducer, left)?;
    let right = Nat::normalize(reducer, right)?;
    // Cancel first, so everything below reads the residuals: the shared part decides nothing on its own, and removing it is what lets `cmp(x + a, x + b)` reach `cmp(a, b)` — and `cmp(a + b, b + a)` reach equality — instead of stalling on two inners that differ only by what they share.
    let (left, right) = Nat::cancel_common(&left, &right);

    let (sl, il) = Nat::decompose(&left);
    let (sr, ir) = Nat::decompose(&right);

    // Same inner ⇒ the floors alone decide: `cmp(x + sl, x + sr) = cmp(sl, sr)` (so `lt(pred, succ pred) = true`). Two literals — inner `0` on both sides — also land here: this is the O(1) literal fold. Otherwise, whichever side keeps successors past the shared floor is larger *iff* the other bottomed out at literal zero (`inner ≥ 0`); equal floors with one zero inner give a non-strict bound (`a ≤ b`/`a ≥ b`) the strict/`ge`/`le` reads still use; anything else is undecidable.
    // Compared up to universe instances for the same reason [`Nat::cancel_common`] matches summands that way: two occurrences of a polymorphic name carry independently fresh instances, and a level is not part of the answer to "are these the same number".
    let outcome = if project_erased_universes(&il) == project_erased_universes(&ir) {
        from_ordering(sl.cmp(&sr))
    } else {
        match sl.cmp(&sr) {
            Ordering::Greater if Nat::is_zero(&ir) => Comparison::Gt,
            Ordering::Less if Nat::is_zero(&il) => Comparison::Lt,
            Ordering::Equal if Nat::is_zero(&il) => Comparison::Le,
            Ordering::Equal if Nat::is_zero(&ir) => Comparison::Ge,
            _ => Comparison::Stuck,
        }
    };

    // A statically bounded operand decides against a literal one where the floors alone cannot: `bound(l) < r` forces `l < r` for every value `l` takes. This is what reduces `x % n < n`, whose left inner is a stuck `NatRem` the structural body has nothing to say about. See `nat_bound` for why each bound holds unconditionally.
    let outcome = match outcome {
        Comparison::Stuck if Nat::is_zero(&ir) => match nat_bound(&il).map(|bound| bound + &sl) {
            Some(bound) if bound < sr => Comparison::Lt,
            _ => Comparison::Stuck,
        },
        Comparison::Stuck if Nat::is_zero(&il) => match nat_bound(&ir).map(|bound| bound + &sr) {
            Some(bound) if bound < sl => Comparison::Gt,
            _ => Comparison::Stuck,
        },
        decided => decided,
    };

    Ok((outcome, left, right))
}

/// Reduce a `Nat` comparison through the shared structural body [`compare_nat`]. `read` projects the outcome to this op's boolean (or `None` when the operands do not decide it), in which case the neutral term is rebuilt from the peeled operands so undecided comparisons land in a normal form.
pub(super) fn reduce_nat_compare(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    read: impl FnOnce(Comparison) -> Option<bool>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let (outcome, left, right) = compare_nat(reducer, left.clone(), right.clone())?;

    Ok(match read(outcome) {
        Some(value) => Subterm::Intrinsic(Intrinsic::Bool(value)),
        None => Subterm::Intrinsic(rebuild(left, right)),
    })
}
