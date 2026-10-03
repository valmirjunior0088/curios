//! Deciding a `Nat` comparison, including under symbols.
//!
//! [`compare_nat`] cancels what the two sides share before it looks at what is left, so `x + a < x + b` decides on `a` and `b` rather than stalling on the whole spine. `curios-algebra`'s `Comparison` is the verdict — which of the three orderings the operands may still take, so an undecided answer is carried back rather than guessed — and every fact that narrows it is the algebra's: the floors, a bound against a literal, a dominator, divisibility. What is decided here is what to observe and in what order: which terms to normalize, which side is bare, which operand bounds which.

use {
    super::{nat_bound, nat_dominators},
    crate::{Intrinsic, Nat, ReduceError, Reducer, Subterm, Term, project_erased_universes},
    curios_algebra::{Comparison, Side, apart_modulo},
    curios_num::Natural,
};

/// One side of a comparison once cancellation has run: its floor, the symbolic part above it, and the whole side.
struct Operand {
    floor: Natural,
    inner: Term,
    whole: Term,
}

impl Operand {
    fn of(whole: Term) -> Self {
        let (floor, inner) = Nat::decompose(&whole);
        Operand {
            floor,
            inner,
            whole,
        }
    }

    /// This side as the algebra reads it for the floor comparison.
    fn side(&self) -> Side<'_> {
        Side {
            floor: &self.floor,
            symbolic: !Nat::is_zero(&self.inner),
        }
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
    let (left, right) = (Operand::of(left), Operand::of(right));

    // The floors decide where one symbolic part stands on both sides, or where a side is bare. Two inners are one part up to universe instances, for the reason [`Nat::cancel_common`] matches summands that way: two occurrences of a polymorphic name carry independently fresh instances, and a level is not part of the answer to "are these the same number".
    let same = project_erased_universes(&left.inner) == project_erased_universes(&right.inner);
    let outcome = Comparison::of_floors(left.side(), right.side(), same);

    // A statically bounded side decides against a bare one where the floors alone cannot: `bound(l) < r` forces `l < r` for every value `l` takes, and `bound(l) = r` forces `l <= r`. This is what reduces `x % n < n`, whose left inner is a stuck `NatRem` the structural body has nothing to say about. See `nat_bound` for why each bound holds unconditionally.
    let outcome = match outcome {
        Comparison::Stuck if Nat::is_zero(&right.inner) => match nat_bound(&left.inner) {
            Some(bound) => Comparison::below(&(bound + &left.floor), &right.floor),
            None => Comparison::Stuck,
        },
        Comparison::Stuck if Nat::is_zero(&left.inner) => match nat_bound(&right.inner) {
            Some(bound) => Comparison::below(&(bound + &right.floor), &left.floor).mirrored(),
            None => Comparison::Stuck,
        },
        decided => decided,
    };

    // A symbolic bound decides by the same criterion, through the operand the value never exceeds: `x - y` is at most `x`, so `x - y <= x + z` is decided by comparing `x` in its place, and `x % (y + 1)` is below `y + 1` outright. The dominator is compared one strict subterm down, so the recursion ends. See `nat_dominators` for why each pair holds unconditionally.
    let outcome = match outcome {
        Comparison::Stuck => dominated(reducer, &left, &right)?,
        decided => decided,
    };

    // Divisibility decides what no ordering does: floors apart modulo the gcd of every coefficient make the sides unequal at every value, which is `x * 2 + 1 == y * 2` reducing to `false`. Read last, and only where equality is still open, so a verdict the stages above reached costs nothing more.
    let outcome = match outcome {
        Comparison::Stuck | Comparison::Le | Comparison::Ge => {
            let coefficients = Nat::summands(&left.inner)
                .into_iter()
                .chain(Nat::summands(&right.inner))
                .map(|summand| Nat::monomial(&summand).0);
            match apart_modulo((&left.floor, &right.floor), coefficients) {
                true => outcome.unequal(),
                false => outcome,
            }
        }
        decided => decided,
    };

    Ok((outcome, left.whole, right.whole))
}

/// The verdict a dominator forces, tried on the left inner and then the right, or `Stuck` when no listed operand compares: the dominator's verdict against the other side, read through `Comparison::through_dominator`.
fn dominated(
    reducer: &mut impl Reducer,
    left: &Operand,
    right: &Operand,
) -> Result<Comparison, ReduceError> {
    for (bound, strict) in nat_dominators(&left.inner) {
        let (verdict, _, _) = compare_nat(
            reducer,
            Nat::rebuild(left.floor.clone(), bound),
            right.whole.clone(),
        )?;
        if let Some(verdict) = verdict.through_dominator(strict) {
            return Ok(verdict);
        }
    }
    for (bound, strict) in nat_dominators(&right.inner) {
        let (verdict, _, _) = compare_nat(
            reducer,
            left.whole.clone(),
            Nat::rebuild(right.floor.clone(), bound),
        )?;
        if let Some(verdict) = verdict.mirrored().through_dominator(strict) {
            return Ok(verdict.mirrored());
        }
    }
    Ok(Comparison::Stuck)
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
