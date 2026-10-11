//! Deciding a `Nat` comparison, including under symbols.
//!
//! [`compare_nat`] cancels what the two sides share before it looks at what is left, so `x + a < x + b` decides on `a` and `b` rather than stalling on the whole spine. `curios-algebra`'s `Comparison` is the verdict — which of the three orderings the operands may still take, so an undecided answer is carried back rather than guessed — and every fact that narrows it is the algebra's: the floors, a bound against a literal, a dominator, divisibility. What is decided here is what to observe and in what order: which terms to normalize, which side is bare, which operand bounds which.

use {
    super::{compare_int_among, lattice_verdict, nat_bound, nat_dominators, nat_least, sum_bound},
    crate::{Intrinsic, Nat, ReduceError, Reducer, Subterm, Term, same_atom},
    curios_algebra::{Carrier, Comparison, Side, apart_modulo},
    curios_num::Natural,
    std::collections::HashMap,
};

/// The verdicts one comparison has reached of the pairs of terms it was led to.
///
/// **A pair is compared once.** A side is compared through each operand it never exceeds and each leaf of a `min` or a `max`, and so is the other side, so two sides nested `n` deep lead to one pair of their subterms along every interleaving of the two descents: as many paths as a binomial coefficient counts, where the pairs are the product of the two sides' sizes. A verdict is a function of the two terms, so the first one reached stands for every path.
///
/// It lives as long as the comparison that made it: an arm's equations and a reducer's state are one thing for the whole of a comparison, and may be another for the next.
#[derive(Default)]
pub(super) struct Asked {
    verdicts: HashMap<(Term, Term), Comparison>,
}

impl Asked {
    /// What `carrier`'s comparison decides of `left` against `right`, compared where this is the first time the pair is asked about.
    pub(super) fn verdict(
        &mut self,
        reducer: &mut impl Reducer,
        carrier: Carrier,
        left: Term,
        right: Term,
    ) -> Result<Comparison, ReduceError> {
        let pair = (left, right);
        if let Some(verdict) = self.verdicts.get(&pair) {
            return Ok(*verdict);
        }
        let (left, right) = pair.clone();
        let verdict = match carrier {
            Carrier::Natural => compare_nat_among(reducer, self, left, right)?.0,
            _ => compare_int_among(reducer, self, left, right)?.0,
        };
        self.verdicts.insert(pair, verdict);
        Ok(verdict)
    }
}

/// One side of a comparison once cancellation has run: its floor, the symbolic part above it, what the side is at least, and the whole side.
struct Operand {
    floor: Natural,
    inner: Term,
    /// The floor over what the symbolic part is at least ([`nat_least`]): the floor itself under a part nothing is known of, and past it under one that holds a power.
    least: Natural,
    whole: Term,
}

impl Operand {
    fn of(whole: Term) -> Self {
        let (floor, inner) = Nat::decompose(&whole);
        let least = floor.clone() + &nat_least(&inner);
        Operand {
            floor,
            inner,
            least,
            whole,
        }
    }

    /// This side as the algebra reads it for the floor comparison: at least `least` where it is symbolic, and exactly it where it is bare, the two being one number there. Two sides over one symbolic part are each raised by what that part is at least, so their order is their floors'.
    fn side(&self) -> Side<'_> {
        Side {
            floor: &self.least,
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
    compare_nat_among(reducer, &mut Asked::default(), left, right)
}

/// [`compare_nat`] inside a comparison that has asked about other pairs: what it is led to in turn is asked of `asked`.
pub(super) fn compare_nat_among(
    reducer: &mut impl Reducer,
    asked: &mut Asked,
    left: Term,
    right: Term,
) -> Result<(Comparison, Term, Term), ReduceError> {
    // The comparison cancels like terms across both sides, so a stuck product on either is distributed first, by name.
    let left = Nat::normalize(reducer, left)?;
    let right = Nat::normalize(reducer, right)?;
    // Cancel first, so everything below reads the residuals: the shared part decides nothing on its own, and removing it is what lets `cmp(x + a, x + b)` reach `cmp(a, b)` — and `cmp(a + b, b + a)` reach equality — instead of stalling on two inners that differ only by what they share.
    let cancelled = Nat::cancellation(&left, &right);
    reducer.spend(sum_bound(Nat::residuals(&cancelled)))?;
    let (left, right) = Nat::rebuild_cancelled(cancelled, &left, &right);
    let (left, right) = (Operand::of(left), Operand::of(right));

    // The floors decide where one symbolic part stands on both sides, or where a side is bare. Two inners are one part up to universe instances, for the reason [`Nat::cancel_common`] matches summands that way: two occurrences of a polymorphic name carry independently fresh instances, and a level is not part of the answer to "are these the same number".
    let same = same_atom(Carrier::Natural, &left.inner, &right.inner);
    let outcome = Comparison::of_floors(left.side(), right.side(), same);

    // A statically bounded side decides against the other side's floor where the floors alone cannot: `bound(l)` below `r`'s floor forces `l < r` for every value either takes, whatever stands above that floor, and `bound(l)` meeting it forces `l <= r`. This is what reduces `x % n < n`, whose left inner is a stuck `NatRem` the structural body has nothing to say about. See `nat_bound` for why each bound holds unconditionally.
    //
    // Read against the floor and not only against a bare literal, because the order this decides has to be transitive: `x % 8 <= 7` and `7 <= y + 7` are each decided, and an operation that keeps the least of its operands gives one answer for both associations only where `x % 8 <= y + 7` is decided too.
    let outcome = match outcome {
        Comparison::Stuck => match bounded(&left, &right) {
            Comparison::Stuck => bounded(&right, &left).mirrored(),
            decided => decided,
        },
        decided => decided,
    };

    // A symbolic bound decides by the same criterion, through the operand the value never exceeds: `x - y` is at most `x`, so `x - y <= x + z` is decided by comparing `x` in its place, and `x % (y + 1)` is below `y + 1` outright. The dominator is compared one strict subterm down, so the recursion ends. See `nat_dominators` for why each pair holds unconditionally.
    let outcome = match outcome {
        Comparison::Stuck => dominated(reducer, asked, &left, &right)?,
        decided => decided,
    };

    // A `min` or a `max` on either side is read through the lattice: at most each of a `min`'s leaves and at least what is at most all of them, and a `max` the mirror. Each verdict it asks is of a leaf, one strict subterm down.
    let outcome = match outcome {
        Comparison::Stuck => {
            lattice_verdict(reducer, asked, Carrier::Natural, &left.whole, &right.whole)?
        }
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

/// The verdict `bounded`'s literal bound forces against what `other` is at least, or `Stuck` where its inner has no bound or the bound passes it: `bounded` is at most its floor and its inner's bound, and `other` at least its own floor and what its inner is at least.
fn bounded(bounded: &Operand, other: &Operand) -> Comparison {
    match nat_bound(&bounded.inner) {
        Some(bound) => Comparison::below(&(bound + &bounded.floor), &other.least),
        None => Comparison::Stuck,
    }
}

/// The verdict a dominator forces, tried on the left inner and then the right, or `Stuck` when no listed operand compares: the dominator's verdict against the other side, read through `Comparison::through_dominator`, and a strict dominator's against the other side's successor, read through `Comparison::through_strict_dominator`.
fn dominated(
    reducer: &mut impl Reducer,
    asked: &mut Asked,
    left: &Operand,
    right: &Operand,
) -> Result<Comparison, ReduceError> {
    let successor = |operand: &Operand| {
        Nat::rebuild(
            operand.floor.clone() + Natural::one(),
            operand.inner.clone(),
        )
    };
    for (bound, strict) in nat_dominators(&left.inner) {
        let dominator = Nat::rebuild(left.floor.clone(), bound);
        let verdict = asked.verdict(
            reducer,
            Carrier::Natural,
            dominator.clone(),
            right.whole.clone(),
        )?;
        if let Some(verdict) = verdict.through_dominator(strict) {
            return Ok(verdict);
        }
        if strict {
            let verdict = asked.verdict(reducer, Carrier::Natural, dominator, successor(right))?;
            if let Some(verdict) = verdict.through_strict_dominator() {
                return Ok(verdict);
            }
        }
    }
    for (bound, strict) in nat_dominators(&right.inner) {
        let dominator = Nat::rebuild(right.floor.clone(), bound);
        let verdict = asked.verdict(
            reducer,
            Carrier::Natural,
            left.whole.clone(),
            dominator.clone(),
        )?;
        if let Some(verdict) = verdict.mirrored().through_dominator(strict) {
            return Ok(verdict.mirrored());
        }
        if strict {
            let verdict = asked.verdict(reducer, Carrier::Natural, successor(left), dominator)?;
            if let Some(verdict) = verdict.mirrored().through_strict_dominator() {
                return Ok(verdict.mirrored());
            }
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
