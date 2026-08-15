use {
    super::{Intrinsic, Subterm, Term},
    curios_num::Natural,
};

/// A type-level natural in successor-floor form: `Zero`, or `Succ(floor, inner)` — a [`Natural`] count of successors stacked on a tail term `inner`, so a closed literal is one node and `x + 3` is `Succ(3, x)`, never a unary chain. Unbounded — the type level pretends ℕ, like `Integer`'s ℤ; the runtime's 31-bit range is enforced only where a literal must materialize (`erase`'s narrowing) and by the runtime's own overflow traps. Reduction keeps the form canonical — nested `Succ` flattened, zero floors collapsed (see `Nat::decompose` and `Nat::rebuild`) — so arithmetic on the floor is [`Natural`] arithmetic.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Nat {
    Zero,
    Succ(Natural, Term),
}

impl Nat {
    /// A closed literal in canonical form: zero is `Zero`, anything positive is a single `Succ` floor over the literal-zero tail — never a unary chain.
    pub fn new(value: impl Into<Natural>) -> Self {
        let value = value.into();

        if value.is_zero() {
            Nat::Zero
        } else {
            Nat::Succ(value, Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)).into())
        }
    }

    /// The magnitude's bit width when this is a closed literal, and zero when it is symbolic — the operand size a fold's price is computed from, read off the spine without materializing anything.
    pub(crate) fn bits(&self) -> u64 {
        match self.as_literal() {
            Some(value) => value.bits(),
            None => 0,
        }
    }

    /// This literal as a `u64`, when it is closed and fits — the shift amount a price is computed from.
    ///
    /// A `u64` rather than a `usize` because a charge may not differ between the native and wasm32 targets, and `usize` differs; [`Natural::to_u64`] carries the argument.
    pub(crate) fn to_u64(&self) -> Option<u64> {
        self.as_literal()?.to_u64()
    }

    /// The stored magnitude of a closed literal, borrowed. `None` for zero — which carries no magnitude to borrow — and for a symbolic successor floor.
    fn as_literal(&self) -> Option<&Natural> {
        match self {
            Nat::Zero => None,
            Nat::Succ(spine, inner) => match inner.as_ref() {
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => Some(spine),
                _ => None,
            },
        }
    }

    pub fn to_natural(&self) -> Option<Natural> {
        match self {
            Nat::Zero => Some(Natural::zero()),
            Nat::Succ(spine, inner) => match inner.as_ref() {
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => Some(spine.clone()),
                _ => None,
            },
        }
    }

    /// `None` on a symbolic operand *or* a zero divisor — never a panic; the reducer reports the zero-divisor case before folding.
    pub(crate) fn checked_div(self, other: Self) -> Option<Self> {
        Some(Self::new(
            self.to_natural()?.checked_div(other.to_natural()?)?,
        ))
    }

    /// `None` on a symbolic operand or a zero divisor, like [`Nat::checked_div`].
    pub(crate) fn checked_rem(self, other: Self) -> Option<Self> {
        Some(Self::new(
            self.to_natural()?.checked_rem(other.to_natural()?)?,
        ))
    }

    /// Unbounded bitwise `and`/`or`/`xor` on the infinite binary expansion. The type level pretends ℕ, so these impose no 31-bit limit; the runtime's i31 carrier is enforced only in the backend. `None` on a symbolic operand, like [`Nat::checked_div`].
    pub(crate) fn checked_bitand(self, other: Self) -> Option<Self> {
        Some(Self::new(self.to_natural()? & other.to_natural()?))
    }

    pub(crate) fn checked_bitor(self, other: Self) -> Option<Self> {
        Some(Self::new(self.to_natural()? | other.to_natural()?))
    }

    pub(crate) fn checked_bitxor(self, other: Self) -> Option<Self> {
        Some(Self::new(self.to_natural()? ^ other.to_natural()?))
    }

    /// `self << amount` as `self * 2^amount`, and `self >> amount` as `⌊self / 2^amount⌋` — both unbounded. `None` on a symbolic operand or an `amount` too large to be a shift count.
    pub(crate) fn checked_shl(self, amount: Self) -> Option<Self> {
        Some(Self::new(
            self.to_natural()?.checked_shl(amount.to_natural()?)?,
        ))
    }

    pub(crate) fn checked_shr(self, amount: Self) -> Option<Self> {
        Some(Self::new(
            self.to_natural()?.checked_shr(amount.to_natural()?)?,
        ))
    }

    /// View a reduced term as a flat successor floor over a symbolic tail: `term = inner + floor`. A non-`Succ` term — literal zero, a variable, any stuck intrinsic — has floor `0` and is its own `inner`; reduction flattens nested `Succ`, so `inner` is never itself successor-headed. The one-value companion to `spine::peel_nat` (which peels the floor shared by *two* values): this is the seam `Nat/add`, `Nat/sub`, `Nat/mul`, and the comparison family share to act on the floor symbolically, then rebuild a canonical neutral.
    pub fn decompose(term: &Term) -> (Natural, Term) {
        match &**term {
            Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(floor, inner))) => {
                (floor.clone(), inner.clone())
            }
            _ => (Natural::zero(), term.clone()),
        }
    }

    /// The inverse of [`Nat::decompose`]: `inner + floor`, collapsing a zero floor back to the bare `inner` so the rebuilt term lands in the same normal form `decompose` expects.
    pub(crate) fn rebuild(floor: Natural, inner: Term) -> Term {
        match floor.is_zero() {
            true => inner,
            false => Term::intrinsic(Intrinsic::Nat(Nat::Succ(floor, inner))),
        }
    }

    /// Whether a reduced term is literal zero — the identity floor.
    pub fn is_zero(term: &Term) -> bool {
        matches!(&**term, Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)))
    }

    /// The summands of a reduced `Nat`'s symbolic inner, flattening the neutral `add` spine. `NatAdd` hoists every literal floor outward, so no summand reached here is successor-headed.
    pub(crate) fn summands(inner: &Term) -> Vec<Term> {
        let mut summands = Vec::new();
        let mut pending = vec![inner.clone()];

        while let Some(term) = pending.pop() {
            match &*term {
                Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) => {
                    pending.push(left.clone());
                    pending.push(right.clone());
                }
                _ if Self::is_zero(&term) => {}
                _ => summands.push(term),
            }
        }

        summands
    }

    /// The sum of `summands` over a literal `floor`, landing in the same normal form [`Nat::decompose`] reads back.
    pub(crate) fn sum_over_floor(summands: Vec<Term>, floor: Natural) -> Term {
        let inner = summands
            .into_iter()
            .filter(|summand| !Self::is_zero(summand))
            .reduce(|left, right| Term::intrinsic(Intrinsic::nat_add(left, right)))
            .unwrap_or_else(|| Term::intrinsic(Intrinsic::Nat(Nat::Zero)));

        Self::rebuild(floor, inner)
    }

    /// Strip what both operands carry in common, so residuals decide where the originals could not: `x + a` against `x + b` becomes `a` against `b`.
    ///
    /// **Why it is sound for every consumer.** `Nat` under `+` is a cancellative commutative monoid. Every order relation reads through it — `x + a ⋈ x + b` iff `a ⋈ b` — and so does truncated subtraction: either `a ≥ b`, where both differences are `a - b` because the `x` cancels in the borrow, or `a < b`, where `x + a < x + b` makes both sides zero. So removing a common addend preserves the answer rather than approximating it.
    ///
    /// **A multiset, never a set.** `a + a + b` against `a + c` cancels *one* `a` and leaves `a + b` against `c`. Cancelling both would read `a + b ⋈ c` off `a + a + b ⋈ a + c`, which is false — and false definitional equations are the route this file's soundness perimeter records as reaching `False` by congruence.
    ///
    /// **Summands pair by equality up to universe instances.** A definitionally equal pair spelled two ways still does not cancel — the match does not reduce candidates against each other, so incompleteness in that direction costs reductions and never correctness. What it *does* see through is an instance, because two occurrences of a polymorphic name are independently instantiated and would otherwise be two terms: `len(xs)` written twice never cancels against itself, and every bound mentioning one stays stuck. Erasing before the comparison is [`crate::project_erased_universes`], the same projection the refinement key already probes under, licensed by the same fact — an instance is deleted before anything runs and no value can depend on one. It is a question about numbers, and a level is not part of the answer.
    ///
    /// The literal floors cancel by the same law, which is why the minimum comes off both: it is the one-summand case of the same rule, and doing it here rather than at each consumer is what keeps the two spellings from drifting.
    pub(crate) fn cancel_common(left: &Term, right: &Term) -> (Term, Term) {
        let (floor_left, inner_left) = Nat::decompose(left);
        let (floor_right, inner_right) = Nat::decompose(right);

        let mut held = Self::summands(&inner_left);
        let mut keys = held
            .iter()
            .map(crate::project_erased_universes)
            .collect::<Vec<_>>();
        let mut residual_right = Vec::new();
        let mut cancelled = false;
        for summand in Self::summands(&inner_right) {
            let key = crate::project_erased_universes(&summand);
            match keys.iter().position(|candidate| *candidate == key) {
                Some(index) => {
                    held.remove(index);
                    keys.remove(index);
                    cancelled = true;
                }
                None => residual_right.push(summand),
            }
        }

        let shared = floor_left.clone().min(floor_right.clone());

        // **A pass that cancels no summand must hand its inners back untouched.** Rebuilding through [`Nat::sum_over_floor`] re-associates and reorders a sum — `a + (b + c)` comes back as `(c + b) + a`, and again as `(a + b) + c` — so a stuck comparison rebuilt from reordered operands is a *different* term, which the caller reduces again, reorders again, and never settles. Taking the floors off the original inners is what the comparison family did before summands were read at all, and it is stable because it rewrites nothing below the floor.
        if !cancelled {
            return (
                Self::rebuild(floor_left - &shared, inner_left),
                Self::rebuild(floor_right - &shared, inner_right),
            );
        }

        (
            Self::sum_over_floor(held, floor_left - &shared),
            Self::sum_over_floor(residual_right, floor_right - &shared),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sym(index: u32, hint: &'static str) -> Term {
        Term::free_var(&crate::Free::local(index, Some(hint)))
    }

    fn add(left: Term, right: Term) -> Term {
        Term::intrinsic(Intrinsic::nat_add(left, right))
    }

    fn occurrences(term: &Term, wanted: &Term) -> usize {
        Nat::summands(term)
            .iter()
            .filter(|summand| *summand == wanted)
            .count()
    }

    // Soundness gate on the cancellation: it is a *multiset* operation. A summand held twice on one side against once on the other must leave one behind, because reading `a + b ⋈ c` off `a + a + b ⋈ a + c` is a false definitional equation — the route this file's perimeter records as reaching `False` by congruence.
    #[test]
    fn nat_cancellation_removes_one_occurrence_per_match() {
        let (a, b, c) = (sym(0, "a"), sym(1, "b"), sym(2, "c"));

        let (left, right) = Nat::cancel_common(
            &add(add(a.clone(), a.clone()), b.clone()),
            &add(a.clone(), c.clone()),
        );

        assert_eq!(occurrences(&left, &a), 1, "one `a` must survive the cancel");
        assert_eq!(occurrences(&left, &b), 1, "`b` is shared with nothing");
        assert_eq!(occurrences(&right, &a), 0, "the right's single `a` cancels");
        assert_eq!(occurrences(&right, &c), 1, "`c` is shared with nothing");
    }

    // Regression: a pass that cancels nothing must return its operands *identically*, not merely equivalently. Rebuilding a sum through `sum_over_floor` re-associates and reorders it, so a stuck comparison rebuilt from reordered operands is a new term the caller reduces again — which reorders again. That oscillation is not a slow reduction, it is an unbounded one, and it overflowed the stack building the fixed prelude.
    #[test]
    fn nat_cancellation_is_stable_when_nothing_is_shared() {
        let (a, b, c, d) = (sym(0, "a"), sym(1, "b"), sym(2, "c"), sym(3, "d"));
        let left = add(a, add(b, c));

        let (settled_left, settled_right) = Nat::cancel_common(&left, &d);

        assert_eq!(
            settled_left, left,
            "an uncancelled sum keeps its own association"
        );
        assert_eq!(
            settled_right, d,
            "and so does the side it was compared against"
        );
    }
}
