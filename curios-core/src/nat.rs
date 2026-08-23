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

    /// The summands of a reduced `Nat`'s symbolic inner, flattening the neutral `add` spine, in the order they are written. `NatAdd` hoists every literal floor outward, so no summand reached here is successor-headed. The order matters: [`Nat::sum_over_floor`] folds the list back left-to-right, so reading it left-to-right is what makes read-then-rebuild the identity on a sum already in normal form — a rebuild that reordered would hand the reducer a new term every pass, and that oscillation once overflowed the stack building the prelude.
    pub(crate) fn summands(inner: &Term) -> Vec<Term> {
        let mut summands = Vec::new();
        let mut pending = vec![inner.clone()];

        while let Some(term) = pending.pop() {
            match &*term {
                Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) => {
                    pending.push(right.clone());
                    pending.push(left.clone());
                }
                _ if Self::is_zero(&term) => {}
                _ => summands.push(term),
            }
        }

        summands
    }

    /// A reduced summand read as `coefficient · factor`: a product with a literal on either side, or the summand itself under the coefficient `1`. The literal side is the left after [`Nat::scaled`], but a product written the other way by a stage that builds terms without reducing them reads the same.
    pub(crate) fn literal_factor(summand: &Term) -> (Natural, Term) {
        if let Subterm::Intrinsic(Intrinsic::NatMul(left, right)) = &**summand {
            if let Some(coefficient) = left.as_nat().and_then(|value| value.to_natural()) {
                return (coefficient, right.clone());
            }
            if let Some(coefficient) = right.as_nat().and_then(|value| value.to_natural()) {
                return (coefficient, left.clone());
            }
        }
        (Natural::one(), summand.clone())
    }

    /// `coefficient · factor` in normal form: a zero coefficient is `0`, a unit coefficient is the factor itself, and anything else is the product with the literal on the left — so `x · 2` and `2 · x` are one term.
    pub(crate) fn scaled(coefficient: Natural, factor: Term) -> Term {
        if coefficient.is_zero() {
            return Term::intrinsic(Intrinsic::Nat(Nat::Zero));
        }
        match coefficient.is_one() {
            true => factor,
            false => Term::intrinsic(Intrinsic::nat_mul(
                Term::intrinsic(Intrinsic::Nat(Nat::new(coefficient))),
                factor,
            )),
        }
    }

    /// `summands` as a linear combination: like factors merged by adding their coefficients, in first-appearance order, keyed up to universe instances exactly as [`Nat::cancel_common`] keys them. This is the sum normal form — `x + x` is `2 · x`, and `2 · x + 3 · x` is `5 · x` — and it is what makes a sum's like terms definitionally equal rather than merely cancellable against each other.
    pub(crate) fn linear(summands: impl IntoIterator<Item = Term>) -> Vec<(Natural, Term)> {
        let mut combination: Vec<(Natural, Term)> = Vec::new();
        let mut keys: Vec<Term> = Vec::new();
        for summand in summands {
            if Self::is_zero(&summand) {
                continue;
            }
            let (coefficient, factor) = Self::literal_factor(&summand);
            let key = crate::project_erased_universes(&factor);
            match keys.iter().position(|candidate| *candidate == key) {
                Some(index) => combination[index].0 += coefficient,
                None => {
                    combination.push((coefficient, factor));
                    keys.push(key);
                }
            }
        }
        combination
    }

    /// The sum of `summands` over a literal `floor`, landing in the same normal form [`Nat::decompose`], [`Nat::summands`] and [`Nat::linear`] read back: like terms merged, each spelled by [`Nat::scaled`], folded left-to-right.
    pub(crate) fn sum_over_floor(summands: Vec<Term>, floor: Natural) -> Term {
        Self::from_linear(Self::linear(summands), floor)
    }

    /// [`Nat::sum_over_floor`] from a combination already merged.
    pub(crate) fn from_linear(combination: Vec<(Natural, Term)>, floor: Natural) -> Term {
        let inner = combination
            .into_iter()
            .map(|(coefficient, factor)| Self::scaled(coefficient, factor))
            .reduce(|left, right| Term::intrinsic(Intrinsic::nat_add(left, right)))
            .unwrap_or_else(|| Term::intrinsic(Intrinsic::Nat(Nat::Zero)));

        Self::rebuild(floor, inner)
    }

    /// The sum of two already-reduced `Nat` terms, landing in the normal form [`Nat::decompose`] and [`Nat::summands`] read back: the literal floors added and hoisted outward, the symbolic summands juxtaposed.
    ///
    /// The one place a sum is needed *without* a reducer in hand. `spine`'s window fusion adds two lengths while flattening a value for comparison, and a flattening walk cannot re-enter reduction — so the form has to be constructed rather than folded into. Stating it here is what keeps it this module's invariant instead of a second opinion about it held next door: a fused window's length is then the same term `NatAdd`'s fold would have produced, which is what lets it still compare against an unfused window's.
    pub(crate) fn sum(left: &Term, right: &Term) -> Term {
        let (floor_left, inner_left) = Nat::decompose(left);
        let (floor_right, inner_right) = Nat::decompose(right);

        let mut summands = Self::summands(&inner_left);
        summands.extend(Self::summands(&inner_right));

        Self::sum_over_floor(summands, floor_left + floor_right)
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

        // Over the linear combination, so a like term cancels by coefficient: `2 · x + a` against `x + b` leaves `x + a` against `b` — the multiset rule below, with the multiplicity read off the coefficient rather than counted.
        let mut held = Self::linear(Self::summands(&inner_left));
        let mut keys = held
            .iter()
            .map(|(_, factor)| crate::project_erased_universes(factor))
            .collect::<Vec<_>>();
        let mut residual_right = Vec::new();
        let mut cancelled = false;
        for (coefficient, factor) in Self::linear(Self::summands(&inner_right)) {
            let key = crate::project_erased_universes(&factor);
            match keys.iter().position(|candidate| *candidate == key) {
                Some(index) => {
                    let shared = held[index].0.clone().min(coefficient.clone());
                    let remaining = held[index].0.clone() - &shared;
                    if remaining.is_zero() {
                        held.remove(index);
                        keys.remove(index);
                    } else {
                        held[index].0 = remaining;
                    }
                    let rest = coefficient - &shared;
                    if !rest.is_zero() {
                        residual_right.push((rest, factor));
                    }
                    cancelled = true;
                }
                None => residual_right.push((coefficient, factor)),
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
            Self::from_linear(held, floor_left - &shared),
            Self::from_linear(residual_right, floor_right - &shared),
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
