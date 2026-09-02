use {
    super::{Cost, Intrinsic, ReduceError, Reducer, Subterm, Term},
    curios_num::Natural,
    curios_utilities::recurse,
    std::collections::HashMap,
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
        curios_profile::profile!("nat::summands");
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

    /// A reduced summand read as a monomial: its literal coefficient and its symbolic factors, the product spine flattened whatever its nesting, with every literal multiplied into the coefficient.
    pub(crate) fn monomial(summand: &Term) -> (Natural, Vec<Term>) {
        let mut coefficient = Natural::one();
        let mut factors = Vec::new();
        let mut pending = vec![summand.clone()];
        while let Some(term) = pending.pop() {
            match &*term {
                Subterm::Intrinsic(Intrinsic::NatMul(left, right)) => {
                    pending.push(right.clone());
                    pending.push(left.clone());
                }
                _ => match term.as_nat().and_then(|value| value.to_natural()) {
                    Some(literal) => coefficient = coefficient * literal,
                    None => factors.push(term),
                },
            }
        }
        (coefficient, factors)
    }

    /// The bare monomial over `factors`, which the caller has already put in canonical order: nested to the left, so `x · y` and `y · x` are one term once sorted. The order is the factors' structural hash, which is deterministic; the sort is stable, so two distinct factors that happen to hash alike keep their written order and simply fail to canonicalize against each other — incompleteness, never a wrong equation. `None` for no factors, since a monomial with none is its coefficient and not a term; [`Nat::multiply`] keys its canonical table on the sorted list and applies the coefficient through [`Nat::scaled`].
    fn spine(factors: &[Term]) -> Option<Term> {
        factors
            .iter()
            .cloned()
            .reduce(|left, right| Term::intrinsic(Intrinsic::nat_mul(left, right)))
    }

    /// The product of two reduced `Nat` terms, in the sum normal form: every summand of one — its floor counted as a constant summand — times every summand of the other, each product a monomial in canonical factor order, and the results summed through [`Nat::sum_over_floor`] so like monomials merge. This is distribution in full — `x · (y + z) = x · y + x · z` for a symbolic `x` — of which the literal-factor floor law, the unit and annihilation laws and the nested-factor fold are the special cases, each of which the value grid still states on its own.
    pub(crate) fn multiply(left: &Term, right: &Term) -> Term {
        curios_profile::profile!("nat::multiply");
        let terms = |term: &Term| {
            let (floor, inner) = Self::decompose(term);
            let mut terms = Self::summands(&inner)
                .iter()
                .map(Self::monomial)
                .collect::<Vec<_>>();
            if !floor.is_zero() {
                terms.push((floor, Vec::new()));
            }
            terms
        };

        let mut floor = Natural::zero();
        let mut summands = Vec::new();
        let left_terms = terms(left);
        let right_terms = terms(right);
        // **One node per distinct monomial, not one per product.** A cross product builds the same monomial many times over — every pair of summands whose factors multiply to it — and each fresh spine had to be cache-warmed on construction and then compared structurally when the sum merged it, because an equal spine built a moment earlier was a different allocation and `Rc::ptr_eq` could not see it. On a nine-definition web of definitions each naming the one before it twice that was 198 793 spines, 204 113 structural comparisons every one of which concluded equal, and 2.4 s of a 6.1 s compile. Keyed on the sorted factor list, whose hash is one cached word per factor, so a lookup walks nothing; scoped to this product, which is where every duplicate the sum will merge is born.
        let mut canonical: HashMap<Vec<Term>, Term> = HashMap::new();
        // The factors are interned too, because the table above compares its keys element-wise and a leaf reached through `left` is a different allocation from the same leaf reached through `right`: each operand is its own reduct. Measured before this, a lookup that should allocate nothing spent 28 allocations walking factor structure — 548 ms of an 827 ms product. One canonical `Rc` per distinct leaf makes every element comparison a pointer test.
        let mut interned: HashMap<Term, Term> = HashMap::new();
        let mut intern = |factor: Term| match interned.get(&factor) {
            Some(canonical) => canonical.clone(),
            None => {
                interned.insert(factor.clone(), factor.clone());
                factor
            }
        };
        for (ca, fa) in left_terms {
            for (cb, fb) in right_terms.iter().cloned() {
                let coefficient = ca.clone() * cb;
                let mut factors = fa.iter().cloned().map(&mut intern).collect::<Vec<_>>();
                factors.extend(fb.iter().cloned().map(&mut intern));
                if factors.is_empty() {
                    floor += coefficient;
                    continue;
                }
                factors.sort_by_key(Term::structural_hash);
                let spine = match canonical.get(&factors) {
                    Some(spine) => spine.clone(),
                    None => {
                        let spine = Self::spine(&factors).expect("a monomial with a factor");
                        canonical.insert(factors, spine.clone());
                        spine
                    }
                };
                summands.push(Self::scaled(coefficient, spine));
            }
        }
        // The two magnitudes that say what distribution in full costs: monomials built against summands kept. On a web of definitions each naming the one before it twice they read 198 793 against 9 083 at nine definitions and 1 222 222 against 25 412 at ten — products grow as the square of what survives, which is what an eager cross product is.
        curios_profile::sample!("multiply::products", summands.len() as u64);
        let merged = Self::sum_over_floor(summands, floor);
        #[cfg(feature = "profile")]
        curios_profile::sample!(
            "multiply::merged",
            Self::summands(&Self::decompose(&merged).1).len() as u64
        );
        merged
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
        curios_profile::profile!("nat::linear");
        let mut combination: Vec<(Natural, Term)> = Vec::new();
        // **The index is a map, and the combination stays a vector.** Those are two separate obligations that a single `Vec<Term>` of keys used to serve at once, badly: finding a like factor was a scan comparing whole terms, one `Term::eq` per candidate, while first-appearance order — which the sum normal form above promises and which a caller relies on to reach a fixed point — only ever needed `combination` to be pushed to in order. Keeping them apart makes the lookup a hash and leaves the order exactly where it was.
        //
        // A key is a *projected* term rather than the factor, so two instances of one polymorphic name merge; `Term`'s hash is memoized per node, and `clippy.toml` names `Term` for `ignore-interior-mutability` on the same grounds the map relies on — a cache fill moves neither hash nor equality.
        let mut index_of: HashMap<Term, usize> = HashMap::new();
        for summand in summands {
            if Self::is_zero(&summand) {
                continue;
            }
            let (coefficient, factor) = Self::literal_factor(&summand);
            let key = crate::project_erased_universes(&factor);
            match index_of.get(&key) {
                Some(&index) => combination[index].0 += coefficient,
                None => {
                    index_of.insert(key, combination.len());
                    combination.push((coefficient, factor));
                }
            }
        }
        combination
    }

    /// The sum of `summands` over a literal `floor`, landing in the same normal form [`Nat::decompose`], [`Nat::summands`] and [`Nat::linear`] read back: like terms merged, each spelled by [`Nat::scaled`], folded left-to-right.
    pub(crate) fn sum_over_floor(summands: Vec<Term>, floor: Natural) -> Term {
        curios_profile::profile!("nat::sum_over_floor");
        Self::from_linear(Self::linear(summands), floor)
    }

    /// [`Nat::sum_over_floor`] from a combination already merged.
    pub(crate) fn from_linear(combination: Vec<(Natural, Term)>, floor: Natural) -> Term {
        curios_profile::profile!("nat::from_linear");
        let inner = combination
            .into_iter()
            .map(|(coefficient, factor)| Self::scaled(coefficient, factor))
            .reduce(|left, right| Term::intrinsic(Intrinsic::nat_add(left, right)))
            .unwrap_or_else(|| Term::intrinsic(Intrinsic::Nat(Nat::Zero)));

        Self::rebuild(floor, inner)
    }

    /// A weak-head `Nat` with every product of two symbolic sums distributed, and the result re-merged — the one normalization the fold no longer performs on its own, asked for by name where a comparison needs the value: `compare_nat`, the converters' rule for two symbolic `Nat`s. See `documentation/design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md`.
    ///
    /// The fold keeps every sum merged and every difference cancelled, so this walks only into products and the sums that hold them; a term with no stuck product comes back untouched. A memo keyed on node identity keeps a shared operand distributed once and holds each input alive beside its answer, since an identity is an address; the descent re-enters [`recurse`] per level. A product is priced here by what it builds — the concat fold's idiom, one collection and one node per product — because `operand_bound` at the fold prices by literal width, and a symbolic cross product read as zero bits.
    pub fn normalize(reducer: &mut impl Reducer, term: Term) -> Result<Term, ReduceError> {
        let mut memo: HashMap<usize, (Term, Term)> = HashMap::new();
        Self::normalize_within(reducer, term, &mut memo)
    }

    fn normalize_within(
        reducer: &mut impl Reducer,
        term: Term,
        memo: &mut HashMap<usize, (Term, Term)>,
    ) -> Result<Term, ReduceError> {
        recurse(|| {
            let key = term.identity();
            if let Some((_, done)) = memo.get(&key) {
                return Ok(done.clone());
            }
            let reduced = reducer.reduce_forced(term.clone())?;
            let result = match &*reduced {
                Subterm::Intrinsic(Intrinsic::NatMul(left, right)) => {
                    let left = Self::normalize_within(reducer, left.clone(), memo)?;
                    let right = Self::normalize_within(reducer, right.clone(), memo)?;
                    let count = |term: &Term| {
                        let (floor, inner) = Self::decompose(term);
                        Self::summands(&inner).len() as u64 + u64::from(!floor.is_zero())
                    };
                    let products = count(&left).saturating_mul(count(&right));
                    reducer.spend(
                        Cost::collection(products)
                            .saturating_add(Cost::term(2).saturating_mul(products)),
                    )?;
                    Self::multiply(&left, &right)
                }
                Subterm::Intrinsic(Intrinsic::NatAdd(..))
                | Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                    let (floor, inner) = Self::decompose(&reduced);
                    let summands = Self::summands(&inner)
                        .into_iter()
                        .map(|summand| Self::normalize_within(reducer, summand, memo))
                        .collect::<Result<Vec<_>, _>>()?;
                    Self::sum_over_floor(summands, floor)
                }
                _ => reduced,
            };
            memo.insert(key, (term, result.clone()));
            Ok(result)
        })
    }

    /// Whether a weak-head `Nat` holds a product of two symbolic sums somewhere under its sums — the one shape [`Nat::normalize`] changes.
    pub fn has_stuck_product(term: &Term) -> bool {
        let mut pending = vec![term.clone()];
        while let Some(term) = pending.pop() {
            match &*term {
                Subterm::Intrinsic(Intrinsic::NatMul(..)) => return true,
                Subterm::Intrinsic(Intrinsic::NatAdd(l, r)) => {
                    pending.push(l.clone());
                    pending.push(r.clone());
                }
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(_, inner))) => {
                    pending.push(inner.clone())
                }
                _ => {}
            }
        }
        false
    }

    /// The sum of two already-reduced `Nat` terms, landing in the normal form [`Nat::decompose`] and [`Nat::summands`] read back: the literal floors added and hoisted outward, the symbolic summands juxtaposed.
    ///
    /// The one place a sum is needed *without* a reducer in hand. `spine`'s window fusion adds two lengths while flattening a value for comparison, and a flattening walk cannot re-enter reduction — so the form has to be constructed rather than folded into. Stating it here is what keeps it this module's invariant instead of a second opinion about it held next door: a fused window's length is then the same term `NatAdd`'s fold would have produced, which is what lets it still compare against an unfused window's.
    pub(crate) fn sum(left: &Term, right: &Term) -> Term {
        curios_profile::profile!("nat::sum");
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
    /// **Summands pair by equality up to universe instances.** A definitionally equal pair spelled two ways still does not cancel — the match does not reduce candidates against each other, so incompleteness in that direction costs reductions and never correctness. What it *does* see through is an instance, because two occurrences of a polymorphic name are independently instantiated and would otherwise be two terms: `len(xs)` written twice never cancels against itself, and every bound mentioning one stays stuck. Erasing before the comparison is [`crate::project_erased_universes`], and what licenses it here is the carrier rather than erasure: Core offers no elimination from a type or a level into a `Nat`, so two summands differing only in their instances denote one number. That is not true of terms in general — `Type u` is a value that differs by its level — which is why the same projection is unsound as a refinement key, as `documentation/soundness/what-the-kernel-consults/the-refinement-key.md` records.
    ///
    /// The literal floors cancel by the same law, which is why the minimum comes off both: it is the one-summand case of the same rule, and doing it here rather than at each consumer is what keeps the two spellings from drifting.
    pub(crate) fn cancel_common(left: &Term, right: &Term) -> (Term, Term) {
        curios_profile::profile!("nat::cancel_common");
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
mod tests;
