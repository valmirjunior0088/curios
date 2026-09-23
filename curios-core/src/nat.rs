use {
    super::{
        Apply, Argument, Bound, Cost, Intrinsic, ReduceError, Reducer, Subterm, Term, Var, Visit,
    },
    curios_num::Natural,
    curios_utilities::recurse,
    std::collections::HashMap,
};

/// A type-level natural in successor-floor form: `Zero`, or `Succ(floor, inner)` — a [`Natural`] count of successors stacked on a tail term `inner`, so a closed literal is one node and `x + 3` is `Succ(3, x)`, never a unary chain. Unbounded — the type level pretends ℕ, like `Integer`'s ℤ, and so does every stage below it; the runtime's 31-bit envelope is enforced at one place only, where `curios-emit` materializes a value into an `i31ref`. Reduction keeps the form canonical — nested `Succ` flattened, zero floors collapsed (see `Nat::decompose` and `Nat::rebuild`) — so arithmetic on the floor is [`Natural`] arithmetic.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Nat {
    Zero,
    Succ(Natural, Term),
}

/// One application of Euclid's identity to a linear combination: the dividend the pair recombines to, the copies of the pair taken, and what each entry of the combination gives up. `C` is the carrier's coefficient — a count on `Nat`, a signed count on `Int`.
pub(crate) struct Recombination<C> {
    pub(crate) dividend: Term,
    pub(crate) copies: C,
    pub(crate) spent: Vec<(usize, C)>,
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
    /// A `u64` rather than a `usize` because a charge may not differ between the native and wasm32 targets, and `usize` differs; the machine narrowings out of [`Natural`] carry the argument.
    pub(crate) fn to_u64(&self) -> Option<u64> {
        u64::try_from(self.as_literal()?).ok()
    }

    /// The stored magnitude of a closed literal, borrowed. `None` for zero — which carries no magnitude to borrow — and for a symbolic successor floor.
    pub(crate) fn as_literal(&self) -> Option<&Natural> {
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
            self.to_natural()?.div(&other.to_natural()?).ok()?,
        ))
    }

    /// `None` on a symbolic operand or a zero divisor, like [`Nat::checked_div`].
    pub(crate) fn checked_rem(self, other: Self) -> Option<Self> {
        Some(Self::new(
            self.to_natural()?.rem(&other.to_natural()?).ok()?,
        ))
    }

    /// Unbounded bitwise `and`/`or`/`xor` on the infinite binary expansion. ℕ is unbounded at every layer, the running program included, so these impose no width. `None` on a symbolic operand, like [`Nat::checked_div`].
    pub(crate) fn checked_bitand(self, other: Self) -> Option<Self> {
        Some(Self::new(self.to_natural()? & other.to_natural()?))
    }

    pub(crate) fn checked_bitor(self, other: Self) -> Option<Self> {
        Some(Self::new(self.to_natural()? | other.to_natural()?))
    }

    pub(crate) fn checked_bitxor(self, other: Self) -> Option<Self> {
        Some(Self::new(self.to_natural()? ^ other.to_natural()?))
    }

    /// `self << amount` as `self * 2^amount`, unbounded. `None` on a symbolic operand or an `amount` too large to be a shift count.
    pub(crate) fn checked_shl(self, amount: Self) -> Option<Self> {
        Some(Self::new(
            self.to_natural()?
                .shl_within(&amount.to_natural()?, u64::MAX)?,
        ))
    }

    /// `self >> amount` as `⌊self / 2^amount⌋`, total on values — a count past the width answers zero, [`Natural`]'s `>>` — and `None` only on a symbolic operand, like [`Nat::checked_bitand`].
    pub(crate) fn checked_shr(self, amount: Self) -> Option<Self> {
        Some(Self::new(&self.to_natural()? >> &amount.to_natural()?))
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

    /// A reduced count read as one generator over the rest: `Some(rest)` when the term carries a positive floor, `None` when it carries none — literal zero, a variable, a stuck intrinsic. The caller tells those two apart with [`Nat::is_zero`], because a count of zero and a count of unknown size mean opposite things to a reader that has to decide whether a generator is there.
    ///
    /// The successor peel a *fill* is taken apart by, shared so `free_monoid`'s destructor and `spine`'s conversion segment cannot come to different answers about what `replicate(n + 1, a)` is. Both need it because a fill is the one packed shape whose leading generator is known without its length being known.
    pub fn peel_succ(term: &Term) -> Option<Term> {
        let (floor, inner) = Self::decompose(term);

        match floor.is_zero() {
            true => None,
            false => Some(Self::rebuild(floor - Natural::from(1usize), inner)),
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

    /// The sum of `summands` over a literal `floor`, landing in the same normal form [`Nat::decompose`], [`Nat::summands`] and [`Nat::linear`] read back: like terms merged, a remainder beside its multiple recombined by [`Nat::recombine`], each spelled by [`Nat::scaled`], folded left-to-right.
    pub(crate) fn sum_over_floor(summands: Vec<Term>, floor: Natural) -> Term {
        curios_profile::profile!("nat::sum_over_floor");
        let (combination, floor) = Self::recombine(Self::linear(summands), floor);
        Self::from_linear(combination, floor)
    }

    /// Euclid's identity read in the direction that shrinks a sum: `k` remainders `x % d` beside `k` copies of the divisor times the matching quotient, `d · (x / d)`, are `k · x`.
    ///
    /// **Unconditional, which is what admits it.** `d · (x / d) + x % d = x` holds for every `x` and every nonzero `d`, and a division node exists only over its proof that the divisor is nonzero, so no value of a symbolic part can falsify a recombination. It is the recombining twin of the split `reduce_nat_division` takes: the split reads a quotient and a remainder *off* a sum, and this reads the sum back.
    ///
    /// **The multiple is matched in the form the fold leaves it in.** A quotient is one symbolic summand, so `d · (x / d)` is always distributed by the product fold — `(x / (y + 1)) · (y + 1)` is `y · (x / (y + 1)) + x / (y + 1)` — and the wanted monomials are computed the same way, through [`Nat::multiply`], so the two spellings meet. A quotient and a remainder pair on their dividend and divisor and never on their proofs, which each division carries as written and which proof irrelevance makes unobservable; a monomial is compared as a multiset of factors, because the factor order is a structural hash that sees the proof.
    ///
    /// Each recombination removes a remainder and adds the summands of its dividend, a strict subterm, so the loop terminates; a combination with nothing to recombine comes back untouched, which keeps read-then-rebuild the identity on a sum already in normal form.
    fn recombine(
        mut combination: Vec<(Natural, Term)>,
        mut floor: Natural,
    ) -> (Vec<(Natural, Term)>, Natural) {
        while let Some(Recombination {
            dividend,
            copies,
            spent,
        }) = Self::euclid_pair(&combination)
        {
            for (index, amount) in spent {
                combination[index].0 = &combination[index].0 - amount;
            }
            let (dividend_floor, dividend_inner) = Self::decompose(&dividend);
            floor += &copies * dividend_floor;
            let mut summands = combination
                .into_iter()
                .map(|(coefficient, factor)| Self::scaled(coefficient, factor))
                .collect::<Vec<_>>();
            summands.extend(
                Self::linear(Self::summands(&dividend_inner))
                    .into_iter()
                    .map(|(coefficient, factor)| Self::scaled(&copies * coefficient, factor)),
            );
            combination = Self::linear(summands);
        }
        (combination, floor)
    }

    /// The first remainder in `combination` held beside at least one copy of its multiple: the dividend it recombines to, how many copies of the pair the combination holds, and what each entry gives up.
    fn euclid_pair(combination: &[(Natural, Term)]) -> Option<Recombination<Natural>> {
        combination
            .iter()
            .enumerate()
            .find_map(|(index, (held, factor))| {
                let Subterm::Intrinsic(Intrinsic::NatRem {
                    dividend,
                    divisor,
                    non_zero,
                }) = &**factor
                else {
                    return None;
                };
                let quotient = Term::intrinsic(Intrinsic::NatDiv {
                    dividend: dividend.clone(),
                    divisor: divisor.clone(),
                    non_zero: non_zero.clone(),
                });
                let (_, multiple) = Self::decompose(&Self::multiply(divisor, &quotient));

                let mut copies = held.clone();
                let mut matched = Vec::new();
                for (per_copy, monomial) in Self::linear(Self::summands(&multiple)) {
                    let (at, (available, _)) = combination
                        .iter()
                        .enumerate()
                        .find(|(_, (_, factor))| Self::same_monomial(factor, &monomial))?;
                    copies = copies.min(available / &per_copy);
                    matched.push((at, per_copy));
                }
                if copies.is_zero() {
                    return None;
                }

                let mut spent = vec![(index, copies.clone())];
                spent.extend(
                    matched
                        .into_iter()
                        .map(|(at, per_copy)| (at, per_copy * &copies)),
                );
                Some(Recombination {
                    dividend: dividend.clone(),
                    copies,
                    spent,
                })
            })
    }

    /// Whether two monomials have one coefficient and one multiset of factors, a quotient matching a quotient on its dividend and divisor alone — the proof it carries is not part of the number.
    fn same_monomial(left: &Term, right: &Term) -> bool {
        let (left_coefficient, left_factors) = Self::monomial(left);
        let (right_coefficient, mut right_factors) = Self::monomial(right);
        if left_coefficient != right_coefficient || left_factors.len() != right_factors.len() {
            return false;
        }
        left_factors.iter().all(|factor| {
            match right_factors
                .iter()
                .position(|candidate| Self::same_factor(factor, candidate))
            {
                Some(position) => {
                    right_factors.swap_remove(position);
                    true
                }
                None => false,
            }
        })
    }

    /// One factor against another up to universe instances, as [`Nat::linear`] keys them, and a quotient against a quotient up to its proof.
    fn same_factor(left: &Term, right: &Term) -> bool {
        let project = crate::project_erased_universes::<Term>;
        match (&**left, &**right) {
            (
                Subterm::Intrinsic(Intrinsic::NatDiv {
                    dividend: left_dividend,
                    divisor: left_divisor,
                    ..
                }),
                Subterm::Intrinsic(Intrinsic::NatDiv {
                    dividend: right_dividend,
                    divisor: right_divisor,
                    ..
                }),
            ) => {
                project(left_dividend) == project(right_dividend)
                    && project(left_divisor) == project(right_divisor)
            }
            _ => project(left) == project(right),
        }
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

    /// `term` with every sum inside it rebuilt in one order, so two spellings of one number become one term.
    ///
    /// **Reduction is not enough on its own, and this is the half it does not do.** [`Nat::summands`] keeps first-appearance order — which is what makes read-then-rebuild the identity, and a rebuild that reordered would hand the reducer a new term every pass — so `x + y` and `y + x` reduce to two distinct `NatAdd` nodes and stay two summands the peel cannot pair. Forcing a summand's arguments only gets as far as turning the concept dispatch into that node; ordering is what makes the pair meet.
    ///
    /// Ordering by [`Term::structural_hash`] is the discipline a monomial's factors already keep — `Nat::multiply` and `int_monomial` both sort that way, so `i · j` and `j · i` are one term — applied to a sum's summands instead of a product's factors. Sound because `Nat` under `+` is a commutative monoid and congruence carries that under the head.
    ///
    /// **Probe-side only.** The one caller hands the result to a peel and falls back to the *original* spelling when that peel still decides nothing, so no reordered sum is ever returned to the reducer and the oscillation [`Nat::summands`] records is not re-entered.
    fn ordered_sums(term: &Term) -> Term {
        recurse(|| {
            term.traverse(&mut Visit::rewriting(
                |_, _: &Var| None,
                // A pre-hook does not descend into what it replaces, so the recursion is this closure's own: each summand is ordered before the sum it sits in is.
                Box::new(|_, node: &Term| {
                    if !matches!(&**node, Subterm::Intrinsic(Intrinsic::NatAdd(..))) {
                        return None;
                    }
                    let mut combination =
                        Self::linear(Self::summands(node).iter().map(Self::ordered_sums));
                    combination.sort_by_key(|(_, factor)| factor.structural_hash());

                    Some(Self::from_linear(combination, Natural::zero()))
                }),
            ))
        })
    }

    /// A weak-head `Nat` with every summand's own arguments forced — the second normalization the fold does not perform on its own, asked for by name where a comparison needs the value.
    ///
    /// **The peel reads summands without reducing them, and the fold leaves a stuck application's arguments exactly as written.** So `f(x + y)` and `f(y + x)` are two summands that never pair, though conversion decides that very pair on its own the moment it compares them directly — which is why each step of `Eq(f(x + y) + g(y + z), g(z + y) + f(y + x))` holds while the composition did not. This is [`Nat::normalize`]'s demand for a different unforced shape, and `normalize_bool`'s for a different carrier: force what the peel reads before it reads it.
    ///
    /// **The head is kept verbatim and only the arguments are forced**, as `canonical_scrutinee` keeps a refinement key's head verbatim: reducing the summand itself would unfold the very definition the sum is stuck on, and the sum would stop being a sum. A summand that is not an application comes back untouched, and a term no summand of which changed comes back as itself — which is what keeps this free where it decides nothing, and keeps the caller's stability arm reading the term it was handed.
    pub fn normalize_atoms(reducer: &mut impl Reducer, term: Term) -> Result<Term, ReduceError> {
        let (floor, inner) = Self::decompose(&term);
        let mut summands = Vec::new();
        let mut changed = false;
        for summand in Self::summands(&inner) {
            let forced = Self::force_arguments(reducer, &summand)?;
            changed |= forced != summand;
            summands.push(forced);
        }

        match changed {
            false => Ok(term),
            true => Ok(Self::sum_over_floor(summands, floor)),
        }
    }

    /// One summand with each argument of its applied head reduced, the head itself untouched.
    fn force_arguments(reducer: &mut impl Reducer, summand: &Term) -> Result<Term, ReduceError> {
        let Subterm::Apply(Apply { head, arguments }) = &**summand else {
            return Ok(summand.clone());
        };

        let mut forced = Vec::with_capacity(arguments.len());
        for argument in arguments {
            let reduced = reducer.reduce(argument.term.clone())?;
            forced.push(Argument {
                term: Self::ordered_sums(&reduced),
                plicity: argument.plicity,
            });
        }

        Ok(Subterm::Apply(Apply {
            head: head.clone(),
            arguments: forced,
        })
        .into())
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
