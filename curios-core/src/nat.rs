use {
    super::{Atoms, Cost, Intrinsic, ReduceError, Reducer, Subterm, Term},
    curios_algebra::{
        Cancelled, Combination, Deduction, Monomial, Progress, Recombination, Summand, Wanted,
        distribute, distribution_size,
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

    /// View a reduced term as a flat successor floor over a symbolic tail: `term = inner + floor`. A non-`Succ` term — literal zero, a variable, any stuck intrinsic — has floor `0` and is its own `inner`; reduction flattens nested `Succ`, so `inner` is never itself successor-headed. The one-value companion to `spine::peel_nat_terms` (which peels the floor shared by *two* values): this is the seam `Nat/add`, `Nat/sub`, `Nat/mul`, and the comparison family share to act on the floor symbolically, then rebuild a canonical neutral.
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
        let mut atoms = Atoms::default();
        let left_terms = Self::distributable(&mut atoms, left);
        let right_terms = Self::distributable(&mut atoms, right);
        let (floor, products) = distribute(&left_terms, &right_terms);

        // **One node per distinct monomial, not one per product.** A cross product builds the same monomial many times over — every pair of summands whose factors multiply to it — and a fresh spine per product would be cache-warmed on construction and compared structurally when the sum merges it, since an equal spine built a moment earlier is a different allocation `Rc::ptr_eq` cannot see. A monomial is a list of handles, so a lookup walks nothing, and each handle stands for the one allocation its factor was first read as — which is what makes every element comparison inside the sum a pointer test.
        let mut canonical: HashMap<Monomial, Term> = HashMap::new();
        let summands = products
            .into_iter()
            .map(|(coefficient, monomial)| {
                let spine = canonical
                    .entry(monomial)
                    .or_insert_with_key(|monomial| {
                        let factors = monomial
                            .atoms()
                            .iter()
                            .map(|atom| atoms.term(*atom).clone())
                            .collect::<Vec<_>>();
                        Self::spine(&factors).expect("a monomial with a factor")
                    })
                    .clone();
                Self::scaled(coefficient, spine)
            })
            .collect::<Vec<_>>();
        // The two magnitudes that say what distribution in full costs: monomials built against summands kept, the first growing as the square of the second, which is what an eager cross product is.
        curios_profile::sample!("multiply::products", summands.len() as u64);
        let merged = Self::sum_over_floor(summands, floor);
        #[cfg(feature = "profile")]
        curios_profile::sample!(
            "multiply::merged",
            Self::summands(&Self::decompose(&merged).1).len() as u64
        );
        merged
    }

    /// A reduced `Nat` as the summands a product distributes over: each summand's literal coefficient and its factors as written, and its floor last, a summand over no factor.
    fn distributable(atoms: &mut Atoms, term: &Term) -> Vec<(Natural, Monomial)> {
        let (floor, inner) = Self::decompose(term);
        let mut terms = Self::summands(&inner)
            .iter()
            .map(|summand| {
                let (coefficient, factors) = Self::monomial(summand);
                let factors = factors.iter().map(|factor| atoms.exact(factor)).collect();
                (coefficient, Monomial::new(factors))
            })
            .collect::<Vec<_>>();
        if !floor.is_zero() {
            terms.push((floor, Monomial::new(Vec::new())));
        }
        terms
    }

    /// How many summands a product distributes, its floor counted as one — what a distribution of it against another costs, through `distribution_size`.
    pub(crate) fn distributed_count(term: &Term) -> usize {
        let (floor, inner) = Self::decompose(term);
        Self::summands(&inner).len() + usize::from(!floor.is_zero())
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

    /// `summands` as a linear combination: like factors merged by adding their coefficients, in first-appearance order, each factor's product spine a monomial of atoms under the carrier's identity (`crate::atoms`). This is the sum normal form — `x + x` is `2 · x`, and `2 · x + 3 · x` is `5 · x` — and it is what makes a sum's like terms definitionally equal rather than merely cancellable against each other. The collection is `curios-algebra`'s; what is read here is which term each summand's factor is, and the factor a merged summand keeps is its first appearance's.
    pub(crate) fn linear(summands: impl IntoIterator<Item = Term>) -> Vec<(Natural, Term)> {
        curios_profile::profile!("nat::linear");
        let mut atoms = Atoms::default();
        let read = summands
            .into_iter()
            .filter(|summand| !Self::is_zero(summand))
            .map(|summand| Self::summand(&mut atoms, &summand))
            .collect::<Vec<_>>();
        Self::terms_of(Combination::collect(Natural::zero(), read))
    }

    /// A reduced summand as `curios-algebra` reads it: its literal coefficient, and its factor's product spine as a monomial of atoms, the factor itself the origin a rebuild restores.
    ///
    /// **The atoms are put in their own order, not the spine's.** The product fold orders a spine's factors by the structural hash of each as written, levels included, so two instances of one polymorphic factor can sort to opposite sides of another and spell one number two ways. An atom's rank is the hash of its projected term, which no level moves, so [`Monomial::product`] puts `g<0> · x` and `g<8> · x` in one order. A factor still holding a literal inside its spine — a term no fold left that way — is read whole, as one atom, so its literal is never dropped from the coefficient.
    fn summand(atoms: &mut Atoms, summand: &Term) -> Summand<Natural, Term> {
        let (coefficient, factor) = Self::literal_factor(summand);
        let (inner, factors) = Self::monomial(&factor);
        let monomial = match inner.is_one() {
            true => Monomial::product(factors.iter().map(|factor| atoms.numeric(factor)).collect()),
            false => Monomial::new(vec![atoms.numeric(&factor)]),
        };
        Summand {
            coefficient,
            monomial,
            origin: factor,
        }
    }

    /// A reduced `Nat` read as its floor beside the combination of its summands.
    fn combination(atoms: &mut Atoms, term: &Term) -> Combination<Natural, Term> {
        let (floor, inner) = Self::decompose(term);
        let summands = Self::summands(&inner)
            .iter()
            .map(|summand| Self::summand(atoms, summand))
            .collect::<Vec<_>>();
        Combination::collect(floor, summands)
    }

    /// A combination's summands as the coefficients and factors they were read from.
    fn terms_of(combination: Combination<Natural, Term>) -> Vec<(Natural, Term)> {
        combination
            .summands
            .into_iter()
            .map(|summand| (summand.coefficient, summand.origin))
            .collect()
    }

    /// The sum of `summands` over a literal `floor`, landing in the same normal form [`Nat::decompose`], [`Nat::summands`] and [`Nat::linear`] read back: like terms merged, a remainder beside its multiple recombined by [`Nat::recombine`], each spelled by [`Nat::scaled`], folded left-to-right.
    pub(crate) fn sum_over_floor(summands: Vec<Term>, floor: Natural) -> Term {
        curios_profile::profile!("nat::sum_over_floor");
        let mut atoms = Atoms::default();
        let read = summands
            .iter()
            .filter(|summand| !Self::is_zero(summand))
            .map(|summand| Self::summand(&mut atoms, summand))
            .collect::<Vec<_>>();
        let combination = Self::recombine(&mut atoms, Combination::collect(floor, read));
        let floor = combination.constant.clone();
        Self::from_linear(Self::terms_of(combination), floor)
    }

    /// `combination` with Euclid's identity applied until no remainder beside its multiple is left: the arithmetic is `curios-algebra`'s, and what is read here is which summand is a remainder and where the combination holds each monomial of its multiple.
    fn recombine(
        atoms: &mut Atoms,
        mut combination: Combination<Natural, Term>,
    ) -> Combination<Natural, Term> {
        while let Some((recombination, dividend)) = Self::euclid_pair(atoms, &combination) {
            combination = combination.recombined(recombination, dividend);
        }
        combination
    }

    /// The first remainder in `combination` held beside at least one copy of its multiple: the recombination the algebra computes for it, and its dividend, read over the combination's atoms.
    ///
    /// **The multiple is matched in the form the fold leaves it in.** A quotient is one symbolic summand, so `d · (x / d)` is always distributed by the product fold — `(x / (y + 1)) · (y + 1)` is `y · (x / (y + 1)) + x / (y + 1)` — and the wanted monomials are computed the same way, through [`Nat::multiply`], so the two spellings meet. A quotient and a remainder pair on their dividend and divisor and never on their proofs, which each division carries as written and which proof irrelevance makes unobservable; a monomial is compared as a multiset of factors, because the factor order is a structural hash that sees the proof.
    fn euclid_pair(
        atoms: &mut Atoms,
        combination: &Combination<Natural, Term>,
    ) -> Option<(Recombination<Natural>, Combination<Natural, Term>)> {
        combination
            .summands
            .iter()
            .enumerate()
            .find_map(|(index, summand)| {
                let Subterm::Intrinsic(Intrinsic::NatRem {
                    dividend,
                    divisor,
                    non_zero,
                }) = &*summand.origin
                else {
                    return None;
                };
                let quotient = Term::intrinsic(Intrinsic::NatDiv {
                    dividend: dividend.clone(),
                    divisor: divisor.clone(),
                    non_zero: non_zero.clone(),
                });
                let (_, multiple) = Self::decompose(&Self::multiply(divisor, &quotient));

                let wanted = Self::linear(Self::summands(&multiple))
                    .into_iter()
                    .map(|(per_copy, monomial)| {
                        let holding =
                            combination
                                .summands
                                .iter()
                                .enumerate()
                                .find(|(_, candidate)| {
                                    Self::same_monomial(&candidate.origin, &monomial)
                                });
                        let holding =
                            holding.map(|(at, candidate)| (at, candidate.coefficient.clone()));
                        (per_copy, holding)
                    })
                    .collect::<Vec<Wanted<Natural>>>();
                let recombination = Recombination::natural(index, &summand.coefficient, &wanted)?;
                Some((recombination, Self::combination(atoms, dividend)))
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

    /// A sum or a product rebuilt with its summands or factors in structural-hash order, each put through `order` first — the spelling `force_atoms` reads an atom's arguments in, and never written back. `None` for any other node.
    ///
    /// A sum is merged as it is rebuilt, like terms into one coefficient, and a product's literal factors are multiplied into its coefficient, so the rebuilt node is in the normal form the fold keeps; only its order is new.
    pub(crate) fn in_order(node: &Term, order: &impl Fn(&Term) -> Term) -> Option<Term> {
        match &**node {
            Subterm::Intrinsic(Intrinsic::NatAdd(..)) => {
                let mut combination = Self::linear(Self::summands(node).iter().map(order));
                combination.sort_by_key(|(_, factor)| factor.structural_hash());
                Some(Self::from_linear(combination, Natural::zero()))
            }
            Subterm::Intrinsic(Intrinsic::NatMul(..)) => {
                let (coefficient, factors) = Self::monomial(node);
                let mut factors = factors.iter().map(order).collect::<Vec<_>>();
                factors.sort_by_key(Term::structural_hash);
                Some(match Self::spine(&factors) {
                    Some(spine) => Self::scaled(coefficient, spine),
                    None => Term::intrinsic(Intrinsic::Nat(Nat::new(coefficient))),
                })
            }
            _ => None,
        }
    }

    /// A weak-head `Nat` with every product of two symbolic sums distributed, and the result re-merged — the one normalization the fold does not perform on its own, asked for by name where a comparison needs the value: `compare_nat`, the converters' rule for two symbolic `Nat`s. See `documentation/design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md`.
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
                    let products = distribution_size(
                        Self::distributed_count(&left),
                        Self::distributed_count(&right),
                    );
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
    /// **Summands pair by equality up to universe instances.** A definitionally equal pair spelled two ways still does not cancel — the match does not reduce candidates against each other, so incompleteness in that direction costs reductions and never correctness. What it *does* see through is an instance, because two occurrences of a polymorphic name are independently instantiated and would otherwise be two terms: `len(xs)` written twice never cancels against itself, and every bound mentioning one stays stuck. Erasing before the comparison is [`crate::project_erased_universes`], and what licenses it here is the carrier rather than erasure: Core offers no elimination from a type or a level into a `Nat`, so two summands differing only in their instances denote one number. That is not true of terms in general — `Type u` is a value that differs by its level — which is why the same projection is unsound as a refinement key, as `documentation/design/soundness/elimination/case-equations-and-their-key.md` records.
    ///
    /// The literal floors cancel by the same law, which is why the minimum comes off both: it is the one-summand case of the same rule, and doing it here rather than at each consumer is what keeps the two spellings from drifting.
    ///
    /// The mathematics is `curios-algebra`'s `Combination::cancel_common`; what is decided here is which terms are one atom, and which terms the residuals are rebuilt from.
    pub(crate) fn cancel_common(left: &Term, right: &Term) -> (Term, Term) {
        curios_profile::profile!("nat::cancel_common");
        Self::rebuild_cancelled(Self::cancellation(left, right), left, right)
    }

    /// What `left = right` concludes over `Nat`, with its residuals rebuilt as terms: the cancellation as the peel reads it, timed under the one span every cancellation passes through.
    pub(crate) fn cancellation_deduced(left: &Term, right: &Term) -> Deduction<(Term, Term)> {
        curios_profile::profile!("nat::cancel_common");
        Self::cancellation(left, right)
            .deduction()
            .map(|cancelled| Self::rebuild_cancelled(cancelled, left, right))
    }

    /// Whether two reduced `Nat` terms are certainly one number: syntactic identity first, then the cancellation, which reads every summand up to universe instances — so `len(xs)` is one number at every instance, bare or inside a sum. `false` declines; it never claims the two differ.
    pub fn same(left: &Term, right: &Term) -> bool {
        if left == right {
            return true;
        }
        curios_profile::profile!("nat::cancel_common");
        matches!(
            Self::cancellation(left, right).deduction(),
            Deduction::Equal
        )
    }

    /// `left` against `right` read over one table of atoms, with what they share taken off both.
    fn cancellation(left: &Term, right: &Term) -> Cancelled<Natural, Term> {
        let mut atoms = Atoms::default();
        let left = Self::combination(&mut atoms, left);
        let right = Self::combination(&mut atoms, right);
        left.cancel_common(right)
    }

    /// The terms a cancellation of `left` against `right` leaves.
    ///
    /// **A pass that cancels no summand hands its inners back untouched.** Rebuilding through [`Nat::from_linear`] re-associates and reorders a sum — `a + (b + c)` comes back as `(c + b) + a`, and again as `(a + b) + c` — so a stuck comparison rebuilt from reordered operands is a *different* term, which the caller reduces again, reorders again, and never settles. Taking the floors off the original inners is what the comparison family did before summands were read at all, and it is stable because it rewrites nothing below the floor.
    fn rebuild_cancelled(
        cancelled: Cancelled<Natural, Term>,
        left: &Term,
        right: &Term,
    ) -> (Term, Term) {
        let rebuild = |combination: Combination<Natural, Term>| {
            let floor = combination.constant.clone();
            Self::from_linear(Self::terms_of(combination), floor)
        };
        match cancelled.progress {
            Progress::Summands => (rebuild(cancelled.left), rebuild(cancelled.right)),
            Progress::Nothing | Progress::Constant => (
                Self::rebuild(cancelled.left.constant, Self::decompose(left).1),
                Self::rebuild(cancelled.right.constant, Self::decompose(right).1),
            ),
        }
    }
}

#[cfg(test)]
mod tests;
