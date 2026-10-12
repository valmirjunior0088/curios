//! The signed carrier's sum normal form: a literal constant over a linear combination of monomials with integer coefficients — [`Nat`]'s form over ℤ, where a subtraction is a negative coefficient rather than a stuck node and a negation is the coefficient `-1`.
//!
//! **Spelled in the nodes the roster already has**, so nothing below reduction meets a new shape: `IntAdd(inner, Int(k))` carries the constant `k` over `inner` and is absent when `k` is zero; `inner` is a left-nested `IntAdd` spine of summands, each `IntMul(Int(c), monomial)` or the bare monomial when `c` is one; a monomial is a left-nested `IntMul` of factors in structural-hash order, as `Nat::product` orders them, so `i · j` and `j · i` are one term. Summands keep first-appearance order, as `Nat::linear` keeps it, so `i + j` and `j + i` are two spellings of one combination — decided by cancellation at the peel, never by reordering in the fold, for the reason `Nat::cancel_common` records: a rebuilt sum is a different term, and a stuck comparison rebuilt from one would never be found again.
//!
//! **A product of two symbolic sums stays stuck until a comparison asks**, the decision `documentation/design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md` measured for `Nat` and applies here verbatim: [`int_product`] distributes when either operand is a constant or a single summand, and [`int_normalize`] distributes the rest on demand.
//!
//! Every function here is total over reduced terms it does not recognize, reading anything that is not an `IntAdd`, `IntMul` or `Int` literal as an opaque monomial factor, which is what makes `i - i` fold to `0` for a symbolic `i` while `f(i)` stays the symbol it is.

use {
    super::{
        Atoms, Footprint, Intrinsic, Nat, ReduceError, Reducer, Subterm, Term, image_bound,
        product_bound, same_atom, sum_bound,
    },
    curios_algebra::{
        Cancelled, Carrier, Combination, Deduction, Progress, Recombination, Summand, Wanted,
        distribute, signed_cofactor,
    },
    curios_num::Integer,
    curios_utilities::recurse,
    std::collections::HashMap,
};

/// One monomial with its coefficient: the factors are already in canonical order, which is what lets two monomials be compared as vectors.
type Monomial = (Integer, Vec<Term>);

/// A summand's monomial as it was read: its factors, in canonical order, and the spine that already spells their product where the summand was written over one.
#[derive(Debug, Clone)]
pub(crate) struct IntOrigin {
    factors: Vec<Term>,
    /// The left-nested product of `factors` as the summand held it. A rebuild hands it back in place of writing the product again, so a sum that keeps a monomial writes a node for it and none for its factors, as `Nat`'s keeps the factor it read.
    spine: Option<Term>,
}

/// A signed combination as this module reads one: each summand's origin is its monomial as it was read, which a rebuild restores.
type Signed = Combination<Integer, IntOrigin>;

fn zero() -> Integer {
    Integer::from(0)
}

fn one() -> Integer {
    Integer::from(1)
}

fn literal(value: Integer) -> Term {
    Term::intrinsic(Intrinsic::Int(value))
}

/// Whether a reduced term is the literal zero — the identity.
pub fn int_is_zero(term: &Term) -> bool {
    term.as_int().is_some_and(|value| value.is_zero())
}

/// Whether a reduced term is the literal `-1`, every bit of its two's-complement expansion set.
pub fn int_is_minus_one(term: &Term) -> bool {
    term.as_int().is_some_and(|value| value == -one())
}

/// A reduced term read as its constant and its symbolic summands, the `IntAdd` spine flattened whatever its nesting and every literal summand folded into the constant.
pub(crate) fn int_terms(term: &Term) -> (Integer, Vec<Term>) {
    let mut constant = zero();
    let mut summands = Vec::new();
    let mut pending = vec![term.clone()];
    while let Some(term) = pending.pop() {
        match &*term {
            Subterm::Intrinsic(Intrinsic::IntAdd(left, right)) => {
                pending.push(right.clone());
                pending.push(left.clone());
            }
            _ => match term.as_int() {
                Some(value) => constant = constant + value,
                None => summands.push(term),
            },
        }
    }
    (constant, summands)
}

/// A reduced summand read as a monomial: its coefficient and its symbolic factors, the `IntMul` spine flattened whatever its nesting and every literal factor multiplied into the coefficient, the factors then sorted into canonical order.
pub(crate) fn int_monomial(summand: &Term) -> Monomial {
    let mut coefficient = one();
    let mut factors = Vec::new();
    let mut pending = vec![summand.clone()];
    while let Some(term) = pending.pop() {
        match &*term {
            Subterm::Intrinsic(Intrinsic::IntMul(left, right)) => {
                pending.push(right.clone());
                pending.push(left.clone());
            }
            _ => match term.as_int() {
                Some(value) => coefficient = coefficient * value,
                None => factors.push(term),
            },
        }
    }
    factors.sort_by_key(Term::structural_hash);
    (coefficient, factors)
}

/// `summands` as a linear combination: like monomials merged by adding their coefficients, in first-appearance order, each factor an atom under the carrier's identity (`crate::atoms`) and a monomial its factors in canonical order, and a monomial whose coefficient cancelled to zero dropped. The collection is `curios-algebra`'s; a merged monomial keeps its first appearance's factors.
pub(crate) fn int_linear(summands: impl IntoIterator<Item = Term>) -> Vec<Monomial> {
    let summands: Vec<Term> = summands.into_iter().collect();
    let mut atoms = Atoms::default();
    int_monomials_of(int_combination(&mut atoms, zero(), &summands))
}

/// A reduced summand as `curios-algebra` reads it: its coefficient, its factors as atoms, and those factors as the origin a rebuild restores. `None` for a summand whose coefficient is zero, which is no summand.
///
/// The atoms are put in their own order, as `Nat`'s are: [`int_monomial`] orders factors by the structural hash of each as written, levels included, so two instances of one polymorphic factor can sort apart, while an atom's rank is its projected term's hash, which no level moves.
fn int_summand(atoms: &mut Atoms, summand: &Term) -> Option<Summand<Integer, IntOrigin>> {
    let (coefficient, factors) = int_monomial(summand);
    (!coefficient.is_zero()).then(|| Summand {
        coefficient,
        monomial: curios_algebra::Monomial::product(
            factors
                .iter()
                .map(|factor| atoms.handle(Carrier::Integer, factor))
                .collect(),
        ),
        origin: IntOrigin {
            spine: int_written(summand, &factors),
            factors,
        },
    })
}

/// The spine `summand` holds where it is written as the fold writes a monomial: `factors` multiplied left to right in their order, under a literal coefficient on its left or under none. `None` for any other spelling — factors in another order, a literal inside the product — which a rebuild writes again in the canonical one.
fn int_written(summand: &Term, factors: &[Term]) -> Option<Term> {
    let spine = match &**summand {
        Subterm::Intrinsic(Intrinsic::IntMul(left, right)) if left.as_int().is_some() => right,
        _ => summand,
    };
    let (first, rest) = factors.split_first()?;
    let mut node = spine;
    for factor in rest.iter().rev() {
        let Subterm::Intrinsic(Intrinsic::IntMul(left, right)) = &**node else {
            return None;
        };
        if right.identity() != factor.identity() {
            return None;
        }
        node = left;
    }

    (node.identity() == first.identity()).then(|| spine.clone())
}

/// `constant` beside the combination of `summands`, like monomials merged and zeros dropped.
fn int_combination(atoms: &mut Atoms, constant: Integer, summands: &[Term]) -> Signed {
    let read = summands
        .iter()
        .filter_map(|summand| int_summand(atoms, summand))
        .collect::<Vec<_>>();
    Combination::collect(constant, read).without_zeros()
}

/// A combination's summands as the coefficients and factors they were read from.
fn int_monomials_of(combination: Signed) -> Vec<Monomial> {
    combination
        .summands
        .into_iter()
        .map(|summand| (summand.coefficient, summand.origin.factors))
        .collect()
}

/// A combination written back into the normal form, each monomial the spine it was read over where it was read over one.
fn int_from_combination(combination: Signed) -> Term {
    let summands = combination
        .summands
        .into_iter()
        .map(|summand| match summand.origin.spine {
            Some(spine) => int_scaled_spine(summand.coefficient, spine),
            None => int_scaled(summand.coefficient, &summand.origin.factors),
        });

    int_summed(combination.constant, summands)
}

/// The bare monomial over `factors`, nested to the left in the order given.
fn int_spine(factors: &[Term]) -> Option<Term> {
    factors
        .iter()
        .cloned()
        .reduce(|left, right| Term::intrinsic(Intrinsic::IntMul(left, right)))
}

/// `coefficient · monomial` in normal form: a unit coefficient is the monomial itself, anything else the product with the literal on the left.
fn int_scaled(coefficient: Integer, factors: &[Term]) -> Term {
    match int_spine(factors) {
        None => literal(coefficient),
        Some(spine) => int_scaled_spine(coefficient, spine),
    }
}

/// `coefficient · spine` in normal form, over a monomial already written: a unit coefficient is the spine itself, anything else the product with the literal on the left.
fn int_scaled_spine(coefficient: Integer, spine: Term) -> Term {
    match coefficient == one() {
        true => spine,
        false => Term::intrinsic(Intrinsic::IntMul(literal(coefficient), spine)),
    }
}

/// A combination and a constant written back into the normal form every reader here reads: summands folded left to right, the constant last and absent when zero, a combination with nothing in it the constant alone.
pub(crate) fn int_from_linear(constant: Integer, combination: Vec<Monomial>) -> Term {
    int_summed(
        constant,
        combination
            .into_iter()
            .map(|(coefficient, factors)| int_scaled(coefficient, &factors)),
    )
}

/// `summands`, each a monomial under its coefficient, folded left to right over `constant`.
fn int_summed(constant: Integer, summands: impl Iterator<Item = Term>) -> Term {
    let inner = summands.reduce(|left, right| Term::intrinsic(Intrinsic::IntAdd(left, right)));
    match (inner, constant.is_zero()) {
        (None, _) => literal(constant),
        (Some(inner), true) => inner,
        (Some(inner), false) => Term::intrinsic(Intrinsic::IntAdd(inner, literal(constant))),
    }
}

/// The sum of two reduced terms, in normal form: constants added, like monomials merged.
pub fn int_sum(left: &Term, right: &Term) -> Term {
    let (constant_left, mut summands) = int_terms(left);
    let (constant_right, summands_right) = int_terms(right);
    summands.extend(summands_right);
    int_merged(constant_left + constant_right, summands)
}

/// A constant and its summands merged into the normal form, a remainder beside its multiple recombined on the way — what every sum this module builds goes through, as `Nat::sum_over_floor` is for `Nat`.
fn int_merged(constant: Integer, summands: Vec<Term>) -> Term {
    let mut atoms = Atoms::default();
    let combination = int_combination(&mut atoms, constant, &summands);
    int_from_combination(int_recombine(&mut atoms, combination))
}

/// `combination` with Euclid's identity applied until no remainder beside its multiple is left — `Nat::recombine`'s twin, the arithmetic `curios-algebra`'s, including the rule that a copy counts only where every sign agrees.
fn int_recombine(atoms: &mut Atoms, mut combination: Signed) -> Signed {
    while let Some((recombination, dividend)) = int_euclid_pair(atoms, &combination) {
        combination = combination.recombined(recombination, dividend);
    }
    combination
}

/// The first remainder in `combination` held beside at least one copy of its multiple: the recombination the algebra computes for it, and its dividend, read over the combination's atoms. The multiple is matched as `Nat::euclid_pair` matches it, proof-insensitively and as a multiset of factors.
fn int_euclid_pair(
    atoms: &mut Atoms,
    combination: &Signed,
) -> Option<(Recombination<Integer>, Signed)> {
    combination
        .summands
        .iter()
        .enumerate()
        .find_map(|(index, summand)| {
            let [factor] = summand.origin.factors.as_slice() else {
                return None;
            };
            let Subterm::Intrinsic(Intrinsic::IntRem {
                dividend,
                divisor,
                non_zero,
            }) = &**factor
            else {
                return None;
            };
            let quotient = Term::intrinsic(Intrinsic::IntDiv {
                dividend: dividend.clone(),
                divisor: divisor.clone(),
                non_zero: non_zero.clone(),
            });
            let (_, multiple) = int_terms(&int_multiply(divisor, &quotient));

            let wanted = int_linear(multiple)
                .into_iter()
                .map(|(per_copy, monomial)| {
                    let holding = combination
                        .summands
                        .iter()
                        .enumerate()
                        .find(|(_, candidate)| {
                            int_same_factors(&candidate.origin.factors, &monomial)
                        });
                    let holding =
                        holding.map(|(at, candidate)| (at, candidate.coefficient.clone()));
                    (per_copy, holding)
                })
                .collect::<Vec<Wanted<Integer>>>();
            let recombination = Recombination::integer(index, &summand.coefficient, &wanted)?;
            let (constant, summands) = int_terms(dividend);
            Some((recombination, int_combination(atoms, constant, &summands)))
        })
}

/// Whether two monomials' factors are one multiset, a quotient matching a quotient on its dividend and divisor alone, as `Nat::same_monomial` compares them.
fn int_same_factors(left: &[Term], right: &[Term]) -> bool {
    let one = |left: &Term, right: &Term| same_atom(Carrier::Integer, left, right);
    let same = |left: &Term, right: &Term| match (&**left, &**right) {
        (
            Subterm::Intrinsic(Intrinsic::IntDiv {
                dividend: left_dividend,
                divisor: left_divisor,
                ..
            }),
            Subterm::Intrinsic(Intrinsic::IntDiv {
                dividend: right_dividend,
                divisor: right_divisor,
                ..
            }),
        ) => one(left_dividend, right_dividend) && one(left_divisor, right_divisor),
        _ => one(left, right),
    };
    if left.len() != right.len() {
        return false;
    }
    let mut unmatched = right.to_vec();
    left.iter().all(|factor| {
        match unmatched
            .iter()
            .position(|candidate| same(factor, candidate))
        {
            Some(position) => {
                unmatched.swap_remove(position);
                true
            }
            None => false,
        }
    })
}

/// `Nat/to_int` of a reduced `Nat`, pushed through its normal form: the successor floor becomes the constant, a sum the sum of the widened summands, a product the product of the widened factors, and anything else — a symbol, a truncated difference, a quotient, a remainder — the widened atom it is. ℕ → ℤ is a semiring homomorphism, so every step is an equation on values; it is what lets a sum of widened naturals cancel as `Int` sums do, and what [`int_preimage`] reads back.
///
/// A product of two symbolic sums stays the stuck product of their images, the line [`int_product`] draws for every `Int` product.
///
/// **Written once, and charged as it is written.** Each level of the form — the term's own summands, and those of a sum standing as a factor of a stuck product — is charged `image_bound` of its footprint before its image is built, and a monomial over atoms is one spine whatever its degree: taken a factor at a time it is rebuilt at every factor, a square of its degree.
pub fn int_of_nat(reducer: &mut impl Reducer, nat: &Term) -> Result<Term, ReduceError> {
    recurse(|| {
        reducer.spend(image_bound(&Nat::footprint(nat)))?;
        let (floor, inner) = Nat::decompose(nat);
        let summands = Nat::summands(&inner)
            .iter()
            .map(|summand| int_of_summand(reducer, summand))
            .collect::<Result<Vec<_>, _>>()?;

        // One summand over no floor is its own image, as it stands; anything else is merged as every `Int` sum is.
        if let (true, [image]) = (floor.is_zero(), summands.as_slice()) {
            return Ok(image.clone());
        }

        Ok(int_merged(Integer::from(floor), summands))
    })
}

/// The image of one summand of a reduced `Nat`: its coefficient over the widened atoms of its monomial, in `Int`'s own canonical order, and the product of its factors' images where one of them is a sum, a stuck product's.
fn int_of_summand(reducer: &mut impl Reducer, summand: &Term) -> Result<Term, ReduceError> {
    let (coefficient, factors) = Nat::monomial(summand);
    let sum = |factor: &Term| {
        matches!(
            &**factor,
            Subterm::Intrinsic(Intrinsic::Nat(_) | Intrinsic::NatAdd(..))
        )
    };
    if factors.iter().any(sum) {
        return int_of_factors(reducer, summand);
    }

    let mut images = factors
        .into_iter()
        .map(|factor| Term::intrinsic(Intrinsic::NatToInt(factor)))
        .collect::<Vec<_>>();
    images.sort_by_key(Term::structural_hash);

    Ok(int_scaled(Integer::from(coefficient), &images))
}

/// The image of a product one of whose factors is a sum, taken as the product is nested: the product of its operands' images as [`int_product`] takes one, which leaves two sums the stuck product of theirs.
fn int_of_factors(reducer: &mut impl Reducer, product: &Term) -> Result<Term, ReduceError> {
    recurse(|| match &**product {
        Subterm::Intrinsic(Intrinsic::NatMul(left, right)) => Ok(int_product(
            &int_of_factors(reducer, left)?,
            &int_of_factors(reducer, right)?,
        )),
        _ => int_of_nat(reducer, product),
    })
}

/// The natural a reduced `Int` is the image of when it is one by construction: a non-negative constant over monomials with positive coefficients whose every factor is a widened natural. `None` for anything else — a negative coefficient, or a factor that is not widened, can make the value negative, and this reads no bound.
///
/// The inverse of [`int_of_nat`] on its image, rebuilt in `Nat`'s normal form. Two readers: `Int/to_nat`, whose inversion arm is the one-atom case of this, and the comparison, which decides a pair of such terms by comparing their preimages — ℕ → ℤ preserves and reflects order.
pub fn int_preimage(term: &Term) -> Option<Term> {
    let mut atoms = Atoms::default();
    let (constant, summands) = int_terms(term);
    let natural = int_combination(&mut atoms, constant, &summands).natural();
    natural.and_then(|natural| {
        let mut preimages = Vec::with_capacity(natural.summands.len());
        for summand in natural.summands {
            let naturals = summand
                .origin
                .factors
                .iter()
                .map(|factor| match &**factor {
                    Subterm::Intrinsic(Intrinsic::NatToInt(nat)) => Some(nat.clone()),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>()?;
            preimages.push(Nat::scaled(summand.coefficient, Nat::product_of(&naturals)));
        }
        Some(Nat::sum_over_floor(preimages, natural.constant))
    })
}

/// A reduced term divided by a nonzero literal that divides its constant and every coefficient, each divided in place: `(i * 4 + k * 6 + 8) / 2` is `i * 2 + k * 3 + 4`, and `i / -1` is `-i`. The quotient of an exact multiple is exact whichever way the division rounds (`curios-algebra`'s `signed_cofactor`), so this reads no sign, and the remainder of such a division is zero. `None` where the divisor leaves a remainder of the constant or of a coefficient: the truncated quotient of such a sum follows its sign.
pub fn int_exact_quotient(term: &Term, divisor: &Integer) -> Option<Term> {
    let (constant, summands) = int_terms(term);
    let constant = signed_cofactor(&constant, divisor)?;
    let monomials = int_linear(summands)
        .into_iter()
        .map(|(coefficient, factors)| Some((signed_cofactor(&coefficient, divisor)?, factors)))
        .collect::<Option<Vec<_>>>()?;
    Some(int_from_linear(constant, monomials))
}

/// `-term`, in normal form: every coefficient and the constant negated. What `IntSub` folds through, so no subtraction node survives reduction.
pub fn int_negate(term: &Term) -> Term {
    let mut atoms = Atoms::default();
    let (constant, summands) = int_terms(term);
    int_from_combination(int_combination(&mut atoms, constant, &summands).negated())
}

/// The monomials of a reduced term with its constant counted as one, which is what a product distributes over.
fn int_monomials(term: &Term) -> Vec<Monomial> {
    let (constant, summands) = int_terms(term);
    let mut monomials = int_linear(summands);
    if !constant.is_zero() {
        monomials.push((constant, Vec::new()));
    }
    monomials
}

/// The product of two reduced terms, distributed in full: every monomial of one times every monomial of the other, each product's factors re-sorted, and the results merged. Asked for by [`int_product`] only where one side is a single monomial, and by [`int_normalize`] for the rest.
pub(crate) fn int_multiply(left: &Term, right: &Term) -> Term {
    let mut atoms = Atoms::default();
    let distributable = |atoms: &mut Atoms, term: &Term| {
        int_monomials(term)
            .into_iter()
            .map(|(coefficient, factors)| {
                let factors = factors.iter().map(|factor| atoms.exact(factor)).collect();
                (coefficient, curios_algebra::Monomial::new(factors))
            })
            .collect::<Vec<_>>()
    };
    let left_terms = distributable(&mut atoms, left);
    let right_terms = distributable(&mut atoms, right);
    let (constant, products) = distribute(&left_terms, &right_terms);
    let summands = products
        .into_iter()
        .map(|(coefficient, monomial)| {
            let factors = monomial
                .atoms()
                .iter()
                .map(|atom| atoms.term(*atom).clone())
                .collect::<Vec<_>>();
            int_scaled(coefficient, &factors)
        })
        .collect();
    int_merged(constant, summands)
}

/// A reduced term's footprint, counted before a fold writes it back, as `Nat::footprint` counts: its monomials with the constant one of them, their factors, and the bits of their coefficients.
pub(crate) fn int_footprint(term: &Term) -> Footprint {
    let monomials = int_monomials(term);
    Footprint {
        summands: monomials.len(),
        factors: monomials.iter().map(|(_, factors)| factors.len()).sum(),
        bits: monomials
            .iter()
            .map(|(coefficient, _)| coefficient.bits())
            .fold(0, u64::saturating_add),
    }
}

/// Whether the fold distributes a product of two reduced terms: where either is a constant or a single monomial — the same line `NatMul` draws, and [`int_normalize`] is what crosses it on demand.
pub(crate) fn int_distributes(left: &Term, right: &Term) -> bool {
    int_single(left) || int_single(right)
}

/// Whether a reduced term is one monomial, its constant counted as one: read off its sum alone, no product taken apart, so asking it of every node of a monomial costs the monomial once.
fn int_single(term: &Term) -> bool {
    let (constant, summands) = int_terms(term);
    summands.len() + usize::from(!constant.is_zero()) <= 1
}

/// The product as the fold takes it: distributed where `int_distributes` says, and left as a stuck `IntMul` of the two reduced operands otherwise.
pub fn int_product(left: &Term, right: &Term) -> Term {
    match int_distributes(left, right) {
        true => int_multiply(left, right),
        false => Term::intrinsic(Intrinsic::IntMul(left.clone(), right.clone())),
    }
}

/// Whether a reduced term holds a product of two symbolic sums somewhere under its sums — the one shape [`int_normalize`] changes.
pub fn int_has_stuck_product(term: &Term) -> bool {
    let mut pending = vec![term.clone()];
    while let Some(term) = pending.pop() {
        match &*term {
            Subterm::Intrinsic(Intrinsic::IntMul(left, right)) => {
                if !int_distributes(left, right) {
                    return true;
                }
                pending.push(left.clone());
                pending.push(right.clone());
            }
            Subterm::Intrinsic(Intrinsic::IntAdd(left, right)) => {
                pending.push(left.clone());
                pending.push(right.clone());
            }
            _ => {}
        }
    }
    false
}

/// A weak-head `Int` with every product of two symbolic sums distributed and the result re-merged — asked for by name where a comparison needs the value, as `Nat::normalize` is. A term with no stuck product comes back untouched.
pub fn int_normalize(reducer: &mut impl Reducer, term: Term) -> Result<Term, ReduceError> {
    let mut memo: HashMap<usize, (Term, Term)> = HashMap::new();
    let reduced = reducer.reduce_forced(term)?;
    int_normalize_within(reducer, reduced, &mut memo)
}

/// [`int_normalize`] of a term that is weak-head already, as every node under one is: forced once at its root, for the reason `Nat::normalize` gives.
fn int_normalize_within(
    reducer: &mut impl Reducer,
    reduced: Term,
    memo: &mut HashMap<usize, (Term, Term)>,
) -> Result<Term, ReduceError> {
    recurse(|| {
        let key = reduced.identity();
        if let Some((_, done)) = memo.get(&key) {
            return Ok(done.clone());
        }
        let result = match &*reduced {
            Subterm::Intrinsic(Intrinsic::IntMul(left, right)) => {
                let read = (left.clone(), right.clone());
                let left = int_normalize_within(reducer, left.clone(), memo)?;
                let right = int_normalize_within(reducer, right.clone(), memo)?;
                // As `Nat::normalize` hands back a monomial the fold wrote, and charges a product it distributes.
                let standing =
                    read.0 == left && read.1 == right && int_single(&left) && int_single(&right);
                match standing {
                    true => reduced.clone(),
                    false => {
                        reducer
                            .spend(product_bound(&int_footprint(&left), &int_footprint(&right)))?;
                        int_multiply(&left, &right)
                    }
                }
            }
            Subterm::Intrinsic(Intrinsic::IntAdd(..)) => {
                let (constant, read) = int_terms(&reduced);
                let summands = read
                    .iter()
                    .map(|summand| int_normalize_within(reducer, summand.clone(), memo))
                    .collect::<Result<Vec<_>, _>>()?;
                // As `Nat::normalize` leaves a sum no product changed, and charges one it writes back.
                match summands == read {
                    true => reduced.clone(),
                    false => {
                        reducer.spend(sum_bound(summands.len()))?;
                        int_merged(constant, summands)
                    }
                }
            }
            _ => reduced.clone(),
        };
        memo.insert(key, (reduced, result.clone()));
        Ok(result)
    })
}

/// Strip what two reduced terms carry in common, so residuals decide where the originals could not — `Nat::cancel_common` over a group, where every term moves to whichever side keeps its coefficient positive: `i + 2 · j - k` against `j` becomes `i + j` against `k`, and a pair that differs in nothing becomes `0` against `0`. A pair sharing no monomial and at most one nonzero constant is handed back untouched, for the stability `Nat::cancel_common` records: a rebuilt sum is a different term, and a stuck comparison rebuilt from one would never be found again.
///
/// Sound for every reader because ℤ under `+` is a group: every order relation and equality reads through `a ⋈ b` iff `a - b ⋈ 0`, and splitting the difference by sign is only adding one term to both sides of that. The mathematics is `curios-algebra`'s `Combination::cancel_common`; what is decided here is which terms are one atom, and which terms the residuals are rebuilt from.
pub fn int_cancel_common(left: &Term, right: &Term) -> (Term, Term) {
    int_rebuild_cancelled(int_cancellation(left, right), left, right)
}

/// Whether two reduced `Int` terms are certainly one number — [`Nat::same`] over the group: syntactic identity first, then the cancellation, which reads every summand up to universe instances. `false` declines; it never claims the two differ.
pub fn int_same(left: &Term, right: &Term) -> bool {
    left == right || matches!(int_cancellation(left, right).deduction(), Deduction::Equal)
}

/// `left` against `right` read over one table of atoms, with what they share split off by sign.
pub(crate) fn int_cancellation(left: &Term, right: &Term) -> Cancelled<Integer, IntOrigin> {
    let mut atoms = Atoms::default();
    let (constant_left, summands_left) = int_terms(left);
    let (constant_right, summands_right) = int_terms(right);
    let left = int_combination(&mut atoms, constant_left, &summands_left);
    let right = int_combination(&mut atoms, constant_right, &summands_right);
    left.cancel_common(right)
}

/// The terms a cancellation of `left` against `right` leaves: the two operands themselves where it took nothing off.
pub(crate) fn int_rebuild_cancelled(
    cancelled: Cancelled<Integer, IntOrigin>,
    left: &Term,
    right: &Term,
) -> (Term, Term) {
    match cancelled.progress {
        Progress::Nothing => (left.clone(), right.clone()),
        Progress::Constant | Progress::Summands => (
            int_from_combination(cancelled.left),
            int_from_combination(cancelled.right),
        ),
    }
}

/// How many summands [`int_rebuild_cancelled`] writes back for `cancelled`: both residuals where anything was taken off, and none where the operands stand.
pub(crate) fn int_residuals(cancelled: &Cancelled<Integer, IntOrigin>) -> usize {
    match cancelled.progress {
        Progress::Nothing => 0,
        Progress::Constant | Progress::Summands => {
            cancelled.left.summands.len() + cancelled.right.summands.len()
        }
    }
}

/// The difference of two reduced terms split by sign for every pair, where [`int_cancel_common`] splits it only once something cancels: every monomial on the side that keeps its coefficient positive, the constant likewise, so two pairs with one difference are one pair — `0 < j - i` and `i < j`, `-i < -j` and `j < i`.
///
/// **Read, never written back.** A stuck comparison is what a guard refines on, and a refinement is keyed on the guard's written spelling; a fold that split every comparison would take each later occurrence past its own key, the failure `documentation/design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md` records for swapped operands. So the one reader is the `Int` comparison fold's `compare_preimages`, which splits a comparison to read the preimages of its widened naturals and only ever decides.
pub fn int_split_by_sign(left: &Term, right: &Term) -> (Term, Term) {
    let mut atoms = Atoms::default();
    let (constant_left, summands_left) = int_terms(left);
    let (constant_right, summands_right) = int_terms(right);
    let (split_left, split_right) = int_combination(&mut atoms, constant_left, &summands_left)
        .split_by_sign(int_combination(&mut atoms, constant_right, &summands_right));
    (
        int_from_combination(split_left),
        int_from_combination(split_right),
    )
}

/// Whether a reduced term is one of the shapes the cancellation reads: a literal, a sum spine, or a product.
pub fn int_shaped(term: &Term) -> bool {
    matches!(
        &**term,
        Subterm::Intrinsic(Intrinsic::Int(_) | Intrinsic::IntAdd(..) | Intrinsic::IntMul(..))
    )
}

#[cfg(test)]
mod tests;
