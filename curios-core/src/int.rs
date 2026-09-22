//! The signed carrier's sum normal form: a literal constant over a linear combination of monomials with integer coefficients — [`Nat`]'s form over ℤ, where a subtraction is a negative coefficient rather than a stuck node and a negation is the coefficient `-1`.
//!
//! **Spelled in the nodes the roster already has**, so nothing below reduction meets a new shape: `IntAdd(inner, Int(k))` carries the constant `k` over `inner` and is absent when `k` is zero; `inner` is a left-nested `IntAdd` spine of summands, each `IntMul(Int(c), monomial)` or the bare monomial when `c` is one; a monomial is a left-nested `IntMul` of factors in structural-hash order, as `Nat::product` orders them, so `i · j` and `j · i` are one term. Summands keep first-appearance order, as `Nat::linear` keeps it, so `i + j` and `j + i` are two spellings of one combination — decided by cancellation at the peel, never by reordering in the fold, for the reason `Nat::cancel_common` records: a rebuilt sum is a different term, and a stuck comparison rebuilt from one would never be found again.
//!
//! **A product of two symbolic sums stays stuck until a comparison asks**, the decision `documentation/design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md` measured for `Nat` and applies here verbatim: [`int_product`] distributes when either operand is a constant or a single summand, and [`int_normalize`] distributes the rest on demand.
//!
//! Every function here is total over reduced terms it does not recognize, reading anything that is not an `IntAdd`, `IntMul` or `Int` literal as an opaque monomial factor, which is what makes `i - i` fold to `0` for a symbolic `i` while `f(i)` stays the symbol it is.

use {
    super::{Cost, Intrinsic, Nat, Recombination, ReduceError, Reducer, Subterm, Term},
    curios_num::{Integer, Natural},
    curios_utilities::recurse,
    std::collections::HashMap,
};

/// One monomial with its coefficient: the factors are already in canonical order, which is what lets two monomials be compared as vectors.
type Monomial = (Integer, Vec<Term>);

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

/// `summands` as a linear combination: like monomials merged by adding their coefficients, in first-appearance order, keyed up to universe instances as `Nat::linear` keys, and a monomial whose coefficient cancelled to zero dropped.
pub(crate) fn int_linear(summands: impl IntoIterator<Item = Term>) -> Vec<Monomial> {
    let mut combination: Vec<Monomial> = Vec::new();
    let mut index_of: HashMap<Vec<Term>, usize> = HashMap::new();
    for summand in summands {
        let (coefficient, factors) = int_monomial(&summand);
        if coefficient.is_zero() {
            continue;
        }
        let key = factors
            .iter()
            .map(crate::project_erased_universes)
            .collect::<Vec<_>>();
        match index_of.get(&key) {
            Some(&index) => combination[index].0 = combination[index].0.clone() + coefficient,
            None => {
                index_of.insert(key, combination.len());
                combination.push((coefficient, factors));
            }
        }
    }
    combination.retain(|(coefficient, _)| !coefficient.is_zero());
    combination
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
        Some(spine) if coefficient == one() => spine,
        Some(spine) => Term::intrinsic(Intrinsic::IntMul(literal(coefficient), spine)),
    }
}

/// A combination and a constant written back into the normal form every reader here reads: summands folded left to right, the constant last and absent when zero, a combination with nothing in it the constant alone.
pub(crate) fn int_from_linear(constant: Integer, combination: Vec<Monomial>) -> Term {
    let inner = combination
        .into_iter()
        .map(|(coefficient, factors)| int_scaled(coefficient, &factors))
        .reduce(|left, right| Term::intrinsic(Intrinsic::IntAdd(left, right)));
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
    let (constant, combination) = int_recombine(constant, int_linear(summands));
    int_from_linear(constant, combination)
}

/// Euclid's identity over ℤ, `Nat::recombine`'s twin: `k` remainders `x % d` beside `k` copies of `d · (x / d)` are `k · x`. Truncated division satisfies the identity exactly as flooring does, for every `x` and every nonzero `d`, so it is unconditional here too.
///
/// **A copy counts only where every coefficient agrees in sign with it.** A group lets any combination be rewritten around `x`, but a recombination that left a negative remainder of a multiple behind would trade one spelling for a longer one; taking only the copies the combination actually holds is what keeps the rewrite a shrinking one, and keeps `x - d · (x / d)` and `x % d` the two terms they were — an incompleteness, never a false equation.
fn int_recombine(
    mut constant: Integer,
    mut combination: Vec<Monomial>,
) -> (Integer, Vec<Monomial>) {
    while let Some(Recombination {
        dividend,
        copies,
        spent,
    }) = int_euclid_pair(&combination)
    {
        for (index, amount) in spent {
            combination[index].0 = combination[index].0.clone() - amount;
        }
        let (dividend_constant, dividend_summands) = int_terms(&dividend);
        constant = constant + copies.clone() * dividend_constant;
        let mut summands = combination
            .iter()
            .map(|(coefficient, factors)| int_scaled(coefficient.clone(), factors))
            .collect::<Vec<_>>();
        summands.extend(
            int_linear(dividend_summands)
                .into_iter()
                .map(|(coefficient, factors)| int_scaled(copies.clone() * coefficient, &factors)),
        );
        combination = int_linear(summands);
    }
    (constant, combination)
}

/// The first remainder in `combination` held beside at least one copy of its multiple, signs agreeing: the dividend it recombines to, the signed number of copies, and what each monomial gives up.
fn int_euclid_pair(combination: &[Monomial]) -> Option<Recombination<Integer>> {
    combination
        .iter()
        .enumerate()
        .find_map(|(index, (held, factors))| {
            let [factor] = factors.as_slice() else {
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

            let positive = *held > zero();
            let mut copies = held.magnitude();
            let mut matched = Vec::new();
            for (per_copy, monomial) in int_linear(multiple) {
                let (at, (available, _)) = combination
                    .iter()
                    .enumerate()
                    .find(|(_, (_, factors))| int_same_factors(factors, &monomial))?;
                if (*available > zero()) != (positive == (per_copy > zero())) {
                    return None;
                }
                copies = copies.min(available.magnitude() / per_copy.magnitude());
                matched.push((at, per_copy));
            }
            if copies.is_zero() {
                return None;
            }

            let copies = match positive {
                true => Integer::from(copies),
                false => -Integer::from(copies),
            };
            let mut spent = vec![(index, copies.clone())];
            spent.extend(
                matched
                    .into_iter()
                    .map(|(at, per_copy)| (at, per_copy * copies.clone())),
            );
            Some(Recombination {
                dividend: dividend.clone(),
                copies,
                spent,
            })
        })
}

/// Whether two monomials' factors are one multiset, a quotient matching a quotient on its dividend and divisor alone, as `Nat::same_monomial` compares them.
fn int_same_factors(left: &[Term], right: &[Term]) -> bool {
    let project = crate::project_erased_universes::<Term>;
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
        ) => {
            project(left_dividend) == project(right_dividend)
                && project(left_divisor) == project(right_divisor)
        }
        _ => project(left) == project(right),
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
pub fn int_of_nat(nat: &Term) -> Term {
    recurse(|| match &**nat {
        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => literal(zero()),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(floor, inner))) => {
            int_sum(&int_of_nat(inner), &literal(Integer::from(floor.clone())))
        }
        Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) => {
            int_sum(&int_of_nat(left), &int_of_nat(right))
        }
        Subterm::Intrinsic(Intrinsic::NatMul(left, right)) => {
            int_product(&int_of_nat(left), &int_of_nat(right))
        }
        _ => Term::intrinsic(Intrinsic::NatToInt(nat.clone())),
    })
}

/// The natural a reduced `Int` is the image of when it is one by construction: a non-negative constant over monomials with positive coefficients whose every factor is a widened natural. `None` for anything else — a negative coefficient, or a factor that is not widened, can make the value negative, and this reads no bound.
///
/// The inverse of [`int_of_nat`] on its image, rebuilt in `Nat`'s normal form. Two readers: `Int/to_nat`, whose inversion arm is the one-atom case of this, and the comparison, which decides a pair of such terms by comparing their preimages — ℕ → ℤ preserves and reflects order.
pub fn int_preimage(term: &Term) -> Option<Term> {
    let (constant, summands) = int_terms(term);
    let floor = Natural::try_from(&constant).ok()?;
    let mut preimages = Vec::new();
    for (coefficient, factors) in int_linear(summands) {
        let coefficient = Natural::try_from(&coefficient).ok()?;
        let mut product = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
        for factor in &factors {
            let Subterm::Intrinsic(Intrinsic::NatToInt(nat)) = &**factor else {
                return None;
            };
            product = Nat::multiply(&product, nat);
        }
        preimages.push(Nat::scaled(coefficient, product));
    }
    Some(Nat::sum_over_floor(preimages, floor))
}

/// `-term`, in normal form: every coefficient and the constant negated. What `IntSub` folds through, so no subtraction node survives reduction.
pub fn int_negate(term: &Term) -> Term {
    let (constant, summands) = int_terms(term);
    let combination = int_linear(summands)
        .into_iter()
        .map(|(coefficient, factors)| (-coefficient, factors))
        .collect();
    int_from_linear(-constant, combination)
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
    let left_monomials = int_monomials(left);
    let right_monomials = int_monomials(right);
    let mut constant = zero();
    let mut summands = Vec::new();
    for (coefficient_left, factors_left) in &left_monomials {
        for (coefficient_right, factors_right) in &right_monomials {
            let coefficient = coefficient_left.clone() * coefficient_right.clone();
            let mut factors = factors_left.clone();
            factors.extend(factors_right.iter().cloned());
            if factors.is_empty() {
                constant = constant + coefficient;
                continue;
            }
            factors.sort_by_key(Term::structural_hash);
            summands.push(int_scaled(coefficient, &factors));
        }
    }
    int_merged(constant, summands)
}

/// The product as the fold takes it: distributed when either operand is a constant or a single monomial, and left as a stuck `IntMul` of the two reduced operands otherwise — the same line `NatMul` draws, and [`int_normalize`] is what crosses it on demand.
pub fn int_product(left: &Term, right: &Term) -> Term {
    let single = |term: &Term| int_monomials(term).len() <= 1;
    match single(left) || single(right) {
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
                if int_monomials(left).len() > 1 && int_monomials(right).len() > 1 {
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
    int_normalize_within(reducer, term, &mut memo)
}

fn int_normalize_within(
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
            Subterm::Intrinsic(Intrinsic::IntMul(left, right)) => {
                let left = int_normalize_within(reducer, left.clone(), memo)?;
                let right = int_normalize_within(reducer, right.clone(), memo)?;
                let products = (int_monomials(&left).len() as u64)
                    .saturating_mul(int_monomials(&right).len() as u64);
                reducer.spend(
                    Cost::collection(products)
                        .saturating_add(Cost::term(2).saturating_mul(products)),
                )?;
                int_multiply(&left, &right)
            }
            Subterm::Intrinsic(Intrinsic::IntAdd(..)) => {
                let (constant, summands) = int_terms(&reduced);
                let summands = summands
                    .into_iter()
                    .map(|summand| int_normalize_within(reducer, summand, memo))
                    .collect::<Result<Vec<_>, _>>()?;
                int_merged(constant, summands)
            }
            _ => reduced,
        };
        memo.insert(key, (term, result.clone()));
        Ok(result)
    })
}

/// Strip what two reduced terms carry in common, so residuals decide where the originals could not — `Nat::cancel_common` over a group, where every term moves to whichever side keeps its coefficient positive: `i + 2 · j - k` against `j` becomes `i + j` against `k`, and a pair that differs in nothing becomes `0` against `0`. A pair sharing no monomial and at most one nonzero constant is handed back untouched, for the stability `Nat::cancel_common` records: a rebuilt sum is a different term, and a stuck comparison rebuilt from one would never be found again.
///
/// Sound for every reader because ℤ under `+` is a group: every order relation and equality reads through `a ⋈ b` iff `a - b ⋈ 0`, and splitting the difference by sign is only adding one term to both sides of that.
pub fn int_cancel_common(left: &Term, right: &Term) -> (Term, Term) {
    let (constant_left, summands_left) = int_terms(left);
    let (constant_right, summands_right) = int_terms(right);
    let combination_left = int_linear(summands_left);
    let combination_right = int_linear(summands_right);

    let key = |factors: &[Term]| {
        factors
            .iter()
            .map(crate::project_erased_universes)
            .collect::<Vec<_>>()
    };
    let shared_monomial = combination_left.iter().any(|(_, factors)| {
        let wanted = key(factors);
        combination_right
            .iter()
            .any(|(_, candidate)| key(candidate) == wanted)
    });
    if !shared_monomial && (constant_left.is_zero() || constant_right.is_zero()) {
        return (left.clone(), right.clone());
    }

    int_split(
        constant_left - constant_right,
        combination_left,
        combination_right,
    )
}

/// The difference of two reduced terms split by sign for every pair, where [`int_cancel_common`] splits it only once something cancels: every monomial on the side that keeps its coefficient positive, the constant likewise, so two pairs with one difference are one pair — `0 < j - i` and `i < j`, `-i < -j` and `j < i`.
///
/// **Conversion's spelling, never the fold's.** A stuck comparison is what a guard refines on, and a refinement is keyed on the guard's written spelling; a fold that split every comparison would take each later occurrence past its own key, the failure `documentation/design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md` records for swapped operands. So the one reader is `align_comparisons`, probe-side, where respelling records nothing.
pub fn int_split_by_sign(left: &Term, right: &Term) -> (Term, Term) {
    let (constant_left, summands_left) = int_terms(left);
    let (constant_right, summands_right) = int_terms(right);
    int_split(
        constant_left - constant_right,
        int_linear(summands_left),
        int_linear(summands_right),
    )
}

/// `left - right` over a constant, split by sign into the two sides it is spelled as.
fn int_split(
    constant: Integer,
    combination_left: Vec<Monomial>,
    combination_right: Vec<Monomial>,
) -> (Term, Term) {
    let mut difference = combination_left;
    difference.extend(
        combination_right
            .into_iter()
            .map(|(coefficient, factors)| (-coefficient, factors)),
    );
    let difference = int_linear(
        difference
            .into_iter()
            .map(|(coefficient, factors)| int_scaled(coefficient, &factors)),
    );

    let mut kept_left = Vec::new();
    let mut kept_right = Vec::new();
    for (coefficient, factors) in difference {
        match coefficient > zero() {
            true => kept_left.push((coefficient, factors)),
            false => kept_right.push((-coefficient, factors)),
        }
    }
    match constant > zero() {
        true => (
            int_from_linear(constant, kept_left),
            int_from_linear(zero(), kept_right),
        ),
        false => (
            int_from_linear(zero(), kept_left),
            int_from_linear(-constant, kept_right),
        ),
    }
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
