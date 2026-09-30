//! Distribution: the product of two sums, every summand of one against every summand of the other.
//!
//! This is the one quadratic step in a sum normal form, which is why its size is stated before it is taken: a caller charges [`distribution_size`] against its budget and only then asks for [`distribute`], so a product the budget cannot afford is refused before a monomial of it exists.

use {
    crate::{Atom, Coefficient, Conclusion, Monomial},
    std::ops::Mul,
};

/// How many products distributing `left` summands over `right` summands forms — the work [`distribute`] does, stated before it does it.
pub fn distribution_size(left: usize, right: usize) -> u64 {
    (left as u64).saturating_mul(right as u64)
}

/// `left · right` distributed in full: every summand of `left` against every summand of `right`, in that order, each product's coefficient the product of the two and its monomial their atoms put in canonical order ([`Monomial::product`]).
///
/// A constant is a summand over no atom, which is how a caller hands one in, and a product over no atom is added into the constant this returns rather than listed. Nothing is merged: the products come back in the order they were formed, a monomial as often as it was formed, for the caller to collect — which is where like monomials meet.
pub fn distribute<C: Coefficient + Mul<Output = C>>(
    left: &[(C, Monomial)],
    right: &[(C, Monomial)],
) -> (C, Vec<(C, Monomial)>) {
    let mut constant = C::zero();
    let mut products = Vec::new();
    for (left_coefficient, left_monomial) in left {
        for (right_coefficient, right_monomial) in right {
            let coefficient = left_coefficient.clone() * right_coefficient.clone();
            let atoms = left_monomial
                .atoms()
                .iter()
                .chain(right_monomial.atoms())
                .copied()
                .collect::<Vec<_>>();
            if atoms.is_empty() {
                constant = constant + coefficient;
                continue;
            }
            products.push((coefficient, Monomial::product(atoms)));
        }
    }
    (constant, products)
}

/// Two monomials of one carrier with their factors paired by identity before anything reads them in order: one coefficient and one multiset of atoms is `Equal`, and one atom left on each side is `Sufficient` over their positions — `left`'s first, then `right`'s. `None` where the coefficients differ, or more than one atom is left on a side, so the caller's shape congruence decides instead.
///
/// **A monomial's factor order is a hash, and a hash is not a value.** An unsolved metavariable ranks as itself and not as the term it is solved to, so `c · ?d · k` against `d · c · k` would pair `c` with `d` read in order, where pairing by identity leaves `?d` against `d` and solves it.
///
/// **Sufficient, never equivalent.** Equal leftovers make equal monomials, and at a zero factor any leftovers do: `x · f = x · g` does not give `f = g`. Two coefficients are two monomials unless a factor is zero, so they decline rather than clash.
pub fn pair_factors<C: PartialEq>(
    left: (&C, &[Atom]),
    right: (&C, &[Atom]),
) -> Option<Conclusion<(usize, usize)>> {
    if left.0 != right.0 {
        return None;
    }
    let mut unmatched = right.1.iter().copied().enumerate().collect::<Vec<_>>();
    let mut residual = Vec::new();
    for (at, atom) in left.1.iter().enumerate() {
        match unmatched
            .iter()
            .position(|(_, candidate)| candidate == atom)
        {
            Some(position) => {
                unmatched.swap_remove(position);
            }
            None => residual.push(at),
        }
    }

    match (residual.as_slice(), unmatched.as_slice()) {
        ([], []) => Some(Conclusion::Equal),
        ([left], [(right, _)]) => Some(Conclusion::Sufficient((*left, *right))),
        _ => None,
    }
}
