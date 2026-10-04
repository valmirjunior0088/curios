//! Exact division over a view's atoms: the quotient a linear equation's solution is, and the divisions that have none.

use super::*;

fn atom(index: u32) -> Atom {
    Atom::new(index, u64::from(index))
}

/// A polynomial from its terms, each a coefficient over the indices of its atoms.
fn polynomial(terms: &[(i64, &[u32])]) -> Polynomial {
    terms
        .iter()
        .map(|(scale, atoms)| {
            let mut atoms = atoms.iter().map(|index| atom(*index)).collect::<Vec<_>>();
            atoms.sort_by(|left, right| atom_order(*left, *right));
            (Integer::from(*scale), atoms)
        })
        .collect()
}

/// The terms of a polynomial in one order, so two are compared as polynomials and not as the lists a division happened to build.
fn sorted(mut polynomial: Polynomial) -> Polynomial {
    polynomial.sort_by(|(_, left), (_, right)| monomial_order(left, right));
    polynomial
}

const X: u32 = 0;
const Y: u32 = 1;
const Z: u32 = 2;

#[test]
fn a_distributed_factor_divides_back_out() {
    // `(x * y + x * z) / (y + z) = x`, whichever way the numerator's terms stand.
    let divisor = polynomial(&[(1, &[Y]), (1, &[Z])]);
    for numerator in [
        polynomial(&[(1, &[X, Y]), (1, &[X, Z])]),
        polynomial(&[(1, &[X, Z]), (1, &[X, Y])]),
    ] {
        assert_eq!(divide(numerator, &divisor), Some(polynomial(&[(1, &[X])])));
    }
}

#[test]
fn a_quotient_of_more_than_one_term_is_found() {
    // `(x * y + y + x * z + z) / (y + z) = x + 1`.
    let numerator = polynomial(&[(1, &[X, Y]), (1, &[Y]), (1, &[X, Z]), (1, &[Z])]);
    let divisor = polynomial(&[(1, &[Y]), (1, &[Z])]);
    assert_eq!(
        divide(numerator, &divisor).map(sorted),
        Some(sorted(polynomial(&[(1, &[X]), (1, &[])])))
    );
}

#[test]
fn a_square_divides_by_its_root() {
    // `(x * x + 2 * x * y + y * y) / (x + y) = x + y`.
    let numerator = polynomial(&[(1, &[X, X]), (2, &[X, Y]), (1, &[Y, Y])]);
    let divisor = polynomial(&[(1, &[X]), (1, &[Y])]);
    assert_eq!(
        divide(numerator, &divisor).map(sorted),
        Some(sorted(polynomial(&[(1, &[X]), (1, &[Y])])))
    );
}

#[test]
fn a_literal_divides_where_every_coefficient_is_its_multiple() {
    let two = polynomial(&[(2, &[])]);
    assert_eq!(
        divide(polynomial(&[(2, &[X]), (-4, &[Y])]), &two).map(sorted),
        Some(sorted(polynomial(&[(1, &[X]), (-2, &[Y])])))
    );
    assert_eq!(divide(polynomial(&[(3, &[X])]), &two), None);
}

#[test]
fn a_division_that_leaves_a_remainder_has_no_quotient() {
    // `x * y + z` is no multiple of `y + z`.
    let divisor = polynomial(&[(1, &[Y]), (1, &[Z])]);
    assert_eq!(
        divide(polynomial(&[(1, &[X, Y]), (1, &[Z])]), &divisor),
        None
    );
    // Nor is `x` a multiple of `x * y`.
    assert_eq!(
        divide(polynomial(&[(1, &[X])]), &polynomial(&[(1, &[X, Y])])),
        None
    );
}

#[test]
fn zero_is_every_divisor_s_multiple_and_nothing_divides_by_zero() {
    let divisor = polynomial(&[(1, &[Y])]);
    assert_eq!(divide(Polynomial::new(), &divisor), Some(Polynomial::new()));
    assert_eq!(divide(polynomial(&[(1, &[X])]), &Polynomial::new()), None);
}
