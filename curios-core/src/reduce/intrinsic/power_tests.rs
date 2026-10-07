//! `pow` folded: a closed power as `curios-num` computes it, and the node that stands, which is read no further.

use {
    super::test_support::*,
    crate::{Intrinsic, Nat, Term},
    curios_num::Natural,
};

fn pow(base: Term, exponent: Term) -> Term {
    Term::intrinsic(Intrinsic::NatPow(base, exponent))
}

fn mul(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_mul(left, right))
}

fn nat(value: Natural) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

// A closed power folds to what `curios-num` computes: at zero and at one, the bases that decide a power alone, at a base in the i31, at one past it and at one past 64 bits, each to exponents that keep the power in the i31, take it past it and take it past 64 bits. `pow(0, 0)` is one, and zero and one answer at an exponent past the host's word, where no count is read. These are the folds a running program's `pow` has to agree with.
#[test]
fn a_closed_power_folds_to_the_number_crates_power() {
    let bases = [0u128, 1, 2, 3, 10, 1 << 31, (1 << 64) + 1];
    for base in bases.map(Natural::from) {
        for exponent in [0u32, 1, 2, 5, 31, 64, 70] {
            assert_eq!(
                fold(pow(nat(base.clone()), lit(exponent))),
                nat(base.pow(exponent)),
                "pow({base:?}, {exponent})"
            );
        }
    }

    let past = nat(Natural::from(u128::from(u64::MAX) + 1));
    assert_eq!(fold(pow(lit(0), past.clone())), lit(0));
    assert_eq!(fold(pow(lit(1), past)), lit(1));
}

// A base that is not one over one bare monomial stands as written, and nothing is read of its base: a power of a power, a power of a product and a power of a literal that is itself a power are atoms, their laws lemmas, and a product in the exponent is one monomial, no product of powers.
//
// Mutation-checked: a fold that reads a power of a product as the product of the powers makes `pow(x * y, z)` and `pow(x, z) * pow(y, z)` one term.
#[test]
fn a_power_over_one_monomial_stands_and_its_base_is_read_no_further() {
    let (x, y, z) = (sym(0, "x"), sym(1, "y"), sym(2, "z"));

    for atom in [
        pow(x.clone(), y.clone()),
        pow(lit(2), y.clone()),
        pow(lit(0), y.clone()),
        pow(pow(x.clone(), y.clone()), z.clone()),
    ] {
        assert_eq!(fold(atom.clone()), atom);
    }
    assert_ne!(
        fold(pow(mul(x.clone(), y.clone()), z.clone())),
        fold(mul(pow(x.clone(), z.clone()), pow(y.clone(), z.clone()))),
    );
    assert_ne!(
        fold(pow(lit(4), y.clone())),
        fold(mul(pow(lit(2), y.clone()), pow(lit(2), y.clone()))),
    );
    assert_ne!(
        fold(pow(x.clone(), mul(y.clone(), z.clone()))),
        fold(mul(pow(x.clone(), y), pow(x, z))),
    );
}
