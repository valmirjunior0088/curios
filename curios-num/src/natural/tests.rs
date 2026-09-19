//! The greatest common divisor, against the cases a comparison reads it at.

use crate::Natural;

fn nat(value: u64) -> Natural {
    Natural::from(value)
}

#[test]
fn a_gcd_divides_both_and_zero_is_its_identity() {
    assert_eq!(nat(12).gcd(&nat(18)), nat(6));
    assert_eq!(nat(18).gcd(&nat(12)), nat(6));
    assert_eq!(nat(7).gcd(&nat(13)), nat(1));
    assert_eq!(nat(0).gcd(&nat(5)), nat(5));
    assert_eq!(nat(5).gcd(&nat(0)), nat(5));
    assert_eq!(nat(0).gcd(&nat(0)), nat(0));
}
