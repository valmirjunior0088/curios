//! What ℤ's division answers: Euclid's quotient and remainder at every sign of either operand.

use crate::*;

fn int(value: i64) -> Integer {
    Integer::from(value)
}

// The remainder lies in `[0, |d|)` and the pair recombines to the dividend, at both signs of both operands, an exact multiple among them; the quotient is the floor at a positive divisor and the ceiling at a negative one.
#[test]
fn a_division_is_euclids_at_every_sign() {
    for dividend in -9..=9 {
        for divisor in [-4, -3, -1, 1, 2, 3] {
            let (x, d) = (int(dividend), int(divisor));
            let (q, r) = (x.div(&d).unwrap(), x.rem(&d).unwrap());
            assert!(
                int(0) <= r && r < int(divisor.abs()),
                "{dividend} % {divisor} = {r:?}"
            );
            assert_eq!(d.clone() * q.clone() + r, x, "{dividend} / {divisor}");
            let floor = dividend.div_euclid(divisor);
            assert_eq!(q, int(floor), "{dividend} / {divisor}");
        }
    }
    assert_eq!(int(-7).div(&int(2)).unwrap(), int(-4));
    assert_eq!(int(-7).rem(&int(2)).unwrap(), int(1));
    assert_eq!(int(-7).div(&int(-2)).unwrap(), int(4));
    assert_eq!(int(7).div(&int(-2)).unwrap(), int(-3));
}

#[test]
fn a_division_by_zero_traps() {
    assert!(int(-7).div(&int(0)).is_err());
    assert!(int(-7).rem(&int(0)).is_err());
}
