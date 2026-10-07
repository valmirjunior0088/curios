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

// A power within its allowance is the power, and one past it is declined before it is built: a power never holds more bits than its base's times its exponent. Zero and one are answered at every exponent, one past every machine word among them, since neither grows.
#[test]
fn a_power_is_declined_past_its_allowance_and_answered_at_zero_and_one() {
    let huge = Natural::from(u128::MAX);

    assert_eq!(
        Natural::from(3u32).pow_within(&Natural::from(5u32), 64),
        Some(Natural::from(243u32))
    );
    assert_eq!(
        Natural::from(3u32).pow_within(&Natural::from(40u32), 64),
        None
    );
    assert_eq!(Natural::from(2u32).pow_within(&huge, u64::MAX), None);
    assert_eq!(Natural::zero().pow_within(&huge, 0), Some(Natural::zero()));
    assert_eq!(Natural::one().pow_within(&huge, 0), Some(Natural::one()));
    assert_eq!(
        Natural::zero().pow_within(&Natural::zero(), 0),
        Some(Natural::one())
    );
    assert_eq!(huge.pow_within(&Natural::zero(), 0), Some(Natural::one()));
}
