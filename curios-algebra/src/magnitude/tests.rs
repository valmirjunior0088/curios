use super::*;

const COEFFICIENTS: [i32; 8] = [-6, -4, -3, -1, 2, 4, 6, 9];
const VALUES: [i64; 5] = [-5, -1, 0, 1, 7];

fn scale(coefficients: &[i32]) -> Scale {
    let coefficients = coefficients
        .iter()
        .map(|coefficient| Integer::from(*coefficient))
        .collect::<Vec<_>>();
    Scale::of(&coefficients).expect("no coefficient is zero")
}

/// A scale's divisor as a machine integer.
fn divisor(scale: &Scale) -> i64 {
    let factor = i64::try_from(u64::try_from(&scale.factor).expect("a small factor"))
        .expect("a small factor");
    match scale.negated {
        true => -factor,
        false => factor,
    }
}

// A combination is its scale's divisor times what is left: at every three coefficients of a grid and every value of its two monomials, each coefficient is an exact multiple of the divisor, and the combination's absolute value is the factor times what is left's and its sign what is left's, flipped where the scale negates. What is left shares no literal factor and leads with a coefficient above zero, so reading it again changes nothing; and a combination and its negation leave the same thing.
//
// Mutation-checked: a scale that negates only where every coefficient is below zero leaves `2 - x` and `-2 + x` two readings.
#[test]
fn a_combination_is_its_scale_over_what_is_left() {
    for constant in COEFFICIENTS {
        for first in COEFFICIENTS {
            for second in COEFFICIENTS {
                let coefficients = [constant, first, second];
                let scale = scale(&coefficients);
                let divisor = divisor(&scale);
                let left = coefficients.map(|coefficient| {
                    assert_eq!(i64::from(coefficient) % divisor, 0, "{coefficients:?}");
                    i64::from(coefficient) / divisor
                });
                let rest = left.map(|coefficient| i32::try_from(coefficient).expect("no larger"));
                assert!(
                    self::scale(&rest).is_unit(),
                    "{coefficients:?} leaves {rest:?}"
                );
                let opposite = coefficients.map(|coefficient| -coefficient);
                let negation = self::scale(&opposite);
                assert_eq!(negation.negated, !scale.negated, "{coefficients:?}");
                assert_eq!(
                    opposite.map(|coefficient| i64::from(coefficient) / self::divisor(&negation)),
                    left,
                    "{coefficients:?} and its negation leave two readings"
                );

                for x in VALUES {
                    for y in VALUES {
                        let value =
                            |[constant, first, second]: [i64; 3]| constant + first * x + second * y;
                        let whole = value(coefficients.map(i64::from));
                        let reading = value(left);
                        assert_eq!(whole, divisor * reading);
                        assert_eq!(whole.abs(), divisor.abs() * reading.abs());
                        assert_eq!(whole.signum(), divisor.signum() * reading.signum());
                    }
                }
            }
        }
    }
}

// The zero combination has no scale, and a combination that shares no factor and is not all below zero is read as it stands.
#[test]
fn the_zero_combination_has_no_scale_and_a_primitive_one_the_unit() {
    assert_eq!(Scale::of(&[]), None);
    assert!(scale(&[3, -2]).is_unit());
    assert!(scale(&[1]).is_unit());
    assert_eq!(
        scale(&[-4, 6]),
        Scale {
            factor: Natural::from(2u32),
            negated: true,
        }
    );
}
