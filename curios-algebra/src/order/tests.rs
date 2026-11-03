//! The divisibility reading through a product: what a layer's floor and divisor are, read without distributing, and what two readings hold apart. And the verdict a difference carries through its residual, held to the carrier's values.

use {
    super::*,
    curios_num::{Integer, Natural},
    std::cmp::Ordering,
};

// Soundness gate for the residual: at every verdict of `a` against `b + c`, and every triple of naturals the verdict allows, the verdict carried to `a - b` against `c` allows the triple's own order, with `c` held positive where the carrying asks it. The carrier's monus is the oracle, so the table is held to values rather than restated; a verdict that carries nothing is held to nothing.
#[test]
fn the_residual_carries_a_verdict_its_values_allow() {
    let verdicts = [
        Comparison::Eq,
        Comparison::Lt,
        Comparison::Gt,
        Comparison::Le,
        Comparison::Ge,
        Comparison::Ne,
        Comparison::Stuck,
    ];
    let allows = |verdict: Comparison, ordering: Ordering| match verdict {
        Comparison::Eq => ordering == Ordering::Equal,
        Comparison::Lt => ordering == Ordering::Less,
        Comparison::Gt => ordering == Ordering::Greater,
        Comparison::Le => ordering != Ordering::Greater,
        Comparison::Ge => ordering != Ordering::Less,
        Comparison::Ne => ordering != Ordering::Equal,
        Comparison::Stuck => true,
    };
    let natural = |value: usize| Natural::from(value);
    let mut carried = 0;
    for verdict in verdicts {
        for positive in [false, true] {
            let Some(through) = verdict.through_residual(positive) else {
                continue;
            };
            for a in 0..8 {
                for b in 0..8 {
                    for c in usize::from(positive)..8 {
                        if !allows(verdict, a.cmp(&(b + c))) {
                            continue;
                        }
                        carried += 1;
                        let difference = natural(a).monus(&natural(b));
                        assert!(
                            allows(through, difference.cmp(&natural(c))),
                            "{verdict:?} carried as {through:?} fails at a = {a}, b = {b}, c = {c}"
                        );
                    }
                }
            }
        }
    }
    assert!(
        carried > 0,
        "a grid that carried nothing would hold vacuously"
    );
}

fn literal(value: i32) -> Modular {
    Modular::literal(Integer::from(value))
}

fn times(coefficient: i32, factor: &Modular) -> Modular {
    literal(coefficient).product(factor)
}

// A product's reading is its distribution's: `(2 · a + 2 · c + 1) · (2 · b + 2 · d + 1)` distributes to a sum over the floor `1` whose every coefficient is a multiple of `2`, and the product of the two factors' readings is that pair with nothing distributed.
#[test]
fn a_products_reading_is_its_distributions() {
    let atom = Modular::atom();
    let factor = times(2, &atom).sum(&times(2, &atom)).sum(&literal(1));
    let product = factor.product(&factor);
    assert_eq!(
        product,
        Modular {
            floor: Integer::from(1),
            divisor: Natural::from(2usize),
        }
    );
    let distributed = times(4, &atom).sum(&times(2, &atom)).sum(&literal(1));
    assert_eq!(product, distributed);
}

// Two readings are apart where their floors differ modulo the gcd of their divisors: an odd product against `2`, and `(3 · a + 3) · (b + 2)`, a sum over the floor `6` with every coefficient a multiple of `3`, against `7`; not where the floors agree, the odd product against `3`, nor where a divisor is one; and below zero as above it.
#[test]
fn two_readings_are_apart_where_their_floors_differ_modulo_their_divisors() {
    let atom = Modular::atom();
    let odd = times(2, &atom).sum(&literal(1));
    let product = odd.product(&odd);
    assert!(product.apart(&literal(2)));
    assert!(!product.apart(&literal(3)));
    let threes = times(3, &atom)
        .sum(&literal(3))
        .product(&atom.sum(&literal(2)));
    assert!(threes.apart(&literal(7)));
    assert!(
        !atom.sum(&literal(1)).apart(&literal(2)),
        "a divisor of one divides every floor"
    );
    assert!(
        literal(-3).sum(&times(-4, &atom)).apart(&times(6, &atom)),
        "`-4 · i - 3` is never `6 · j`"
    );
}
