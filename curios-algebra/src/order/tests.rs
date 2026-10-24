//! The divisibility reading through a product: what a layer's floor and divisor are, read without distributing, and what two readings hold apart.

use {
    super::*,
    curios_num::{Integer, Natural},
};

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
