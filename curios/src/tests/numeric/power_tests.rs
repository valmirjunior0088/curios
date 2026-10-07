//! A power's exponent crosses exactly, a base of zero or one answers at every exponent, and only a base at least two raised past the limb array's capacity refuses.

use {
    super::test_support::{run_row, table},
    crate::tests::compile,
};

/// An exponent wider than the wire saturates beyond every bit the runtime can hold, and a base of zero or one still answers there, as an exponent of zero does under a base of any size: none of those powers grows.
#[test]
fn a_base_of_zero_or_one_answers_at_an_exponent_past_every_representable_bit() {
    let rows = [
        "Nat/to_str(Nat/pow(n, 137438953472 + n))",
        "Nat/to_str(Nat/pow(n, 18446744073709551616 + n))",
        "Nat/to_str(Nat/pow(1 + n, 18446744073709551616 + n))",
        "Nat/to_str(Nat/pow(18446744073709551616 + n, n))",
    ];
    let compiled = compile(&table(&rows, true)).expect("the power table compiles");
    let expected: &[&[u8]] = &[b"0", b"0", b"1", b"1"];
    for (index, expected) in expected.iter().enumerate() {
        assert_eq!(
            run_row(&compiled, index).expect("the power runs"),
            *expected,
            "wrong value for {}",
            rows[index]
        );
    }
}

/// A base at least two raised to a count holds more than that many bits, so a count at the array's `2³² - 1` limbs of 32 bits needs one limb more and refuses before a product is taken, whatever the base's size.
#[test]
fn a_power_past_the_limb_array_capacity_refuses_before_multiplying() {
    let rows = [
        "Nat/to_str(Nat/pow(2 + n, 137438953440 + n))",
        "Nat/to_str(Nat/pow(18446744073709551616 + n, 137438953472 + n))",
        "Nat/to_str(Nat/pow(2 + n, 18446744073709551615 + n))",
        "Nat/to_str(Nat/pow(3 + n, 18446744073709551616 + n))",
    ];
    let compiled = compile(&table(&rows, true)).expect("the power table compiles");
    for (index, row) in rows.iter().enumerate() {
        let error = run_row(&compiled, index).expect_err("the power exceeds the array capacity");
        assert!(
            error.contains("panicked: a value is past what the runtime can hold"),
            "wrong refusal for {row}: {error}"
        );
        assert!(
            error.contains("2^32 - 1 elements"),
            "the refusal names the array's limit: {error}"
        );
    }
}
