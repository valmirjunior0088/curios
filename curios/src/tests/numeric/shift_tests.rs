//! Shift counts cross exactly until they exceed every representable magnitude; only a nonzero left shift past the array's capacity refuses.

use {
    super::test_support::{run_row, table},
    crate::tests::compile,
};

/// Counts at both sides of a limb and the unsigned wire boundary preserve zero and the sign, including counts wider than the wire, which saturate beyond every bit the runtime can hold.
#[test]
fn a_count_past_every_representable_bit_preserves_zero_and_the_sign() {
    let rows = [
        "Nat/to_str(Nat/shr(1180591620717411303424 + n, 4294967295 + n))",
        "Nat/to_str(Nat/shr(1180591620717411303424 + n, 4294967296 + n))",
        "Nat/to_str(Nat/shr(1180591620717411303424 + n, 18446744073709551615 + n))",
        "Nat/to_str(Nat/shr(1180591620717411303424 + n, 18446744073709551616 + n))",
        "Int/to_str(Int/shr(Int/sub(i, 1180591620717411303425), 4294967296 + n))",
        "Int/to_str(Int/shr(Int/sub(i, 1180591620717411303425), 18446744073709551616 + n))",
        "Nat/to_str(Nat/shl(n, 4294967296 + n))",
        "Nat/to_str(Nat/shl(n, 18446744073709551615 + n))",
        "Nat/to_str(Nat/shl(n, 18446744073709551616 + n))",
        "Int/to_str(Int/shl(i, 18446744073709551616 + n))",
    ];
    let compiled = compile(&table(&rows, true)).expect("the shift table compiles");
    let expected: &[&[u8]] = &[
        b"0", b"0", b"0", b"0", b"-1", b"-1", b"0", b"0", b"0", b"+0",
    ];
    for (index, expected) in expected.iter().enumerate() {
        assert_eq!(
            run_row(&compiled, index).expect("the shift runs"),
            *expected,
            "wrong value for {}",
            rows[index]
        );
    }
}

/// The maximum array has `2³² - 1` limbs, or that many times 32 bits. Shifting one by that width needs one extra limb and must refuse before allocating; a sign does not change that bound.
#[test]
fn a_nonzero_result_past_the_limb_array_capacity_refuses_before_allocating_the_result() {
    let rows = [
        "Nat/to_str(Nat/shl(1 + n, 137438953440 + n))",
        "Int/to_str(Int/shl(Int/sub(i, 1), 137438953440 + n))",
        "Nat/to_str(Nat/shl(2147483648 + n, 137438953409 + n))",
        "Nat/to_str(Nat/shl(1 + n, 18446744073709551615 + n))",
        "Nat/to_str(Nat/shl(1 + n, 18446744073709551616 + n))",
    ];
    let compiled = compile(&table(&rows, true)).expect("the shift table compiles");
    for (index, row) in rows.iter().enumerate() {
        let error =
            run_row(&compiled, index).expect_err("the magnitude exceeds the array capacity");
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

/// A count is read from the host-tainted table so neither folder can answer it. Each run retains and shifts a half-gibibyte magnitude, reaching both sides of the old saturation boundary.
#[test]
#[ignore = "requires about 1 GiB of guest heap per run"]
fn a_count_past_one_word_keeps_each_bit_of_the_result() {
    let rows = [
        "Nat/to_str(Nat/shr(Nat/shl(1, 4294967295 + n), 4294967294 + n))",
        "Nat/to_str(Nat/shr(Nat/shl(1, 4294967296 + n), 4294967295 + n))",
        "Nat/to_str(Nat/shr(Nat/shl(1, 4294967295 + n), 4294967296 + n))",
        "Int/to_str(Int/shr(Int/shl(-1, 4294967296 + n), 4294967295 + n))",
        "Int/to_str(Int/shr(Int/shl(+1, 4294967295 + n), 4294967296 + n))",
    ];
    let compiled = compile(&table(&rows, true)).expect("the shift table compiles");
    let expected: &[&[u8]] = &[b"2", b"2", b"0", b"-2", b"+0"];
    for (index, expected) in expected.iter().enumerate() {
        assert_eq!(
            run_row(&compiled, index).expect("the shift runs"),
            *expected,
            "wrong value for {}",
            rows[index]
        );
    }
}
