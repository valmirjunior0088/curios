//! The packed carrier's shared vocabulary: the `/sys` fill and pointwise combinations, and the `/std` shifts and rotations derived over them.

use super::test_support::folded_matches_runtime;

/// **Every row is tainted in an operand, never in a length.** The pointwise operations state `len(a) == len(b)` as a decided bound, so a tainted length would leave the obligation stuck and the row would not compile at all — the taint goes in a *byte* (`Nat/to_byte(Nat/and(60 + n, 255))` is a `Byte` the folder cannot see through) or in a fill's generator, both of which leave the length structural. `replicate`'s own count is safe to taint precisely because a fill carries no bound.
#[test]
fn the_packed_bitwise_operations_agree_between_the_folder_and_the_backend() {
    let outputs = folded_matches_runtime(&[
        "Nat/to_str(Byte/to_nat(Option/unwrap_or(Bytes/try_get(/std/Bytes/and(x[240, 15], x[Nat/to_byte(Nat/and(60 + n, 255)), 255]), 0), 0)))",
        "Nat/to_str(Byte/to_nat(Option/unwrap_or(Bytes/try_get(/std/Bytes/replicate(3 + n, Nat/to_byte(Nat/and(0xA5 + n, 255))), 1), 0)))",
    ]);

    assert_eq!(outputs, [b"48".to_vec(), b"165".to_vec()]);
}

/// The derived vocabulary: `not` over the fill, and a rotation over `drop` and `slice`.
///
/// **The count is tainted here, where the operands were tainted above.** A rotation takes its count modulo the width, so a count the folder cannot read exercises `Nat/rem`'s own bound under a guard rather than over a literal, which is the half of these definitions the constant-folded spelling never reaches. Whole bytes move: `rotl` by one puts the last byte first.
#[test]
fn the_derived_shifts_and_rotations_agree_between_the_folder_and_the_backend() {
    let outputs = folded_matches_runtime(&[
        "Nat/to_str(Byte/to_nat(Option/unwrap_or(Bytes/try_get(/std/Bytes/not(x[240, 15]), 0), 0)))",
        "Nat/to_str(Byte/to_nat(Option/unwrap_or(Bytes/try_get(/std/Bytes/rotl(x[1, 2, 3], 1 + n), 0), 0)))",
    ]);

    assert_eq!(outputs, [b"15".to_vec(), b"3".to_vec()]);
}
