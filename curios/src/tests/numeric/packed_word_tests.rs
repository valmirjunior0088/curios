//! The packed carriers' shared vocabulary: the `/sys` fill, pointwise combinations and grain reinterpretation, the `/std` shifts and rotations derived over them, at both grains and at a length that is not a whole number of bytes.

use super::test_support::folded_matches_runtime;

/// **Every row is tainted in an operand, never in a length.** The pointwise operations state `len(a) == len(b)` as a decided bound, so a tainted length would leave the obligation stuck and the row would not compile at all — the taint goes in a *bit* (`Nat/eql(n, 1)` is a `Bool` the folder cannot see through) or in a fill's generator, both of which leave the length structural. `replicate`'s own count is safe to taint precisely because a fill carries no bound.
///
/// **The partial-byte rows are the ones that matter.** A three-bit run occupies one stored byte with five bits of padding, and `Binary`'s equality and hash read stored bytes and trust that padding to be zero. So `xor` against a three-bit fill — which is what `/std`'s `not` will be — is where an unmasked `replicate` would leak: the value would print correctly and still compare unequal to itself. The `eql` row is there to catch exactly that, since `show` alone would not.
#[test]
fn the_packed_bitwise_operations_agree_between_the_folder_and_the_backend() {
    let outputs = folded_matches_runtime(&[
        // A fill, including one whose length crosses a byte boundary and one that is all zeros.
        "/std/Show/show(/std/Bits/replicate(3 + n, 1))",
        "/std/Show/show(/std/Bits/replicate(11 + n, 1))",
        "/std/Show/show(/std/Bits/replicate(5 + n, 0))",
        // The three combinations, over a run whose last bit the folder cannot read.
        "/std/Show/show(/std/Bits/and(b[1, 1, 0, Nat/eql(n, 1)], b[1, 0, 1, 0]))",
        "/std/Show/show(/std/Bits/or(b[1, 1, 0, Nat/eql(n, 1)], b[1, 0, 1, 0]))",
        "/std/Show/show(/std/Bits/xor(b[1, 1, 0, Nat/eql(n, 1)], b[1, 0, 1, 0]))",
        // `not` at three bits: the fill's length is literal so the bound discharges, and its generator is not.
        "/std/Show/show(/std/Bits/xor(b[1, 0, 1], /std/Bits/replicate(3, Nat/eql(n, 0))))",
        // The same value, compared rather than printed — the padding invariant, which printing cannot see.
        "match /std/Bits/eql(/std/Bits/xor(b[1, 0, 1], /std/Bits/replicate(3, Nat/eql(n, 0))), b[0, 1, 0]) | true => \"clean\" | false => \"leaked\" end",
        // `len(replicate(count, x)) = count` symbolically, which is what lets a fill stand under the bound above.
        "Nat/to_str(/std/Bits/len(/std/Bits/replicate(11 + n, 1)))",
        // The byte grain, where every length is already a whole number of bytes.
        "Nat/to_str(Byte/to_nat(Option/unwrap_or(Bytes/try_get(/std/Bytes/and(x[240, 15], x[Nat/to_byte(Nat/and(60 + n, 255)), 255]), 0), 0)))",
        "Nat/to_str(Byte/to_nat(Option/unwrap_or(Bytes/try_get(/std/Bytes/replicate(3 + n, Nat/to_byte(Nat/and(0xA5 + n, 255))), 1), 0)))",
    ]);

    assert_eq!(
        outputs,
        [
            b"111".to_vec(),
            b"11111111111".to_vec(),
            b"00000".to_vec(),
            b"1000".to_vec(),
            b"1110".to_vec(),
            b"0110".to_vec(),
            b"010".to_vec(),
            b"clean".to_vec(),
            b"11".to_vec(),
            b"48".to_vec(),
            b"165".to_vec(),
        ]
    );
}

/// The derived vocabulary: `not` over the fill, and the four movements over `drop`, `slice` and the fill.
///
/// **The counts are tainted here, where the operands were tainted above.** A shift guards on `count <= len`, so a count the folder cannot read turns that guard into a real branch — which is the half of these definitions the constant-folded spelling never reaches. The rotations take their count modulo the width, so a tainted one also exercises `Nat/rem`'s own bound under a guard rather than over a literal.
///
/// `not` at three bits is the row that ties this suite to the one above: `/std` writes it as an `xor` against a fill of the operand's length, so it is the composition where an unmasked fill would leak padding — now through the library's own definition rather than a hand-spelled one.
#[test]
fn the_derived_shifts_and_rotations_agree_between_the_folder_and_the_backend() {
    let outputs = folded_matches_runtime(&[
        "/std/Show/show(/std/Bits/not(b[1, 1, 0, Nat/eql(n, 1)]))",
        "/std/Show/show(/std/Bits/shl(b[1, 1, 0, 0], 1 + n))",
        "/std/Show/show(/std/Bits/shr(b[1, 1, 0, 0], 1 + n))",
        "/std/Show/show(/std/Bits/rotl(b[1, 1, 0, 0], 1 + n))",
        "/std/Show/show(/std/Bits/rotr(b[1, 1, 0, 0], 1 + n))",
        // A count past the width answers zeros at the width, not a shorter run.
        "/std/Show/show(/std/Bits/shl(b[1, 1, 0, 0], 99 + n))",
        // A rotation by the whole width is the identity, which is what says the modulo is really there.
        "/std/Show/show(/std/Bits/rotl(b[1, 1, 0, 0], 4 + n))",
        // The empty run has no width to take a count modulo.
        "/std/Show/show(/std/Bits/rotl(b[], 3 + n))",
        // `not` at a partial byte, compared rather than printed.
        "match /std/Bits/eql(/std/Bits/not(b[1, 0, Nat/eql(n, 0)]), b[0, 1, 0]) | true => \"clean\" | false => \"leaked\" end",
        // The byte grain moves whole bytes: `rotl` by one puts the last byte first.
        "Nat/to_str(Byte/to_nat(Option/unwrap_or(Bytes/try_get(/std/Bytes/not(x[240, 15]), 0), 0)))",
        "Nat/to_str(Byte/to_nat(Option/unwrap_or(Bytes/try_get(/std/Bytes/rotl(x[1, 2, 3], 1 + n), 0), 0)))",
    ]);

    assert_eq!(
        outputs,
        [
            b"0011".to_vec(),
            b"0110".to_vec(),
            b"1000".to_vec(),
            b"0110".to_vec(),
            b"1001".to_vec(),
            b"0000".to_vec(),
            b"1100".to_vec(),
            b"".to_vec(),
            b"clean".to_vec(),
            b"15".to_vec(),
            b"3".to_vec(),
        ]
    );
}

/// Reading one run at the other grain, in both directions and across the three shapes the fold distinguishes.
///
/// **The window row is the one that earns its place.** Sixteen bits starting at bit four hold a whole number of bytes at an offset that is not one — the only shape whose payload cannot be shared, so it is the only one that reaches the repack arm. Every other row here takes the retag.
///
/// The bound is not tested from this side: a run that is not a whole number of bytes does not elaborate at all, so its refusal is a compile-time fact rather than a row.
#[test]
fn reading_a_run_at_the_other_grain_agrees_between_the_folder_and_the_backend() {
    let outputs = folded_matches_runtime(&[
        "/std/Show/show(/std/Bytes/to_bits(x[Nat/to_byte(Nat/and(5 + n, 255))]))",
        "Nat/to_str(/std/Bits/len(/std/Bytes/to_bits(x[1, 2, Nat/to_byte(Nat/and(3 + n, 255))])))",
        // Round trip: the byte that went in comes back.
        "Nat/to_str(Byte/to_nat(Option/unwrap_or(Bytes/try_get(/std/Bits/to_bytes(/std/Bytes/to_bits(x[Nat/to_byte(Nat/and(7 + n, 255)), 9])), 0), 0)))",
        // The repack arm: a byte-sized window at a bit offset that is not byte-aligned.
        "Nat/to_str(Byte/to_nat(Option/unwrap_or(Bytes/try_get(/std/Bits/to_bytes(/std/Bits/slice(/std/Bytes/to_bits(x[Nat/to_byte(Nat/and(255 + n, 255)), 0, 170]), 4, 16)), 0), 0)))",
        // The empty run crosses in both directions.
        "Nat/to_str(Bytes/len(/std/Bits/to_bytes(/std/Bytes/to_bits(x[]))))",
    ]);

    assert_eq!(
        outputs,
        [
            b"10100000".to_vec(),
            b"24".to_vec(),
            b"7".to_vec(),
            b"15".to_vec(),
            b"0".to_vec(),
        ]
    );
}
