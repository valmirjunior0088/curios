//! The packed carriers' bitwise vocabulary: the fill and the three pointwise combinations, at both grains and at a length that is not a whole number of bytes.

use super::test_support::folded_matches_runtime;

/// **Every row is tainted in an operand, never in a length.** The pointwise operations state `len(a) == len(b)` as a decided bound, so a tainted length would leave the obligation stuck and the row would not compile at all — the taint goes in a *bit* (`Nat/eql(n, 1)` is a `Bool` the folder cannot see through) or in a fill's generator, both of which leave the length structural. `replicate`'s own count is safe to taint precisely because a fill carries no bound.
///
/// **The partial-byte rows are the ones that matter.** A three-bit run occupies one stored byte with five bits of padding, and `PackedBin`'s equality and hash read stored bytes and trust that padding to be zero. So `xor` against a three-bit fill — which is what `/std`'s `not` will be — is where an unmasked `replicate` would leak: the value would print correctly and still compare unequal to itself. The `eql` row is there to catch exactly that, since `show` alone would not.
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
