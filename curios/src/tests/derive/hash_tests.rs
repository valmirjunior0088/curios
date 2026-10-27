//! Derived `Hash`: the constructor tagged and each payload framed, a struct at ordinal zero, a proof taking no part, and the witness each payload needs.

use crate::tests::{error, run};

// --- The `Hash` derivation: a constructor's ordinal, then its explicit payloads' encodings, every part framed. ---

/// A derived encoding tags the constructor and frames each payload, so no two distinct values share a byte string.
///
/// The rendering below is one dot per byte, decimal. The frame is a four-byte length written least-significant first and zero-padded on its high side, so `leaf(7)` reads as tag `0` — an empty numeral, hence four zero bytes and no payload — followed by the length `1` as `.1.0.0.0` and then the byte `135`: a natural below sixteen encodes as the one byte `0x80` and itself, in the order of the numbers.
#[test]
fn derived_hash_tags_the_constructor_and_frames_each_payload() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Str, Hash, print};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Hash(Tree) { .. }
        let dots(b: Bytes) -> Str =
            Bytes/fold(b, "", (byte, acc) =>
                Str/concat(Str/concat(acc, "."), Nat/to_str(Byte/to_nat(byte))));
        print(Str/join(" ", [
            dots(Hash/hash(Tree/leaf(7))),
            dots(Hash/hash(Tree/node(Tree/leaf(1), Tree/leaf(2))))]))
        "#;

    assert_eq!(
        run(source),
        b".0.0.0.0.1.0.0.0.135 \
.1.0.0.0.1.9.0.0.0.0.0.0.0.1.0.0.0.129.9.0.0.0.0.0.0.0.1.0.0.0.130"
    );
}

/// A struct has one shape, so it encodes at ordinal zero with its fields framed in declaration order.
#[test]
fn a_derived_hash_encodes_a_struct_at_ordinal_zero() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Str, Hash, print};
        struct Point: pub Type { x: Nat, y: Nat }
        satisfy Hash(Point) { .. }
        let dots(b: Bytes) -> Str =
            Bytes/fold(b, "", (byte, acc) =>
                Str/concat(Str/concat(acc, "."), Nat/to_str(Byte/to_nat(byte))));
        print(dots(Hash/hash(Point { x = 1, y = 2 })))
        "#;

    assert_eq!(run(source), b".0.0.0.0.1.0.0.0.129.1.0.0.0.130");
}

/// A proof payload takes no part, and that is the difference from `Spell`: two values differing only in a proof *are* the same value, so an encoding that told them apart would be wrong — and erasure drops the payload, so nothing could produce the bytes at run time.
#[test]
fn a_proof_payload_takes_no_part_in_an_encoding() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Str, Hash, print};
        use /std/Bool/{True, Holds};
        struct Small: pub Type { value: Nat, ok: Holds(Nat/lt(value, 10)) }
        satisfy Hash(Small) { .. }
        let dots(b: Bytes) -> Str =
            Bytes/fold(b, "", (byte, acc) =>
                Str/concat(Str/concat(acc, "."), Nat/to_str(Byte/to_nat(byte))));
        print(dots(Hash/hash(Small { value = 3, ok = True/qed() })))
        "#;

    assert_eq!(run(source), b".0.0.0.0.1.0.0.0.131");
}

/// A payload whose type is a telescope variable asks for that type's own witness, reported against the payload when there is none.
#[test]
fn a_hash_derivation_needs_its_payloads_own_witness() {
    let source = r#"
        use /std/{Hash};
        induct Box(A: Type): pub Type | wrap(A) end
        satisfy (@A: Type) => Hash(Box(A)) { .. }
        /std/print("")
        "#;

    let report = error(source);
    assert!(
        report.contains("Hash"),
        "the report should name the missing witness: {report}"
    );
}
