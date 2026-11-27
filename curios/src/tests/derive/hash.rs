//! Derived `Hash`: the constructor's position where there are several, each payload but the last delimited, a struct with no position, a proof taking no part, the order the encodings stand in, and the witness each payload needs.

use crate::test_support::*;

// --- The `Hash` derivation: a constructor's position, then its explicit payloads' encodings, every one but the last delimited. ---

/// A derived encoding writes the constructor's position and then its payloads, every one but the last delimited, so no two distinct values share a byte string.
///
/// The rendering below is one dot per byte, decimal. A natural below sixteen encodes as the one byte `0x80` and itself, so `leaf(7)` reads as its position `0`, the byte `128`, and then `135`. A delimited part has each zero byte written `0, 255` and is closed by `0, 1`, so `node`'s first payload, `leaf(1)`, reads as `128.129` closed by `.0.1`, and its last as it stands.
#[test]
fn derived_hash_writes_the_position_and_delimits_every_payload_but_the_last() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Str, Hash, print};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Hash(Tree) { .. }
        let dots(b: Bytes) -> Str =>
            Bytes/fold(b, "", (byte, acc) =>
                Str/concat(Str/concat(acc, "."), Nat/to_str(Byte/to_nat(byte))));
        print(Str/join(" ", [
            dots(Hash/hash(Tree/leaf(7))),
            dots(Hash/hash(Tree/node(Tree/leaf(1), Tree/leaf(2))))]))
        "#;

    assert_eq!(run(source), b".128.135 .129.128.129.0.1.128.130");
}

/// A struct has one shape, so it writes no position, its fields in declaration order, every one but the last delimited.
#[test]
fn a_derived_hash_writes_no_position_for_a_struct() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Str, Hash, print};
        struct Point: pub Type { x: Nat, y: Nat }
        satisfy Hash(Point) { .. }
        let dots(b: Bytes) -> Str =>
            Bytes/fold(b, "", (byte, acc) =>
                Str/concat(Str/concat(acc, "."), Nat/to_str(Byte/to_nat(byte))));
        print(dots(Hash/hash(Point { x = 1, y = 2 })))
        "#;

    assert_eq!(run(source), b".129.0.1.130");
}

/// A derived encoding stands in the order the derived `Ord` puts its values in, a text that begins a longer one below it whatever follows: framed by their lengths, `("b", 1)` would encode below `("ab", 1)`, which it is above.
#[test]
fn a_derived_encoding_orders_as_the_derived_order_does() {
    let source = r#"
        use /std/{Nat, Bytes, Str, Hash, Ordering, print};
        use /std/ops/{Eql, Ord};
        struct Pair: pub Type { s: Str, n: Nat }
        satisfy Eql(Pair) { .. }
        satisfy Ord(Pair) { .. }
        satisfy Hash(Pair) { .. }
        let shown(o: Ordering) -> Str => match o | lt() => "lt" | eq() => "eq" | gt() => "gt" end;
        let both(a: Pair, b: Pair) -> Str =>
            Str/concat(shown(Ord/ord(a, b)), shown(Bytes/ord(Hash/hash(a), Hash/hash(b))));
        print(Str/join(" ", [
            both(Pair { s = "b", n = 1 }, Pair { s = "ab", n = 1 }),
            both(Pair { s = "a", n = 5 }, Pair { s = "a\u{0}", n = 0 }),
            both(Pair { s = "a", n = 1 }, Pair { s = "a", n = 2 })]))
        "#;

    assert_eq!(run(source), b"gtgt ltlt ltlt");
}

/// A proof payload takes no part, and that is the difference from `Spell`: two values differing only in a proof *are* the same value, so an encoding that told them apart would be wrong — and erasure drops the payload, so nothing could produce the bytes at run time.
#[test]
fn a_proof_payload_takes_no_part_in_an_encoding() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Str, Hash, print};
        use /std/Bool/{True, Holds};
        struct Small: pub Type { value: Nat, ok: Holds(Nat/lt(value, 10)) }
        satisfy Hash(Small) { .. }
        let dots(b: Bytes) -> Str =>
            Bytes/fold(b, "", (byte, acc) =>
                Str/concat(Str/concat(acc, "."), Nat/to_str(Byte/to_nat(byte))));
        print(dots(Hash/hash(Small { value = 3, ok = True/qed() })))
        "#;

    assert_eq!(run(source), b".131");
}

/// A payload whose type is a telescope variable asks for that type's own witness, reported against the payload when there is none.
#[test]
fn a_hash_derivation_needs_its_payloads_own_witness() {
    let source = r#"
        use /std/{Hash};
        induct Box(A: Type): pub Type | wrap(A) end
        satisfy (@A: Type) -> Hash(Box(A)) { .. }
        /std/print("")
        "#;

    let report = error(source);
    assert!(
        report.contains("Hash"),
        "the report should name the missing witness: {report}"
    );
}
