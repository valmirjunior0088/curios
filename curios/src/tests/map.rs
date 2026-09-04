//! The `Key` law, which a witness must discharge where it is declared. `Map` and `Set` themselves — lookups, the canonical shape and the rewriting functions — are the corpus's `/data/map`.

// The trie's identity is the byte string a `Key` produces, so a colliding encoding does not fail a lookup — it silently merges two keys into one entry. `Key/injective` states that obligation where it can be checked, and a witness whose encoding provably collides cannot discharge it: both constructors here encode to the empty byte string, so the law demands `Eq(a, b)` for values that are not equal.
#[test]
fn a_colliding_key_witness_is_rejected() {
    let source = r#"
        use /std/{Handle, Str, Bytes, Map, Eq};
        pub induct Side : pub Type
        | left()
        | right()
        end
        satisfy Map/Key(Side) {
            to_bytes(s) = x[],
            injective(a, b, same) = Eq/refl(),
        }
        /std/print("unreachable")
        "#;

    let message = super::error(source);
    assert!(message.contains("type mismatch"), "{message}");
}
