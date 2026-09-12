//! The `Key` law, which a witness must discharge where it is declared. `Map` and `Set` themselves — lookups, the canonical shape and the rewriting functions — are the corpus's `/data/map`.

// The trie's identity is the byte string a key's `Hash` produces, so a colliding encoding does not fail a lookup — it silently merges two keys into one entry. `Key/injective` states that obligation where it can be checked, and an encoding that provably collides cannot discharge it: both constructors here hash to the empty byte string, so the law demands `Eq(a, b)` for values that are not equal. The collision is declared once, in the `Hash` witness — `Key` reaches the encoding through its superclass edge and has no second field to disagree with it — so there is one place for a collision to come from and the law still catches it there.
#[test]
fn a_colliding_key_witness_is_rejected() {
    let source = r#"
        use /std/{Bytes, Map, Hash, Eq};
        pub induct Side : pub Type
        | left()
        | right()
        end
        satisfy Hash(Side) {
            hash(s) = x[],
        }
        satisfy Map/Key(Side) {
            injective(a, b, same) = Eq/refl(),
        }
        /std/print("unreachable")
        "#;

    let message = super::error(source);
    assert!(message.contains("type mismatch"), "{message}");
}
