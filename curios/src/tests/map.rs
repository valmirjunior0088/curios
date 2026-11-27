//! The `Hash/Key` laws, which a witness must discharge where it is declared. `Map` and `Set` themselves — lookups, the canonical shape and the rewriting functions — are `/std/Map`'s own tests'.

use crate::test_support::*;

// The trie's identity is the byte string a key's `Hash` produces, so a colliding encoding does not fail a lookup — it silently merges two keys into one entry. The key's one law refuses it where it is declared: an order holds two values apart unless they are one (`Ord/eq`), and two values of one encoding tie in the encodings' order, so `mono` is false at any two distinct keys of one encoding, which is why injectivity is a lemma of it (`Hash/Key/inj`). Both constructors here hash to the empty byte string, so the law at `left` and `right` asks that `lt` be `eq`. The collision is declared once, in the `Hash` witness — `Hash/Key` reaches the encoding through its superclass edge and has no second field to disagree with it — so there is one place for a collision to come from and the law catches it there.
#[test]
fn a_colliding_key_witness_is_rejected() {
    let source = r#"
        use /std/{Bytes, Map, Hash, Eq};
        use /std/ops/{Eql, Ord};
        pub induct Side : pub Type
        | left()
        | right()
        end
        satisfy Eql(Side) { .. }
        satisfy Ord(Side) { .. }
        satisfy Hash(Side) {
            hash(s) => 0x[],
        }
        satisfy Hash/Key(Side) {
            mono(a, b) =>
                match a
                | left() => match b | left() => Eq/refl() | right() => Eq/refl() end
                | right() => match b | left() => Eq/refl() | right() => Eq/refl() end
                end,
        }
        /std/print("unreachable")
        "#;

    let message = error(source);
    assert!(message.contains("type mismatch"), "{message}");
    assert!(
        message.contains("expected: `Eq(@/std/Ordering)(/std/Ordering/lt(), /std/Ordering/eq())`"),
        "{message}"
    );
}

// A walk of the trie is in the order of the encodings, so an encoding out of its type's order does not fail a lookup either — it enumerates a map's keys in an order no reader of the type expects. `Hash/Key/mono` states that obligation: the order of two keys is `Bytes/ord` of their encodings. `left` is below `right` by their declaration and its byte is above `right`'s, and the law is refused at the one pair it is false of.
#[test]
fn a_key_witness_out_of_its_order_is_rejected() {
    let source = r#"
        use /std/{Bytes, Map, Hash, Eq};
        use /std/ops/{Eql, Ord};
        pub induct Side : pub Type
        | left()
        | right()
        end
        satisfy Eql(Side) { .. }
        satisfy Ord(Side) { .. }
        satisfy Hash(Side) {
            hash(s) => match s | left() => 0x[0x1] | right() => 0x[0x0] end,
        }
        satisfy Hash/Key(Side) {
            mono(a, b) =>
                match a
                | left() => match b | left() => Eq/refl() | right() => Eq/refl() end
                | right() => match b | left() => Eq/refl() | right() => Eq/refl() end
                end,
        }
        /std/print("unreachable")
        "#;

    let message = error(source);
    assert!(message.contains("type mismatch"), "{message}");
    assert!(
        message.contains("expected: `Eq(@/std/Ordering)(/std/Ordering/lt(), /std/Ordering/gt())`"),
        "{message}"
    );
}

// A function under `use Hash/Key(K)` alone reads its keys' order through the dictionary: the sort takes `Ord(K)` from the key's superclass edge, and the law that the sort's answer is in order reads the order's own laws through that edge, so no caller states an order or proves a law beside the key it names.
#[test]
fn a_function_under_a_key_alone_sorts_its_keys_and_states_their_order() {
    let source = r#"
        use /std/{Nat, Int, Bool, Str, List, Map, Hash}; use /std/ops/{Ord};
        use /std/Bool/{Holds};
        let sorted(@K: Type, use Hash/Key(K), keys: List(K)) -> List(K) =>
            List/sort(keys);
        let in_order(
            @K: Type,
            use Hash/Key(K),
            keys: List(K),
        ) -> Holds(List/ordered(sorted(keys), Ord/le_by((a: K, b: K) => Ord/ord(a, b)))) =>
            List/sort/sorted(keys);
        /std/print(
            Str/join(
                " ",
                [
                    Str/join(",", sorted(["b", "a", "ab"])),
                    Str/join(",", List/map(sorted([256, 1, 65536, 0]), Nat/to_str)),
                    Str/join(",", List/map(sorted([+2, -3, +0, -1]), Int/to_str))]))
        "#;

    assert_eq!(run(source), b"a,ab,b 0,1,256,65536 -3,-1,+0,+2");
}
