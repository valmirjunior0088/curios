//! The `Key` laws, which a witness must discharge where it is declared. `Map` and `Set` themselves — lookups, the canonical shape and the rewriting functions — are the corpus's `/data/map`.

use crate::tests::{error, run};

// The trie's identity is the byte string a key's `Hash` produces, so a colliding encoding does not fail a lookup — it silently merges two keys into one entry. `Key/injective` states that obligation where it can be checked, and an encoding that provably collides cannot discharge it: both constructors here hash to the empty byte string, so the law demands `Eq()(a, b)` for values that are not equal. The collision is declared once, in the `Hash` witness — `Key` reaches the encoding through its superclass edge and has no second field to disagree with it — so there is one place for a collision to come from and the law still catches it there. The order ties every pair as the encodings do, so `monotone` holds and `injective` alone is what refuses the witness.
#[test]
fn a_colliding_key_witness_is_rejected() {
    let source = r#"
        use /std/{Bytes, Map, Hash, Eq, Ord, Ordering};
        use /std/ops/{Eql};
        pub induct Side : pub Type
        | left()
        | right()
        end
        satisfy Eql(Side) {
            eql(a, b) = true,
            neq(a, b) = false,
        }
        satisfy Ord(Side) {
            ord(a, b) = Ordering/eq(),
        }
        satisfy Hash(Side) {
            hash(s) = x[],
        }
        satisfy Map/Key(Side) {
            injective(a, b, same) = Eq/refl(),
            monotone(a, b) = Eq/refl(),
        }
        /std/print("unreachable")
        "#;

    let message = error(source);
    assert!(message.contains("type mismatch"), "{message}");
    assert!(message.contains("expected: Eq(@Side)(a, b)"), "{message}");
}

// A walk of the trie is in the order of the encodings, so an encoding out of its type's order does not fail a lookup either — it enumerates a map's keys in an order no reader of the type expects. `Key/monotone` states that obligation: the order of two keys is `Bytes/ord` of their encodings. `left` is below `right` by their declaration and its byte is above `right`'s, the encodings are distinct so `injective` is discharged, and the law is refused at the one pair it is false of.
#[test]
fn a_key_witness_out_of_its_order_is_rejected() {
    let source = r#"
        use /std/{Byte, Bytes, Bool, Map, Hash, Eq, Ord};
        use /std/Bool/{True, False, Holds};
        use /std/ops/{Eql};
        pub induct Side : pub Type
        | left()
        | right()
        end
        satisfy Eql(Side) { .. }
        satisfy Ord(Side) { .. }
        satisfy Hash(Side) {
            hash(s) = match s | left() => x[0x1] | right() => x[0x0] end,
        }
        let apart(e: Eq(@Byte)(0x1, 0x0)) -> False =
            Eq/subst((m: Byte) => Holds(0x1 == m), e, True/qed());
        satisfy Map/Key(Side) {
            injective(a, b, same) =
                match a
                | left() =>
                    match b
                    | left() => Eq/refl()
                    | right() => match apart(Bytes/cons_inj_head(same)) end
                    end
                | right() =>
                    match b
                    | left() => match apart(Eq/sym(Bytes/cons_inj_head(same))) end
                    | right() => Eq/refl()
                    end
                end,
            monotone(a, b) =
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
        message.contains("expected: Eq(@Ord/Ordering)(Ord/Ordering/lt, Ord/Ordering/gt)"),
        "{message}"
    );
}

// A function under `use Key(K)` alone reads its keys' order through the dictionary: the sort takes `Ord(K)` from the key's superclass edge, and the law that the sort's answer is in order takes the order's flip from the key, so no caller states an order or proves a law beside the key it names.
#[test]
fn a_function_under_a_key_alone_sorts_its_keys_and_states_their_order() {
    let source = r#"
        use /std/{Nat, Int, Bool, Str, List, Map, Ord};
        use /std/Map/{Key};
        use /std/Bool/{Holds};
        let sorted(@K: Type, use Key(K), keys: List(K)) -> List(K) =
            List/sort(keys);
        let in_order(
            @K: Type,
            use Key(K),
            keys: List(K),
        ) -> Holds(List/ordered(sorted(keys), Ord/le((a: K, b: K) => Ord/ord(a, b)))) =
            List/sort/sorted(Key/flips(), keys);
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
