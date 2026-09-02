//! `Map`, the crit-bit trie over `Key`-encoded bytes, and `Set` over it: lookups, the canonical shape, the rewriting functions, and the `Key` law.

use super::run;

#[test]
fn get_on_empty_is_none() {
    let source = r#"
        use /std/{Handle, Str, Map, Option};
        let m : Map(Str) = Map/empty();
        /std/print(Option/unwrap_or(Map/get(m, "missing"), "none"))
        "#;
    assert_eq!(run(source), b"none");
}

#[test]
fn roundtrips_prefix_related_keys() {
    // "", "a", "ab", "abc", "b" force the trie to branch on the presence-marker bits: a key that is a proper prefix of another differs from it only at a marker position, the case a plain per-byte comparison would miss.
    let source = r#"
        use /std/{Handle, Str, Map, Option};
        let m : Map(Str) =
            Map/of([("", "nil"), ("ab", "AB"), ("a", "A"), ("abc", "ABC"), ("b", "B")]);
        let at(k : Str) -> Str = Option/unwrap_or(Map/get(m, k), "?");
        /std/print(Str/join(",", [at(""), at("a"), at("ab"), at("abc"), at("b"), at("c")]))
        "#;
    assert_eq!(run(source), b"nil,A,AB,ABC,B,?");
}

#[test]
fn set_replaces_without_growing() {
    // Setting an existing key must go down the `replace` path: same size, new value, no duplicate leaf for the key.
    let source = r#"
        use /std/{Handle, Str, Map, Nat, Option};
        let m : Map(Nat) = Map/insert(Map/insert(Map/empty(), "k", 1), "k", 2);
        /std/print(Str/join(",", [
            Nat/to_str(Map/len(m)),
            Nat/to_str(Option/unwrap_or(Map/get(m, "k"), 0))]))
        "#;
    assert_eq!(run(source), b"1,2");
}

#[test]
fn del_removes_and_collapses() {
    // Deleting a present key collapses its parent fork (the sibling is spliced up), deleting an absent key is the identity, and deleting the last key returns to the empty map.
    let source = r#"
        use /std/{Handle, Str, Map, Nat, Option};
        let m : Map(Nat) = Map/of([("a", 1), ("b", 2), ("c", 3)]);
        let d : Map(Nat) = Map/remove(m, "b");
        let at(k : Str) -> Str = Nat/to_str(Option/unwrap_or(Map/get(d, k), 9));
        let gone : Map(Nat) = Map/remove(Map/remove(d, "a"), "c");
        /std/print(Str/join(",", [
            Nat/to_str(Map/len(d)),
            at("a"), at("b"), at("c"),
            Nat/to_str(Map/len(Map/remove(d, "b"))),
            Nat/to_str(Map/len(gone))]))
        "#;
    assert_eq!(run(source), b"2,1,9,3,2,0");
}

#[test]
fn iterates_in_lexicographic_key_order() {
    // Iteration order is a property of the canonical shape, not of insertion order: the zero side of a fork holds the smaller keys, and the marker bits sort a prefix before its extensions ("" first, "ab" before "abc").
    let source = r#"
        use /std/{Handle, Str, Map, Nat, Option, List};
        let m : Map(Nat) =
            Map/of([("b", 0), ("abc", 0), ("", 0), ("a", 0), ("ab", 0)]);
        /std/print(Str/join(",",
            List/map(Map/keys(m), (k) => Option/unwrap_or(Str/of_bytes(k), "?"))))
        "#;
    assert_eq!(run(source), b",a,ab,abc,b");
}

#[test]
fn entries_agree_across_insertion_orders() {
    // Canonicity, observed through the API: the same entry set reached by two different insertion histories (including a detour through a later-deleted key) folds to the same entry sequence.
    let source = r#"
        use /std/{Handle, Str, Map, Nat, Option, List};
        let show(m : Map(Nat)) -> Str =
            Str/join(",", List/map(
                Map/entries(m),
                ((k, v)) => Str/concat(Option/unwrap_or(Str/of_bytes(k), "?"), Nat/to_str(v))));
        let m1 : Map(Nat) = Map/of([("x", 1), ("y", 2), ("z", 3)]);
        let m2 : Map(Nat) = Map/remove(Map/of([("z", 3), ("w", 0), ("y", 2), ("x", 1)]), "w");
        /std/print(Str/join(";", [show(m1), show(m2)]))
        "#;
    assert_eq!(run(source), b"x1,y2,z3;x1,y2,z3");
}

#[test]
fn holds_many_keys_with_shared_prefixes() {
    // 300 sequential keys, rendered as decimal, share long prefixes — "1" against "10" against "100" — exercising deep shared paths and the presence markers that separate a key from its own extensions; deleting the even half exercises collapse at scale. Keys are `Str` because there is no `Key(Nat)`: see `/std/Map`, which records why a base-256 encoding of an intrinsic `Nat` cannot discharge `Key/injective`.
    let source = r#"
        use /std/{Handle, Str, Map, Nat, Option};
        let build(i : Nat, acc : Map(Str)) -> Map(Str) =
            match i
            | 0 => acc
            | _ => build(i - 1, Map/insert(acc, Nat/to_str(i), Nat/to_str(i)))
            end;
        let drop_even(i : Nat, acc : Map(Str)) -> Map(Str) =
            match i
            | 0 => acc
            | _ =>
                match i % 2 == 0
                | true => drop_even(i - 1, Map/remove(acc, Nat/to_str(i)))
                | false => drop_even(i - 1, acc)
                end
            end;
        let m : Map(Str) = build(300, Map/empty());
        let d : Map(Str) = drop_even(300, m);
        let at(n : Nat) -> Str = Option/unwrap_or(Map/get(d, Nat/to_str(n)), "?");
        /std/print(Str/join(",", [
            Nat/to_str(Map/len(m)),
            Nat/to_str(Map/len(d)),
            at(2), at(3), at(255), at(256), at(257)]))
        "#;
    assert_eq!(run(source), b"300,150,?,3,255,?,257");
}

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

// `update` is the one rewriting entry point: the function sees the current value or `none`, and answers the value to keep or `none` to remove. `map` and `filter` walk the entries, `union` is right-biased, and `get_or` reads with a default; every `Show` spells the keys as the `Bytes` the trie holds.
#[test]
fn update_map_filter_union_and_get_or_rewrite_entries() {
    let source = r#"
        use /std/{Str, Nat, Map, Option, Show, Bool, Bytes};
        let m: Map(Nat) = Map/of([("a", 1), ("b", 2), ("c", 3)]);
        let bumped = Map/update(m, "a", (o: Option(Nat)) => Option/map(o, (n: Nat) => n + 10));
        let dropped = Map/update(m, "b", (o: Option(Nat)) => Option/none());
        let added = Map/update(m, "d", (o: Option(Nat)) => Option/some(4));
        let doubled = Map/map(m, (n: Nat) => n * 2);
        let odd = Map/filter(m, (k: Bytes, n: Nat) => Bool/not(Nat/is_even(n)));
        let both = Map/union(Map/of([("a", 1), ("x", 9)]), Map/of([("a", 100), ("y", 8)]));
        /std/print(Str/join(" ", [
            Show/show(bumped), Nat/to_str(Map/len(dropped)), Show/show(added), Show/show(doubled),
            Show/show(odd), Show/show(both), Nat/to_str(Map/get_or(m, "z", 0)), Nat/to_str(Map/get_or(m, "c", 0))
        ]))
        "#;
    assert_eq!(
        run(source),
        b"{61: 11, 62: 2, 63: 3} 2 {61: 1, 62: 2, 63: 3, 64: 4} {61: 2, 62: 4, 63: 6} {61: 1, 63: 3} {61: 100, 78: 9, 79: 8} 0 3"
    );
}

// A `Set(K)` is a `Map(K)` storing each key as its own value, so `to_list` hands the elements back typed — `List(Str)` here, joined without a decoder — where a set over `Map({})` would hand back the trie's `Bytes`.
#[test]
fn a_set_stores_its_keys_typed_and_unions() {
    let source = r#"
        use /std/{Str, Set, List, Nat, Show, Bool};
        let s = Set/of(["b", "a", "c", "a"]);
        let t = Set/insert(Set/of(["d"]), "a");
        let u = Set/union(s, t);
        /std/print(Str/join(",", [
            Nat/to_str(Set/len(s)), Bool/to_str(Set/has(s, "a")), Bool/to_str(Set/has(s, "z")),
            Str/join("", Set/to_list(Set/remove(s, "b"))), Str/join("", Set/to_list(u)),
            Show/show(u), Bool/to_str(u == Set/of(["a", "b", "c", "d"])),
            Nat/to_str(Set/fold(Set/of(["x", "yy", "zzz"]), 0, (x: Str, acc: Nat) => Str/len(x) + acc))
        ]))
        "#;
    assert_eq!(run(source), b"3,true,false,ac,abcd,[a, b, c, d],true,6");
}
