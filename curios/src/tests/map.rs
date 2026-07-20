use super::run;

#[test]
fn map_get_on_empty_is_none() {
    let source = r#"
        use /std/{Io, Str, Map, Option};
        let m : Map(Str) = Map/empty();
        Io/print(Option/unwrap_or(Map/get(m, "missing"), "none"))
        "#;
    assert_eq!(run(source), b"none");
}

#[test]
fn map_roundtrips_prefix_related_keys() {
    // "", "a", "ab", "abc", "b" force the trie to branch on the presence-marker
    // bits: a key that is a proper prefix of another differs from it only at a
    // marker position, the case a plain per-byte comparison would miss.
    let source = r#"
        use /std/{Io, Str, Map, Option};
        let m : Map(Str) =
            Map/of([("", "nil"), ("ab", "AB"), ("a", "A"), ("abc", "ABC"), ("b", "B")]);
        let at(k : Str) -> Str = Option/unwrap_or(Map/get(m, k), "?");
        Io/print(Str/join(",", [at(""), at("a"), at("ab"), at("abc"), at("b"), at("c")]))
        "#;
    assert_eq!(run(source), b"nil,A,AB,ABC,B,?");
}

#[test]
fn map_set_replaces_without_growing() {
    // Setting an existing key must go down the `replace` path: same size, new
    // value, no duplicate leaf for the key.
    let source = r#"
        use /std/{Io, Str, Map, Nat, Option};
        let m : Map(Nat) = Map/set(Map/set(Map/empty(), "k", 1), "k", 2);
        Io/print(Str/join(",", [
            Nat/to_str(Map/len(m)),
            Nat/to_str(Option/unwrap_or(Map/get(m, "k"), 0))]))
        "#;
    assert_eq!(run(source), b"1,2");
}

#[test]
fn map_del_removes_and_collapses() {
    // Deleting a present key collapses its parent fork (the sibling is spliced
    // up), deleting an absent key is the identity, and deleting the last key
    // returns to the empty map.
    let source = r#"
        use /std/{Io, Str, Map, Nat, Option};
        let m : Map(Nat) = Map/of([("a", 1), ("b", 2), ("c", 3)]);
        let d : Map(Nat) = Map/del(m, "b");
        let at(k : Str) -> Str = Nat/to_str(Option/unwrap_or(Map/get(d, k), 9));
        let gone : Map(Nat) = Map/del(Map/del(d, "a"), "c");
        Io/print(Str/join(",", [
            Nat/to_str(Map/len(d)),
            at("a"), at("b"), at("c"),
            Nat/to_str(Map/len(Map/del(d, "b"))),
            Nat/to_str(Map/len(gone))]))
        "#;
    assert_eq!(run(source), b"2,1,9,3,2,0");
}

#[test]
fn map_iterates_in_lexicographic_key_order() {
    // Iteration order is a property of the canonical shape, not of insertion
    // order: the zero side of a fork holds the smaller keys, and the marker
    // bits sort a prefix before its extensions ("" first, "ab" before "abc").
    let source = r#"
        use /std/{Io, Str, Map, Nat, Option, Lst};
        let m : Map(Nat) =
            Map/of([("b", 0), ("abc", 0), ("", 0), ("a", 0), ("ab", 0)]);
        Io/print(Str/join(",",
            Lst/map(Map/keys(m), (k) => Option/unwrap_or(Str/of_bytes(k), "?"))))
        "#;
    assert_eq!(run(source), b",a,ab,abc,b");
}

#[test]
fn map_entries_agree_across_insertion_orders() {
    // Canonicity, observed through the API: the same entry set reached by two
    // different insertion histories (including a detour through a later-deleted
    // key) folds to the same entry sequence.
    let source = r#"
        use /std/{Io, Str, Map, Nat, Option, Lst};
        let show(m : Map(Nat)) -> Str =
            Str/join(",", Lst/map(
                Map/entries(m),
                ((k, v)) => Str/concat(Option/unwrap_or(Str/of_bytes(k), "?"), Nat/to_str(v))));
        let m1 : Map(Nat) = Map/of([("x", 1), ("y", 2), ("z", 3)]);
        let m2 : Map(Nat) = Map/del(Map/of([("z", 3), ("w", 0), ("y", 2), ("x", 1)]), "w");
        Io/print(Str/join(";", [show(m1), show(m2)]))
        "#;
    assert_eq!(run(source), b"x1,y2,z3;x1,y2,z3");
}

#[test]
fn map_nat_keys_encode_injectively() {
    // The `Key(Nat)` witness must keep 0 (empty encoding), one-byte, boundary
    // (255/256), and multi-byte keys distinct.
    let source = r#"
        use /std/{Io, Str, Map, Nat, Option, Lst};
        let entries : Lst({Nat, Str}) =
            [(0, "zero"), (1, "one"), (255, "ff"), (256, "big"), (65536, "bigger")];
        let m : Map(Str) = Map/of(entries);
        let at(n : Nat) -> Str = Option/unwrap_or(Map/get(m, n), "?");
        Io/print(Str/join(",", [
            Nat/to_str(Map/len(m)),
            at(0), at(1), at(255), at(256), at(65536), at(2)]))
        "#;
    assert_eq!(run(source), b"5,zero,one,ff,big,bigger,?");
}

#[test]
fn map_holds_many_keys_with_shared_prefixes() {
    // 300 sequential Nat keys share long big-endian prefixes, exercising deep
    // shared paths; deleting the even half exercises collapse at scale.
    let source = r#"
        use /std/{Io, Str, Map, Nat, Option};
        rec build(i : Nat, acc : Map(Str)) -> Map(Str) =
            match i
            | 0 => acc
            | _ => build(i - 1, Map/set(acc, i, Nat/to_str(i)))
            end;
        rec drop_even(i : Nat, acc : Map(Str)) -> Map(Str) =
            match i
            | 0 => acc
            | _ =>
                match i % 2 == 0
                | true => drop_even(i - 1, Map/del(acc, i))
                | false => drop_even(i - 1, acc)
                end
            end;
        let m : Map(Str) = build(300, Map/empty());
        let d : Map(Str) = drop_even(300, m);
        let at(n : Nat) -> Str = Option/unwrap_or(Map/get(d, n), "?");
        Io/print(Str/join(",", [
            Nat/to_str(Map/len(m)),
            Nat/to_str(Map/len(d)),
            at(2), at(3), at(255), at(256), at(257)]))
        "#;
    assert_eq!(run(source), b"300,150,?,3,255,?,257");
}
