//! Derived `Hash/Key`: a key's law proved of the derived order and the derived encoding, a struct's, an inductive's and a key's under a premise, a payload's own key, a family's that holds itself, and the refusal of a key whose order or encoding is not the one a derivation writes.

use crate::test_support::*;

// --- What a derived key proves: that the derived order of two values is the order of their derived encodings. ---

/// A struct of a text and a number keys a map with no written proof, and the map walks its keys in their order: by the text, a text that begins a longer one first, and then by the number.
#[test]
fn a_struct_derives_its_key_and_a_map_walks_its_keys_in_their_order() {
    let source = r#"
        use /std/{Nat, Str, Hash, Map, print};
        use /std/ops/{Eql, Ord};
        struct Pair: pub Type { s: Str, n: Nat }
        satisfy Eql(Pair) { .. }
        satisfy Ord(Pair) { .. }
        satisfy Hash(Pair) { .. }
        satisfy Hash/Key(Pair) { .. }
        let m = Map/of_list([
            (Pair { s = "b", n = 1 }, "4"),
            (Pair { s = "ab", n = 1 }, "3"),
            (Pair { s = "a", n = 2 }, "2"),
            (Pair { s = "a", n = 1 }, "1")]);
        print(Str/join(" ", Map/values(m)))
        "#;

    assert_eq!(run(source), b"1 2 3 4");
}

/// An inductive's key orders its constructors by their declaration and then their payloads, an enumeration's case among them, so a map of its values walks them that way.
#[test]
fn an_inductive_derives_its_key_its_constructors_first_and_then_their_payloads() {
    let source = r#"
        use /std/{Nat, Str, Hash, Map, print};
        use /std/ops/{Eql, Ord};
        induct Shape: pub Type | dot() | line(Nat) | square(Nat, Nat) end
        satisfy Eql(Shape) { .. }
        satisfy Ord(Shape) { .. }
        satisfy Hash(Shape) { .. }
        satisfy Hash/Key(Shape) { .. }
        let m = Map/of_list([
            (Shape/square(1, 1), "5"),
            (Shape/line(2), "3"),
            (Shape/dot(), "1"),
            (Shape/line(1), "2"),
            (Shape/square(0, 9), "4")]);
        print(Str/join(" ", Map/values(m)))
        "#;

    assert_eq!(run(source), b"1 2 3 4 5");
}

/// A key under a premise asks its payload's key of the telescope, and its order and encoding resolve through that premise's edges.
#[test]
fn a_key_derives_under_a_premise_on_its_payloads_type() {
    let source = r#"
        use /std/{Str, Hash, Map, print};
        use /std/ops/{Eql, Ord};
        induct Box(A: Type): pub Type | wrap(A) end
        satisfy (@A: Type, use Eql(A)) => Eql(Box(A)) { .. }
        satisfy (@A: Type, use Ord(A)) => Ord(Box(A)) { .. }
        satisfy (@A: Type, use Hash(A)) => Hash(Box(A)) { .. }
        satisfy (@A: Type, use Hash/Key(A)) => Hash/Key(Box(A)) { .. }
        let m = Map/of_list([(Box/wrap("b"), "2"), (Box/wrap("a"), "1")]);
        print(Str/join(" ", Map/values(m)))
        "#;

    assert_eq!(run(source), b"1 2");
}

/// A family that holds itself derives its key: a recursive payload's part is the key's own law, read through the witness's own entry, whose superclasses read there as the registered order and encoding.
#[test]
fn an_inductive_that_holds_itself_derives_its_key() {
    let source = r#"
        use /std/{Nat, Str, Hash, Map, print};
        use /std/ops/{Eql, Ord};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Eql(Tree) { .. }
        satisfy Ord(Tree) { .. }
        satisfy Hash(Tree) { .. }
        satisfy Hash/Key(Tree) { .. }
        let m = Map/of_list([
            (Tree/node(Tree/leaf(1), Tree/leaf(0)), "3"),
            (Tree/node(Tree/leaf(0), Tree/leaf(9)), "2"),
            (Tree/leaf(5), "1")]);
        print(Str/join(" ", Map/values(m)))
        "#;

    assert_eq!(run(source), b"1 2 3");
}

/// And so does one that holds itself through another key's type: the payload's part is `Box`'s key at the family, its premise the witness's own entry.
#[test]
fn a_family_holding_itself_through_another_keys_type_derives_its_key() {
    let source = r#"
        use /std/{Nat, Str, Hash, Map, print};
        use /std/ops/{Eql, Ord};
        induct Box(A: Type): pub Type | wrap(A) end
        satisfy (@A: Type, use Eql(A)) => Eql(Box(A)) { .. }
        satisfy (@A: Type, use Ord(A)) => Ord(Box(A)) { .. }
        satisfy (@A: Type, use Hash(A)) => Hash(Box(A)) { .. }
        satisfy (@A: Type, use Hash/Key(A)) => Hash/Key(Box(A)) { .. }
        induct Tree: pub Type | leaf(Nat) | node(Box(Tree)) end
        satisfy Eql(Tree) { .. }
        satisfy Ord(Tree) { .. }
        satisfy Hash(Tree) { .. }
        satisfy Hash/Key(Tree) { .. }
        let m = Map/of_list([
            (Tree/node(Box/wrap(Tree/leaf(1))), "3"),
            (Tree/node(Box/wrap(Tree/leaf(0))), "2"),
            (Tree/leaf(5), "1")]);
        print(Str/join(" ", Map/values(m)))
        "#;

    assert_eq!(run(source), b"1 2 3");
}

// --- Refusals. ---

/// A payload's part is its own key's law, so a payload whose type orders and encodes but keys nothing is reported against that payload.
#[test]
fn a_payload_with_no_key_is_reported_against_the_payload() {
    let source = r#"
        use /std/{Nat, Hash};
        use /std/ops/{Eql, Ord};
        struct Inner: pub Type { n: Nat }
        satisfy Eql(Inner) { .. }
        satisfy Ord(Inner) { .. }
        satisfy Hash(Inner) { .. }
        struct Held: pub Type { kept: Inner }
        satisfy Eql(Held) { .. }
        satisfy Ord(Held) { .. }
        satisfy Hash(Held) { .. }
        satisfy Hash/Key(Held) { .. }
        /std/print("")
        "#;

    let report = error(source);
    assert!(
        report.contains("Hash/Key") && report.contains("kept"),
        "{report}"
    );
}

/// A derived key's law is proved of the order a derivation writes, so a key over a written order that compares otherwise, here the declaration's reversed, is refused at its `satisfy`, naming the order.
#[test]
fn a_key_over_a_written_order_is_refused_naming_the_order() {
    let source = r#"
        use /std/{Hash, Ordering, Eq};
        use /std/ops/{Eql, Ord};
        induct Two: pub Type | low() | high() end
        satisfy Eql(Two) { .. }
        let reversed(x: Two, y: Two) -> Ordering =
            match x
            | low() => match y | low() => Ordering/eq() | high() => Ordering/gt() end
            | high() => match y | low() => Ordering/lt() | high() => Ordering/eq() end
            end;
        satisfy Ord(Two) {
            ord = reversed,
            lt(x, y) = Ordering/is_lt(reversed(x, y)),
            le(x, y) = Ordering/is_le(reversed(x, y)),
            flips(x, y) =
                match x
                | low() => match y | low() => Eq/refl() | high() => Eq/refl() end
                | high() => match y | low() => Eq/refl() | high() => Eq/refl() end
                end,
            trans(x, y, z, p, q) =
                match x
                | high() => match z | low() => /std/Bool/True/qed() | high() => /std/Bool/True/qed() end
                | low() =>
                    match z
                    | low() => /std/Bool/True/qed()
                    | high() => match y | low() => match q end | high() => match p end end
                    end
                end,
            eq(x, y, h) =
                match x
                | low() => match y | low() => Eq/refl() | high() => match h end end
                | high() => match y | low() => match h end | high() => Eq/refl() end
                end,
            is_lt(_, _) = Eq/refl(),
            is_le(_, _) = Eq/refl(),
        }
        satisfy Hash(Two) { .. }
        satisfy Hash/Key(Two) { .. }
        /std/print("")
        "#;

    let report = error(source);
    assert!(
        report.contains("`Hash/Key` cannot be derived for Two")
            && report.contains("its `Ord` is not the one a derivation writes")
            && report.contains("derive its `Ord` with `{ .. }`, or write the body"),
        "{report}"
    );
}

/// And so is a key over a written encoding that writes otherwise, naming the encoding.
#[test]
fn a_key_over_a_written_encoding_is_refused_naming_the_encoding() {
    let source = r#"
        use /std/{Hash};
        use /std/ops/{Eql, Ord};
        induct Two: pub Type | low() | high() end
        satisfy Eql(Two) { .. }
        satisfy Ord(Two) { .. }
        satisfy Hash(Two) {
            hash(t) = match t | low() => 0x[0x2] | high() => 0x[0x1] end,
        }
        satisfy Hash/Key(Two) { .. }
        /std/print("")
        "#;

    let report = error(source);
    assert!(
        report.contains("its `Hash` is not the one a derivation writes")
            && report.contains("derive its `Hash` with `{ .. }`, or write the body"),
        "{report}"
    );
}

/// What a derived key reads of its superclasses is what each computes, not whether it was derived: a written encoding that writes what a derivation writes keys a derived key.
#[test]
fn a_written_encoding_that_writes_the_derived_one_keys_a_derived_key() {
    let source = r#"
        use /std/{Nat, Str, Hash, Map, print};
        use /std/ops/{Eql, Ord};
        struct Point: pub Type { x: Nat, y: Nat }
        satisfy Eql(Point) { .. }
        satisfy Ord(Point) { .. }
        satisfy Hash(Point) {
            hash(p) = 0x[..Hash/delimited(Hash/hash(p.x)), ..Hash/hash(p.y)],
        }
        satisfy Hash/Key(Point) { .. }
        let m = Map/of_list([(Point { x = 1, y = 0 }, "2"), (Point { x = 0, y = 7 }, "1")]);
        print(Str/join(" ", Map/values(m)))
        "#;

    assert_eq!(run(source), b"1 2");
}
