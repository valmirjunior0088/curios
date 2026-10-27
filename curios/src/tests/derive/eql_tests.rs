//! Derived `Eql`: structural and fieldwise, a proof taking no part and a hidden member taking part, and the standard library's own.

use crate::tests::{error, run};

// --- The `Eql` derivation: structural equality, constructor by constructor and payload by payload, through each payload's own witness. ---

#[test]
fn derived_equality_is_structural() {
    let source = r#"
        use /std/{Nat, Bool, Str, print};
        use /std/ops/{Eql};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Eql(Tree) { .. }
        let show(b: Bool) -> Str = Str/concat(Bool/to_str(b), " ");
        let entry() -> /std/Io({}) =
            let _ = print(show(Tree/leaf(1) == Tree/leaf(1)))!;
            let _ = print(show(Tree/leaf(1) == Tree/leaf(2)))!;
            let _ = print(show(Tree/node(Tree/leaf(1), Tree/leaf(2)) == Tree/node(Tree/leaf(1), Tree/leaf(2))))!;
            let _ = print(show(Tree/node(Tree/leaf(1), Tree/leaf(2)) == Tree/node(Tree/leaf(2), Tree/leaf(1))))!;
            let _ = print(show(Tree/leaf(1) != Tree/node(Tree/leaf(1), Tree/leaf(1))))!;
            print(show(Tree/leaf(1) != Tree/leaf(1)));
        entry()
        "#;

    assert_eq!(run(source), b"true false true false true false ");
}

#[test]
fn a_struct_and_a_parameterized_family_compare_fieldwise() {
    let source = r#"
        use /std/{Nat, Bool, Str, print};
        use /std/ops/{Eql};
        struct Point: pub Type { x: Nat, y: Nat }
        induct Box(A: Type): pub Type | boxed(A) end
        satisfy Eql(Point) { .. }
        satisfy (@A: Type, use Eql(A)) => Eql(Box(A)) { .. }
        let show(b: Bool) -> Str = Str/concat(Bool/to_str(b), " ");
        let entry() -> /std/Io({}) =
            let _ = print(show(Point { x = 1, y = 2 } == Point { x = 1, y = 2 }))!;
            let _ = print(show(Point { x = 1, y = 2 } != Point { x = 1, y = 3 }))!;
            let _ = print(show(Box/boxed("a") == Box/boxed("a")))!;
            print(show(Box/boxed(Point { x = 1, y = 2 }) == Box/boxed(Point { x = 2, y = 1 })));
        entry()
        "#;

    assert_eq!(run(source), b"true true true false ");
}

#[test]
fn proofs_take_no_part_in_equality() {
    // The proof erases and cannot be compared; the index is implicit and fixed by the payloads it indexes, so comparing it, as a hidden member that is a value is compared, agrees with comparing them. Both witnesses derive.
    let source = r#"
        use /std/{Nat, Bool, Str, Eq, print};
        use /std/ops/{Eql};
        induct Certified: pub Type | cert(n: Nat, proof: Eq()(n, n)) end
        induct Vec(T: Type): (n: Nat) -> pub Type
        | nil(): (0)
        | cons(@n: Nat, head: T, tail: Vec(T)(n)): (n + 1)
        end
        satisfy Eql(Certified) { .. }
        satisfy (@T: Type, @n: Nat, use Eql(T)) => Eql(Vec(T)(n)) { .. }
        let show(b: Bool) -> Str = Str/concat(Bool/to_str(b), " ");
        let entry() -> /std/Io({}) =
            let _ = print(show(Certified/cert(1, Eq/refl()) == Certified/cert(1, Eq/refl())))!;
            let _ = print(show(Certified/cert(1, Eq/refl()) == Certified/cert(2, Eq/refl())))!;
            let _ = print(show(Vec/cons(1, Vec/cons(2, Vec/nil())) == Vec/cons(1, Vec/cons(2, Vec/nil()))))!;
            print(show(Vec/cons(1, Vec/cons(2, Vec/nil())) == Vec/cons(1, Vec/cons(3, Vec/nil()))));
        entry()
        "#;

    assert_eq!(run(source), b"true false true false ");
}

/// A hidden member that is a value takes part in a derived witness: nothing written determines `size`, so two values a match tells apart by it are unequal and hash apart, and `Spell` writes it under its mark, so the text reads back, as a structure's hidden field does.
#[test]
fn a_hidden_member_takes_part_in_a_derived_witness_under_its_mark() {
    let source = r#"
        use /std/{Bool, Nat, Str, Bytes, Hash, Spell, print};
        use /std/ops/{Eql};
        induct Tagged: Type | tag(@size: Nat, k: Nat) end
        satisfy Eql(Tagged) { .. }
        satisfy Hash(Tagged) { .. }
        satisfy Spell(Tagged) { .. }
        struct Sized: Type { @len: Nat, items: Nat }
        satisfy Eql(Sized) { .. }
        satisfy Spell(Sized) { .. }
        let size(t: Tagged) -> Nat = match t | tag(@s, _) => s end;
        let (a, b) = (Tagged/tag(@9, 1), Tagged/tag(@2, 1));
        let _ = print(Str/flatten([Bool/to_str(a == b), " ", Bool/to_str(Bytes/eql(Hash/hash(a), Hash/hash(b))), " ", Spell/spell(a), " ", Nat/to_str(size(a)), " ", Nat/to_str(size(b))]))!;
        print(Str/flatten([" ", Spell/spell(Sized { @len = 9, items = 1 }), " ", Bool/to_str(Sized { @len = 9, items = 1 } == Sized { @len = 2, items = 1 })]))
        "#;

    assert_eq!(
        run(source),
        b"false false Tagged/tag(@9, 1) 9 2 Sized { @len = 9, items = 1 } false"
    );
}

#[test]
fn the_eql_derivation_shares_the_provenance() {
    let premise = r#"
        use /std/{Str};
        use /std/ops/{Eql};
        induct Box(A: Type): pub Type | boxed(A) end
        satisfy (@A: Type) => Eql(Box(A)) { .. }
        /std/print("")
        "#;
    let report = error(premise);
    assert!(
        report.contains("no witness of Eql(A) found\n  needed by 'Box/boxed' for payload #1 — add `use Eql(A)` to the telescope"),
        "{report}"
    );
}

// --- The standard library's structural types derive both witnesses, so every workspace check exercises the derivations over the prelude; the spellings are the ones the hand-written witnesses produced, byte for byte. ---

#[test]
fn the_standard_library_derives_option_result_and_order() {
    let source = r#"
        use /std/{Nat, Bool, Str, Option, Result, Ordering, Spell, print};
        use /std/ops/{Eql};
        let show(b: Bool) -> Str = Str/concat(Bool/to_str(b), " ");
        let failing: Result(Str, Nat) = Result/failure("no");
        let entry() -> /std/Io({}) =
            let _ = print(Spell/spell(Option/some(1)))!;
            let _ = print("\n")!;
            let _ = print(Spell/spell(Option/none(@Nat)))!;
            let _ = print("\n")!;
            let _ = print(Spell/spell(Result/success(@Str, @Nat, 1)))!;
            let _ = print("\n")!;
            let _ = print(Spell/spell(failing))!;
            let _ = print("\n")!;
            let _ = print(Spell/spell(Ordering/lt()))!;
            let _ = print(Spell/spell(Ordering/eq()))!;
            let _ = print(Spell/spell(Ordering/gt()))!;
            let _ = print("\n")!;
            let _ = print(show(Option/some(1) == Option/some(1)))!;
            let _ = print(show(Option/some(1) == Option/none()))!;
            let _ = print(show(failing == Result/failure("no")))!;
            let _ = print(show(failing != Result/success(1)))!;
            let _ = print(show(Ordering/lt() == Ordering/lt()))!;
            print(show(Ordering/lt() != Ordering/gt()));
        entry()
        "#;

    assert_eq!(
        run(source),
        b"Option/some(1)\nOption/none()\nResult/success(1)\nResult/failure(\"no\")\nOrdering/lt()Ordering/eq()Ordering/gt()\ntrue false true true true true "
    );
}
