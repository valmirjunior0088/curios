//! Derived `Eql`: structural and fieldwise, a proof taking no part and a hidden member taking part, the standard library's own, and the concept's two laws, which a derivation proves and a written witness is held to.

use {crate::tests::test_support::*, curios_common::fill};

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
        report.contains("wanted: `Eql(A)`\n│  needed by `Box/boxed` for payload #1 — add `use Eql(A)` to the telescope"),
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

// --- The laws: `eq`, that two values `eql` holds equal are one, and `refl`, that a value is equal to itself, proved where a witness is declared. ---

// The derivation writes both proofs, which a program reads through the concept as it reads the method: over a recursive family, a family nested through `List`'s witness and through a pair's inside it, a parameterized family under a premise, a constructor holding a proof, and a struct holding a proof of its own field. Each law is used at a variable, where only the proof answers, and the bridge at two closed values, where the comparison reduces.
#[test]
fn a_derived_witness_proves_both_laws() {
    let source = r#"
        use /std/{Nat, Bool, Str, List, Eq, print};
        use /std/Bool/{True, Holds};
        use /std/ops/{Eql};

        induct Tree: Type | leaf(Nat) | node(Tree, Tree) end
        induct Doc: Type | num(Nat) | arr(List(Doc)) | obj(List({Str, Doc})) end
        induct Rose(A: Type): Type | rose(A, List(Rose(A))) end
        induct Certified: Type | cert(n: Nat, proof: Eq()(n, n)) end
        struct Small: Type { n: Nat, @below: Holds(n < 10) }

        satisfy Eql(Tree) { .. }
        satisfy Eql(Doc) { .. }
        satisfy (@A: Type, use Eql(A)) => Eql(Rose(A)) { .. }
        satisfy Eql(Certified) { .. }
        satisfy Eql(Small) { .. }

        let one(@A: Type, use Eql(A), a: A, b: A, h: Holds(a == b)) -> Eq()(a, b) =
            Eql/eq(a, b, h);
        let itself(@A: Type, use Eql(A), a: A) -> Holds(a == a) = Eql/refl(a);

        let _trees(a: Tree, b: Tree, h: Holds(a == b)) -> Eq()(a, b) = one(a, b, h);
        let _docs(a: Doc, b: Doc, h: Holds(a == b)) -> Eq()(a, b) = one(a, b, h);
        let _roses(a: Rose(Doc), b: Rose(Doc), h: Holds(a == b)) -> Eq()(a, b) = one(a, b, h);
        let _certified(a: Certified, b: Certified, h: Holds(a == b)) -> Eq()(a, b) = one(a, b, h);
        let _small(a: Small, b: Small, h: Holds(a == b)) -> Eq()(a, b) = one(a, b, h);

        let _a_tree(a: Tree) -> Holds(a == a) = itself(a);
        let _a_doc(a: Doc) -> Holds(a == a) = itself(a);
        let _a_rose(a: Rose(Doc)) -> Holds(a == a) = itself(a);
        let _a_small(a: Small) -> Holds(a == a) = itself(a);

        let _closed: Eq()(Doc/obj([("a", Doc/arr([Doc/num(1)]))]), Doc/obj([("a", Doc/arr([Doc/num(1)]))])) =
            Eql/eq(
                Doc/obj([("a", Doc/arr([Doc/num(1)]))]),
                Doc/obj([("a", Doc/arr([Doc/num(1)]))]),
                True/qed());

        print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A witness is held to the laws where it is declared: an `eql` that holds two constructors equal has no proof that they are one, one that holds a value apart from itself has no proof that it is not, and a witness that writes the method alone is missing two fields.
#[test]
fn a_witness_whose_equality_breaks_a_law_is_refused_where_it_is_declared() {
    let program = |witness: &str| {
        fill!(
            r#"
            use /std/{Bool, Eq, print};
            use /std/Bool/{True};
            use /std/ops/{Eql};
            induct Side: Type | left() | right() end
            ${witness}
            print("unreachable")
            "#,
            witness
        )
    };
    let lawful = program(
        r#"satisfy Eql(Side) {
                eql(a, b) =
                    match a
                    | left() => match b | left() => true | right() => false end
                    | right() => match b | left() => false | right() => true end
                    end,
                eq(a, b, h) =
                    match a
                    | left() => match b | left() => Eq/refl() | right() => match h end end
                    | right() => match b | left() => match h end | right() => Eq/refl() end
                    end,
                refl(a) = match a | left() => True/qed() | right() => True/qed() end,
            }"#,
    );
    let conflating = program(
        r#"satisfy Eql(Side) {
                eql(a, b) = true,
                eq(a, b, h) = Eq/refl(),
                refl(a) = True/qed(),
            }"#,
    );
    let parting = program(
        r#"satisfy Eql(Side) {
                eql(a, b) = false,
                eq(a, b, h) = match h end,
                refl(a) = True/qed(),
            }"#,
    );
    let lawless = program("satisfy Eql(Side) { eql(a, b) = true, }");

    assert_eq!(
        run(&lawful.replace("print(\"unreachable\")", "print(\"ok\")")),
        b"ok"
    );

    let report = error(&conflating);
    assert!(
        report.contains("type mismatch") && report.contains("expected: `Eq(@Side)(a, b)`"),
        "{report}"
    );
    let report = error(&parting);
    assert!(
        report.contains("type mismatch") && report.contains("inferred: `True`"),
        "{report}"
    );
    let report = error(&lawless);
    assert!(
        report.contains("method count mismatch") && report.contains("missing: `eq`, `refl`"),
        "{report}"
    );
}
