//! Derived `Spell`: constructor paths and literals, payloads through their own witnesses, a family under its premise or indices, and text that re-parses.

use crate::tests::run;

// --- The `Spell` derivation: a value spells as its constructor's absolute path applied to its explicit payloads, each spelled by its own witness, so the text re-parses wherever the names are visible. ---

#[test]
fn an_enumeration_spells_as_its_constructor_paths() {
    let source = r#"
        use /std/{Str, Spell, print};
        induct Color: pub Type | red() | green() end
        satisfy Spell(Color) { .. }
        let entry() -> /std/Io({}) =
            let _ = print(Spell/spell(Color/red()))!;
            let _ = print("\n")!;
            print(Spell/spell(Color/green()));
        entry()
        "#;

    assert_eq!(run(source), b"Color/red()\nColor/green()");
}

#[test]
fn payloads_spell_through_their_own_witnesses() {
    // Carriers, a string, and nesting through `List` — every payload resolves its own `Spell`, exactly as a written `Spell/spell(x)` would.
    let source = r#"
        use /std/{Nat, Str, Bool, List, Spell, print};
        induct Shape: pub Type | dot(Nat) | tag(Str, Bool) | many(List(Nat)) end
        satisfy Spell(Shape) { .. }
        let entry() -> /std/Io({}) =
            let _ = print(Spell/spell(Shape/dot(1)))!;
            let _ = print("\n")!;
            let _ = print(Spell/spell(Shape/tag("a\n", true)))!;
            let _ = print("\n")!;
            print(Spell/spell(Shape/many([1, 2])));
        entry()
        "#;

    assert_eq!(
        run(source),
        b"Shape/dot(1)\nShape/tag(\"a\\n\", true)\nShape/many([1, 2])"
    );
}

#[test]
fn a_parameterized_family_spells_under_its_premise() {
    // The telescope's `use Spell(A)` is what spells the payload; the implicit parameter is omitted from the text, since the re-parsed call infers it.
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        induct Box(A: Type): pub Type | boxed(A) end
        satisfy (@A: Type, use Spell(A)) => Spell(Box(A)) { .. }
        let entry() -> /std/Io({}) =
            let _ = print(Spell/spell(Box/boxed(3)))!;
            let _ = print("\n")!;
            print(Spell/spell(Box/boxed(Box/boxed("x"))));
        entry()
        "#;

    assert_eq!(run(source), b"Box/boxed(3)\nBox/boxed(Box/boxed(\"x\"))");
}

#[test]
fn a_recursive_family_spells_through_its_own_entry_and_re_parses() {
    // The recursive payload resolves through the witness's own table entry, and the spelled text is a program: written back as the absolute-path literal it spells to, it denotes the same value.
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Spell(Tree) { .. }
        let entry() -> /std/Io({}) =
            let built = Tree/node(Tree/leaf(1), Tree/node(Tree/leaf(2), Tree/leaf(3)));
            let written: Tree = Tree/node(Tree/leaf(1), Tree/node(Tree/leaf(2), Tree/leaf(3)));
            let _ = print(Spell/spell(built))!;
            let _ = print("\n")!;
            print(Spell/spell(written));
        entry()
        "#;

    let spelled = "Tree/node(Tree/leaf(1), Tree/node(Tree/leaf(2), Tree/leaf(3)))";
    assert_eq!(run(source), format!("{spelled}\n{spelled}").as_bytes());
}

#[test]
fn a_mutual_group_derives_as_one() {
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        induct Tree: pub Type | leaf(Nat) | forest(Forest)
        and Forest: pub Type | nil() | cons(Tree, Forest)
        end
        satisfy Spell(Tree) { .. }
        and Spell(Forest) { .. }
        print(Spell/spell(Tree/forest(Forest/cons(Tree/leaf(1), Forest/nil()))))
        "#;

    assert_eq!(
        run(source),
        b"Tree/forest(Forest/cons(Tree/leaf(1), Forest/nil()))"
    );
}

#[test]
fn a_struct_spells_as_its_literal() {
    // Labeled always where the field has a label — the literal grammar reads `x = 1` — and positional for the one unlabeled field of a newtype-like struct.
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        struct Point: pub Type { x: Nat, y: Nat }
        struct Meters: pub Type { Nat }
        satisfy Spell(Point) { .. }
        satisfy Spell(Meters) { .. }
        let entry() -> /std/Io({}) =
            let _ = print(Spell/spell(Point { x = 1, y = 2 }))!;
            let _ = print("\n")!;
            print(Spell/spell(Meters { 5 }));
        entry()
        "#;

    assert_eq!(run(source), b"Point { x = 1, y = 2 }\nMeters { 5 }");
}

#[test]
fn a_tuple_payload_spells_as_its_literal() {
    let source = r#"
        use /std/{Nat, Bool, Str, Spell, print};
        induct Pair: pub Type | pair({Nat, Bool}) end
        satisfy Spell(Pair) { .. }
        print(Spell/spell(Pair/pair((1, true))))
        "#;

    assert_eq!(run(source), b"Pair/pair((1, true))");
}

#[test]
fn a_proof_payload_spells_as_a_goal() {
    // Evidence erases and has no literal; the written goal is the one thing that re-parses at a proposition, and a reader fills it in.
    let source = r#"
        use /std/{Nat, Str, Eq, Spell, print};
        induct Certified: pub Type | cert(n: Nat, proof: Eq()(n, n)) end
        satisfy Spell(Certified) { .. }
        print(Spell/spell(Certified/cert(1, Eq/refl())))
        "#;

    assert_eq!(run(source), b"Certified/cert(1, ?)");
}

#[test]
fn an_indexed_family_spells_each_constructor() {
    // The implicit index payload is bound by the arm and written under its mark, as a call may write it, so the text reads back at the index it states; each arm is checked at its own target indices as a written match's would be.
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        induct Vec(T: Type): (n: Nat) -> pub Type
        | nil(): (0)
        | cons(@n: Nat, head: T, tail: Vec(T)(n)): (n + 1)
        end
        satisfy (@T: Type, @n: Nat, use Spell(T)) => Spell(Vec(T)(n)) { .. }
        print(Spell/spell(Vec/cons(1, Vec/cons(2, Vec/nil()))))
        "#;

    assert_eq!(run(source), b"Vec/cons(@1, 1, Vec/cons(@0, 2, Vec/nil()))");
}

// The renderers spell the two shapes a derived body is built from, pinned as the text a re-parse reads: a derivation's output is only ever their output over spelled pieces. An empty label is the positional field of a newtype-like struct.
#[test]
fn the_renderers_spell_a_call_and_a_record() {
    let source = r#"
        use /std/{Str, print};
        use /std/Spell/{call, record};
        let entry() -> /std/Io({}) =
            let _ = print(call("/Tree/leaf", []))!;
            let _ = print("\n")!;
            let _ = print(call("/Tree/node", ["1", "/Tree/leaf()"]))!;
            let _ = print("\n")!;
            let _ = print(record("/Point", [("x", "1"), ("y", "2")]))!;
            let _ = print("\n")!;
            let _ = print(record("/Meters", [("", "5")]))!;
            let _ = print("\n")!;
            print(record("/Unit", []));
        entry()
        "#;

    assert_eq!(
        run(source),
        b"/Tree/leaf()\n/Tree/node(1, /Tree/leaf())\n/Point { x = 1, y = 2 }\n/Meters { 5 }\n/Unit {}"
    );
}

// A tuple spells as its literal — the one-field form keeping the comma that separates it from a parenthesized term — through the positional-shape witnesses `/std/Tuple` writes up to three fields.
#[test]
fn a_tuple_spells_as_its_literal() {
    let source = r#"
        use /std/{Nat, Bool, Str, Spell, print};
        let entry() -> /std/Io({}) =
            let _ = print(Spell/spell(()))!;
            let _ = print("\n")!;
            let _ = print(Spell/spell((1,)))!;
            let _ = print("\n")!;
            let _ = print(Spell/spell((1, true)))!;
            let _ = print("\n")!;
            print(Spell/spell((1, true, "s")));
        entry()
        "#;

    assert_eq!(run(source), b"()\n(1,)\n(1, true)\n(1, true, \"s\")");
}
