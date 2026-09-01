//! Derived witnesses: the body-less `satisfy C(T);` form, the transient it lowers to, and what the compiler writes — or refuses — in its place.

use crate::tests::{core, core_elab, error, run};

// --- The `Spell` derivation: a value spells as its constructor's absolute path applied to its explicit payloads, each spelled by its own witness, so the text re-parses wherever the names are visible. ---

#[test]
fn an_enumeration_spells_as_its_constructor_paths() {
    let source = r#"
        use /std/{Str, Spell, print};
        induct Color: pub Type | red() | green() end
        satisfy Spell(Color);
        let _ = print(Spell/spell(Color/red()))!;
        let _ = print("\n")!;
        print(Spell/spell(Color/green()))
        "#;

    assert_eq!(run(source), b"/Color/red()\n/Color/green()");
}

#[test]
fn payloads_spell_through_their_own_witnesses() {
    // Carriers, a string, and nesting through `List` — every payload resolves its own `Spell`, exactly as a written `Spell/spell(x)` would.
    let source = r#"
        use /std/{Nat, Str, Bool, List, Spell, print};
        induct Shape: pub Type | dot(Nat) | tag(Str, Bool) | many(List(Nat)) end
        satisfy Spell(Shape);
        let _ = print(Spell/spell(Shape/dot(1)))!;
        let _ = print("\n")!;
        let _ = print(Spell/spell(Shape/tag("a\n", true)))!;
        let _ = print("\n")!;
        print(Spell/spell(Shape/many([1, 2])))
        "#;

    assert_eq!(
        run(source),
        b"/Shape/dot(1)\n/Shape/tag(\"a\\n\", true)\n/Shape/many([1, 2])"
    );
}

#[test]
fn a_parameterized_family_spells_under_its_premise() {
    // The telescope's `use Spell(A)` is what spells the payload; the implicit parameter is omitted from the text, since the re-parsed call infers it.
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        induct Box(A: Type): pub Type | boxed(A) end
        satisfy (@A: Type, use Spell(A)) => Spell(Box(A));
        let _ = print(Spell/spell(Box/boxed(3)))!;
        let _ = print("\n")!;
        print(Spell/spell(Box/boxed(Box/boxed("x"))))
        "#;

    assert_eq!(run(source), b"/Box/boxed(3)\n/Box/boxed(/Box/boxed(\"x\"))");
}

#[test]
fn a_recursive_family_spells_through_its_own_entry_and_re_parses() {
    // The recursive payload resolves through the witness's own table entry, and the spelled text is a program: written back as the absolute-path literal it spells to, it denotes the same value.
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Spell(Tree);
        let built = Tree/node(Tree/leaf(1), Tree/node(Tree/leaf(2), Tree/leaf(3)));
        let written: Tree = /Tree/node(/Tree/leaf(1), /Tree/node(/Tree/leaf(2), /Tree/leaf(3)));
        let _ = print(Spell/spell(built))!;
        let _ = print("\n")!;
        print(Spell/spell(written))
        "#;

    let spelled = "/Tree/node(/Tree/leaf(1), /Tree/node(/Tree/leaf(2), /Tree/leaf(3)))";
    assert_eq!(run(source), format!("{spelled}\n{spelled}").as_bytes());
}

#[test]
fn a_mutual_group_derives_as_one() {
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        induct Tree: pub Type | leaf(Nat) | forest(Forest)
        and Forest: pub Type | nil() | cons(Tree, Forest)
        end
        satisfy Spell(Tree);
        and Spell(Forest);
        print(Spell/spell(Tree/forest(Forest/cons(Tree/leaf(1), Forest/nil()))))
        "#;

    assert_eq!(
        run(source),
        b"/Tree/forest(/Forest/cons(/Tree/leaf(1), /Forest/nil()))"
    );
}

#[test]
fn a_struct_spells_as_its_literal() {
    // Labeled always where the field has a label — the literal grammar reads `x = 1` — and positional for the one unlabeled field of a newtype-like struct.
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        struct Point: pub Type { x: Nat, y: Nat }
        struct Meters: pub Type { Nat }
        satisfy Spell(Point);
        satisfy Spell(Meters);
        let _ = print(Spell/spell(Point { x = 1, y = 2 }))!;
        let _ = print("\n")!;
        print(Spell/spell(Meters { 5 }))
        "#;

    assert_eq!(run(source), b"/Point { x = 1, y = 2 }\n/Meters { 5 }");
}

#[test]
fn a_tuple_payload_spells_as_its_literal() {
    let source = r#"
        use /std/{Nat, Bool, Str, Spell, print};
        induct Pair: pub Type | pair({Nat, Bool}) end
        satisfy Spell(Pair);
        print(Spell/spell(Pair/pair((1, true))))
        "#;

    assert_eq!(run(source), b"/Pair/pair((1, true))");
}

#[test]
fn a_proof_payload_spells_as_a_goal() {
    // Evidence erases and has no literal; the written goal is the one thing that re-parses at a proposition, and a reader fills it in.
    let source = r#"
        use /std/{Nat, Str, Eq, Spell, print};
        induct Certified: pub Type | cert(n: Nat, proof: Eq(n, n)) end
        satisfy Spell(Certified);
        print(Spell/spell(Certified/cert(1, Eq/refl())))
        "#;

    assert_eq!(run(source), b"/Certified/cert(1, ?)");
}

#[test]
fn an_indexed_family_spells_each_constructor() {
    // The implicit index payload is bound by the arm and omitted from the text; each arm is checked at its own target indices as a written match's would be.
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        induct Vec(T: Type): (n: Nat) -> pub Type
        | nil(): (0)
        | cons(@n: Nat, head: T, tail: Vec(T, n)): (n + 1)
        end
        satisfy (@T: Type, @n: Nat, use Spell(T)) => Spell(Vec(T, n));
        print(Spell/spell(Vec/cons(1, Vec/cons(2, Vec/nil()))))
        "#;

    assert_eq!(run(source), b"/Vec/cons(1, /Vec/cons(2, /Vec/nil()))");
}

#[test]
fn the_stages_show_the_transient_and_its_expansion() {
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Spell(Tree);
        print(Spell/spell(Tree/leaf(1)))
        "#;

    let lowered = core(source);
    assert!(lowered.contains("derive"), "{lowered}");
    let elaborated = core_elab(source);
    assert!(!elaborated.contains("derive"), "{elaborated}");
    assert!(elaborated.contains("Spell/call"), "{elaborated}");
}

// --- The `Eql` derivation: structural equality, constructor by constructor and payload by payload, through each payload's own witness. ---

#[test]
fn derived_equality_is_structural() {
    let source = r#"
        use /std/{Nat, Bool, Str, Eql, print};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Eql(Tree);
        let show(b: Bool) -> Str = Str/concat(Bool/to_str(b), " ");
        let _ = print(show(Tree/leaf(1) == Tree/leaf(1)))!;
        let _ = print(show(Tree/leaf(1) == Tree/leaf(2)))!;
        let _ = print(show(Tree/node(Tree/leaf(1), Tree/leaf(2)) == Tree/node(Tree/leaf(1), Tree/leaf(2))))!;
        let _ = print(show(Tree/node(Tree/leaf(1), Tree/leaf(2)) == Tree/node(Tree/leaf(2), Tree/leaf(1))))!;
        let _ = print(show(Tree/leaf(1) != Tree/node(Tree/leaf(1), Tree/leaf(1))))!;
        print(show(Tree/leaf(1) != Tree/leaf(1)))
        "#;

    assert_eq!(run(source), b"true false true false true false ");
}

#[test]
fn a_struct_and_a_parameterized_family_compare_fieldwise() {
    let source = r#"
        use /std/{Nat, Bool, Str, Eql, print};
        struct Point: pub Type { x: Nat, y: Nat }
        induct Box(A: Type): pub Type | boxed(A) end
        satisfy Eql(Point);
        satisfy (@A: Type, use Eql(A)) => Eql(Box(A));
        let show(b: Bool) -> Str = Str/concat(Bool/to_str(b), " ");
        let _ = print(show(Point { x = 1, y = 2 } == Point { x = 1, y = 2 }))!;
        let _ = print(show(Point { x = 1, y = 2 } != Point { x = 1, y = 3 }))!;
        let _ = print(show(Box/boxed("a") == Box/boxed("a")))!;
        print(show(Box/boxed(Point { x = 1, y = 2 }) == Box/boxed(Point { x = 2, y = 1 })))
        "#;

    assert_eq!(run(source), b"true true true false ");
}

#[test]
fn proofs_and_implicit_payloads_take_no_part_in_equality() {
    // The proof erases and cannot be compared; the index is implicit and fixed by the payloads it indexes. Both witnesses derive, and equality reads the values alone.
    let source = r#"
        use /std/{Nat, Bool, Str, Eq, Eql, print};
        induct Certified: pub Type | cert(n: Nat, proof: Eq(n, n)) end
        induct Vec(T: Type): (n: Nat) -> pub Type
        | nil(): (0)
        | cons(@n: Nat, head: T, tail: Vec(T, n)): (n + 1)
        end
        satisfy Eql(Certified);
        satisfy (@T: Type, @n: Nat, use Eql(T)) => Eql(Vec(T, n));
        let show(b: Bool) -> Str = Str/concat(Bool/to_str(b), " ");
        let _ = print(show(Certified/cert(1, Eq/refl()) == Certified/cert(1, Eq/refl())))!;
        let _ = print(show(Certified/cert(1, Eq/refl()) == Certified/cert(2, Eq/refl())))!;
        let _ = print(show(Vec/cons(1, Vec/cons(2, Vec/nil())) == Vec/cons(1, Vec/cons(2, Vec/nil()))))!;
        print(show(Vec/cons(1, Vec/cons(2, Vec/nil())) == Vec/cons(1, Vec/cons(3, Vec/nil()))))
        "#;

    assert_eq!(run(source), b"true false true false ");
}

#[test]
fn the_eql_derivation_shares_the_eligibility_and_the_provenance() {
    let proposition = r#"
        use /std/{Str, Eql};
        induct Holds: pub Prop | yes() end
        satisfy Eql(Holds);
        /std/print("")
        "#;
    let report = error(proposition);
    assert!(
        report.contains("cannot derive '/syn/Eql' for Holds\n  Holds is a proposition, whose values erase; write the body"),
        "{report}"
    );

    let premise = r#"
        use /std/{Str, Eql};
        induct Box(A: Type): pub Type | boxed(A) end
        satisfy (@A: Type) => Eql(Box(A));
        /std/print("")
        "#;
    let report = error(premise);
    assert!(
        report.contains("no witness of Eql(A) found\n  needed by '/Box/boxed' for payload #1 — add `use Eql(A)` to the telescope"),
        "{report}"
    );
}

// --- Refusals, each at the declaration and naming what to write instead. ---

#[test]
fn an_ineligible_key_is_refused_by_its_shape() {
    let proposition = r#"
        use /std/{Str, Spell};
        induct Holds: pub Prop | yes() end
        satisfy Spell(Holds);
        /std/print("")
        "#;
    let report = error(proposition);
    assert!(
        report.contains("cannot derive '/syn/Spell/Spell' for Holds\n  Holds is a proposition, whose values erase; write the body"),
        "{report}"
    );

    let concept = r#"
        use /std/{Nat, Str, Spell};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Spell(Tag(Nat));
        /std/print("")
        "#;
    let report = error(concept);
    assert!(
        report.contains("cannot derive '/syn/Spell/Spell' for Tag(Nat)\n  Tag(Nat) is a concept's record; write the body"),
        "{report}"
    );

    let type_valued = r#"
        use /std/{Str, Spell};
        induct Holder: pub Type | holds(Prop) end
        satisfy Spell(Holder);
        /std/print("")
        "#;
    let report = error(type_valued);
    assert!(
        report.contains("cannot derive '/syn/Spell/Spell' for Holder\n  payload #1 of '/Holder/holds' is a type, which no value spells; write the body"),
        "{report}"
    );
}

#[test]
fn a_private_representation_refuses_the_derivation_outside_its_module() {
    let source = r#"
        use /std/{Nat, Str, Spell};
        mod Guard
            use /std/{Nat};
            pub induct Secret: Type | s(Nat) end
        end
        use Guard/{Secret};
        satisfy Spell(Secret);
        /std/print("")
        "#;

    let report = error(source);
    assert!(
        report.contains("the representation of type '/Guard/Secret' is private"),
        "{report}"
    );
}

#[test]
fn a_missing_payload_witness_names_the_payload_and_the_premise_to_add() {
    let premise = r#"
        use /std/{Str, Spell};
        induct Box(A: Type): pub Type | boxed(A) end
        satisfy (@A: Type) => Spell(Box(A));
        /std/print("")
        "#;
    let report = error(premise);
    assert!(
        report.contains("no witness of Spell(A) found\n  needed by '/Box/boxed' for payload #1 — add `use Spell(A)` to the telescope"),
        "{report}"
    );

    let unwitnessed = r#"
        use /std/{Str, Spell};
        induct Opaque: pub Type | o() end
        induct Holder: pub Type | holds(inner: Opaque) end
        satisfy Spell(Holder);
        /std/print("")
        "#;
    // The goal's spelling is the deferred-goal report's, which today renders a nominal type through its recursive-group projection (a written `Spell/spell(i)` reports identically); the derivation's contribution is the provenance line, located at the declaration.
    let report = error(unwitnessed);
    assert!(report.contains("no witness of Spell("), "{report}");
    assert!(
        report.contains("needed by '/Holder/holds' for payload 'inner'\n"),
        "{report}"
    );
    assert!(report.contains("satisfy Spell(Holder);"), "{report}");
}

// The renderers spell the two shapes a derived body is built from, pinned as the text a re-parse reads: a derivation's output is only ever their output over spelled pieces. An empty label is the positional field of a newtype-like struct.
#[test]
fn the_renderers_spell_a_call_and_a_record() {
    let source = r#"
        use /std/{Str, print};
        use /syn/Spell/{call, record};
        let _ = print(call("/Tree/leaf", []))!;
        let _ = print("\n")!;
        let _ = print(call("/Tree/node", ["1", "/Tree/leaf()"]))!;
        let _ = print("\n")!;
        let _ = print(record("/Point", [("x", "1"), ("y", "2")]))!;
        let _ = print("\n")!;
        let _ = print(record("/Meters", [("", "5")]))!;
        let _ = print("\n")!;
        print(record("/Unit", []))
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
        let _ = print(Spell/spell(()))!;
        let _ = print("\n")!;
        let _ = print(Spell/spell((1,)))!;
        let _ = print("\n")!;
        let _ = print(Spell/spell((1, true)))!;
        let _ = print("\n")!;
        print(Spell/spell((1, true, "s")))
        "#;

    assert_eq!(run(source), b"()\n(1,)\n(1, true)\n(1, true, \"s\")");
}

// The lowered module carries the declaration as the same anonymous definition a written witness produces, with the `derive` transient in body position — under the telescope where it has one, so the premises are in scope when the body is checked.
#[test]
fn a_body_less_witness_lowers_to_the_derive_transient() {
    let source = r#"
        use /std/{Str, Show};
        induct Point: pub Type | origin() end
        induct Wrap(A: Type): pub Type | wrap(A) end
        satisfy Show(Point);
        satisfy (@A: Type, use Show(A)) => Show(Wrap(A));
        /std/print("")
        "#;

    let lowered = core(source);
    assert_eq!(lowered.matches("derive").count(), 2, "{lowered}");
}

// `Show` is deliberately underivable — the human-facing display is written by hand — so a standard concept refuses exactly as a user's own does, and a telescope changes nothing: the body is checked under it and refused there.
#[test]
fn a_concept_without_a_derivation_is_refused_by_name() {
    let standard = r#"
        use /std/{Str, Show};
        induct Point: pub Type | origin() end
        satisfy Show(Point);
        /std/print("")
        "#;
    assert!(
        error(standard).contains("no derivation exists for '/std/Show/Show'; write the body"),
        "{}",
        error(standard)
    );

    let own = r#"
        use /std/{Nat, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag(Nat);
        /std/print("")
        "#;
    assert!(
        error(own).contains("no derivation exists for '/Tag'; write the body"),
        "{}",
        error(own)
    );

    let premised = r#"
        use /std/{Str, Show};
        induct Wrap(A: Type): pub Type | wrap(A) end
        satisfy (@A: Type, use Show(A)) => Show(Wrap(A));
        /std/print("")
        "#;
    assert!(
        error(premised).contains("no derivation exists for '/std/Show/Show'; write the body"),
        "{}",
        error(premised)
    );
}

// Registration reads the signature alone, so the orphan rule and the one-witness-per-key rule refuse a body-less declaration with the written form's exact reports (`concepts/coherence_tests.rs`).
#[test]
fn the_signature_refusals_fire_on_a_body_less_witness_as_on_a_written_one() {
    let orphan = r#"
        use /std/{Bool, Ord};
        satisfy Ord(Bool);
        /std/print("")
        "#;
    assert!(error(orphan).ends_with(
        "orphan witness of '/std/Ord/Ord' for head 'Bool', declared in the entry module\n  \
         a witness may only be declared where the concept or a type in its head is already declared"
    ));

    let duplicate = r#"
        use /std/{Nat, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        satisfy Show(Nat);
        /std/print("")
        "#;
    assert!(error(duplicate).ends_with(
        "duplicate witness of '/Show' for head 'Nat'\n  \
         one is declared in the entry module, another in the entry module\n  \
         every concept-head pair has at most one witness, program-wide"
    ));
}

// A parameterized family is a type constructor, not a type, and the head refuses before any body is reached — with the written form's exact report, the telescope form `(@A: Type, …) => C(Tree(A))` being what to write instead.
#[test]
fn a_parameterized_head_is_refused_as_a_written_witness_is() {
    let written = r#"
        use /std/{Str, Show};
        induct Tree(A: Type): pub Type | leaf(A) end
        satisfy Show(Tree) {
            show(t) = "",
        }
        /std/print("")
        "#;
    let body_less = r#"
        use /std/{Str, Show};
        induct Tree(A: Type): pub Type | leaf(A) end
        satisfy Show(Tree);
        /std/print("")
        "#;

    let report = error(body_less);
    let written = error(written);
    assert!(
        report.contains("type mismatch\n  inferred: (A: Type) -> Type\n  expected: Type"),
        "{report}"
    );
    // Identical but for the quoted source line: the same frame, message and caret column, because the body is not where the refusal comes from.
    fn frame(report: &str) -> Vec<&str> {
        report.lines().take(4).collect()
    }
    assert_eq!(frame(&report), frame(&written));
    assert_eq!(report.lines().last(), written.lines().last());
}

// Sealing is decided at the declaration, before the derivation lookup: the refusal is the written form's (`concepts/sealed_tests.rs`), so no derivation could become a door through representation privacy.
#[test]
fn a_sealed_concept_refuses_a_body_less_witness_outside_its_module() {
    let source = r#"
        use /std/{Nat, Str};
        mod Guard
            use /std/{Nat, Str};
            pub concept Tag(A : Type) : Type {
                tag(A) -> Str
            }
            satisfy Tag(Nat) {
                tag(n) = Nat/to_str(n)
            }
        end
        use Guard/{Tag};
        satisfy Tag(Str);
        /std/print("no")
        "#;

    let report = error(source);
    assert!(
        report.contains("is private to its declaring module"),
        "{report}"
    );
    assert!(!report.contains("no derivation"), "{report}");
}
