//! Derived witnesses: the body-less `satisfy C(T);` form, the transient it lowers to, and what the compiler writes — or refuses — in its place.

use crate::tests::{core, error, run};

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
