//! Derived witnesses: the body-less `satisfy C(T);` form, the transient it lowers to, and what the compiler writes — or refuses — in its place.

use crate::tests::{core, error};

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
