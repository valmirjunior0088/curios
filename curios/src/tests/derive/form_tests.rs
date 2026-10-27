//! The derived witness's form: the transient it lowers to and its expansion, and what the compiler refuses in its place, as it refuses a written witness.

use crate::tests::{core, core_elab, error};

#[test]
fn the_stages_show_the_transient_and_its_expansion() {
    let source = r#"
        use /std/{Nat, Str, Spell, print};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Spell(Tree) { .. }
        print(Spell/spell(Tree/leaf(1)))
        "#;

    let lowered = core(source);
    assert!(lowered.contains("derive"), "{lowered}");
    let elaborated = core_elab(source);
    assert!(!elaborated.contains("derive"), "{elaborated}");
    assert!(elaborated.contains("Spell/call"), "{elaborated}");
}

// --- Refusals, each at the declaration and naming what to write instead. ---

// A derived body is a term judged as an authored one is, its termination included: over a recursive family it reaches its own entry, as the body written out does, and a proof that reads either is refused, so no derivation is a recursion graded total that a written one is not.
#[test]
fn a_derived_body_reaching_a_proof_is_held_to_termination_as_its_written_body_is() {
    let derived = r#"
        use /std/{Nat};
        use /std/ops/{Eql};
        use /std/Bool/{False, Holds};
        induct Tree: Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Eql(Tree) { .. }
        let _apart(h: Holds(Tree/leaf(1) == Tree/leaf(2))) -> False = h;
        /std/print("")
        "#;
    let written = r#"
        use /std/{Nat, Bool};
        use /std/ops/{Eql};
        use /std/Bool/{False, Holds};
        induct Tree: Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Eql(Tree) {
            eql(a, b) = match a
                | leaf(x) => match b | leaf(y) => x == y | _ => false end
                | node(l, r) => match b | node(m, s) => Eql/eql(l, m) && Eql/eql(r, s) | _ => false end
                end,
            neq(a, b) = Bool/not(Eql/eql(a, b)),
        }
        let _apart(h: Holds(Tree/leaf(1) == Tree/leaf(2))) -> False = h;
        /std/print("")
        "#;

    for source in [derived, written] {
        let report = error(source);
        assert!(
            report.contains("a proof in '_apart' is a proof position but reaches '/witness@/std/ops/Eql(/Tree)', which is not known to terminate"),
            "{report}"
        );
    }
}

// A proposition is no key: every derivable concept is over a `Type`, so the signature is refused before a body is written, and no derived body takes a proof apart into data.
#[test]
fn a_proposition_is_no_key_of_a_derived_witness() {
    let source = r#"
        use /std/ops/{Eql};
        induct Two: Prop | a() | b() end
        satisfy Eql(Two) { .. }
        /std/print("")
        "#;

    let report = error(source);
    assert!(
        report.contains("type mismatch\n  inferred: Prop\n  expected: Type"),
        "{report}"
    );
}

#[test]
fn an_ineligible_key_is_refused_by_its_shape() {
    let concept = r#"
        use /std/{Nat, Str, Spell};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Spell(Tag(Nat)) { .. }
        /std/print("")
        "#;
    let report = error(concept);
    assert!(
        report.contains(
            "cannot derive 'Spell' for Tag(Nat)\n  Tag(Nat) is a concept's record; write the body"
        ),
        "{report}"
    );

    let type_valued = r#"
        use /std/{Str, Spell};
        induct Holder: pub Type | holds(Prop) end
        satisfy Spell(Holder) { .. }
        /std/print("")
        "#;
    let report = error(type_valued);
    assert!(
        report.contains("cannot derive 'Spell' for Holder\n  payload #1 of 'Holder/holds' is a type, which no value spells; write the body"),
        "{report}"
    );

    // A key the refusal must *name*: an unparameterized family whose own constructors mention it unfolds to its recursion block, and the site printed three lines of `rec #0: Type = R; #0` where the author wrote `R`. The struct and parameterized cases above never showed it, because neither reduces to one.
    let recursive = r#"
        use /std/{Str, Spell};
        induct R: pub Type | node(R, Type) | leaf() end
        satisfy Spell(R) { .. }
        /std/print("")
        "#;
    let report = error(recursive);
    assert!(
        report.contains("cannot derive 'Spell' for R\n  payload #2 of 'R/node' is a type, which no value spells; write the body"),
        "{report}"
    );
    assert!(!report.contains("rec #0"), "{report}");
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
        satisfy Spell(Secret) { .. }
        /std/print("")
        "#;

    let report = error(source);
    assert!(
        report.contains("the representation of type 'Secret' is private"),
        "{report}"
    );
}

#[test]
fn a_missing_payload_witness_names_the_payload_and_the_premise_to_add() {
    let premise = r#"
        use /std/{Str, Spell};
        induct Box(A: Type): pub Type | boxed(A) end
        satisfy (@A: Type) => Spell(Box(A)) { .. }
        /std/print("")
        "#;
    let report = error(premise);
    assert!(
        report.contains("no witness of Spell(A) found\n  needed by 'Box/boxed' for payload #1 — add `use Spell(A)` to the telescope"),
        "{report}"
    );

    let unwitnessed = r#"
        use /std/{Str, Spell};
        induct Opaque: pub Type | o() end
        induct Holder: pub Type | holds(inner: Opaque) end
        satisfy Spell(Holder) { .. }
        /std/print("")
        "#;
    // The goal's spelling is the deferred-goal report's, rendered as every report renders a type — the nominal name, not its recursive-group projection; the derivation's contribution is the provenance line, located at the declaration.
    let report = error(unwitnessed);
    assert!(
        report.contains(
            "no witness of Spell(Opaque) found\n  needed by 'Holder/holds' for payload 'inner'\n"
        ),
        "{report}"
    );
    assert!(report.contains("satisfy Spell(Holder) { .. }"), "{report}");
}

// The lowered module carries the declaration as the same anonymous definition a written witness produces, with the `derive` transient in body position — under the telescope where it has one, so the premises are in scope when the body is checked.
#[test]
fn a_derived_witness_lowers_to_the_derive_transient() {
    let source = r#"
        use /std/{Str, Show};
        induct Point: pub Type | origin() end
        induct Wrap(A: Type): pub Type | wrap(A) end
        satisfy Show(Point) { .. }
        satisfy (@A: Type, use Show(A)) => Show(Wrap(A)) { .. }
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
        satisfy Show(Point) { .. }
        /std/print("")
        "#;
    assert!(
        error(standard).contains("no derivation exists for 'Show'; write the body"),
        "{}",
        error(standard)
    );

    let own = r#"
        use /std/{Nat, Str};
        pub concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }
        satisfy Tag(Nat) { .. }
        /std/print("")
        "#;
    assert!(
        error(own).contains("no derivation exists for 'Tag'; write the body"),
        "{}",
        error(own)
    );

    let premised = r#"
        use /std/{Str, Show};
        induct Wrap(A: Type): pub Type | wrap(A) end
        satisfy (@A: Type, use Show(A)) => Show(Wrap(A)) { .. }
        /std/print("")
        "#;
    assert!(
        error(premised).contains("no derivation exists for 'Show'; write the body"),
        "{}",
        error(premised)
    );
}

// Registration reads the signature alone, so the orphan rule and the one-witness-per-key rule refuse a derived declaration with the written form's exact reports (`concepts/coherence_tests.rs`).
#[test]
fn the_signature_refusals_fire_on_a_derived_witness_as_on_a_written_one() {
    let orphan = r#"
        use /std/{Bool, Ord};
        satisfy Ord(Bool) { .. }
        /std/print("")
        "#;
    let rendered = error(orphan);
    assert!(
        rendered.contains(
            "orphan witness of 'Ord' for head 'Bool', declared in the entry module\n  \
             a witness may only be declared where the concept or a type in its head is already declared"
        ),
        "{rendered}"
    );
    // Located at the written concept application, exactly as the written form is: a derived declaration has the same declared type.
    assert!(rendered.contains("satisfy Ord(Bool) { .. }"), "{rendered}");

    let duplicate = r#"
        use /std/{Nat, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        satisfy Show(Nat) { .. }
        /std/print("")
        "#;
    let rendered = error(duplicate);
    assert!(
        rendered.contains(
            "duplicate witness of 'Show' for head 'Nat'\n  \
             both are declared in the entry module\n  \
             every concept-head pair has at most one witness, program-wide"
        ),
        "{rendered}"
    );
    assert!(rendered.contains("satisfy Show(Nat) { .. }"), "{rendered}");
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
    let derived = r#"
        use /std/{Str, Show};
        induct Tree(A: Type): pub Type | leaf(A) end
        satisfy Show(Tree) { .. }
        /std/print("")
        "#;

    let report = error(derived);
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
fn a_sealed_concept_refuses_a_derived_witness_outside_its_module() {
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
        satisfy Tag(Str) { .. }
        /std/print("no")
        "#;

    let report = error(source);
    assert!(
        report.contains("is private to its declaring module"),
        "{report}"
    );
    assert!(!report.contains("no derivation"), "{report}");
}
