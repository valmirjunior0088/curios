//! Where a refusal lands: on the term it is about, and on the name of its declaration where it is about the declaration as a whole; and the second place it notes where it is about two.

use {
    super::test_support::*,
    crate::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_text::RootSource,
    curios_utilities::Report,
};

/// What `source` is refused with, as a compilation reports it.
fn refusal(source: &str) -> Vec<Report> {
    let entrypoint = entrypoint_of(source);
    let refused = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .map(|_| ())
    .expect_err("the fixture is refused");

    refused.reports().to_vec()
}

/// The text each report of `source`'s refusal is placed at, in order: what its caret underlines, or `None` for a report with no place.
fn placed(source: &str) -> Vec<Option<String>> {
    refusal(source)
        .iter()
        .map(|report| {
            let span = report.span.as_ref()?;

            Some(span.source.text[span.start..span.end].to_string())
        })
        .collect()
}

fn at(text: &str) -> Vec<Option<String>> {
    vec![Some(text.to_string())]
}

/// The second places of `source`'s refusal: the text each note of its first report is placed at, with the note's line.
fn noted(source: &str) -> Vec<(String, String)> {
    refusal(source)[0]
        .notes
        .iter()
        .map(|note| {
            let span = &note.span;

            (
                span.source.text[span.start..span.end].to_string(),
                note.message.clone(),
            )
        })
        .collect()
}

fn note(text: &str, message: &str) -> (String, String) {
    (text.to_string(), message.to_string())
}

/// Whether the one note of `source`'s refusal is placed before the refusal itself, in the text.
fn noted_above(source: &str) -> bool {
    let reports = refusal(source);
    let refused = reports[0].span.as_ref().expect("the refusal is placed");

    reports[0].notes[0].span.start < refused.start
}

#[test]
fn a_payload_that_is_not_positive_is_placed_at_its_type() {
    let source = r#"
        use /std/{Nat};

        induct Bad: pub Type
        | ok(Nat)
        | mk(Nat, (Bad) -> Nat)
        end

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("(Bad) -> Nat"));
}

#[test]
fn a_field_that_is_not_positive_is_placed_at_its_type() {
    let source = r#"
        use /std/{Nat};

        struct Bad: pub Type {
            run: (Bad) -> Nat,
        }

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("(Bad) -> Nat"));
}

// A refusal about a declaration as a whole holds no position of its own, and lands on the name the declaration was written under.
#[test]
fn a_type_that_does_not_terminate_is_placed_at_its_name() {
    let source = r#"
        use /std/{Nat};

        let loop(n: Nat) -> Type = loop(n);

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("loop"));
}

// An erasure obligation is decided over a declaration's final terms, after its elaboration: it is reported under the declaration's name as a refusal of it is, at the name it blames.
#[test]
fn a_type_reaching_a_partial_definition_is_placed_at_the_name_it_reaches() {
    let source = r#"
        use /std/{Nat};

        let spin(n: Nat) -> Nat = spin(n);

        let T(n: Nat) -> Type =
            match spin(n) == 0
            | true => Nat
            | false => {}
            end;

        /std/print("x")
    "#;
    let reports = refusal(source);
    assert!(
        reports[0].message.starts_with("while elaborating T:"),
        "{}",
        reports[0].message
    );
    assert_eq!(placed(source), at("spin"));
}

#[test]
fn a_proof_reaching_a_partial_definition_is_placed_at_the_name_it_reaches() {
    let source = r#"
        use /std/{Nat};

        let loop(P: Prop, n: Nat) -> P = loop(P, n);

        /std/print("x")
    "#;
    let reports = refusal(source);
    let span = reports[0].span.as_ref().expect("the obligation is placed");
    assert_eq!(&span.source.text[span.start..span.end], "loop");
    // The occurrence in the body, not the name the declaration is written under.
    assert!(
        span.source.text[..span.start].ends_with("-> P = "),
        "placed after {:?}",
        &span.source.text[..span.start]
    );
}

#[test]
fn a_structure_refused_as_a_whole_is_placed_at_its_name() {
    let source = r#"
        use /std/{Nat};

        struct P: pub Prop {
            n: Nat,
        }

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("P"));
}

#[test]
fn a_cycle_is_placed_at_its_first_members_name() {
    let source = r#"
        use /std/{Nat};

        let f(n : Nat) -> Nat = g(n);

        let g(n : Nat) -> Nat = f(n);

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("f"));
}

#[test]
fn a_cycle_through_a_witness_is_placed_at_its_first_member() {
    let source = r#"
        use /std/{Nat, Str, Show};

        let shown: Str = Show/show(mine());

        struct Mine: pub Type {
            n: Nat,
        }

        let mine() -> Mine = Mine { 1 };

        satisfy Show(Mine) {
            show(m) = shown,
        }

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("shown"));
}

#[test]
fn a_private_name_in_a_public_signature_is_placed_at_the_item_that_exposes_it() {
    let source = r#"
        use /std/{Nat};

        mod Inner
            use /std/{Nat};

            struct Hidden: pub Type {
                n: Nat,
            }

            pub let leak(h: Hidden) -> Nat = h.n;
        end

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("leak"));
}

#[test]
fn a_private_name_in_a_public_representation_is_placed_at_the_type_that_exposes_it() {
    let source = r#"
        use /std/{Nat};

        mod Inner
            use /std/{Nat};

            struct Hidden: pub Type {
                n: Nat,
            }

            pub struct Shown: pub Type {
                hidden: Hidden,
            }
        end

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("Shown"));
}

#[test]
fn a_conflicting_export_is_placed_at_the_selector_that_arrives_second() {
    let source = r#"
        mod A
            pub let x: /std/Nat = 1;
        end

        mod B
            pub let y: /std/Nat = 2;
            pub let x: /std/Nat = 2;
        end

        mod C
            pub use /A/{x};
            pub use /B/{y, x};
        end

        /std/print("x")
    "#;
    let reports = refusal(source);
    let span = reports[0].span.as_ref().expect("the conflict is placed");
    assert_eq!(&span.source.text[span.start..span.end], "x");
    assert!(
        span.source.text[..span.start].ends_with("pub use /B/{y, "),
        "placed after {:?}",
        &span.source.text[..span.start]
    );
}

// A glob writes no selector, so a refusal of what it brings is placed at its path.
#[test]
fn a_conflicting_glob_export_is_placed_at_its_path() {
    let source = r#"
        mod A
            pub let x: /std/Nat = 1;
        end

        mod B
            pub let x: /std/Nat = 2;
        end

        mod C
            pub use /A/*;
            pub use /B/*;
        end

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("/B"));
}

#[test]
fn a_re_export_of_a_missing_name_is_placed_at_its_selector() {
    let source = r#"
        mod A
            pub let x: /std/Nat = 1;
        end

        mod C
            pub use /A/{x, nope};
        end

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("nope"));
}

#[test]
fn a_re_export_through_a_private_module_is_placed_at_its_path() {
    let source = r#"
        mod A
            pub induct Sealed: Type
            | mk()
            end
        end

        mod B
            pub use /A/Sealed/*;
        end

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("/A/Sealed"));
}

// A refusal about two places stands at one and notes the other, so neither is a line number in its sentence.
#[test]
fn a_cycle_notes_every_member_but_the_one_it_stands_at() {
    let source = r#"
        use /std/{Nat};

        let a(n: Nat) -> Nat = b(n);

        let b(n: Nat) -> Nat = c(n);

        let c(n: Nat) -> Nat = a(n);

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("a"));
    assert_eq!(
        noted(source),
        [
            note("b", "'b' is declared here"),
            note("c", "'c' is declared here"),
        ]
    );
}

#[test]
fn a_cycle_through_a_witness_notes_where_the_witness_is_declared() {
    let source = r#"
        use /std/{Nat, Str, Show};

        let shown: Str = Show/show(mine());

        struct Mine: pub Type {
            n: Nat,
        }

        let mine() -> Mine = Mine { 1 };

        satisfy Show(Mine) {
            show(m) = shown,
        }

        /std/print("x")
    "#;
    assert_eq!(
        noted(source),
        [note(
            "Show(Mine)",
            "the witness of Show(Mine) is declared here"
        )]
    );
}

#[test]
fn a_duplicate_witness_notes_the_one_that_stood_first() {
    let source = r#"
        use /std/{Nat, Str};

        concept Tag(A: Type): pub Type {
            tag(A) -> Str,
        }

        satisfy Tag(Nat) {
            tag(n) = "a",
        }

        satisfy Tag(Nat) {
            tag(n) = "b",
        }

        /std/print("x")
    "#;
    assert_eq!(placed(source), at("Tag(Nat)"));
    assert_eq!(
        noted(source),
        [note("Tag(Nat)", "the other witness is declared here")]
    );
    assert!(noted_above(source));
}

#[test]
fn a_sort_decided_twice_notes_the_term_that_decided_it_first() {
    let source = r#"
        use /std/{Nat};
        let at_type(B: Type) -> Nat = 0;
        let at_prop(P: Prop) -> Nat = 0;
        pub let both(A: Sort) -> Nat = at_type(A) + at_prop(A);
        /std/print("")
    "#;
    assert_eq!(placed(source), at("A"));
    assert_eq!(noted(source), [note("A", "this term decides it as `Type`")]);
    assert!(noted_above(source));
}

#[test]
fn a_conversion_held_up_by_a_goal_notes_the_goal() {
    let source = r#"
        use /std/{Nat, Eq};
        let f(@A : Type, a : A) -> {} = ();
        let stuck(k : Nat, h : Eq()(k, 7)) -> {} = f((n : Nat) => Eq/cong(?, h));
        /std/Io/pure(())
    "#;
    assert_eq!(noted(source), [note("?", "this goal `?` was never solved")]);
}

#[test]
fn a_label_imported_twice_notes_the_other_import() {
    let source = r#"
        use /std/{Nat, print, Nat};

        print("x")
    "#;
    assert_eq!(placed(source), at("Nat"));
    assert_eq!(
        noted(source),
        [note("Nat", "`Nat` is also brought into scope here")]
    );
    assert!(noted_above(source));
}

// A module's declarations are in scope before its `use` lines are read, so the import is the claim refused and the declaration the one noted, wherever each is written.
#[test]
fn an_import_of_a_declared_label_notes_the_declaration() {
    let source = r#"
        use /std/{Nat};

        let Nat: Type = /std/Bool;

        /std/print("x")
    "#;
    let reports = refusal(source);
    let refused = reports[0].span.as_ref().expect("the conflict is placed");
    assert!(
        refused.source.text[..refused.start].ends_with("use /std/{"),
        "placed after {:?}",
        &refused.source.text[..refused.start]
    );
    let other = &reports[0].notes[0].span;
    assert!(
        other.source.text[..other.start].ends_with("let "),
        "noted after {:?}",
        &other.source.text[..other.start]
    );
    assert_eq!(
        reports[0].notes[0].message,
        "`Nat` is also brought into scope here"
    );
}
