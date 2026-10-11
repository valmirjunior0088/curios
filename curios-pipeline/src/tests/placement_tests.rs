//! Where a refusal lands: on the term it is about, and on the name of its declaration where it is about the declaration as a whole.

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
