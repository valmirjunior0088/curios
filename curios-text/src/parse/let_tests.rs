//! `let` and its `and` groups, `test` declarations, and the span each declaration's label takes.

use crate::*;

#[test]
fn top_let_without_pub() {
    assert_eq!(
        "let x : Type = Type;".parse::<Module>().unwrap().items,
        vec![TopItem::Let(vec![TopLet {
            doc: None,
            vis_pub: false,
            label: "x".into(),
            signature: LetSignature::Name {
                type_: Some(Subterm::Type.into()),
                body: Subterm::Type.into(),
            },
        }])]
    );
}

#[test]
fn top_let_with_pub() {
    assert_eq!(
        "pub let x : Type = Type;".parse::<Module>().unwrap().items,
        vec![TopItem::Let(vec![TopLet {
            doc: None,
            vis_pub: true,
            label: "x".into(),
            signature: LetSignature::Name {
                type_: Some(Subterm::Type.into()),
                body: Subterm::Type.into(),
            },
        }])]
    );
}

/// The optional-type form is local-only: a module-level `let` without a type is refused by the rule, with the caret on the `=`, rather than by the sugar's `Expected '('`, which named one of the two tokens that would have served.
#[test]
fn let_requires_a_type() {
    let report = "let x = Type;".parse::<Module>().unwrap_err().format();
    assert!(
        report.contains("this definition states no type")
            && report.contains("only a local `let` may leave its type out"),
        "reported {report}"
    );
    assert!(
        report.ends_with("    1 | let x = Type;\n      |       ^"),
        "reported {report}"
    );
    assert!(!report.contains("Expected '('"), "reported {report}");
}

/// A member after `and` cannot have its type inferred from a body that may mention its siblings, so a typeless one is refused by the same rule — at the top level and locally.
#[test]
fn a_group_member_states_its_type() {
    let module = "let f : Type = Type and g = Type;"
        .parse::<Module>()
        .unwrap_err()
        .format();
    let local = "let f : Type = Type and g = Type; f"
        .parse::<Term>()
        .unwrap_err()
        .format();
    for report in [module, local] {
        assert!(
            report.contains("every member after `and` state theirs"),
            "reported {report}"
        );
        assert!(!report.contains("Expected '('"), "reported {report}");
    }
}

#[test]
fn top_let_group_mixed_pub() {
    assert_eq!(
        r#"
            pub let id : Type = Type
            and helper : Type = Type;
        "#
        .parse::<Module>()
        .unwrap()
        .items,
        vec![TopItem::Let(vec![
            TopLet {
                doc: None,
                vis_pub: true,
                label: "id".into(),
                signature: LetSignature::Name {
                    type_: Some(Subterm::Type.into()),
                    body: Subterm::Type.into(),
                },
            },
            TopLet {
                doc: None,
                vis_pub: false,
                label: "helper".into(),
                signature: LetSignature::Name {
                    type_: Some(Subterm::Type.into()),
                    body: Subterm::Type.into(),
                },
            },
        ])]
    );
}

#[test]
fn a_test_declaration_parses_and_round_trips() {
    let source = "test the_answer_holds = Test/assert(42 == 42);";
    let module = source.parse::<Module>().unwrap();
    let [TopItem::Test(test)] = module.items.as_slice() else {
        panic!("expected one test item, got {:?}", module.items);
    };
    assert_eq!(test.label, "the_answer_holds");
    let printed = module.to_string();
    assert_eq!(
        printed.trim(),
        "test the_answer_holds =\n    Test/assert(42 == 42);"
    );
    assert_eq!(printed.parse::<Module>().unwrap().items, module.items);
}

#[test]
fn a_test_takes_no_parameters() {
    // A name and a description, with nothing between them. A telescope would say the claim holds of every instantiation, which nothing but a proof decides — that is a `let` whose type states the claim — so parentheses are not part of this form, empty ones included.
    assert!(
        "test add_commutes(n: Nat, m: Nat) = Test/assert(true);"
            .parse::<Module>()
            .is_err()
    );
    assert!(
        "test empty() = Test/assert(true);"
            .parse::<Module>()
            .is_err()
    );
}

/// A program's `test` item reports its own fault rather than becoming the program's tail.
#[test]
fn a_malformed_test_in_a_program_reports_its_own_fault() {
    // The fall-through a contextual word needs ends at the label, so each of these commits and names what it wanted. Falling through instead, the report would land on `ok`, where the tail the item became ran out.
    for (source, expected) in [
        ("test ok(n: Nat) = Test/assert(true);", "Expected '='"),
        ("test ok Test/assert(true);", "Expected '='"),
        ("test ok = Test/assert(true)", "Expected ';'"),
    ] {
        let error = source.parse::<Entrypoint>().unwrap_err().format();
        assert!(error.contains(expected), "{source}\n{error}");
    }
}

/// A declaration's label is spanned over the word alone, whatever whitespace follows it: the span is what a report about the declaration underlines.
#[test]
fn every_declaration_label_spans_its_word_alone() {
    let module = "pub let  x  : Type = Type;\nmod  Inner ;\ninduct  Foo  : Type | bar() end\nstruct  Pt  : Type { }\nconcept  Sh (A : Type) : Type { }\ntest  it  = Type;\nforeign  clock() -> Nat;"
        .parse::<Module>()
        .unwrap();
    let spelled = module
        .items
        .iter()
        .map(|item| match item {
            TopItem::Let(items) => super::test_support::spelled(&items[0].label),
            TopItem::Mod(item) => super::test_support::spelled(&item.label),
            TopItem::Induct(items) => super::test_support::spelled(&items[0].label),
            TopItem::Struct(items) => super::test_support::spelled(&items[0].label),
            TopItem::Concept(items) => super::test_support::spelled(&items[0].label),
            TopItem::Test(item) => super::test_support::spelled(&item.label),
            TopItem::Foreign(item) => super::test_support::spelled(&item.label),
            other => panic!("unexpected item {other:?}"),
        })
        .collect::<Vec<_>>();
    assert_eq!(spelled, ["x", "Inner", "Foo", "Pt", "Sh", "it", "clock"]);
}
