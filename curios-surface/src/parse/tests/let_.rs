//! `let` and its `and` groups, `test` declarations, and the span each declaration's label takes.

use {crate::*, curios_common::Plicity, curios_module::SYNTAX};

#[test]
fn top_let_without_pub() {
    assert_eq!(
        "let x : Type = Type;".parse::<Module>().unwrap().items,
        vec![TopItem::Let(vec![TopLet {
            word: WordAt::default(),
            core: (),
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
            word: WordAt::default(),
            core: (),
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

/// The optional-type form is local-only: a module-level `let` without a type is refused by the rule, with the caret on the `=`, rather than by the sugar's `expected: `(``, which named one of the two tokens that would have served.
#[test]
fn let_requires_a_type() {
    let report = "let x = Type;".parse::<Module>().unwrap_err().format();
    assert!(
        report.contains("this definition states no type")
            && report.contains("only a local `let` may leave its type out"),
        "reported {report}"
    );
    assert!(
        report.contains("│  1 │ let x = Type;\n│    │       ^\n"),
        "reported {report}"
    );
    assert!(!report.contains("expected: `(`"), "reported {report}");
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
        assert!(!report.contains("expected: `(`"), "reported {report}");
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
                word: WordAt::default(),
                core: (),
                doc: None,
                vis_pub: true,
                label: "id".into(),
                signature: LetSignature::Name {
                    type_: Some(Subterm::Type.into()),
                    body: Subterm::Type.into(),
                },
            },
            TopLet {
                word: WordAt::default(),
                core: (),
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
    let source = "test the_answer_holds => Test/assert(42 == 42);";
    let module = source.parse::<Module>().unwrap();
    let [TopItem::Test(test)] = module.items.as_slice() else {
        panic!("expected one test item, got {:?}", module.items);
    };
    assert_eq!(test.label, "the_answer_holds");
    let printed = module.printed(&SYNTAX).to_string();
    assert_eq!(
        printed.trim(),
        "test the_answer_holds =>\n    Test/assert(42 == 42);"
    );
    assert_eq!(printed.parse::<Module>().unwrap().items, module.items);
}

#[test]
fn a_test_takes_no_parameters() {
    // A name and a description, with nothing between them. A telescope would say the claim holds of every instantiation, which nothing but a proof decides — that is a `let` whose type states the claim — so parentheses are not part of this form, empty ones included.
    assert!(
        "test add_commutes(n: Nat, m: Nat) => Test/assert(true);"
            .parse::<Module>()
            .is_err()
    );
    assert!(
        "test empty() => Test/assert(true);"
            .parse::<Module>()
            .is_err()
    );
}

/// A program's `test` item reports its own fault rather than becoming the program's tail.
#[test]
fn a_malformed_test_in_a_program_reports_its_own_fault() {
    // The fall-through a contextual word needs ends at the label, so each of these commits and names what it wanted. Falling through instead, the report would land on `ok`, where the tail the item became ran out.
    for (source, expected) in [
        ("test ok(n: Nat) => Test/assert(true);", "expected: `=>`"),
        ("test ok Test/assert(true);", "expected: `=>`"),
        ("test ok => Test/assert(true)", "expected: `;`"),
    ] {
        let error = source.parse::<Entrypoint>().unwrap_err().format();
        assert!(error.contains(expected), "{source}\n{error}");
    }
}

/// A declaration's label is spanned over the word alone, whatever whitespace follows it: the span is what a report about the declaration underlines.
#[test]
fn every_declaration_label_spans_its_word_alone() {
    let module = "pub let  x  : Type = Type;\nmod  Inner ;\ninduct  Foo  : Type | bar() end\nstruct  Pt  : Type { }\nconcept  Sh (A : Type) : Type { }\ntest  it  => Type;\nforeign  clock() -> Nat;"
        .parse::<Module>()
        .unwrap();
    let spelled = module
        .items
        .iter()
        .map(|item| match item {
            TopItem::Let(items) => crate::parse::test_support::spelled(&items[0].label),
            TopItem::Mod(item) => crate::parse::test_support::spelled(&item.label),
            TopItem::Induct(items) => crate::parse::test_support::spelled(&items[0].label),
            TopItem::Struct(items) => crate::parse::test_support::spelled(&items[0].label),
            TopItem::Concept(items) => crate::parse::test_support::spelled(&items[0].label),
            TopItem::Test(item) => crate::parse::test_support::spelled(&item.label),
            TopItem::Foreign(item) => crate::parse::test_support::spelled(&item.label),
            other => panic!("unexpected item {other:?}"),
        })
        .collect::<Vec<_>>();
    assert_eq!(spelled, ["x", "Inner", "Foo", "Pt", "Sh", "it", "clock"]);
}

// A `let` chain stands only as a body, so a value, an argument, a list element, a field's value and a parenthesized term each refuse one, naming the rule at its `let`.
#[test]
fn a_chain_where_a_term_is_read_is_refused_naming_the_rule() {
    for source in [
        "let f() -> Nat => let x = let y = 1; y; x;",
        "let f() -> Nat => g(1, let y = 1; y);",
        "let f() -> List(Nat) => [let y = 1; y];",
        "let f() -> P => P { x = let y = 1; y };",
        "let f() -> Nat => 1 + (let y = 1; y);",
    ] {
        let report = source.parse::<Module>().unwrap_err().format();
        assert!(
            report.contains("a `let` chain stands only as a body"),
            "{source:?} reported {report}"
        );
    }
}

// A chain is read where a body is: a function's and a top-level value's, a lambda's, an arm's, a test's, a function field's and a chain's own tail.
#[test]
fn a_chain_is_read_wherever_a_body_is() {
    for source in [
        "let f() -> Nat => let y = 1; y;",
        "let v : Nat = let y = 1; y;",
        "let f() -> (Nat) -> Nat => (n) => let y = n; y;",
        "let f(b: Bool) -> Nat => match b | true => let y = 1; y | false => 0 end;",
        "test t => let y = 1; Test/pass();",
        "let f() -> P => P { g(n) => let y = n; y };",
        "let f() -> Nat => let y = 1; let z = y; z;",
    ] {
        if let Err(error) = source.parse::<Module>() {
            panic!("{source:?} refused: {}", error.format());
        }
    }
}

/// A `let` written `name(…) =` holds its value to the case `name`, its payloads binder patterns under their marks, and prints back as written; written with `=>`, as a lambda's body is, it defines a local function, its types left out where a lambda's may be, and a result in parentheses is the result and never a lambda's parameters. Mutation-checked: with the constructor pattern left out of the binder patterns, the first `let` is refused; with the lambda form left out of a local `let`'s signatures, the untyped function is refused; and with the result read as any term, the parenthesized one takes the body as a lambda's.
#[test]
fn a_let_tells_a_constructor_pattern_from_a_local_function_at_its_arrow() {
    let first = |source: &str| {
        let Subterm::Let(let_) = source.parse::<Term>().unwrap().as_subterm().clone() else {
            panic!("expected a let");
        };
        let_.groups[0].members[0].clone()
    };

    let held = first("let cons(head, @_, (a, b)) = whole; head");
    let Pattern::Variant { tag, args } = &held.binder else {
        panic!("expected a constructor pattern, got {:?}", held.binder);
    };
    assert_eq!(tag.as_str(), "cons");
    assert_eq!(
        args.iter().map(|(mark, _)| *mark).collect::<Vec<_>>(),
        [Plicity::Explicit, Plicity::Implicit, Plicity::Explicit]
    );
    assert!(matches!(
        held.signature,
        LetSignature::Name { type_: None, .. }
    ));
    assert!(matches!(
        first("let refl() = p; u").binder,
        Pattern::Variant { .. }
    ));

    let defined = first("let one() -> Nat => 1; one()");
    assert!(matches!(defined.binder, Pattern::Binder(_)));
    assert!(matches!(defined.signature, LetSignature::Func { .. }));

    let untyped = first("let increment(n) => n + 1; increment(1)");
    assert!(matches!(untyped.binder, Pattern::Binder(_)));
    assert!(matches!(
        &untyped.signature,
        LetSignature::Lambda { params, output: None, .. } if params.len() == 1
    ));

    let parenthesized = first("let wrap(n: Nat) -> (Nat) => n; wrap(1)");
    let LetSignature::Func { output, body, .. } = &parenthesized.signature else {
        panic!("expected a function, got {:?}", parenthesized.signature);
    };
    assert!(
        !matches!(output.as_subterm(), Subterm::Func(_)),
        "the result read as a lambda: {output:?}"
    );
    assert!(
        matches!(body.as_subterm(), Subterm::Name(_)),
        "the body is {body:?}"
    );

    for source in [
        "let cons(head, @_, (a, b)) = whole; head",
        "let refl() = p; u",
        "let increment(n) => n + 1; increment(1)",
        "let wrap(n: Nat) -> (Nat) => n; wrap(1)",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.printed(&SYNTAX).to_string().parse::<Term>().unwrap(),
            term,
            "round-trip failed for {source:?}"
        );
    }
}
