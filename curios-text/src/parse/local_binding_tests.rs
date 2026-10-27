//! Local bindings: a `let` with or without its type and its `and` groups, and a function with or without its annotations.

use {super::test_support::*, crate::*, curios_utilities::Plicity};

#[test]
fn local_let_without_type() {
    // A local `let x = e` omits the type; it parses to `Name { type_: None }`, and the core elaborator infers the body's type.
    assert_eq!(
        "let x = Type; x".parse::<Term>().unwrap(),
        Subterm::Let(Let {
            groups: vec![LetGroup {
                members: vec![LetBinding {
                    binder: Pattern::Binder("x".into()),
                    signature: LetSignature::Name {
                        type_: None,
                        body: Subterm::Type.into(),
                    },
                }]
            }],
            tail: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn local_let_with_type_still_works() {
    assert_eq!(
        "let x : Type = Type; x".parse::<Term>().unwrap(),
        Subterm::Let(Let {
            groups: vec![LetGroup {
                members: vec![LetBinding {
                    binder: Pattern::Binder("x".into()),
                    signature: LetSignature::Name {
                        type_: Some(Subterm::Type.into()),
                        body: Subterm::Type.into(),
                    },
                }]
            }],
            tail: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn func_with_annotation() {
    assert_eq!(
        "(x : Type) => x".parse::<Term>().unwrap(),
        Subterm::Func(Func {
            params: vec![FuncParam {
                plicity: Plicity::Explicit,
                pattern: Pattern::Binder("x".into()),
                annotation: Some(Subterm::Type.into()),
            }],
            body: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn func_with_mixed_annotations() {
    // Annotations are per-parameter and optional; a bare param is `None`.
    assert_eq!(
        "(x : Type, y) => x".parse::<Term>().unwrap(),
        Subterm::Func(Func {
            params: vec![
                FuncParam {
                    plicity: Plicity::Explicit,
                    pattern: Pattern::Binder("x".into()),
                    annotation: Some(Subterm::Type.into()),
                },
                FuncParam {
                    plicity: Plicity::Explicit,
                    pattern: Pattern::Binder("y".into()),
                    annotation: None,
                },
            ],
            body: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn func_without_annotation_still_works() {
    assert_eq!(
        "(x) => x".parse::<Term>().unwrap(),
        Subterm::Func(Func {
            params: vec![FuncParam {
                plicity: Plicity::Explicit,
                pattern: Pattern::Binder("x".into()),
                annotation: None,
            }],
            body: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn local_let_group() {
    // `and` joins members into one statement, each a plain label with a mandatory type.
    let member = |label: &str, body: &str| LetBinding {
        binder: Pattern::Binder(label.into()),
        signature: LetSignature::Name {
            type_: Some(Subterm::Type.into()),
            body: name(body),
        },
    };
    assert_eq!(
        "let a : Type = b and b : Type = a; a"
            .parse::<Term>()
            .unwrap(),
        Subterm::Let(Let {
            groups: vec![LetGroup {
                members: vec![member("a", "b"), member("b", "a")],
            }],
            tail: name("a"),
        })
        .into()
    );
}
