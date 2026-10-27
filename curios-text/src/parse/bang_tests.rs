//! The postfix `!`: where it may stand, how tightly it binds, and that it reads back.

use {super::test_support::*, crate::*, curios_utilities::Plicity};

#[test]
fn let_bang_is_no_longer_grammar() {
    // `!` sequences through the `Monad` concept with no `let ! = <bind>;` header, and `!` is not a binder identifier, so that form is a parse error rather than a `let`.
    assert!("let ! = bind; body".parse::<Term>().is_err());
}

#[test]
fn parse_bang_suffix() {
    assert_eq!(
        "x!".parse::<Term>().unwrap(),
        Subterm::Bang(name("x")).into()
    );
}

#[test]
fn multi_bang_in_apply() {
    // Each argument keeps its own `!`; the desugarer hoists them left-to-right.
    assert_eq!(
        "f(x!, y!)".parse::<Term>().unwrap(),
        Subterm::Apply(Apply {
            head: name("f"),
            arguments: vec![
                Argument {
                    term: Subterm::Bang(name("x")).into(),
                    plicity: Plicity::Explicit
                },
                Argument {
                    term: Subterm::Bang(name("y")).into(),
                    plicity: Plicity::Explicit
                }
            ],
        })
        .into()
    );
}

#[test]
fn bang_in_let_binding() {
    assert_eq!(
        "let x = e!; x".parse::<Term>().unwrap(),
        Subterm::Let(Let {
            groups: vec![LetGroup {
                members: vec![LetBinding {
                    binder: Pattern::Binder("x".into()),
                    signature: LetSignature::Name {
                        type_: None,
                        body: Subterm::Bang(name("e")).into(),
                    },
                }]
            }],
            tail: name("x"),
        })
        .into()
    );
}

#[test]
fn bang_in_match_scrutinee_and_arm() {
    // A `!` in the scrutinee and a `!` inside an arm are distinct `Bang` nodes; the elaborator hoists them into different regions.
    let term = "match x! | foo(z) => y! end".parse::<Term>().unwrap();
    match term.into_subterm() {
        Subterm::Match(m) => {
            assert_eq!(m.head, Subterm::Bang(name("x")).into());
            let foo = m.arms.iter().find_map(|arm| {
                matches!(&arm.pattern, MatchPattern::Variant { tag, .. } if tag == "foo")
                    .then_some(&arm.body)
            });
            assert_eq!(foo, Some(&Subterm::Bang(name("y")).into()));
        }
        other => panic!("expected inductive match, got {other:?}"),
    }
}

#[test]
fn bang_binds_tighter_than_application() {
    // `f(x)!` bangs the whole application; the `!` is the outermost node.
    assert_eq!(
        "f(x)!".parse::<Term>().unwrap(),
        Subterm::Bang(
            Subterm::Apply(Apply {
                head: name("f"),
                arguments: vec![Argument {
                    term: name("x"),
                    plicity: Plicity::Explicit
                }],
            })
            .into()
        )
        .into()
    );
}

#[test]
fn bang_binds_tighter_than_projection() {
    // `p.0!` bangs the projection (`!` outermost) …
    assert_eq!(
        "p.0!".parse::<Term>().unwrap(),
        Subterm::Bang(
            Subterm::Proj(Proj {
                head: name("p"),
                field: Field::Index(0),
            })
            .into()
        )
        .into()
    );
    // … while `x!.0` projects out of the banged action (`Proj` outermost).
    assert_eq!(
        "x!.0".parse::<Term>().unwrap(),
        Subterm::Proj(Proj {
            head: Subterm::Bang(name("x")).into(),
            field: Field::Index(0),
        })
        .into()
    );
}

#[test]
fn bang_round_trips() {
    for source in ["f(x!, y!)", "p.0!", "x!.0", "let x = e!; x"] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "round-trip failed for {source:?}"
        );
    }
}
