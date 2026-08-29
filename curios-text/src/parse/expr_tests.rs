//! Expressions: application, infix, implicit marks, goals, local bindings, function sugar and the postfix `!`.

use {
    crate::*,
    curios_num::Floating,
    curios_utilities::{InfixOp, Plicity},
};

use super::test_support::*;

#[test]
fn rec_func_and_apply() {
    assert_eq!(
        "rec id : (x : Type) -> Type = (x) => x; id(a)"
            .parse::<Term>()
            .unwrap(),
        Subterm::Let(Let {
            groups: vec![LetGroup {
                members: vec![LetBinding {
                    binder: Pattern::Binder(Some("id".to_string())),
                    signature: LetSignature::Name {
                        type_: Some(
                            Subterm::FuncType(FuncType {
                                params: vec![FuncTypeParam {
                                    plicity: Plicity::Explicit,
                                    label: Some("x".to_string()),
                                    type_: Subterm::Type.into(),
                                }],
                                output: Subterm::Type.into(),
                            })
                            .into(),
                        ),
                        body: Subterm::Func(Func {
                            params: vec![FuncParam {
                                plicity: Plicity::Explicit,
                                pattern: Pattern::Binder(Some("x".to_string())),
                                annotation: None,
                            }],
                            body: Subterm::Name(Name::from(["x".to_string()])).into(),
                        })
                        .into(),
                    },
                }]
            }],
            tail: Subterm::Apply(Apply {
                head: Subterm::Name(Name::from(["id".to_string()])).into(),
                params: vec![(
                    Plicity::Explicit,
                    Subterm::Name(Name::from(["a".to_string()])).into()
                )],
            })
            .into(),
        })
        .into()
    );
}

#[test]
fn parse_intrinsic() {
    assert_eq!("42".parse::<Term>().unwrap(), num_lit(42, false, false));
    assert_eq!(
        "1.5".parse::<Term>().unwrap(),
        Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from_f32(1.5))))
    );
    assert_eq!(
        "false".parse::<Term>().unwrap(),
        Term::from(Subterm::Intrinsic(Intrinsic::Bool(false)))
    );
    assert_eq!(
        "true".parse::<Term>().unwrap(),
        Term::from(Subterm::Intrinsic(Intrinsic::Bool(true)))
    );
}

#[test]
fn infix_precedence_and_associativity() {
    // `a + b * c` → `a + (b * c)` (× binds tighter); `a - b - c` → `(a - b) - c` (left-associative); comparison binds looser than arithmetic.
    let name = |n: &str| -> Term { Subterm::Name(Name::from([n.to_string()])).into() };
    let infix = |op, left, right| -> Term { Subterm::Infix(Infix { op, left, right }).into() };

    assert_eq!(
        "a + b * c".parse::<Term>().unwrap(),
        infix(
            InfixOp::Add,
            name("a"),
            infix(InfixOp::Mul, name("b"), name("c")),
        )
    );
    assert_eq!(
        "a - b - c".parse::<Term>().unwrap(),
        infix(
            InfixOp::Sub,
            infix(InfixOp::Sub, name("a"), name("b")),
            name("c"),
        )
    );
    assert_eq!(
        "a + b < c".parse::<Term>().unwrap(),
        infix(
            InfixOp::Lt,
            infix(InfixOp::Add, name("a"), name("b")),
            name("c"),
        )
    );
}

#[test]
fn infix_requires_spaces_and_disambiguates_signs() {
    // A spaced `-` is subtraction; a glued `-` is part of a negative literal.
    let name = |n: &str| -> Term { Subterm::Name(Name::from([n.to_string()])).into() };

    assert_eq!(
        "a - 42".parse::<Term>().unwrap(),
        Subterm::Infix(Infix {
            op: InfixOp::Sub,
            left: name("a"),
            right: num_lit(42, false, false),
        })
        .into()
    );
    // No space ⇒ the operator is not recognised, leaving a trailing token: a parse error rather than a silent reinterpretation.
    assert!("a-42".parse::<Term>().is_err());
    assert!("a +42".parse::<Term>().is_err());
    // `!=` is the not-equal operator, not a postfix bang followed by `=`.
    assert_eq!(
        "a != b".parse::<Term>().unwrap(),
        Subterm::Infix(Infix {
            op: InfixOp::Neq,
            left: name("a"),
            right: name("b"),
        })
        .into()
    );
}

#[test]
fn a_long_operator_chain_parses_without_native_recursion() {
    // Chain length was a stack bound twice over: the recursive infix spelling nested one native frame per operator (aborting at 1k on the default test-thread stack), and after that went iterative, the packrat cache's per-success deep clone of the Box-backed tree still recursed per level (aborting near 3k). The infix loop folds by move and `Term` is Rc-backed now, so this depth exercises both cures.
    let depth = 10_000;
    let source = "1".to_string() + &" + 1".repeat(depth);
    let mut term = source.parse::<Term>().unwrap();
    let mut count = 0;
    // Dismantle by value, one level per iteration: each round frees one `Infix` shell and its leaf right operand, so the deep left spine never hits a recursive drop.
    while let Subterm::Infix(infix) = term.into_subterm() {
        count += 1;
        term = infix.left;
    }
    assert_eq!(count, depth);
}

#[test]
fn implicit_marks_on_binders_and_arguments() {
    // `@` marks a Π-type binder implicit, anywhere in the telescope.
    let t = "(@T : Type, x : T) -> T".parse::<Term>().unwrap();
    match t.as_subterm() {
        Subterm::FuncType(ft) => {
            assert_eq!(ft.params[0].plicity, Plicity::Implicit);
            assert_eq!(ft.params[1].plicity, Plicity::Explicit);
        }
        other => panic!("expected a func type, got {other:?}"),
    }

    // ...and a call-site argument, independently of its position.
    let t = "foo(x, @Nat)".parse::<Term>().unwrap();
    match t.as_subterm() {
        Subterm::Apply(apply) => {
            assert_eq!(apply.params[0].0, Plicity::Explicit);
            assert_eq!(apply.params[1].0, Plicity::Implicit);
        }
        other => panic!("expected an apply, got {other:?}"),
    }
}

#[test]
fn implicit_marks_on_let_shorthand_and_inductive_params() {
    let m = "let foo(@T : Type, x : T) -> T = x;"
        .parse::<Module>()
        .unwrap();
    match &m.items[0] {
        TopItem::Let(items) => {
            let [
                TopLet {
                    signature: LetSignature::Func { params, .. },
                    ..
                },
            ] = items.as_slice()
            else {
                panic!("expected one func let, got {items:?}");
            };
            assert_eq!(params[0].plicity, Plicity::Implicit);
            assert_eq!(params[1].plicity, Plicity::Explicit);
        }
        other => panic!("expected a func let, got {other:?}"),
    }

    // An inductive parameter may carry `@`, making it implicit at the type constructor (it is implicit at the value constructors either way).
    let m = "induct Result(@A : Type, E : Type) : Type\n| success(A)\nend"
        .parse::<Module>()
        .unwrap();
    match &m.items[0] {
        TopItem::Induct(group) => {
            assert_eq!(group[0].params[0].0, Plicity::Implicit);
            assert_eq!(group[0].params[1].0, Plicity::Explicit);
        }
        other => panic!("expected an inductive, got {other:?}"),
    }
}

#[test]
fn at_on_a_binder_type_is_a_parse_error() {
    // Erasure is sort-driven now: the erasure axis is retired, so `@` on a binder's *type* (the old erased marker) no longer parses. `@` on a *name* is plicity and still parses; the two positions never collide.
    let implicit = "(@n : Nat) -> Nat".parse::<Term>().unwrap();
    match &*implicit {
        Subterm::FuncType(ft) => assert_eq!(ft.params[0].plicity, Plicity::Implicit),
        other => panic!("expected a function type, got {other:?}"),
    }

    // `@` on the type is rejected in every binder position it once marked.
    for src in ["(x : @Nat) -> Nat", "(@x : @Nat) -> Nat"] {
        assert!(src.parse::<Term>().is_err(), "{src} should not parse");
    }

    assert!(
        "let foo(n : @Nat) -> Nat = n;".parse::<Module>().is_err(),
        "@ on a def-form parameter type should not parse",
    );

    assert!(
        "induct Boxed : Type | box(ghost : @Nat) end"
            .parse::<Module>()
            .is_err(),
        "@ on an inductive payload type should not parse",
    );
}

#[test]
fn parse_goal() {
    // A written `?` is a goal — reported at zonk — never a silent `Subterm::Hole`, which only desugars mint.
    assert_eq!("?".parse::<Term>().unwrap(), Subterm::Goal.into());
}

#[test]
fn goal_as_argument() {
    let term = "id(?)".parse::<Term>().unwrap();
    match term.into_subterm() {
        Subterm::Apply(apply) => {
            assert_eq!(apply.params.len(), 1);
            assert_eq!(apply.params[0], (Plicity::Explicit, Subterm::Goal.into()));
        }
        other => panic!("expected apply, got {other:?}"),
    }
}

#[test]
fn underscore_prefixed_name_is_not_a_hole() {
    assert!(matches!(
        "_foo".parse::<Term>().unwrap().into_subterm(),
        Subterm::Name(_)
    ));
}

#[test]
fn local_let_without_type() {
    // A local `let x = e` omits the type; it parses to `Name { type_: None }`, and the core elaborator infers the body's type.
    assert_eq!(
        "let x = Type; x".parse::<Term>().unwrap(),
        Subterm::Let(Let {
            groups: vec![LetGroup {
                members: vec![LetBinding {
                    binder: Pattern::Binder(Some("x".to_string())),
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
                    binder: Pattern::Binder(Some("x".to_string())),
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
                pattern: Pattern::Binder(Some("x".to_string())),
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
                    pattern: Pattern::Binder(Some("x".to_string())),
                    annotation: Some(Subterm::Type.into()),
                },
                FuncParam {
                    plicity: Plicity::Explicit,
                    pattern: Pattern::Binder(Some("y".to_string())),
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
                pattern: Pattern::Binder(Some("x".to_string())),
                annotation: None,
            }],
            body: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn let_bang_is_no_longer_grammar() {
    // The `let ! = <bind>;` header is gone: `!` sequences through the `Monad` concept without one. `!` is not a binder identifier, so the old form is a parse error rather than a `let`.
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
            params: vec![
                (Plicity::Explicit, Subterm::Bang(name("x")).into()),
                (Plicity::Explicit, Subterm::Bang(name("y")).into()),
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
                    binder: Pattern::Binder(Some("x".to_string())),
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
                params: vec![(Plicity::Explicit, name("x"))],
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

#[test]
fn local_let_group() {
    // `and` joins members into one statement, each a plain label with a mandatory type.
    let member = |label: &str, body: &str| LetBinding {
        binder: Pattern::Binder(Some(label.to_string())),
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

#[test]
fn rec_is_a_synonym_for_a_let_group() {
    assert_eq!(
        "rec a : Type = b and b : Type = a; a"
            .parse::<Term>()
            .unwrap(),
        "let a : Type = b and b : Type = a; a"
            .parse::<Term>()
            .unwrap(),
    );
}
