//! Application, intrinsics and infix operators, implicit marks and goals, and the span a term and a binder take.

use {
    super::test_support::*,
    crate::*,
    curios_num::Floating,
    curios_utilities::{InfixOp, Plicity, Sign},
};

#[test]
fn let_func_and_apply() {
    assert_eq!(
        "let id : (x : Type) -> Type = (x) => x; id(a)"
            .parse::<Term>()
            .unwrap(),
        Subterm::Let(Let {
            groups: vec![LetGroup {
                members: vec![LetBinding {
                    binder: Pattern::Binder("id".into()),
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
                                pattern: Pattern::Binder("x".into()),
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
                arguments: vec![Argument {
                    term: Subterm::Name(Name::from(["a".to_string()])).into(),
                    plicity: Plicity::Explicit
                }],
            })
            .into(),
        })
        .into()
    );
}

#[test]
fn parse_intrinsic() {
    assert_eq!("42".parse::<Term>().unwrap(), num_lit(42, Sign::Unmarked));
    assert_eq!(
        "1.5".parse::<Term>().unwrap(),
        Term::from(Subterm::Literal(Literal::Flt(Floating::from(1.5))))
    );
    assert_eq!(
        "false".parse::<Term>().unwrap(),
        Term::from(Subterm::Literal(Literal::Bool(false)))
    );
    assert_eq!(
        "true".parse::<Term>().unwrap(),
        Term::from(Subterm::Literal(Literal::Bool(true)))
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
            right: num_lit(42, Sign::Unmarked),
        })
        .into()
    );
    // A space missing on either side ⇒ the operator is not recognised, and the refusal names the rule rather than the token the enclosing form wanted next: `parse_infix_op`'s own refusal is caught so the loop can end, so the probe after it is what says why.
    for source in ["a-42", "a +42", "a/ 2", "a -2", "a- 2", "a+b", "a==b"] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            report.contains("an infix operator takes whitespace on both sides"),
            "{source:?} reported {report}"
        );
    }
    // The caret underlines the symbol alone. A symbol followed by punctuation rather than an operand — `->`, `--` — keeps the enclosing form's report, and so does a `/` opening an absolute path after a space, which is what a missing `end` looks like.
    let report = "a- 2".parse::<Term>().unwrap_err().format();
    assert!(
        report.ends_with("1 | a- 2\n      |  ^"),
        "reported {report}"
    );
    for source in ["a ->2", "a /std/x"] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            !report.contains("whitespace on both sides"),
            "{source:?} reported {report}"
        );
    }
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
fn a_position_that_begins_no_term_names_the_input_rather_than_an_alternative() {
    // Every alternative of the term grammar declines at the choice point here, and `or`'s tie-break reports the first of them. Left to it the reader was told `Expected keyword 'let'` for each of these, naming the chain's order rather than their mistake.
    for (source, obtained) in [
        (")", ")"),
        (",", ","),
        ("@", "@"),
        ("=> 1", "="),
        ("}", "}"),
    ] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            report.contains(&format!("expected a term, obtained '{obtained}'")),
            "{source:?} reported {report}"
        );
    }
    // Nothing is left to consume at the end of input, so that case is named ahead of the alternatives instead of after them.
    let report = "".parse::<Term>().unwrap_err().format();
    assert!(
        report.contains("expected a term, obtained 'end-of-file'"),
        "reported {report}"
    );
    // The caret underlines the offending character rather than standing after it.
    let report = ")".parse::<Term>().unwrap_err().format();
    assert!(report.ends_with("1 | )\n      | ^"), "reported {report}");
    // The refusal stays recoverable, so a speculative caller still backtracks past it: a trailing separator is read as trailing only because `parse_term` fails softly at the closing delimiter.
    for source in ["(1, true,)", "[1, 2,]", "(1,)", "()"] {
        assert!(source.parse::<Term>().is_ok(), "{source} should parse");
    }
}

#[test]
fn a_long_operator_chain_parses_without_native_recursion() {
    // Chain length must bound no stack: a recursive infix spelling would nest one native frame per operator, and a packrat cache deep-cloning a Box-backed tree per success would recurse per level. The infix loop folds by move and `Term` is Rc-backed, so this depth exercises both.
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
            assert_eq!(apply.arguments[0].plicity, Plicity::Explicit);
            assert_eq!(apply.arguments[1].plicity, Plicity::Implicit);
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
                    doc: None,
                    signature: LetSignature::Func { params, .. },
                    ..
                },
            ] = items.as_slice()
            else {
                panic!("expected one func let, got {items:?}");
            };
            assert_eq!(params[0].plicity(), Plicity::Implicit);
            assert_eq!(params[1].plicity(), Plicity::Explicit);
        }
        other => panic!("expected a func let, got {other:?}"),
    }

    // An inductive parameter may carry `@`, making it implicit at the type constructor (it is implicit at the value constructors either way).
    let m = "induct Result(@A : Type, E : Type) : Type\n| success(A)\nend"
        .parse::<Module>()
        .unwrap();
    match &m.items[0] {
        TopItem::Induct(group) => {
            assert_eq!(group[0].params[0].plicity, Plicity::Implicit);
            assert_eq!(group[0].params[1].plicity, Plicity::Explicit);
        }
        other => panic!("expected an inductive, got {other:?}"),
    }
}

#[test]
fn at_on_a_binder_type_is_a_parse_error() {
    // Erasure is sort-driven, with no erasure axis, so `@` on a binder's *type* does not parse. `@` on a *name* is plicity and still parses; the two positions never collide.
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
            assert_eq!(apply.arguments.len(), 1);
            assert_eq!(
                apply.arguments[0],
                Argument {
                    term: Subterm::Goal.into(),
                    plicity: Plicity::Explicit,
                }
            );
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

/// A term's span is its own text: the whitespace after it, and a comment there, belong to whatever follows.
///
/// Every atom parser consumes the whitespace after it inside the span wrapper, so an operand's span ran on to the operator and the caret underlined the blanks — `n   ` under `n   + true` — and a test body sliced from its span ended in the comment that followed it.
#[test]
fn a_term_spans_its_own_text_without_the_whitespace_after_it() {
    let source = "n   + true";
    let term = source.parse::<Term>().unwrap();
    let Subterm::Infix(infix) = term.as_subterm() else {
        panic!("not an infix: {term:?}")
    };
    let left = infix.left.span().unwrap();
    assert_eq!(&source[left.start..left.end], "n");
    let whole = term.span().unwrap();
    assert_eq!(&source[whole.start..whole.end], source);

    let source = "f(a)  -- a remark\n  -- another\n";
    let term = source.parse::<Term>().unwrap();
    let span = term.span().unwrap();
    assert_eq!(&source[span.start..span.end], "f(a)");
}

/// A binder is spanned over its word at every binder position: a lambda parameter, a `let` pattern leaf and a function-sugar parameter.
#[test]
fn a_binder_spans_its_word_alone() {
    let term = "let ( a , b ) = p; let f( n : Nat) -> Nat = n; ( x ) => x"
        .parse::<Term>()
        .unwrap();
    let Subterm::Let(outer) = &*term else {
        panic!("a let");
    };
    let Pattern::Tuple(fields) = &outer.groups[0].members[0].binder else {
        panic!("a tuple pattern");
    };
    let leaves = fields
        .iter()
        .map(|field| match &field.value {
            Pattern::Binder(label) => spelled(label),
            other => panic!("a binder leaf, not {other:?}"),
        })
        .collect::<Vec<_>>();
    assert_eq!(leaves, ["a", "b"]);

    // Consecutive statements are the groups of one `Let`, so the second is the next group rather than a nested node.
    let LetBinding {
        binder: Pattern::Binder(f),
        signature: LetSignature::Func { params, .. },
    } = &outer.groups[1].members[0]
    else {
        panic!("function sugar");
    };
    let FuncSugarParam::Bound {
        binder: Pattern::Binder(n),
        ..
    } = &params[0]
    else {
        panic!("a named parameter");
    };
    assert_eq!((spelled(f), spelled(n)), ("f".to_string(), "n".to_string()));

    let Subterm::Func(func) = &*outer.tail else {
        panic!("a lambda");
    };
    let Pattern::Binder(x) = &func.params[0].pattern else {
        panic!("a named parameter");
    };
    assert_eq!(spelled(x), "x");
}
