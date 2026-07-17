use {
    super::into_cont,
    crate::{Apply, Func, Let, Module, Name, NatMatch, Prim, PurePrim, Rec, Subterm, Term, Tuple},
    curios_base::{Grain, PackedBin},
    curios_cont::{CpsAtom, CpsCallee, CpsLiteral, CpsNode, CpsValueExpr},
    std::collections::BTreeMap,
};

fn lower(term: Term) -> curios_cont::CpsModule {
    into_cont(&Module {
        items: vec![],
        body: term,
    })
    .expect("test module lowers")
}

fn identity_func() -> Func {
    Func {
        captures: vec![],
        params: vec!["arg".into()],
        body: Subterm::Name(Name::from("arg")).into(),
    }
}

fn nodes(module: &curios_cont::CpsModule) -> impl Iterator<Item = &CpsNode> {
    module.nodes().iter().flatten()
}

#[test]
fn lowers_tail_apply_as_indirect_call_to_resume() {
    let term = Subterm::Apply(Apply {
        head: Subterm::Func(identity_func()).into(),
        params: vec![Subterm::Prim(Prim::Pure(PurePrim::Int(7))).into()],
    });
    let module = lower(term.into());

    assert_eq!(module.functions().iter().flatten().count(), 2);
    assert!(nodes(&module).any(|node| matches!(
        node,
        CpsNode::ApplyFun {
            callee: CpsCallee::Closure(_),
            ..
        }
    )));
}

#[test]
fn lowers_arr_into_high_cps_value() {
    let term = Subterm::Let(Let {
        bindings: vec![(
            "a".into(),
            Subterm::Prim(Prim::Pure(PurePrim::Nat(1))).into(),
        )],
        tail: Subterm::Let(Let {
            bindings: vec![(
                "b".into(),
                Subterm::Prim(Prim::Pure(PurePrim::Nat(2))).into(),
            )],
            tail: Subterm::Prim(Prim::Pure(PurePrim::Lst(vec![
                Subterm::Name(Name::from("a")).into(),
                Subterm::Name(Name::from("b")).into(),
            ])))
            .into(),
        })
        .into(),
    });
    let module = lower(term.into());

    assert!(nodes(&module).any(|node| matches!(
        node,
        CpsNode::LetValue {
            value: CpsValueExpr::List(elements),
            ..
        } if elements.len() == 2
    )));
}

#[test]
fn lowers_arr_with_apply_element_through_continuation() {
    let term = Subterm::Prim(Prim::Pure(PurePrim::Lst(vec![
        Subterm::Apply(Apply {
            head: Subterm::Func(identity_func()).into(),
            params: vec![Subterm::Prim(Prim::Pure(PurePrim::Nat(1))).into()],
        })
        .into(),
        Subterm::Prim(Prim::Pure(PurePrim::Nat(2))).into(),
    ])));
    let module = lower(term.into());

    assert!(module.continuations().iter().flatten().count() >= 2);
    assert!(nodes(&module).any(|node| matches!(
        node,
        CpsNode::LetValue {
            value: CpsValueExpr::List(elements),
            ..
        } if elements.len() == 2
    )));
}

#[test]
fn lowers_apply_in_value_position_through_continuation() {
    let term = Subterm::Tuple(Tuple {
        fields: vec![
            Subterm::Apply(Apply {
                head: Subterm::Func(identity_func()).into(),
                params: vec![Subterm::Prim(Prim::Pure(PurePrim::Int(7))).into()],
            })
            .into(),
            Subterm::Prim(Prim::Pure(PurePrim::Int(1))).into(),
        ],
    });
    let module = lower(term.into());

    assert!(nodes(&module).any(|node| matches!(node, CpsNode::ApplyFun { .. })));
    assert!(nodes(&module).any(|node| matches!(
        node,
        CpsNode::LetValue {
            value: CpsValueExpr::Tuple(fields),
            ..
        } if fields.len() == 2
    )));
}

#[test]
fn peels_a_let_whose_tuple_body_hides_an_apply_field() {
    let term = Subterm::Let(Let {
        bindings: vec![(
            "t".into(),
            Subterm::Tuple(Tuple {
                fields: vec![
                    Subterm::Apply(Apply {
                        head: Subterm::Func(identity_func()).into(),
                        params: vec![Subterm::Prim(Prim::Pure(PurePrim::Int(7))).into()],
                    })
                    .into(),
                    Subterm::Prim(Prim::Pure(PurePrim::Int(1))).into(),
                ],
            })
            .into(),
        )],
        tail: Subterm::Name(Name::from("t")).into(),
    });
    let module = lower(term.into());

    assert!(nodes(&module).any(|node| matches!(node, CpsNode::ApplyFun { .. })));
    assert!(nodes(&module).any(|node| matches!(
        node,
        CpsNode::LetValue {
            value: CpsValueExpr::Tuple(_),
            ..
        }
    )));
}

#[test]
fn lowers_nat_match_as_sparse_switch() {
    let term = Subterm::NatMatch(NatMatch::Dispatch {
        head: Subterm::Prim(Prim::Pure(PurePrim::Nat(7))).into(),
        cases: BTreeMap::from([
            (2, Subterm::Prim(Prim::Pure(PurePrim::Nat(10))).into()),
            (7, Subterm::Prim(Prim::Pure(PurePrim::Nat(20))).into()),
        ]),
        default: Subterm::Prim(Prim::Pure(PurePrim::Nat(0))).into(),
    });
    let module = lower(term.into());

    assert!(nodes(&module).any(|node| matches!(
        node,
        CpsNode::Switch { cases, default: Some(_), .. }
            if cases.keys().copied().collect::<Vec<_>>() == vec![2, 7]
    )));
}

#[test]
fn lowers_bin_literal_as_an_atom() {
    let term = Subterm::Prim(Prim::Pure(PurePrim::Bin(
        Grain::X,
        PackedBin::from_bytes(vec![1, 2, 3]),
    )));
    let module = lower(term.into());

    assert!(nodes(&module).any(|node| matches!(
        node,
        CpsNode::ApplyCont(edge)
            if matches!(edge.args.as_slice(), [CpsAtom::Literal(CpsLiteral::Bin(Grain::X, bytes))]
                if bytes.as_bytes() == Some(&[1, 2, 3]))
    )));
}

#[test]
fn rejects_mutually_referential_tuples() {
    let term = Subterm::Rec(Rec {
        names: vec!["x".into(), "y".into()],
        items: vec![
            Subterm::Tuple(Tuple {
                fields: vec![
                    Subterm::Name(Name::from("y")).into(),
                    Subterm::Prim(Prim::Pure(PurePrim::Int(1))).into(),
                ],
            })
            .into(),
            Subterm::Tuple(Tuple {
                fields: vec![
                    Subterm::Prim(Prim::Pure(PurePrim::Int(2))).into(),
                    Subterm::Name(Name::from("x")).into(),
                ],
            })
            .into(),
        ],
        tail: Subterm::Name(Name::from("x")).into(),
    });
    let error = into_cont(&Module {
        items: vec![],
        body: term.into(),
    })
    .expect_err("mutually referential tuples must be rejected");
    assert!(error.to_string().contains("value-level mutual recursion"));
}

#[test]
fn lowers_cross_region_rec_as_explicit_rec_init() {
    let term = Subterm::Rec(Rec {
        names: vec!["f".into(), "g".into()],
        items: vec![
            Subterm::Func(Func {
                captures: vec!["g".into()],
                params: vec!["x".into()],
                body: Subterm::Name(Name::from("g")).into(),
            })
            .into(),
            Subterm::Apply(Apply {
                head: Subterm::Func(identity_func()).into(),
                params: vec![Subterm::Name(Name::from("f")).into()],
            })
            .into(),
        ],
        tail: Subterm::Name(Name::from("f")).into(),
    });
    let module = lower(term.into());
    assert!(nodes(&module).any(|node| matches!(node, CpsNode::RecInit { .. })));
}

#[test]
fn rejects_apply_apply_cycle() {
    let term = Subterm::Rec(Rec {
        names: vec!["a".into(), "b".into()],
        items: vec![
            Subterm::Apply(Apply {
                head: Subterm::Func(identity_func()).into(),
                params: vec![Subterm::Name(Name::from("b")).into()],
            })
            .into(),
            Subterm::Apply(Apply {
                head: Subterm::Func(identity_func()).into(),
                params: vec![Subterm::Name(Name::from("a")).into()],
            })
            .into(),
        ],
        tail: Subterm::Name(Name::from("a")).into(),
    });
    let error = into_cont(&Module {
        items: vec![],
        body: term.into(),
    })
    .expect_err("an apply/apply cycle must be rejected");
    assert!(error.to_string().contains("value-level mutual recursion"));
}

#[test]
fn lowers_a_long_straight_line_let_chain_without_overflowing_the_stack() {
    const N: usize = 2_000;
    let mut term: Term = Subterm::Name(Name::from(format!("x{}", N - 1))).into();
    for i in (0..N).rev() {
        term = Subterm::Let(Let {
            bindings: vec![(
                format!("x{i}"),
                Subterm::Prim(Prim::Pure(PurePrim::Nat(i as u32))).into(),
            )],
            tail: term,
        })
        .into();
    }
    let module = lower(term);
    module.verify().unwrap();
}
