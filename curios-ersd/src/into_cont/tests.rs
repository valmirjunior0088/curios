use {
    super::into_cont,
    crate::{Apply, Func, Let, Module, Name, NatMatch, Prim, PurePrim, Rec, Subterm, Term, Tuple},
    curios_cont::{CallTarget, Data, MatchTarget, Tail, Value},
    std::collections::BTreeMap,
};

// These tests exercise the lowerer on whole erased *terms*; wrap each as the
// entrypoint `body` of an item-less `Module` (the shape `into_cont` now takes).
fn lower(term: Term) -> curios_cont::Module {
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

#[test]
fn lowers_tail_apply_as_indirect_call_to_resume() {
    let term = Subterm::Apply(Apply {
        head: Subterm::Func(Func {
            captures: vec![],
            params: vec!["x".into()],
            body: Subterm::Name(Name::from("x")).into(),
        })
        .into(),
        params: vec![Subterm::Prim(Prim::Pure(PurePrim::Int(7))).into()],
    });

    let module = lower(term.into());

    assert_eq!(module.clsrs().len(), 1);

    let func = &module.funcs()[0].1;
    assert!(func.region.blocks.is_empty());

    let Tail::Call(CallTarget::Indirect { resume, .. }) = &func.region.tail else {
        panic!("expected indirect call in main tail");
    };

    assert_eq!(resume.as_str(), func.resume.as_str());
}

#[test]
fn lowers_arr_into_main_region_value() {
    let term = Subterm::Let(Let {
        name: "a".into(),
        body: Subterm::Prim(Prim::Pure(PurePrim::Nat(1))).into(),
        tail: Subterm::Let(Let {
            name: "b".into(),
            body: Subterm::Prim(Prim::Pure(PurePrim::Nat(2))).into(),
            tail: Subterm::Prim(Prim::Pure(PurePrim::Lst(vec![
                Subterm::Name(Name::from("a")).into(),
                Subterm::Name(Name::from("b")).into(),
            ])))
            .into(),
        })
        .into(),
    });

    let module = lower(term.into());
    let func = &module.funcs()[0].1;

    assert!(func.region.values.iter().any(|(_, value)| matches!(
        value,
        Value::Pure(Data::Lst(elems)) if elems.len() == 2
    )));
}

#[test]
fn lowers_arr_with_apply_element_through_join_block() {
    let term = Subterm::Prim(Prim::Pure(PurePrim::Lst(vec![
        Subterm::Apply(Apply {
            head: Subterm::Func(Func {
                captures: vec![],
                params: vec!["x".into()],
                body: Subterm::Name(Name::from("x")).into(),
            })
            .into(),
            params: vec![Subterm::Prim(Prim::Pure(PurePrim::Nat(1))).into()],
        })
        .into(),
        Subterm::Prim(Prim::Pure(PurePrim::Nat(2))).into(),
    ])));

    let module = lower(term.into());
    let func = &module.funcs()[0].1;

    assert_eq!(func.region.blocks.len(), 1);

    let (_, block) = &func.region.blocks[0];

    assert!(block.region.values.iter().any(|(_, value)| matches!(
        value,
        Value::Pure(Data::Lst(elems)) if elems.len() == 2
    )));
}

#[test]
fn lowers_apply_in_value_position_through_join_block() {
    let term = Subterm::Tuple(Tuple {
        fields: vec![
            Subterm::Apply(Apply {
                head: Subterm::Func(Func {
                    captures: vec![],
                    params: vec!["x".into()],
                    body: Subterm::Name(Name::from("x")).into(),
                })
                .into(),
                params: vec![Subterm::Prim(Prim::Pure(PurePrim::Int(7))).into()],
            })
            .into(),
            Subterm::Prim(Prim::Pure(PurePrim::Int(1))).into(),
        ],
    });

    let module = lower(term.into());
    let func = &module.funcs()[0].1;

    assert_eq!(func.region.blocks.len(), 1);

    let Tail::Call(CallTarget::Indirect { resume, .. }) = &func.region.tail else {
        panic!("expected root indirect call");
    };

    let (block_name, block) = &func.region.blocks[0];
    assert_eq!(block_name.as_str(), resume.as_str());
    assert_eq!(block.params.len(), 1);
    assert!(
        block
            .region
            .values
            .iter()
            .any(|(_, value)| matches!(value, Value::Pure(Data::Tpl(_))))
    );
}

#[test]
fn lowers_nat_match_as_sparse_match() {
    let term = Subterm::NatMatch(NatMatch::Dispatch {
        head: Subterm::Prim(Prim::Pure(PurePrim::Nat(7))).into(),
        cases: BTreeMap::from([
            (2, Subterm::Prim(Prim::Pure(PurePrim::Nat(10))).into()),
            (7, Subterm::Prim(Prim::Pure(PurePrim::Nat(20))).into()),
        ]),
        default: Subterm::Prim(Prim::Pure(PurePrim::Nat(0))).into(),
    });

    let module = lower(term.into());
    let func = &module.funcs()[0].1;

    let Tail::Match(MatchTarget { cases, default, .. }) = &func.region.tail else {
        panic!("expected Tail::Match");
    };

    let keys: Vec<u32> = cases.keys().copied().collect();
    assert_eq!(keys, vec![2, 7]);
    assert!(default.is_some());
}

#[test]
fn lowers_bin_literal() {
    let term = Subterm::Prim(Prim::Pure(PurePrim::Bin(vec![1, 2, 3])));
    let module = lower(term.into());

    let func = &module.funcs()[0].1;
    let has_bin =
        func.region.values.iter().any(
            |(_, value)| matches!(value, Value::Pure(Data::Bin(bytes)) if bytes == &[1, 2, 3]),
        );

    assert!(has_bin);
}

#[test]
fn rejects_mutually_referential_tuples() {
    // `rec x = (y, 1) and y = (2, x)`: genuinely self-referential data. Tuples are lowered as
    // computed items now (not prealloc'd shells), so the x→y→x cycle is caught by
    // `rec_computed_order` and rejected — cyclic recursion is confined to closures, which is
    // what keeps `tpl`/`lst` fields immutable.
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
fn lowers_cross_region_rec_through_resume_block() {
    // `rec f = (x) => g and g = id(f)`: the aggregate `f` captures the call-valued `g`,
    // while `g`'s call references `f`. `f` must be prealloc'd (shell before the call) and
    // filled in the resume region after `g` returns.
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
    let func = &module.funcs()[0].1;

    // `f`'s closure shell is declared at region entry (preallocs are closure shells only).
    assert!(!func.region.preallocs.is_empty());

    // `g`'s call leaves the region, so its fill lands in a resume block, not the entry region.
    assert!(matches!(func.region.tail, Tail::Call(_)));
    assert!(!func.region.blocks.is_empty());
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
