use curios::{cont, ersd};

fn main() {
    let ersd_term: ersd::Term = ersd::Subterm::Let(ersd::Let {
        name: "bias".into(),
        body: ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::IntMul(
            ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::IntAdd(
                ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Int(2))).into(),
                ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Int(3))).into(),
            )))
            .into(),
            ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::IntSub(
                ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Int(10))).into(),
                ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Int(5))).into(),
            )))
            .into(),
        )))
        .into(),
        tail: ersd::Subterm::Let(ersd::Let {
            name: "scaled".into(),
            body: ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::FltAdd(
                ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::FltMul(
                    ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Flt(1.5))).into(),
                    ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Flt(2.0))).into(),
                )))
                .into(),
                ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::FltSub(
                    ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Flt(4.0))).into(),
                    ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Flt(1.0))).into(),
                )))
                .into(),
            )))
            .into(),
            tail: ersd::Subterm::Let(ersd::Let {
                name: "unit".into(),
                body: ersd::Subterm::Erased.into(),
                tail: ersd::Subterm::Rec(ersd::Rec {
                    names: vec!["self_pair".into(), "make".into()],
                    items: vec![
                        ersd::Subterm::Tuple(ersd::Tuple {
                            fields: vec![
                                ersd::Subterm::Name(ersd::Name::from("self_pair")).into(),
                                ersd::Subterm::Name(ersd::Name::from("bias")).into(),
                            ],
                        })
                        .into(),
                        ersd::Subterm::Func(ersd::Func {
                            captures: vec!["bias".into(), "self_pair".into()],
                            params: vec!["input".into()],
                            body: ersd::Subterm::Let(ersd::Let {
                                name: "sum".into(),
                                body: ersd::Subterm::Prim(ersd::Prim::Pure(
                                    ersd::PurePrim::IntAdd(
                                        ersd::Subterm::Name(ersd::Name::from("input")).into(),
                                        ersd::Subterm::Name(ersd::Name::from("bias")).into(),
                                    ),
                                ))
                                .into(),
                                tail: ersd::Subterm::Let(ersd::Let {
                                    name: "is_bias".into(),
                                    body: ersd::Subterm::Prim(ersd::Prim::Pure(
                                        ersd::PurePrim::IntEql(
                                            ersd::Subterm::Name(ersd::Name::from("sum")).into(),
                                            ersd::Subterm::Name(ersd::Name::from("bias")).into(),
                                        ),
                                    ))
                                    .into(),
                                    tail: ersd::Subterm::Tuple(ersd::Tuple {
                                        fields: vec![
                                            ersd::Subterm::Name(ersd::Name::from("sum")).into(),
                                            ersd::Subterm::Name(ersd::Name::from("self_pair"))
                                                .into(),
                                        ],
                                    })
                                    .into(),
                                })
                                .into(),
                            })
                            .into(),
                        })
                        .into(),
                    ],
                    tail: ersd::Subterm::Let(ersd::Let {
                        name: "applied".into(),
                        body: ersd::Subterm::Apply(ersd::Apply {
                            head: ersd::Subterm::Name(ersd::Name::from("make")).into(),
                            params: vec![
                                ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Int(2)))
                                    .into(),
                            ],
                        })
                        .into(),
                        tail: ersd::Subterm::Let(ersd::Let {
                            name: "sum".into(),
                            body: ersd::Subterm::Proj(ersd::Proj {
                                head: ersd::Subterm::Name(ersd::Name::from("applied")).into(),
                                index: 0,
                            })
                            .into(),
                            tail: ersd::Subterm::Let(ersd::Let {
                                name: "pair".into(),
                                body: ersd::Subterm::Proj(ersd::Proj {
                                    head: ersd::Subterm::Name(ersd::Name::from("applied")).into(),
                                    index: 1,
                                })
                                .into(),
                                tail: ersd::Subterm::Match(ersd::Match {
                                    head: ersd::Subterm::Atom(ersd::Atom { index: 0 }).into(),
                                    cases: vec![
                                        ersd::Subterm::Proj(ersd::Proj {
                                            head: ersd::Subterm::Name(ersd::Name::from("pair"))
                                                .into(),
                                            index: 1,
                                        })
                                        .into(),
                                        ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Int(
                                            0,
                                        )))
                                        .into(),
                                    ],
                                })
                                .into(),
                            })
                            .into(),
                        })
                        .into(),
                    })
                    .into(),
                })
                .into(),
            })
            .into(),
        })
        .into(),
    })
    .into();

    println!("=== ersd ===");
    println!("{ersd_term}");

    let cont_module = ersd::to_cont(&ersd::Module {
        items: vec![],
        body: ersd_term,
    });

    println!();
    println!("=== cont ===");
    println!("{cont_module}");

    let wasm_module = cont::to_wasm(&cont_module);

    println!();
    println!("=== wasm ===");
    println!("{wasm_module}");

    println!();
    println!("=== result ===");
    curios::run_wasm(&wasm_module, curios::StdioHost::new()).unwrap();
}
