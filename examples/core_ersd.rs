use curios::{cont, ersd};

fn main() {
    let term = ersd::Term::Let(ersd::Let {
        name: "bias".into(),
        body: ersd::Term::Prim(ersd::Prim::IntMul(
            ersd::Term::Prim(ersd::Prim::IntAdd(
                ersd::Term::Prim(ersd::Prim::Int(2)).into(),
                ersd::Term::Prim(ersd::Prim::Int(3)).into(),
            ))
            .into(),
            ersd::Term::Prim(ersd::Prim::IntSub(
                ersd::Term::Prim(ersd::Prim::Int(10)).into(),
                ersd::Term::Prim(ersd::Prim::Int(5)).into(),
            ))
            .into(),
        ))
        .into(),
        tail: ersd::Term::Let(ersd::Let {
            name: "scaled".into(),
            body: ersd::Term::Prim(ersd::Prim::FltAdd(
                ersd::Term::Prim(ersd::Prim::FltMul(
                    ersd::Term::Prim(ersd::Prim::Flt(1.5)).into(),
                    ersd::Term::Prim(ersd::Prim::Flt(2.0)).into(),
                ))
                .into(),
                ersd::Term::Prim(ersd::Prim::FltSub(
                    ersd::Term::Prim(ersd::Prim::Flt(4.0)).into(),
                    ersd::Term::Prim(ersd::Prim::Flt(1.0)).into(),
                ))
                .into(),
            ))
            .into(),
            tail: ersd::Term::Let(ersd::Let {
                name: "unit".into(),
                body: ersd::Term::Erased.into(),
                tail: ersd::Term::LetRec(ersd::LetRec {
                    names: vec!["self_pair".into(), "make".into()],
                    items: vec![
                        ersd::Term::Tuple(ersd::Tuple {
                            fields: vec![
                                ersd::Term::Name(ersd::Name::from("self_pair")).into(),
                                ersd::Term::Name(ersd::Name::from("bias")).into(),
                            ],
                        })
                        .into(),
                        ersd::Term::Func(ersd::Func {
                            captures: vec!["bias".into(), "self_pair".into()],
                            param: "input".into(),
                            body: ersd::Term::Let(ersd::Let {
                                name: "sum".into(),
                                body: ersd::Term::Prim(ersd::Prim::IntAdd(
                                    ersd::Term::Name(ersd::Name::from("input")).into(),
                                    ersd::Term::Name(ersd::Name::from("bias")).into(),
                                ))
                                .into(),
                                tail: ersd::Term::Let(ersd::Let {
                                    name: "is_bias".into(),
                                    body: ersd::Term::Prim(ersd::Prim::IntEql(
                                        ersd::Term::Name(ersd::Name::from("sum")).into(),
                                        ersd::Term::Name(ersd::Name::from("bias")).into(),
                                    ))
                                    .into(),
                                    tail: ersd::Term::Tuple(ersd::Tuple {
                                        fields: vec![
                                            ersd::Term::Name(ersd::Name::from("sum")).into(),
                                            ersd::Term::Name(ersd::Name::from("self_pair")).into(),
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
                    tail: ersd::Term::Let(ersd::Let {
                        name: "applied".into(),
                        body: ersd::Term::Apply(ersd::Apply {
                            head: ersd::Term::Name(ersd::Name::from("make")).into(),
                            param: ersd::Term::Prim(ersd::Prim::Int(2)).into(),
                        })
                        .into(),
                        tail: ersd::Term::Split(ersd::Split {
                            head: ersd::Term::Name(ersd::Name::from("applied")).into(),
                            fields: vec!["sum".into(), "pair".into()],
                            tail: ersd::Term::Match(ersd::Match {
                                head: ersd::Term::Atom(ersd::Atom { index: 0 }).into(),
                                cases: vec![
                                    ersd::Term::Split(ersd::Split {
                                        head: ersd::Term::Name(ersd::Name::from("pair")).into(),
                                        fields: vec!["loop".into(), "captured_bias".into()],
                                        tail: ersd::Term::Name(ersd::Name::from("captured_bias"))
                                            .into(),
                                    })
                                    .into(),
                                    ersd::Term::Prim(ersd::Prim::Int(0)).into(),
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
    });

    let cont_module = ersd::to_cont(&term);

    println!("=== cont ===");
    println!("{cont_module}");

    println!();
    println!("=== wasm ===");
    println!("{}", cont::to_wasm(&cont_module));
}
