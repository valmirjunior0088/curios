use curios::{
    cont,
    core::{
        self, ErasedApply, ErasedAtom, ErasedFunc, ErasedLet, ErasedLetRec, ErasedMatch,
        ErasedName, ErasedPair, ErasedPrim, ErasedSplit, ErasedTerm,
    },
};

fn main() {
    let term = ErasedTerm::Let(ErasedLet {
        name: "bias".into(),
        body: ErasedTerm::Prim(ErasedPrim::IntMul(
            ErasedTerm::Prim(ErasedPrim::IntAdd(
                ErasedTerm::Prim(ErasedPrim::Int(2)).into(),
                ErasedTerm::Prim(ErasedPrim::Int(3)).into(),
            ))
            .into(),
            ErasedTerm::Prim(ErasedPrim::IntSub(
                ErasedTerm::Prim(ErasedPrim::Int(10)).into(),
                ErasedTerm::Prim(ErasedPrim::Int(5)).into(),
            ))
            .into(),
        ))
        .into(),
        tail: ErasedTerm::Let(ErasedLet {
            name: "scaled".into(),
            body: ErasedTerm::Prim(ErasedPrim::FltAdd(
                ErasedTerm::Prim(ErasedPrim::FltMul(
                    ErasedTerm::Prim(ErasedPrim::Flt(1.5)).into(),
                    ErasedTerm::Prim(ErasedPrim::Flt(2.0)).into(),
                ))
                .into(),
                ErasedTerm::Prim(ErasedPrim::FltSub(
                    ErasedTerm::Prim(ErasedPrim::Flt(4.0)).into(),
                    ErasedTerm::Prim(ErasedPrim::Flt(1.0)).into(),
                ))
                .into(),
            ))
            .into(),
            tail: ErasedTerm::Let(ErasedLet {
                name: "unit".into(),
                body: ErasedTerm::Prim(ErasedPrim::Unit).into(),
                tail: ErasedTerm::LetRec(ErasedLetRec {
                    names: vec!["self_pair".into(), "make".into()],
                    items: vec![
                        ErasedTerm::Pair(ErasedPair {
                            fst: ErasedTerm::Name(ErasedName::from("self_pair")).into(),
                            snd: ErasedTerm::Name(ErasedName::from("bias")).into(),
                        })
                        .into(),
                        ErasedTerm::Func(ErasedFunc {
                            captures: vec!["bias".into(), "self_pair".into()],
                            param: "input".into(),
                            body: ErasedTerm::Let(ErasedLet {
                                name: "sum".into(),
                                body: ErasedTerm::Prim(ErasedPrim::IntAdd(
                                    ErasedTerm::Name(ErasedName::from("input")).into(),
                                    ErasedTerm::Name(ErasedName::from("bias")).into(),
                                ))
                                .into(),
                                tail: ErasedTerm::Let(ErasedLet {
                                    name: "is_bias".into(),
                                    body: ErasedTerm::Prim(ErasedPrim::IntEql(
                                        ErasedTerm::Name(ErasedName::from("sum")).into(),
                                        ErasedTerm::Name(ErasedName::from("bias")).into(),
                                    ))
                                    .into(),
                                    tail: ErasedTerm::Pair(ErasedPair {
                                        fst: ErasedTerm::Name(ErasedName::from("sum")).into(),
                                        snd: ErasedTerm::Name(ErasedName::from("self_pair")).into(),
                                    })
                                    .into(),
                                })
                                .into(),
                            })
                            .into(),
                        })
                        .into(),
                    ],
                    tail: ErasedTerm::Let(ErasedLet {
                        name: "applied".into(),
                        body: ErasedTerm::Apply(ErasedApply {
                            head: ErasedTerm::Name(ErasedName::from("make")).into(),
                            param: ErasedTerm::Prim(ErasedPrim::Int(2)).into(),
                        })
                        .into(),
                        tail: ErasedTerm::Split(ErasedSplit {
                            head: ErasedTerm::Name(ErasedName::from("applied")).into(),
                            fst: "sum".into(),
                            snd: "pair".into(),
                            tail: ErasedTerm::Match(ErasedMatch {
                                head: ErasedTerm::Atom(ErasedAtom { index: 0 }).into(),
                                cases: vec![
                                    ErasedTerm::Split(ErasedSplit {
                                        head: ErasedTerm::Name(ErasedName::from("pair")).into(),
                                        fst: "loop".into(),
                                        snd: "captured_bias".into(),
                                        tail: ErasedTerm::Name(ErasedName::from("captured_bias"))
                                            .into(),
                                    })
                                    .into(),
                                    ErasedTerm::Prim(ErasedPrim::Int(0)).into(),
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

    println!("{}", cont::to_wasm(&core::to_cont(&term)));
}
