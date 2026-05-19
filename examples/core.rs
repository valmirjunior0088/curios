use curios::core::{
    Apply, Atom, AtomType, Func, FuncType, Let, Match, Rec, Split, Term, Tuple, TupleType, Type,
    Var,
};

fn main() {
    let program: Term = Rec::new(
        vec![(
            "id",
            FuncType::new("_", Type, Type),
            Func::new("x", Var::free("x")),
        )],
        Let::new(
            "tuple_ty",
            Type,
            TupleType::new([
                ("label", Term::from(AtomType::new(["left", "right"]))),
                (
                    "value",
                    Term::from(Match::new(
                        Var::free("label"),
                        "_",
                        Type,
                        [("left", Type), ("right", Type)],
                    )),
                ),
            ]),
            Let::new(
                "p",
                Var::free("tuple_ty"),
                Tuple::new([Term::from(Atom::from("left")), Type.into()]),
                Split::new(
                    Var::free("p"),
                    "_",
                    Type,
                    ["label", "value"],
                    Match::new(
                        Var::free("label"),
                        "_",
                        Type,
                        [
                            ("left", Apply::many(Var::free("id"), [Var::free("value")])),
                            ("right", Type.into()),
                        ],
                    ),
                ),
            ),
        ),
    )
    .into();

    println!("{program}");
}
