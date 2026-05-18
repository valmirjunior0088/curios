use curios::core::{
    Apply, Atom, AtomType, Func, FuncType, Let, LetRec, Match, Split, Term, Tuple, TupleType,
    Type, Var,
};

fn main() {
    let program: Term = LetRec::new(
        vec![(
            "id",
            FuncType::new("_", Type, Type),
            Func::new("x", Var::free("x")),
        )],
        Let::new(
            "tuple_ty",
            Type,
            TupleType::new([
                ("tag", Term::from(AtomType::new(["left", "right"]))),
                (
                    "value",
                    Term::from(Match::new(
                        Var::free("tag"),
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
                    ["x", "y"],
                    Match::new(
                        Var::free("x"),
                        "_",
                        Type,
                        [
                            ("left", Apply::many(Var::free("id"), [Var::free("y")])),
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
