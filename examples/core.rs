use curios::core::{
    Apply, Atom, AtomType, Func, FuncType, Let, LetRec, Match, Pair, PairType, Split, Term, Type,
    Var,
};

fn main() {
    let program: Term = LetRec::new(
        vec![(
            "id",
            FuncType::new("_", Type, Type),
            Func::new("x", Var::free("x")),
        )],
        Let::new(
            "pair_ty",
            Type,
            PairType::new(
                "tag",
                AtomType::new(["left", "right"]),
                Match::new(
                    Var::free("tag"),
                    "_",
                    Type,
                    [("left", Type), ("right", Type)],
                ),
            ),
            Let::new(
                "p",
                Var::free("pair_ty"),
                Pair::new(Atom::from("left"), Type),
                Split::new(
                    Var::free("p"),
                    "_",
                    Type,
                    "x",
                    "y",
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
