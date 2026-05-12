use curios::core::{
    Apply, Atom, AtomType, Func, FuncType, Let, LetRec, Match, Name, Pair, PairType, Split, Term,
    Type,
};

fn main() {
    let program: Term = LetRec::new(
        vec![(
            "id",
            FuncType::new("_", Type, Type),
            Func::new("x", Name::label("x")),
        )],
        Let::new(
            "pair_ty",
            Type,
            PairType::new(
                "tag",
                AtomType::new(["left", "right"]),
                Match::new(
                    Name::label("tag"),
                    "_",
                    Type,
                    [("left", Type), ("right", Type)],
                ),
            ),
            Let::new(
                "p",
                Name::label("pair_ty"),
                Pair::new(Atom::from("left"), Type),
                Split::new(
                    Name::label("p"),
                    "_",
                    Type,
                    "x",
                    "y",
                    Match::new(
                        Name::label("x"),
                        "_",
                        Type,
                        [
                            ("left", Apply::many(Name::label("id"), [Name::label("y")])),
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
