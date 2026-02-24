use curios::core::Term;

fn main() {
    let program = Term::let_rec(
        vec![(
            "id",
            Term::func_type("x", Term::Type, Term::Type),
            Term::func("x", Term::label("x")),
        )],
        Term::let_(
            "pair_ty",
            Term::Type,
            Term::pair_type(
                "tag",
                Term::atom_type(["left", "right"]),
                Term::match_(
                    Term::label("tag"),
                    "m",
                    Term::Type,
                    [("left", Term::Type), ("right", Term::Type)],
                ),
            ),
            Term::let_(
                "p",
                Term::label("pair_ty"),
                Term::pair(Term::atom("left"), Term::Type),
                Term::split(
                    Term::label("p"),
                    "q",
                    Term::Type,
                    "x",
                    "y",
                    Term::match_(
                        Term::label("x"),
                        "k",
                        Term::Type,
                        [
                            (
                                "left".into(),
                                Term::apply(Term::label("id"), [Term::label("y")]),
                            ),
                            ("right", Term::Type),
                        ],
                    ),
                ),
            ),
        ),
    );

    println!("{program}");
}
