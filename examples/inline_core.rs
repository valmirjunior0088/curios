use {
    curios::{cont, core, ersd},
    std::time::Duration,
};

fn main() {
    let core_term: core::Term = core::Rec::new(
        vec![(
            "id",
            core::FuncType::new("_", core::Type, core::Type),
            core::Func::new("x", core::Var::free("x")),
        )],
        core::Let::new(
            "tuple_ty",
            core::Type,
            core::TupleType::new([
                (
                    "label",
                    core::Term::from(core::AtomType::new(["left", "right"])),
                ),
                (
                    "value",
                    core::Term::from(core::Match::new(
                        core::Var::free("label"),
                        "_",
                        core::Type,
                        [("left", core::Type), ("right", core::Type)],
                    )),
                ),
            ]),
            core::Let::new(
                "p",
                core::Var::free("tuple_ty"),
                core::Tuple::new([
                    core::Term::from(core::Atom::from("left")),
                    core::Type.into(),
                ]),
                core::Let::new(
                    "label",
                    core::AtomType::new(["left", "right"]),
                    core::Proj::new(core::Var::free("p"), 0),
                    core::Let::new(
                        "value",
                        core::Type,
                        core::Proj::new(core::Var::free("p"), 1),
                        core::Match::new(
                            core::Var::free("label"),
                            "_",
                            core::Type,
                            [
                                (
                                    "left",
                                    core::Apply::many(
                                        core::Var::free("id"),
                                        [core::Var::free("value")],
                                    ),
                                ),
                                ("right", core::Type.into()),
                            ],
                        ),
                    ),
                ),
            ),
        ),
    )
    .into();

    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &core::Type.into(),
    )
    .expect("expected erased term");

    println!();
    println!("=== ersd ===");
    println!("{ersd_term}");

    let cont_module = ersd::to_cont(&ersd_term);

    println!();
    println!("=== cont ===");
    println!("{cont_module}");

    let wasm_module = cont::to_wasm(&cont_module);

    println!();
    println!("=== wasm ===");
    println!("{wasm_module}");

    println!();
    println!("=== result ===");
    curios::run_wasm(&wasm_module, curios::StdoutProvider).unwrap();
}
