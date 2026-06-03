use {
    curios::{cont, core, ersd},
    std::time::Duration,
};

fn main() {
    let core_term: core::Term = core::Term::rec(
        vec![(
            "id",
            core::Term::func_type([("_", core::Term::type_())], core::Term::type_()),
            core::Term::func(["x"], core::Term::var(core::Var::free("x"))),
        )],
        core::Term::let_(
            "tuple_ty",
            core::Term::type_(),
            core::Term::tuple_type([
                ("label", core::Term::atom_type(["left", "right"])),
                (
                    "value",
                    core::Term::match_(
                        core::Term::var(core::Var::free("label")),
                        None,
                        core::Term::type_(),
                        [
                            ("left", core::Term::type_()),
                            ("right", core::Term::type_()),
                        ],
                    ),
                ),
            ]),
            core::Term::let_(
                "p",
                core::Term::var(core::Var::free("tuple_ty")),
                core::Term::tuple([
                    core::Term::atom(core::Atom::from("left")),
                    core::Term::type_(),
                ]),
                core::Term::let_(
                    "label",
                    core::Term::atom_type(["left", "right"]),
                    core::Term::proj(core::Term::var(core::Var::free("p")), 0),
                    core::Term::let_(
                        "value",
                        core::Term::type_(),
                        core::Term::proj(core::Term::var(core::Var::free("p")), 1),
                        core::Term::match_(
                            core::Term::var(core::Var::free("label")),
                            None,
                            core::Term::type_(),
                            [
                                (
                                    "left",
                                    core::Term::apply(
                                        core::Term::var(core::Var::free("id")),
                                        [core::Term::var(core::Var::free("value"))],
                                    ),
                                ),
                                ("right", core::Term::type_()),
                            ],
                        ),
                    ),
                ),
            ),
        ),
    );

    println!("=== core ===");
    println!("{core_term}");

    let ersd_term = core::erase(
        &mut core::Context::new(Duration::from_secs(5)),
        &core_term,
        &core::Term::type_(),
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
    curios::run_wasm(&wasm_module, curios::StdioHost).unwrap();
}
