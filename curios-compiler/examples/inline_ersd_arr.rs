use curios::{cont, ersd};

fn main() {
    let ersd_term: ersd::Term = ersd::Subterm::Let(ersd::Let {
        name: "double".into(),
        body: ersd::Subterm::Func(ersd::Func {
            captures: vec![],
            params: vec!["x".into()],
            body: ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::NatAdd(
                ersd::Subterm::Name(ersd::Name::from("x")).into(),
                ersd::Subterm::Name(ersd::Name::from("x")).into(),
            )))
            .into(),
        })
        .into(),
        tail: ersd::Subterm::Let(ersd::Let {
            name: "a".into(),
            body: ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Nat(10))).into(),
            tail: ersd::Subterm::Let(ersd::Let {
                name: "arr".into(),
                body: ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Arr(vec![
                    ersd::Subterm::Apply(ersd::Apply {
                        head: ersd::Subterm::Name(ersd::Name::from("double")).into(),
                        params: vec![ersd::Subterm::Name(ersd::Name::from("a")).into()],
                    })
                    .into(),
                    ersd::Subterm::Name(ersd::Name::from("a")).into(),
                ])))
                .into(),
                tail: ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::ArrLen(
                    ersd::Subterm::Name(ersd::Name::from("arr")).into(),
                )))
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
    curios_compiler::run_wasm(&wasm_module, curios_rt::OsHost::new()).unwrap();
}
