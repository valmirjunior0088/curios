use curios::{cont, ersd};

fn main() {
    let ersd_term = ersd::Term::Let(ersd::Let {
        name: "double".into(),
        body: ersd::Term::Func(ersd::Func {
            captures: vec![],
            params: vec!["x".into()],
            body: ersd::Term::Prim(ersd::Prim::NatAdd(
                ersd::Term::Name(ersd::Name::from("x")).into(),
                ersd::Term::Name(ersd::Name::from("x")).into(),
            ))
            .into(),
        })
        .into(),
        tail: ersd::Term::Let(ersd::Let {
            name: "a".into(),
            body: ersd::Term::Prim(ersd::Prim::Nat(10)).into(),
            tail: ersd::Term::Let(ersd::Let {
                name: "arr".into(),
                body: ersd::Term::Prim(ersd::Prim::Arr(vec![
                    ersd::Term::Apply(ersd::Apply {
                        head: ersd::Term::Name(ersd::Name::from("double")).into(),
                        params: vec![ersd::Term::Name(ersd::Name::from("a")).into()],
                    })
                    .into(),
                    ersd::Term::Name(ersd::Name::from("a")).into(),
                ]))
                .into(),
                tail: ersd::Term::Prim(ersd::Prim::ArrLen(
                    ersd::Term::Name(ersd::Name::from("arr")).into(),
                ))
                .into(),
            })
            .into(),
        })
        .into(),
    });

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
