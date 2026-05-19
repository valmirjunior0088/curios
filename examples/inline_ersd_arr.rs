use curios::{cont, ersd};

fn main() {
    let ersd_term = ersd::Term::Let(ersd::Let {
        name: "double".into(),
        body: ersd::Term::Func(ersd::Func {
            captures: vec![],
            param: "x".into(),
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
                        param: ersd::Term::Name(ersd::Name::from("a")).into(),
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

    println!();
    println!("=== wasm ===");
    println!("{}", cont::to_wasm(&cont_module));
}
