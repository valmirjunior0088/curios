use {
    curios::{cont, core, ersd, text},
    std::{path::Path, time::Duration},
};

fn main() {
    let source = r#"
        pub mod std;
        pub mod parser;
        pub mod json;

        let value : json/Value = ('obj, [
            ("name", ('str, "Alice")),
            ("score", ('num, +9.5)),
            ("active", ('bln, true)),
            ("tags", ('arr, [('str, "x"), ('str, "y")])),
            ("extra", ('null, ()))
        ]);

        let encoded : Bin = json/encode value;

        let decoded : parser/Result { Nat, json/Value } = json/decode encoded 0;

        match decoded.0 : {};
        | 'ok  => Sys.print (json/encode decoded.1.1);
        | 'err => Sys.print decoded.1;
        "#;

    let text_entrypoint: text::Entrypoint = source.parse().expect("failed to parse source");
    println!("=== text ===");
    println!("{text_entrypoint}");

    let core_term = text::to_core(
        &text_entrypoint,
        &text::FileLoader::new(Path::new(file!()).parent().unwrap().join("crs")),
    )
    .expect("expected core term");
    println!();
    println!("=== core ===");
    println!("{core_term}");

    let timeout = Duration::from_secs(5);
    let ersd_term = core::erase(
        &mut core::Context::new(timeout),
        &core_term,
        &core::infer(&mut core::Context::new(timeout), &core_term)
            .unwrap_or_else(|e| panic!("failed to infer type: {e}")),
    )
    .unwrap_or_else(|e| panic!("failed to erase term: {e}"));
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

    let (system, receiver) = curios::ChannelProvider::out();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"{\"name\":\"Alice\",\"score\":9.5,\"active\":true,\"tags\":[\"x\",\"y\"],\"extra\":null}".to_vec()]
    );
}
