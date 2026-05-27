use {
    curios::{cont, core, ersd, text},
    std::{path::Path, time::Duration, time::Instant},
};

fn main() {
    let source = r#"
        pub mod std;
        use /std/Parse;

        pub mod json;

        let value : json/Value = ('obj, [
            ("name", ('str, "Alice")),
            ("score", ('num, +9.5)),
            ("active", ('bln, true)),
            ("tags", ('arr, [('str, "x"), ('str, "y")])),
            ("extra", ('null, ()))
        ]);

        let encoded : Bin = json/encode(value);

        let decoded : Parse/Result({ Nat, json/Value }) = json/decode(encoded, 0);

        match decoded.0 : {}
        | 'ok  => Sys.print(json/encode(decoded.1.1))
        | 'err => Sys.print(decoded.1)
        end
        "#;

    let t = Instant::now();
    let text_entrypoint: text::Entrypoint = source.parse().expect("failed to parse source");
    println!("text: {:?}", t.elapsed());

    let t = Instant::now();
    let core_term = text::to_core(
        &text_entrypoint,
        &text::FileLoader::new(Path::new(file!()).parent().unwrap().join("crs")),
    )
    .expect("expected core term");
    println!("core: {:?}", t.elapsed());

    let timeout = Duration::from_secs(10);
    let t = Instant::now();
    let ersd_term = core::erase(
        &mut core::Context::new(timeout),
        &core_term,
        &core::infer(&mut core::Context::new(timeout), &core_term)
            .unwrap_or_else(|e| panic!("failed to infer type: {e}")),
    )
    .unwrap_or_else(|e| panic!("failed to erase term: {e}"));
    println!("ersd: {:?}", t.elapsed());

    let t = Instant::now();
    let cont_module = ersd::to_cont(&ersd_term);
    println!("cont: {:?}", t.elapsed());

    let t = Instant::now();
    let wasm_module = cont::to_wasm(&cont_module);
    println!("wasm: {:?}", t.elapsed());

    let (system, receiver) = curios::ChannelProvider::out();
    let t = Instant::now();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    println!("run:  {:?}", t.elapsed());
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"{\"name\":\"Alice\",\"score\":9.5,\"active\":true,\"tags\":[\"x\",\"y\"],\"extra\":null}".to_vec()]
    );
}
