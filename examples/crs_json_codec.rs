use {
    curios::{Stage, compile, text},
    std::{
        path::Path,
        time::{Duration, Instant},
    },
};

fn main() {
    let source = r#"
        pub mod std;
        use /std/{Parse, Io, Bin, Nat, Result};

        pub mod json;
        use json/{Value};

        let value : Value = Value/obj([
            ("name", Value/str("Alice")),
            ("score", Value/num(+9.5)),
            ("active", Value/bln(true)),
            ("tags", Value/arr([Value/str("x"), Value/str("y")])),
            ("extra", Value/null())
        ]);

        let encoded : Bin = json/encode(value);

        let decoded : Result({ Nat, Value }, Bin) = json/decode(encoded, 0);

        match decoded : {}
        | success(pair) => Io/print(json/encode(pair.1))
        | failure(msg) => Io/print(msg)
        end
        "#;

    let loader = text::FileLoader::new(Path::new(file!()).parent().unwrap().join("crs"));
    let mut last = Instant::now();

    let wasm_module = compile(Duration::from_secs(10), &loader, None, source, |stage| {
        let now = Instant::now();
        let elapsed = now - last;
        last = now;
        let name = match stage {
            Stage::Text(_) => "text",
            Stage::Core(_) => "core",
            Stage::Ersd(_) => "ersd",
            Stage::Cont(_) => "cont",
            Stage::Wasm(_) => "wasm",
        };
        println!("{name}: {elapsed:?}");
    })
    .expect("expected wasm module");

    let (system, receiver) = curios::ChannelHost::out();
    let t = Instant::now();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    println!("run:  {:?}", t.elapsed());
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"{\"name\":\"Alice\",\"score\":9.5,\"active\":true,\"tags\":[\"x\",\"y\"],\"extra\":null}".to_vec()]
    );
}
