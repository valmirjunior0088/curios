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
        use /std/{Parse, Io, Bin, Nat};

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
        | 'ok  => Io/print(json/encode(decoded.1.1))
        | 'err => Io/print(decoded.1)
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
