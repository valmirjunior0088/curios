use {
    curios::{Stage, compile_entrypoint},
    std::time::Duration,
};

fn main() {
    let source = r#"
        use /std/{Nat, Int, Flt, Bin, Arr};
        induct Pair
        | left(Int)
        | right(Flt)
        end
        let pair : Pair = Pair/left(+42);
        let score : (Pair) -> Int = (p) =>
            match p : Int
            | left(_) => +42
            | right(_) => +7
            end;
        let my_list : Arr(Nat) = [|1, 2, 3|];
        let my_bin : Bin = \01\02\03;
        let my_str : Bin = /std/Str/to_bin("hello");
        let list_len : Nat = Arr/len(my_list);
        let bin_len : Nat = Bin/len(my_bin);
        let str_len : Nat = Bin/len(my_str);
        Int/add(score(pair), Nat/to_int(Nat/add(list_len, Nat/add(bin_len, str_len))))
        "#;

    let entrypoint = source
        .parse::<curios::text::Entrypoint>()
        .unwrap()
        .with_type("/std/Int".parse().unwrap());

    let wasm_module = compile_entrypoint(
        Duration::from_secs(1),
        &entrypoint,
        &curios::text::NullLoader,
        |stage| match stage {
            Stage::Text(entrypoint) => {
                println!("=== text ===");
                println!("{entrypoint}");
            }
            Stage::Core(term) => {
                println!();
                println!("=== core ===");
                println!("{term}");
            }
            Stage::Ersd(term) => {
                println!();
                println!("=== ersd ===");
                println!("{term}");
            }
            Stage::ErsdOptm(term) => {
                println!();
                println!("=== ersd-optm ===");
                println!("{term}");
            }
            Stage::Cont(module) => {
                println!();
                println!("=== cont ===");
                println!("{module}");
            }
            Stage::ContOptm(module) => {
                println!();
                println!("=== cont-optm ===");
                println!("{module}");
            }
            Stage::Wasm(module) => {
                println!();
                println!("=== wasm ===");
                println!("{module}");
            }
        },
    )
    .expect("expected wasm module");

    println!();
    println!("=== result ===");
    curios_compiler::run_wasm(&wasm_module, curios_runtime::OsHost::new()).unwrap();
}
