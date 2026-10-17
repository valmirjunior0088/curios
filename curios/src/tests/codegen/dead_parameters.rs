//! Unused arguments preserve ordering, effects and captures across suspension through both Wasm paths.
use {
    super::compile_raw_program,
    curios_runtime::{Compiled, ForeignBindings, MockHost},
    curios_wasm::to_bytes,
};

fn answers(source: &str, expected: [&str; 4]) {
    let (module, foreigns) = compile_raw_program(source);
    let raw = to_bytes(&module);
    for bytes in [raw.clone(), curios_binaryen::optimize(raw, false, |_| {})] {
        let code = curios_cranelift::compile(&bytes).expect("the module validates and compiles");
        let compiled = Compiled::of(code, foreigns.clone());
        for (n, expected) in [0, 1, 4, 8].into_iter().zip(expected) {
            let (host, io) = MockHost::builder().args(vec!["argument"; n]).build();
            assert_eq!(compiled.run(host, ForeignBindings::empty()), Ok(0));
            assert_eq!(io.output(), expected.as_bytes(), "{n} host arguments");
            assert!(io.errors().is_empty());
        }
    }
}

#[test]
fn recursive_arguments_keep_their_positions_and_effects() {
    answers(
        r#"
        use /std/{Nat, List, Process, Io};
        let walk(n: Nat, a: Nat, unused: Nat, b: Nat) -> Nat =
            match n
            | 0 => a * 1000 + b
            | k + 1 => walk(k, b + 2, unused, a + 1)
            end;
        let entry() -> Io({}) =
            let n = List/len(Process/args()!);
            let _ = /std/print("before:")!;
            let first = walk(n, n + 3, (let _ = /std/print("argument:")!; n + 100), n + 5);
            let _ = Io/yield()!;
            let _ = /std/print("after:")!;
            /std/print(Nat/to_str(first + walk(n + 1, n + 7, n + 200, n + 11)));
        entry()
    "#,
        [
            "before:argument:after:16013",
            "before:argument:after:19020",
            "before:argument:after:36033",
            "before:argument:after:56053",
        ],
    );
}

#[test]
fn a_nested_capture_survives_cleanup_and_suspension() {
    answers(
        r#"
        use /std/{Nat, List, Process, Io};
        let capture(n: Nat, unused: Nat, a: Nat, b: Nat) -> (Nat) -> Nat =
            match n
            | 0 => (x) => a * 10 + b + x
            | k + 1 => (x) => capture(k, unused, b + 2, a + 1)(x) + 1
            end;
        let entry() -> Io({}) =
            let n = List/len(Process/args()!);
            let f = capture(n, n + 100, n + 3, n + 5);
            let _ = /std/print("before:")!;
            let _ = Io/yield()!;
            let _ = /std/print("after:")!;
            /std/print(Nat/to_str(f(n)));
        entry()
    "#,
        [
            "before:after:35",
            "before:after:87",
            "before:after:153",
            "before:after:271",
        ],
    );
}
