//! Unused arguments preserve ordering, effects and captures across suspension through both Wasm paths, and a parameter read only to build its own next value is no use.
use {
    crate::test_support::*,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{Compiled, ForeignBindings, test_support::*},
    curios_pipeline::{Stage, compile_with_prelude},
    curios_surface::Entrypoint,
    curios_wasm::{Names, to_bytes},
};

/// A loop parameter whose one reader builds its own next value is dead, and so is that work: `seen`, a hidden argument carrying what the walk has read, is read only by the append that hands it back as the next `seen`, so the loop takes neither the parameter nor the append. With the append gone no byte is read, so the loop carries the bytes' length and its count alone. Mutation-checked: with droppable work asking of its operands whether or not its result is read, the loop keeps `seen` and appends a byte at every step.
#[test]
fn a_parameter_read_only_to_build_its_own_next_value_is_dead() {
    let source = r#"
        use /std/{Nat, Bytes, List, Option, Proc};
        let count(b: Bytes, k: Nat, @seen: Bytes) -> Nat =
            match b
            | 0x[] => k
            | 0x[h, ..t] => count(t, k + 1, @0x[..seen, h])
            end;
        let entry() -> /std/Io({}) =
            let arguments = Proc/args()!;
            let bytes = Option/unwrap_or(List/try_get(arguments, 0), 0x[]);
            /std/print(Nat/to_str(count(bytes, 0, @0x[])));
        entry()
        "#;
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let mut captured = None;
    let (wasm, foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |stage| {
            if let Stage::CpsOptm(module) = stage {
                captured = Some(module.clone());
            }
        })
        .expect("the count compiles");
    let module = captured.expect("the pipeline observes cps-optm");
    let count = module
        .continuation_ids()
        .find(|&id| module.continuation(id).unwrap().debug_name.as_deref() == Some("/count"))
        .expect("the recursive count is contified");
    assert_eq!(
        module.continuation(count).unwrap().params.len(),
        2,
        "the loop carries the bytes' length and the count: {module}"
    );

    let compiled = to_compiled(&wasm, foreigns).expect("the count compiles to a program");
    let (system, io) = MockHost::builder().args(["abcd"]).build();
    assert_eq!(compiled.run(system, ForeignBindings::empty()), Ok(0));
    assert_eq!(io.output(), b"4");
}

fn answers(source: &str, expected: [&str; 4]) {
    let (module, foreigns) = compile_raw_program(source);
    let raw = to_bytes(&module, &Names::default());
    start_test_engine();
    for bytes in [raw.clone(), curios_binaryen::optimize(raw)] {
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
        use /std/{Nat, List, Proc, Io};
        let walk(n: Nat, a: Nat, unused: Nat, b: Nat) -> Nat =
            match n
            | 0 => a * 1000 + b
            | k + 1 => walk(k, b + 2, unused, a + 1)
            end;
        let after(_: {}, x: Nat) -> Nat = x;
        let entry() -> Io({}) =
            let n = List/length(Proc/args()!);
            let _ = /std/print("before:")!;
            let first = walk(n, n + 3, after(/std/print("argument:")!, n + 100), n + 5);
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
        use /std/{Nat, List, Proc, Io};
        let capture(n: Nat, unused: Nat, a: Nat, b: Nat) -> (Nat) -> Nat =
            match n
            | 0 => (x) => a * 10 + b + x
            | k + 1 => (x) => capture(k, unused, b + 2, a + 1)(x) + 1
            end;
        let entry() -> Io({}) =
            let n = List/length(Proc/args()!);
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
