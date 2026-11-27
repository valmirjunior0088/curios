use {
    curios_binaryen::optimize,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint,
    curios_wasm::{Module, Names, to_bytes},
};

#[test]
fn optimizes_to_a_smaller_valid_module() {
    // `curios_binaryen::optimize` is bytes -> bytes; producing a real input still needs the compiler, hence this test living alongside the pipeline rather than in `curios-binaryen` itself. That the optimized module still *behaves* identically is covered by the rest of this suite, whose run path optimizes on every execution.
    let source = r#"
        let sum(n : /std/Nat) -> /std/Nat =>
            match n
            | 0 => 0
            | pred + 1 => /std/Nat/add(/std/Nat/succ(pred), sum(pred))
            end;

        /std/Fmt/print("%")(sum(10))
    "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let (module, _foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
            .expect("expected wasm module");

    let bytes = to_bytes(&module, &Names::default());
    // `optimize` validates the result internally (asserting on an invalid module).
    let optimized = optimize(bytes.clone());

    assert!(optimized.starts_with(b"\0asm"));
    assert!(
        optimized.len() < bytes.len(),
        "expected the optimized module ({} bytes) to be smaller than the input ({} bytes)",
        optimized.len(),
        bytes.len()
    );

    // Binaryen keeps a name section exactly where it is handed one: the names a module carries are its writer's to decide (`Names`), so a module written with none is optimized to one with none, and one written with its functions' names keeps a section a sampler reads them off.
    let unnamed = optimize(to_bytes(&module, &Names::NONE));

    assert!(unnamed.starts_with(b"\0asm"));
    assert!(
        optimized.len() > unnamed.len(),
        "expected the named module ({} bytes) to keep the name section the unnamed one ({} bytes) never had",
        optimized.len(),
        unnamed.len()
    );
    assert!(
        !unnamed.windows(5).any(|window| window == b"\x04name"),
        "an unnamed module is optimized to one with no name section"
    );
}

/// The feature mask is what keeps the emitter and the optimizer agreeing on the envelope, and `optimize` aborts the process on a module it cannot read rather than returning an error — so a mask missing a feature `curios-wasm` can emit fails hard here rather than surprising a caller later. Each module reaches one construct the mask had to grow for, or one the grown mask must still accept beside it.
#[test]
fn passes_the_full_memory_and_table_surface_through() {
    let sources = [
        (
            "an active data segment",
            r#"
            (module $active_data
                (memory $m i32 1)
                (data $d (memory $m) (offset i32.const 0) "\68\69"))
"#,
        ),
        (
            "the bulk-memory instructions over two memories",
            r#"
            (module $bulk
                (type $f (func))
                (func $a (type $f)
                    i32.const 0
                    i32.const 0
                    i32.const 1
                    memory.copy $first $second
                    i32.const 0
                    i32.const 0
                    i32.const 1
                    memory.fill $first
                    i32.const 0
                    i32.const 0
                    i32.const 1
                    memory.init $first $d
                    data.drop $d)
                (memory $first i32 1)
                (memory $second i32 1)
                (data $d "\00")
                (export "a" (func $a)))
"#,
        ),
        (
            "a table called through `call_indirect`",
            r#"
            (module $indirect
                (type $f (func))
                (func $a (type $f)
                    i32.const 0
                    call_indirect $t (type $f))
                (table $t i32 1 (ref null func))
                (elem $e (table $t) (offset i32.const 0) func $a)
                (export "a" (func $a)))
"#,
        ),
        (
            "a 64-bit memory",
            r#"
            (module $memory64
                (type $f (func (result i64)))
                (func $a (type $f)
                    memory.size $m)
                (memory $m i64 1)
                (export "a" (func $a)))
"#,
        ),
        (
            "a 64-bit table",
            r#"
            (module $table64
                (type $f (func (result i64)))
                (func $a (type $f)
                    table.size $t)
                (table $t i64 1 (ref null func))
                (export "a" (func $a)))
"#,
        ),
    ];

    for (construct, source) in sources {
        let module = source.parse::<Module>().expect("expected a module");
        let optimized = optimize(to_bytes(&module, &Names::default()));

        assert!(
            optimized.starts_with(b"\0asm"),
            "expected {construct} to survive optimization"
        );
    }
}
