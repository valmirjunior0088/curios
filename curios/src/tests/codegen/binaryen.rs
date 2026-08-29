use {
    curios_binaryen::optimize,
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_pipeline::{Stage, compile_with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_wasm::{Module, to_bytes},
};

#[test]
fn optimizes_to_a_smaller_valid_module() {
    // `curios_binaryen::optimize` is bytes -> bytes; producing a real input still needs the compiler, hence this test living alongside the pipeline rather than in `curios-binaryen` itself. That the optimized module still *behaves* identically is covered by the rest of this suite, whose run path optimizes on every execution.
    let source = r#"
        let sum(n : /std/Nat) -> /std/Nat =
            match n : (_) => /std/Nat
            | 0 => 0
            | pred + 1; ih => /std/Nat/add(/std/Nat/succ(pred), ih)
            end;

        /std/Fmt/print("%")(sum(10))
    "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let (module, _foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("expected wasm module");

    let bytes = to_bytes(&module);
    // `optimize` validates the result internally (asserting on an invalid module).
    let optimized = optimize(bytes.clone(), false);

    assert!(optimized.starts_with(b"\0asm"));
    assert!(
        optimized.len() < bytes.len(),
        "expected the optimized module ({} bytes) to be smaller than the input ({} bytes)",
        optimized.len(),
        bytes.len()
    );

    // The `names` flag is the difference between a profile that reads `$func/<N>$hint` and one that reads bare addresses, and it is off for shipped binaries — so what pins it is that asking for names produces a *larger* module than not asking. Binaryen drops the section by default, which is what made every runtime profile of a Curios program unreadable until this was threaded through.
    let named = optimize(bytes, true);

    assert!(named.starts_with(b"\0asm"));
    assert!(
        named.len() > optimized.len(),
        "expected the named module ({} bytes) to retain the name section the unnamed one ({} bytes) drops",
        named.len(),
        optimized.len()
    );
}

/// `wasm_optm` emits `Stage::WasmOptm` at its production site — the second and only other emission site beside the driver's — and its payload is Binaryen's own rendering, captured in the session that optimized. The `(module` head pins that the payload is the folded text form; the export string pins that it renders *this* module, since exports survive optimization and print verbatim.
///
/// [`crate::to_cwasm`] is asserted beside it rather than through it: rendering and precompiling are two things the Binaryen path does, and one function doing both is what made every `wonder stage wasm-optm` pay for a payload it discarded. The double optimization is this test's alone.
#[test]
fn dumping_emits_the_optimized_module_as_text() {
    let source = r#"
        let sum(n : /std/Nat) -> /std/Nat =
            match n : (_) => /std/Nat
            | 0 => 0
            | pred + 1; ih => /std/Nat/add(/std/Nat/succ(pred), ih)
            end;

        /std/Fmt/print("%")(sum(10))
    "#;

    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    let (module, _foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("expected wasm module");

    let mut text = None;

    crate::wasm_optm(&module, |stage| match stage {
        Stage::WasmOptm(dump) => text = Some(dump.to_string()),
        other => panic!("expected only Stage::WasmOptm, got {:?}", other.name()),
    });

    let cwasm = crate::to_cwasm(&module).expect("binaryen path precompiles");

    assert!(!cwasm.is_empty());

    let text = text.expect("Stage::WasmOptm observed");

    assert!(
        text.starts_with("(module"),
        "expected Binaryen's folded text form, got {:?}...",
        &text[..text.len().min(40)]
    );

    let (export, _) = &module.exports()[0];

    assert!(
        text.contains(&format!("(export \"{export}\"")),
        "expected the optimized module to keep the {export:?} export"
    );
}

/// The feature mask is what keeps the emitter and the optimizer agreeing on the envelope, and `optimize` aborts the process on a module it cannot read rather than returning an error — so a mask missing a feature `curios-wasm` can now emit fails hard here rather than surprising a caller later. Each module reaches one construct the mask had to grow for, or one the grown mask must still accept beside it.
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
                (data $d passive "\00")
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
                    call_indirect $t $f)
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
        let optimized = optimize(to_bytes(&module), false);

        assert!(
            optimized.starts_with(b"\0asm"),
            "expected {construct} to survive optimization"
        );
    }
}
