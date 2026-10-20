//! Small suspension programs stay bounded before downstream optimization and run through both backend paths.

use {
    super::compile_raw_program,
    curios_native::{Compiled, ForeignBindings, MockHost},
    curios_wasm::to_bytes,
};

#[test]
fn small_suspension_programs_do_not_expand_into_callback_families() {
    for (source, ceiling, status, output, errors) in [
        (
            r#"/std/print("Hello, world!\n")"#,
            128 * 1024,
            0,
            b"Hello, world!\n".as_slice(),
            b"".as_slice(),
        ),
        (
            "/std/Io/wait_filled([])",
            64 * 1024,
            1,
            curios_abi::DEADLOCK.as_bytes(),
            curios_abi::DEADLOCK.as_bytes(),
        ),
        (
            "/std/Io/yield()",
            64 * 1024,
            0,
            b"".as_slice(),
            b"".as_slice(),
        ),
    ] {
        let (module, foreigns) = compile_raw_program(source);
        let raw = to_bytes(&module);
        // Names are retained, as on the browser path. These coarse ceilings leave room for ordinary emission changes while rejecting replicated suspension callback families.
        assert!(
            raw.len() < ceiling,
            "{source}: {} raw bytes reaches the {ceiling}-byte ceiling",
            raw.len()
        );
        for bytes in [raw.clone(), curios_binaryen::optimize(raw, false, |_| {})] {
            let code = curios_cranelift::compile(&bytes).expect("module validates and compiles");
            let compiled = Compiled::of(code, foreigns.clone());
            let (host, io) = MockHost::builder().build();
            assert_eq!(
                compiled
                    .run(host, ForeignBindings::empty())
                    .expect("module executes"),
                status,
                "{source}"
            );
            assert_eq!(io.output(), output, "{source}");
            assert_eq!(io.errors(), errors, "{source}");
        }
    }
}
