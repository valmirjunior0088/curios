//! A foreign declaration's Wasm import, and the namespace it shares with `/sys`.

use super::test_support::*;

#[test]
fn declaration_produces_a_wasm_import() {
    // Must actually *run* `frobnicate` — a foreign call yields a description (`Io(Nat)`), and one the program never forces is pruned by `curios_ersd::optimize` along with its import, before codegen ever sees it.
    let module = compile(
        r#"
            foreign frobnicate : (Nat, Bytes) -> Nat;
            let n = frobnicate(5, x[0x00, 0x01])!;
            /std/print(/std/Nat/to_str(n))
        "#,
        None,
    )
    .unwrap();

    assert!(
        module
            .imports()
            .iter()
            .any(|(namespace, name, _)| namespace == "ffi" && name == "/frobnicate"),
        "expected an ffi./frobnicate import, got {:?}",
        module.imports()
    );
}

#[test]
fn sys_and_foreign_calls_import_under_separate_namespaces() {
    // Must actually run both — an unforced description is pruned before codegen ever sees it (see the note above).
    let module = compile(
        r#"
            foreign frobnicate : (Nat) -> Nat;
            let _ = /std/Handle/write(/std/Handle/stdout, x[0x00])!;
            let n = frobnicate(5)!;
            /std/print(/std/Nat/to_str(n))
        "#,
        None,
    )
    .unwrap();

    let imports = module.imports();

    assert!(
        imports
            .iter()
            .any(|(namespace, name, _)| namespace == "sys" && name == "write"),
        "expected a sys.write import, got {imports:?}"
    );
    assert!(
        imports
            .iter()
            .any(|(namespace, name, _)| namespace == "ffi" && name == "/frobnicate"),
        "expected an ffi./frobnicate import, got {imports:?}"
    );
}
