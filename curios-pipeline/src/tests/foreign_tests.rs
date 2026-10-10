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

/// What a package's manifest says a module answers is held to the rows its own unit declares, where the unit is compiled: a claim on a `foreign` the unit declares passes, and one on no declaration is refused against the ones it does declare.
#[test]
fn a_claim_is_held_to_a_declaration_of_its_own_unit() {
    let library = "pub foreign sha256: (Bytes) -> Bytes;\n";
    let claiming = |declaration: &str| {
        mounted("crypto", library).answering([(declaration.to_string(), "sha2".to_string())])
    };

    compile_each(&[&claiming("/crypto/sha256")]).expect("a claim on a declaration of the unit");

    let refusal = compile_each(&[&claiming("/crypto/sha265")])
        .err()
        .expect("a claim on no declaration");

    assert!(
        refusal.contains(
            "the foreign row \"sha2\" says an export answers `/crypto/sha265`, and /crypto declares no such `foreign`: it declares `/crypto/sha256`"
        ),
        "{refusal}"
    );
}

/// A program is handed the rows it reaches and no other: a row a unit declares and nothing calls asks its embedder for nothing.
#[test]
fn a_program_is_handed_the_rows_it_reaches() {
    let entry = entrypoint_of(
        r#"
            foreign reached : (Nat) -> Nat;
            foreign unreached : (Nat) -> Nat;
            let n = reached(5)!;
            /std/print(/std/Nat/to_str(n))
        "#,
    );
    let (_, foreigns) = crate::compile_with_prelude(
        crate::DEFAULT_STEP_BUDGET,
        &entry,
        &curios_text::RootSource::none(),
        |_| {},
    )
    .expect("the program compiles");

    assert!(foreigns.get("/reached").is_some());
    assert!(foreigns.get("/unreached").is_none());
}

#[test]
fn a_library_row_and_a_program_row_import_by_the_names_they_are_declared_under() {
    // Must actually run both — an unforced description is pruned before codegen ever sees it (see the note above).
    let module = compile(
        r#"
            foreign frobnicate : (Nat) -> Nat;
            let _ = /std/Io/write(/std/Io/stdout, x[0x00])!;
            let n = frobnicate(5)!;
            /std/print(/std/Nat/to_str(n))
        "#,
    )
    .unwrap();

    let imports = module.imports();

    assert!(
        imports
            .iter()
            .any(|(namespace, name, _)| namespace == "ffi" && name == "/std/Handle/handle_write"),
        "expected an ffi./std/Handle/handle_write import, got {imports:?}"
    );
    assert!(
        imports
            .iter()
            .any(|(namespace, name, _)| namespace == "ffi" && name == "/frobnicate"),
        "expected an ffi./frobnicate import, got {imports:?}"
    );
}
