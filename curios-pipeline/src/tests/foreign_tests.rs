//! A foreign declaration's Wasm import, and the namespace it shares with `/sys`.

use super::test_support::*;

#[test]
fn declaration_produces_a_wasm_import() {
    // Must actually *run* `frobnicate` — a foreign call yields a description (`Io(Nat)`), and one the program never forces is pruned by `curios_ersd::optimize` along with its import, before codegen ever sees it.
    let module = compile(
        r#"
            foreign frobnicate(Nat, Bytes) -> Nat;
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
    let library = "pub foreign sha256(Bytes) -> Bytes;\n";
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
            foreign reached(Nat) -> Nat;
            foreign unreached(Nat) -> Nat;
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
            foreign frobnicate(Nat) -> Nat;
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

/// A row is a function of its operands, none included: one with no operand is called where its description is wanted and is no description itself, and one that does not return takes the type it yields into the same list, so it is called once.
#[test]
fn a_row_of_no_operand_is_called() {
    typecheck(
        r#"
            foreign clock() -> Nat;
            foreign gone() -> Never;
            let called : /std/Io(/std/Nat) = clock();
            let held : () -> /std/Io(/std/Nat) = clock;
            let ended : /std/Io(/std/Nat) = gone();
            /std/Io/pure(())
        "#,
    )
    .expect("each is used as the function it is");

    let refusal = typecheck(
        r#"
            foreign clock() -> Nat;
            let description : /std/Io(/std/Nat) = clock;
            /std/Io/pure(())
        "#,
    )
    .expect_err("a function of no operand is no description");
    assert!(refusal.contains("type mismatch"), "{refusal}");
}

/// A report spells a call of a row as a call of its declaration, by the spelling resolution takes back to it where the reader stands: a child module's row under its path, where the bare label is another declaration's name beside the reader.
#[test]
fn a_report_spells_a_row_as_its_declaration_is_reached() {
    let (_directory, modules) = written_files(
        "row-spelling",
        &[
            (
                "lib.crs",
                "mod inner;\n\nuse /std/{Eq, Io, Nat};\n\nlet deep(n: Nat) -> Io(Nat) = Io/pure(n);\n\nlet p: Eq()(inner/deep(1), deep(1)) = Eq/refl();\n",
            ),
            ("inner.crs", "pub foreign deep(Nat) -> Nat;\n"),
        ],
    );

    let refusal = compile_modules(&modules)
        .err()
        .expect("the two calls are not one");

    assert!(
        refusal.contains("expected: Eq(@Io(Nat))(inner/deep(1), Io/pure(@Nat, 1))"),
        "{refusal}"
    );
}

/// A row no name its reader can write reaches keeps its declaration's whole path: a private row of the standard library, met through the function that calls it.
#[test]
fn a_report_spells_a_row_its_reader_cannot_reach_by_its_path() {
    let refusal = typecheck(
        r#"
            use /std/{Eq, Handle, Io};
            pub let p: Eq()(Handle/close(Handle/stdin), Io/pure(())) = Eq/refl();
            /std/print("")
        "#,
    )
    .expect_err("the two descriptions are not one");

    assert!(
        refusal.contains(
            "expected: Eq(@Io({}))(/std/Handle/handle_close(Handle/stdin), Io/pure(@{}, ()))"
        ),
        "{refusal}"
    );
}
