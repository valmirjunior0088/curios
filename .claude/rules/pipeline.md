---
paths:
  - "curios-pipeline/**"
  - "curios-unit/**"
  - "curios-package/**"
  - "curios-verdict/**"
  - "curios-wonder/**"
---

# The pipeline, packages and the cabinet

- `curios-pipeline` is the compiler boundary: no dependency on Binaryen, Wasmtime, the runtime or the CLI. It names the fixed prelude in `standard.rs` and, for the puts behind its `test-support` feature, `test_support.rs` alone; `compile_entrypoint` takes a scope and cannot tell which unit is `/std`.
- `curios-package` sits beside the boundary: `curios-pipeline` does not depend on it, and `curios-js` does not touch it. `curios-verdict` and `curios-wonder` sit above both and below `curios`; `cargo tree -p <crate> --edges normal` contains none of `curios-binaryen`, `curios-native` and `curios-cranelift`, and what each needs from a back end is handed in.
- What a unit hands its successors also reaches every stage whose artifact `Unit` holds, the fold, and the stored-unit format.
- A program's own declarations are a unit (`enter`, `curios-pipeline/src/compile.rs`): how a unit is compiled, kept or filed also reaches the entry step, `Verdicts::executing` with every command that opens a handle for a program, and the two instruments that put a program whole (`typecheck_entrypoint`, `examine_entrypoint`).
- Manifests, resolution or the cabinet also reach the CLI commands wrapping them, `Qualifier` and `Mount`, `curios-verdict`'s keys, and `documentation/design/soundness/cached-verdicts.md`.
- A lint, or a fact a stage records for one, also reaches the stage that decides it — the lowering's `unused-import` and `unused-declaration`, elaboration's binder lints as it files a record (`curios_elab::binder_lints`, from each declaration's `curios_core::Mints::unread`), `curios-wonder`'s `unused-dependency` — `curios-core`'s `Lint`, what gathers a unit's lints (`Unit::new`, `curios-pipeline`'s `Findings`), `documentation/usage.md`'s `lint` table, the decision `documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md`, and the cabinet's object schema tag (`OBJECT_SCHEMA`, `curios-verdict`) when what a unit stores moves.
- A `wonder` query, a record or what a diagnostic carries also reaches `curios-common`'s `Report`, every stage's `report`/`reports`, `CompileError` and `Fold::check`, both transports (`curios-wonder/src/ask.rs`, `server.rs`) and `curios-package`'s `Selection`.
