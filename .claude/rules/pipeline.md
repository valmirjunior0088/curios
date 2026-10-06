---
paths:
  - "curios-pipeline/**"
  - "curios-unit/**"
  - "curios-package/**"
  - "curios-verdicts/**"
  - "curios-wonder/**"
---

# The pipeline, packages and the store

- `curios-pipeline` is the compiler boundary: no dependency on Binaryen, Wasmtime, the runtime or the CLI. It names the fixed prelude in `standard.rs` alone; `compile_entrypoint` takes a scope and cannot tell which unit is `/std`.
- `curios-package` sits beside the boundary: `curios-pipeline` does not depend on it, and `curios-js` does not touch it. `curios-verdicts` and `curios-wonder` sit above both and below `curios`; `cargo tree -p <crate> --edges normal` contains neither `curios-binaryen` nor `curios-runtime`, and what each needs from a back end is handed in.
- `cargo tree -p curios-unit --edges normal` contains no `curios-cert`.
- What a unit hands its successors also reaches every stage whose artifact `Unit` holds, the fold, and the stored-unit format.
- Manifests, resolution or the store also reach the CLI commands wrapping them, `Qualifier` and `Mount`, `curios-verdicts`' keys, and `documentation/design/soundness/cached-verdicts.md`.
- A `wonder` query, a record or what a diagnostic carries also reaches `curios-utilities`' `Report`, every stage's `report`/`reports_with_hints`, `CompileError` and `Fold::check`, both transports (`curios-wonder/src/ask.rs`, `server.rs`) and `curios-package`'s `Selection`.
