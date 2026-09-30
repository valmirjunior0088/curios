---
paths:
  - "curios/src/**"
  - "curios-document/**"
  - "xtask/**"
  - "editors/**"
  - "curios-js/**"
  - ".github/**"
---

# The CLI, documentation pages, recipes and editors

- A build recipe (`xtask`) also reaches `curios/build.rs`, the CI workflows calling it and `README.md`'s build steps.
- The documentation record or its pages also reach `curios-document`'s `record.rs` and `pages.rs` with their templates and static files, the builder in `curios-text/src/into_core/document.rs` with `curios/src/tests/document.rs`, the engine in `curios-wonder/src/document.rs`, the build in `curios/src/pipeline.rs`, the store's verdict schema tag when the record's layout changes, and `documentation/usage.md`'s Documenting.
- `curios-js` is built by `cargo x js`; `wasm-pack` or `wasm-opt` enter only by a design decision.
- `editors/grammar/src/` is the one committed generated artifact, committed with the `grammar.js` it was generated from; `editors/grammar`'s `npm test` refuses drift between them.
