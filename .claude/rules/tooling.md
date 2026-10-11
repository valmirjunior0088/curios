---
paths:
  - "curios/src/**"
  - "curios-document/**"
  - "xtask/**"
  - "editors/**"
  - "curios-js/**"
  - ".github/**"
---

# The CLI, documentation pages, steps and editors

- A step is a row of `xtask/src/steps.rs`, and its spelling also reaches `curios/build.rs`, both workflows under `.github/`, `README.md`'s build steps, `CLAUDE.md` and `.claude/commands/`; `every_step_has_a_job` holds the check workflow to the table.
- JavaScript, JSON and CSS are formatted and linted by Biome at the version `xtask/src/tools.rs` states, under the root's `biome.json`. A tree's `format` and `lint` reach every tracked file it holds, so a new file needs no listing.
- The documentation record or its pages also reach `curios-document`'s `record.rs` and `pages.rs` with their templates and static files, the builder in `curios-text/src/into_core/document.rs` with `curios/src/tests/document.rs`, the fold's door in `curios-pipeline/src/standard.rs`, the prelude's records image in `curios-prelude-archive`, the engine in `curios-wonder/src/document.rs`, the build in `curios/src/pipeline.rs`, the store's verdict schema tag when the record's layout changes, and `documentation/usage.md`'s Documenting.
- `curios-js` is built by `cargo xtask bundle build`; `wasm-pack` or `wasm-opt` enter only by a design decision.
- `editors/grammar/src/` is the one committed generated artifact, committed with the `grammar.js` it was generated from; `editors/grammar`'s `npm test` refuses drift between them.
