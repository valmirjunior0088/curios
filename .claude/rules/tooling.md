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

- A step is a table of the task files `xtask/tasks.toml` and `xtask/tasks/*.toml`, and its path also reaches `curios/build.rs`, both workflows under `.github/`, `README.md`'s build steps, `CLAUDE.md` and `.claude/commands/`; `every_step_the_gate_runs_has_a_job` holds the check workflow to the files.
- JavaScript, JSON and CSS are formatted and linted by Biome at the version `xtask/tasks.toml`'s `programs` pins, under the root's `biome.json`, which lists the directories Biome reads, each with its languages: a file of a directory it does not list is read by nothing until the directory is added.
- The documentation record or its pages also reach `curios-document`'s `record.rs` and `pages.rs` with their templates and static files, the builder in `curios-module/src/into_core/document.rs` with `curios/src/tests/document.rs`, the fold's door in `curios-pipeline/src/standard.rs`, the prelude's records image in `curios-prelude/compile.rs`, the engine in `curios-wonder/src/document.rs`, the build in `curios/src/pipeline.rs`, the store's verdict schema tag when the record's layout changes, and `documentation/usage.md`'s Documenting.
- `curios-js` is built by `cargo xtask bundle build`; `wasm-pack` or `wasm-opt` enter only by a design decision.
- `editors/grammar/base/src/`, the program's, and `editors/grammar/core/src/`, Core text's, are the committed generated artifacts, each committed with the `grammar.js` it was generated from; `editors/grammar`'s `npm test` refuses drift between them. Core text's grammar extends the program's, and its Zed highlight query opens with the program's verbatim (`the_core_text_query_opens_with_the_programs`); its TextMate grammar includes the program's by scope.
