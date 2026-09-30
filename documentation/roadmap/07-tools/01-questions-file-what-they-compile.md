# Questions file what they compile

Working specification for bringing `lint` and the `wonder` queries under [A command compiles only as far as its answer needs, and a project keeps what it compiled](../../design/tools/a-command-compiles-only-as-far-as-its-answer-needs-and-a-project-keeps-what-it-compiled.md): a question files, for a project, each unit it compiled whole from the text on disk, so the next command starts from the store rather than cold.

## What this builds on

- **The engine's cache.** `ReadOnly` (`curios-wonder/src/diagnostics.rs`) believes a stored unit on a re-read through the overlay (`Verdicts::get_overlaid`), offers the nearest baseline — what the session kept, then the slot the tree filed, then the scope's (`Verdicts::kept`, `Verdicts::earlier`) — and on `put` keeps the unit for the session (`Verdicts::keep`) and places it where something follows (`Verdicts::place`), filing nothing.
- **The contract.** Each command's `Contract` (`curios/src/contract.rs`) states how it reaches the store; `lint` and every `wonder` query read one and file nothing, which the engine holds whatever store it is handed.
- **The overlay.** The server consults every open document's text before the disk (`RootSource::with_overlay`); the command line has none.
- **The baseline's filing rule.** [A stored unit is a baseline for an item-level recompile](../../design/architecture/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md) files no unit compiled over a baseline until the differential test earns it.

## The gap

A question files nothing but the `compiler` memo. A dependency no build has filed is compiled in memory by every `lint` and every `wonder` invocation that reaches it, and again by every server session, so a question over a package nothing has built starts cold every time.

## Decisions

1. **What is filed.** A unit the question compiled whole, reading no open document whose text differs from the disk's. A unit compiled over a baseline is kept and placed, as today, until the baseline's filing rule changes.
2. **Where.** The store a build files into: beside the governing manifest, or `CURIOS_CACHE`'s shared half. A loose file and standard input open no store.
3. **How far.** The judged unit. A question files no payload: its crates link no runtime.

## Stages

1. **The engine files.** The engine's cache files a unit compiled whole from disk text through the store it holds, and keeps and places the rest as `ReadOnly` does; its name and documentation say what it now does.
2. **The contracts.** `lint` and the `wonder` queries state the store access a build has, and `curios/src/contract/tests.rs` pins it.
3. **The server.** It files as every command does: each unit it compiles whole from disk text, the dependencies a session starts cold on among them. A unit that read an open document whose text differs from the disk is kept, never filed; once the document is saved, a whole compile of its unit files, while a compile over the session's baseline, the usual one after an edit, stays unfiled under the baseline's filing rule.

## Verification

- Over a package no build has filed, `wonder diagnostics` run twice: the second compiles no dependency, which its profile shows as no elaboration span for one, and `lint` after it compiles nothing already filed.
- The server: a dependency it compiles is filed when it is compiled, keystrokes in an open document file nothing, and a saved document's unit compiled whole is filed while one compiled over the session's baseline is not.
- A loose file and standard input leave the store as they found it.
- A unit compiled over a baseline is not filed.
- [`curios-wonder`'s latency protocol](../../../curios-wonder/README.md#measuring-a-questions-latency), retaken cold and warm, naming the stage.

## Retirement

`usage.md`'s surface table and its "Reusing what was already built" say what a question files, and the documentation that states today's rule follows the code: `curios-wonder`'s `//!` and `diagnostics.rs`'s `ReadOnly`, `curios/src/contract.rs`'s module documentation and store access, `curios/src/pipeline.rs`'s `documentation_of`, `curios-pipeline`'s `lib.rs` and `Cache::baseline` (`compile.rs`), and the tests that say it — `curios-pipeline/src/tests/baseline_tests.rs`, `curios/tests/payload.rs`, `curios-verdicts/src/verdicts/tests.rs`. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
