# A question about a program is answered by the compilation that would build it

## Status

The first landing is in: `curios wonder diagnostics`, `curios wonder stage`, and `curios wonder server` publishing diagnostics. This file now states only what is left, against what landed. The landed contracts live with the code — `curios/src/wonder.rs` and its submodules, `curios-package/src/membership.rs`, `curios-utilities`'s `Report`, `curios-pipeline`'s `CompileError`/`check_with_units`/`check_units_with_prelude`, and `curios-text`'s `Overlay` — and in `documentation/usage.md`'s "Asking about a program".

## Mission, unchanged

`curios wonder` lets an agent and an editor ask the compiler what it knows about a program, and get the same answer. One engine, one record per fact, two ways to reach it: a query addressed by name, and a language server addressed by cursor. Neither consumer is secondary.

## What landed, in one paragraph each

**The record is the compiler's own report.** Every stage's error now produces a `Report` — a message at a span, or at nothing — and its rendered text is that report rendered, so the located form and the printed form are one value; a goal batch is one report per `?`. `CompileError` carries the reports rather than the text. That is the whole of the engine's "facts are recorded where the compiler decides them": the engine reads what the compile path already said.

**The transports are two, in one module.** `ask` renders records to stdout and answers exit 0 once the question is answered, whatever the answer; `server` is the synchronous `lsp-server` loop on a protocol thread beside one analyst thread that owns the compiler, the editor's documents as the overlay, edits coalesced on the analyst, `textDocument/formatting` answered by the protocol thread from `curios format`'s machinery so a request never waits on a compile, and UTF-16 derived from each span's own text at the boundary. Both live under `curios::wonder` rather than in a crate of their own, because the crate's one argued consumer — a browser editor through `curios-js` — does not exist yet; the day it does is the day the engine splits out from under Binaryen and Wasmtime.

**A file is placed, not compiled alone.** `Membership::of` decides from the file's own location: the library whose directory holds it, the executable whose entry or stem tree it is, or standalone when no manifest above it claims it. `run` keeps its standalone reading; the divergence is deliberate and stated in `membership.rs`.

**A snapshot stops at the first failure**, as the compile path does, and a question never writes the store.

## What is left

Each item below is what the first landing deliberately did not do, with the change it needs.

- **`--json`.** Records derive a serialization trait behind a default-off feature, and every query gets a second projection asserted for shape beside its rendering. The renderings are already computed from the records, so this is a projection, not a refactor.
- **Snapshot identity and `--depth`.** A snapshot digested from the compiler's identity, the scope's unit addresses, the depth and every source's path and content; every answer names it. `--depth` parameterizes a compilation beside `--budget`, `--unit` and `--manifest`, and may ask for more than a query needs, never less. Today `diagnostics` always runs to `certified` and `stage` to whatever rung was named.
- **Declaration label spans**, on `TopLet`, `TopInduct`, `TopStruct`, `TopConcept`, their cases and fields, following `TopMod.span`'s exclusion from `PartialEq`. This unlocks `symbols` (`documentSymbol`), and `at`/`type` under a selector (`binding:/std/Lst/map`, `module:/std/Lst`).
- **Anchors under locations.** `at <path:line:column>` at a written goal and at a declaration name, which is `hover`. A goal's type and scope are already batched; a declaration's elaborated type is on its `Definition`. A position inside neither yields nothing rather than a guess.
- **`witnesses [<concept>]`**, one read of the `Context`'s witness table before it is discarded.
- **`modules`**, the module tree from `RootSource`'s reads, each with origin and source.
- **The server's other methods**: `hover` and `documentSymbol`, once the anchors and label spans exist.
- **The measurement test.** A front-end snapshot costs an order of magnitude less than a compile, which is why whole-unit re-elaboration per edit is the starting position; the ignored test that carries the figure and the command that retakes it is not written.
- **The `wasm-optm` rung in a unit-less snapshot** is the transport's, not the engine's, and stays so.

## Not in any near landing

- **Per-item recovery** in parse, lowering and elaboration — the only way a broken file answers more than the failure that stopped it. A substantial change to three loops on the shared compile path.
- **Occurrence ids and per-occurrence types**, and with them `references`, `definition` at an arbitrary cursor, semantic tokens, and rename.
- **`scope`/completion**, which waits on recovery specifically.
- **The binder table stored with a unit**, so `definition` reaches into a dependency.
- **`search`/`workspaceSymbol`**, and the rule a set-scoped answer needs when the server has invalidated one member of the set.
