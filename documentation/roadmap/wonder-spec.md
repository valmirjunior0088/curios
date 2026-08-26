# A question about a program is answered by the compilation that would build it

## Status

The first landing is in: `curios wonder diagnostics`, `curios wonder stage`, and `curios wonder server` publishing diagnostics. Written goals report in every position they can be written — a local `let` annotation, a lambda domain and a `match` motive used to be mistaken for silent holes and swallowed, which `MetavarOrigin` now rules out. This file states only what is left, against what landed. The landed contracts live with the code — `curios/src/wonder.rs` and its submodules, `curios-package/src/membership.rs`, `curios-utilities`'s `Report`, `curios-pipeline`'s `CompileError`/`check_with_units`/`check_units_with_prelude`, and `curios-text`'s `Overlay` — and in `documentation/usage.md`'s "Asking about a program".

## Mission, unchanged

`curios wonder` lets an agent and an editor ask the compiler what it knows about a program, and get the same answer. One engine, one record per fact, two ways to reach it. Neither consumer is secondary — but they ask differently, and that difference is what organizes the rest of this file: **an agent addresses by text**, editing a `?` into the program and reading `diagnostics`, where **an editor addresses by cursor**. A query belongs to the surface whose addressing it needs.

## What landed, in one paragraph each

**The record is the compiler's own report.** Every stage's error now produces a `Report` — a message at a span, or at nothing — and its rendered text is that report rendered, so the located form and the printed form are one value; a goal batch is one report per `?`. `CompileError` carries the reports rather than the text. That is the whole of the engine's "facts are recorded where the compiler decides them": the engine reads what the compile path already said.

**The transports are two, in one module.** `ask` renders records to stdout and answers exit 0 once the question is answered, whatever the answer; `server` is the synchronous `lsp-server` loop on a protocol thread beside one analyst thread that owns the compiler, the editor's documents as the overlay, edits coalesced on the analyst, `textDocument/formatting` answered by the protocol thread from `curios format`'s machinery so a request never waits on a compile, and UTF-16 derived from each span's own text at the boundary. Both live under `curios::wonder` rather than in a crate of their own, because the crate's one argued consumer — a browser editor through `curios-js` — does not exist yet; the day it does is the day the engine splits out from under Binaryen and Wasmtime.

**A file is placed, not compiled alone.** `Membership::of` decides from the file's own location: the library whose directory holds it, the executable whose entry or stem tree it is, or standalone when no manifest above it claims it. `run` keeps its standalone reading; the divergence is deliberate and stated in `membership.rs`.

**A snapshot stops at the first failure**, as the compile path does, and a question never writes the store.

## The agent surface

Complete except for one query. An agent's two needs are *does this compile* and *what type goes here*, and the compiler already answers both.

- **`diagnostics`** is the loop: every error and goal, rendered as `run` reports it, exit 0 once answered.
- **`?`** is the type oracle, addressed by text. `let y: ? = e` reports `? = ` the type of `e`; a bare `?` reports the scope, the expected type, the obligations that hold it up and the candidate fits. Several `?` in one program report in one compile. This is not a query, and none is planned beside it: a `type`/`at` for the agent would answer by coordinate what `?` already answers by text, and a coordinate is the one thing an agent produces badly.
- **`witnesses [<concept>]`**, to add: one read of the witness table before the `Context` is discarded — per entry, the concept, the head key, the declaring name and its type. It is the one fact no source line holds. Everything else an agent needs is on disk — `/std` in the tree, dependencies materialized under `.curios/src/` — and grep answers it better than a query would, so nothing else joins this list.

## The editor surface

`hover` and `documentSymbol`, once two things exist: declaration label spans on `TopLet`, `TopInduct`, `TopStruct`, `TopConcept`, their cases and fields, following `TopMod.span`'s exclusion from `PartialEq`; and an anchor under a cursor, at a written goal and at a declaration name. `hover` is then the goal record read by position rather than by text — the same engine, a second address — and a position inside neither yields nothing rather than a guess.

## Hygiene, when a caller exists

- **`--json`.** A projection of records already computed, asserted for shape beside each rendering. Built for the first harness that consumes it, not before: an agent reads the rendering.
- **Snapshot identity and `--depth`.** A snapshot digested from the compiler's identity, the scope's unit addresses, the depth and every source's path and content, named by every answer. `--depth` is how far down the pipeline a query runs, beside `--budget`, `--unit` and `--manifest`; it may ask for more than a query needs, never less. Today `diagnostics` always runs to `certified` and `stage` to whatever rung was named, and the measurement that would justify a shallower default — a front-end snapshot against a compile, in the ignored test that carries the figure and the command that retakes it — is not written. The server, re-elaborating per edit, is where the figure pays.
- **`modules`**, the module tree from `RootSource`'s reads. `ls` answers it today.
- **The `wasm-optm` rung in a unit-less snapshot** is the transport's, not the engine's, and stays so.

## Not in any near landing

- **Per-item recovery** in parse, lowering and elaboration — the only way a broken file answers more than the failure that stopped it, and the reason an agent's loop is one error per compile. A substantial change to three loops on the shared compile path.
- **Occurrence ids and per-occurrence types**, and with them `references`, `definition` at an arbitrary cursor, semantic tokens, and rename.
- **`scope`/completion**, which waits on recovery specifically.
- **The binder table stored with a unit**, so `definition` reaches into a dependency.
- **`search`/`workspaceSymbol`**, and the rule a set-scoped answer needs when the server has invalidated one member of the set.
