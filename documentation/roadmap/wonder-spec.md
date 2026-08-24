# A question about a program is answered by the compilation that would build it

## Status

Specification only; nothing is started.

## Mission

`curios wonder` lets an agent and an editor ask the compiler what it knows about a program, and get the same answer. One engine, one record per fact, two ways to reach it: a query addressed by name, and a language server addressed by cursor. Neither consumer is secondary — the CLI is how an LLM reads a Curios codebase, and the protocol is how an editor does.

The first landing serves three things: hovering a `?`, reading the error on one definition, and letting an agent query a codebase without reconstructing it from prose.

## What Curios already has

The engine is mostly a recording of work the compiler already does.

- Every surface term and name carries a byte-range `Span` into a shared `Rc<Source>` (`curios-utilities/src/span.rs`), and lowering stamps it onto the core term it constructs (`curios-text/src/into_core/lowerer.rs:492`). Spans survive elaboration because they sit on the wrapper rather than the shared node (`curios-core/src/term.rs:43`).
- Written goals already batch with their span, scope and type — `Error::Goals` carries one `GoalReport` per `?`, each with its own occurrence span (`curios-elab/src/error.rs:358`). A goal is minted with a `MetaOrigin::Goal` birth record, and synthesized implicits and witnesses carry `ImplicitOrigin`/`WitnessOrigin` naming the function and binder that induced them (`curios-core/src/term.rs:2169`).
- Errors attribute to an item: `Error::in_declaration` names the declaration every failure came from (`curios-elab/src/elaborate/module.rs:921`).
- The front end stops cleanly after elaboration — `typecheck_reporting` lowers and elaborates against a `Prefix` and returns the core module without touching Ersd or below (`curios-pipeline/src/compile.rs:93`).
- Sources can be supplied instead of read (`curios-text/src/root_source.rs:83`), which is how `curios-js` compiles with no filesystem.
- Scope is decided, never probed (`curios-package/src/govern.rs:53`, `run.rs:65`, `graph.rs:20`), and the store serves dependencies already built.
- The kernel reports per item and walks past a refusal (`curios-cert/src/recheck.rs:149`).
- The pipeline publishes its representations in order, pinned by `every_stage_is_observed_once_in_names_order` (`curios-pipeline/src/stage.rs:27`).
- The witness table sits on the `Context` until it is discarded, and the formatter's parse product already carries item spans and comments (`curios-text/src/module.rs:307`).

What it does not have: a span on any declaration label, a record of which occurrence resolved to what, a record of what type a subterm was given, or any parse or elaboration past the first failure.

## Shape

Three layers, and the middle one is the contract.

- **Engine** — a snapshot request (scope, source overlay, depth) becomes a snapshot.
- **Query** — a pure function from a snapshot and its arguments to a record.
- **Transport** — decides scope, owns snapshot lifetime, schedules work, projects records.

The membership test for the vocabulary follows from the middle layer: a query is a pure function of a snapshot, and state, scheduling and mutation are transport concerns that get no row. `publishDiagnostics` is `diagnostics` on the server's schedule; `didChange` mutates the overlay; `formatting` is `curios format`'s machinery over the overlay; `server` is a transport. None of them is a fact.

`curios-wonder` owns the engine and the queries. It depends on `curios-pipeline`, `curios-text`, `curios-elab` and `curios-core`, and names no filesystem, no JSON and no LSP type — an engine that spells `lsp_types::Position` has chosen UTF-16 for every consumer, including a browser editor. `curios` adds `curios-package`, both transports, and the workspace's only `lsp-server` and `lsp-types` rows. Records derive `Serialize` behind a default-off `serialize` feature that `curios` enables, following `curios-core`'s `archive` feature, so the engine's default build names no serde and a wasm bundle carries none.

## The snapshot

**Depth** is the pipeline's own ladder, `Stage::NAMES` with `certified` inserted after `core-elab`:

```text
text → core → core-elab → certified → ersd → ersd-optm → cont → cont-optm → wasm → wasm-optm
```

Each rung is a whole-unit representation. Every query declares the depth it needs, and the root flag `--depth` may ask for more, never less — a depth parameterizes a compilation, which is why it sits beside `--budget`, `--unit` and `--manifest` rather than becoming an option on a row. `wasm-optm` is constructed by the native product rather than the driver, so it is a rung `curios` can reach and `curios-js` cannot.

**The item is the unit of record.** A snapshot is a list of item records: span, rung reached, symbols introduced, goals written inside, diagnostics. `diagnostics` is then a fold over records the snapshot already holds rather than a pass of its own, which matters because the server runs it on every rebuild.

**A snapshot stops at the first failure.** The front end fails fast, as it does today. Per-item recovery — parse resync, per-item lowering, an assumed opaque item at the elaboration mark — is what would let a snapshot answer about a broken file, and it is a substantial change to three loops on the shared compile path. The consequence, stated once: a file with any failure answers only `diagnostics`, and `diagnostics` reports one failure.

**Anchors** are the positions a location can resolve to: a declaration label, a written goal, an item. Hovering a goal yields its type and scope, which elaboration already batches. Hovering a declaration's name yields that definition's elaborated type. A position inside neither yields nothing rather than a guess. Resolving an arbitrary occurrence — a name in the middle of a body, an inserted implicit — needs ids minted in lowering and a type recorded against each, and that machinery is not here.

**Sources are supplied, never assumed.** An overlay `path → text` is consulted before the disk for every module read, and the engine never opens a file the overlay does not route. The one-shot transport builds it from the target or from standard input; the server builds it from the documents the editor has open. One door for both means the query mode is tested on exactly the path the server uses.

**Local units compile fresh; dependencies come from the store; a query never writes it.** The store addresses a unit by content, so a server that filed what it compiled would file a unit per keystroke. A dependency missing from the store is a diagnostic on the manifest, never a fetch; `curate` remains the only network actor.

**Identity.** A snapshot is digested from the compiler's identity, the scope's unit addresses, the depth, and every source's path and content. Every answer names it, and every snapshot-local identifier is valid only under it. A selector by absolute path is the only identity that crosses snapshots.

**Coordinates.** Bytes are authoritative: a source identity plus a half-open UTF-8 range, with 1-based line and scalar-counted column derived beside it as `Span::line_column` does (`curios-utilities/src/span.rs:66`). LSP's UTF-16 exists only in `curios`'s server adapter, converted at the boundary in both directions. On the command line a location is `path:line:column` — the diagnostic header's own form — so an answer's coordinates paste back as a question.

**Facts are recorded where the compiler decides them, never re-derived.** The engine reads goals from the batch elaboration already builds, types from the elaborated `Definition`, witnesses from the `Context` before it is discarded, provenance from the `MetaOrigin` birth records, and the module tree from `RootSource`'s reads. It resolves no name and types no term, on the `Intrinsic::signature` principle: one source of truth that consumers walk, so what the engine says and what the compiler did cannot disagree.

## Command surface

`curios wonder <QUERY> [ARGS] [TARGET]`. The query is first and drawn from a closed vocabulary of fixed arity; the target is last and optional.

**Dispatch is lexical; scope is resolved.** The target's form comes from its shape, never from probing: `-` is standard input, a path is a file, an identifier is a declared executable of the governing package, and absent is the governing package entire — its library and every executable it declares, each a unit in the snapshot. What a form is compiled *against* is then resolved: a file declared by a package's library or executables is analysed as part of that unit, a file no unit declares is analysed standalone, and standard input is standalone by construction. A file's project is decided from the file's own location rather than the working directory; `--manifest` overrides.

This diverges from `run`, deliberately. `run` conflates form and scope because for it they are one decision, and its standalone reading of a file argument is what makes project scope reachable only through a declared artifact. `wonder` executes nothing, so no capability is escalated by supplying context; what is at stake is only whether the answer is true, and a library module analysed without its library reports every import unresolved. The ambiguity `run`'s law guards against is answered differently here: every answer names its snapshot identity, which carries the scope's unit addresses, so an answer states the context it used.

**A reference is a location or a selector**, decided lexically: `path:line:column`, or a selector carrying its namespace prefix — `binding:/std/Lst/map`, `module:/std/Lst`. A location resolves to an anchor; a selector resolves to a declaration. An editor has a cursor and no name, an agent has a name and no cursor, and both reach one internal reference before any query runs. A bare path without its prefix is not accepted, since deciding by probing is what this CLI refuses everywhere else.

| Query | Answers | Depth | LSP twin |
| --- | --- | --- | --- |
| `diagnostics` | every diagnostic and goal, by item | `certified` | `publishDiagnostics` |
| `symbols` | a target's items and the symbols they introduce, with ranges | `core-elab` | `documentSymbol` |
| `at <ref>` | the anchor's record — symbol, type, goal scope, origin | `core-elab` | `hover` |
| `type <ref>` | the type alone, display and canonical | `core-elab` | — |
| `witnesses [<concept>]` | the witness table, keyed as elaboration keys it | `core-elab` | — |
| `modules` | the module tree, each with origin and source | `core` | — |
| `stage <name>` | the program's representation at one rung, reprinted | the named rung | — |
| `server` | *(transport — no record, no rendering, no `--json`, no fixture)* | — | *(all of the above)* |

`at` and `type` are two projections of one record, which is why a narrower fact is a narrower row rather than an option on a wider one. The empty twin column is not a gap: LSP has no way to ask which witnesses satisfy a concept, and that row exists for the consumer that can.

`server` occupies the query position rather than being a `--server` flag. As a flag it would have to conflict with every query, with `--json`, and with the bare invocation, while server-only flags required it — four conflict rules is the CLI saying these are two commands wearing one name. `wonder` is the umbrella for codebase knowledge, LSP included, so the transport lives under it, and the exception is stated in the table where a reader meets it.

**`--print` is deleted.** `stage` replaces it, taking `Stage::NAMES`'s vocabulary and the `usage.md` table with it; the pipeline's observer is untouched. The global flags split two ways: `--budget`, `--unit`, `--manifest` and now `--depth` parameterize a compilation, while `--print` observed one, which is a question — and questions about a program belong under `wonder`. What is lost is composition, and it costs nothing, because the representations are a deterministic function of the sources and the budget.

## The record

Every query produces a record: `curios-wonder`'s own plain data, deliberately distinct from `curios_core`'s and `curios_elab`'s internal enums so a compiler refactor is never a protocol change by accident. The default invocation renders it for a reader, `--json` emits it, and the server adapts it to `lsp-types`. All three are projections of the one record, and the rendering is computed from the record rather than from the compiler beside it, so the three cannot disagree about a fact.

The rendering reuses what the compiler already renders with: `Span::render_snippet`'s header and caret, `curios-print`'s width-aware algebra for a type, and the text `Error::format` produces on the compile path — so `wonder diagnostics` reads as `curios run` would have reported it. The record carries that message as text *and* the structured payload as data, so `--json` loses nothing the rendering shows and the rendering invents nothing the record lacks. A default invocation is the right shape for both consumers: an agent reads a caret snippet better than a byte range in braces.

Source structure and semantic facts are tagged apart. A record carrying verbatim source carries it as bytes sliced from the snapshot and says so; a record carrying an elaborated rendering says that instead; a symbol record may carry both, because the association is the point. Types render twice — `display`, scoped and short, and `canonical`, absolute and collision-free.

Exit 0 means the question was answered, including when the answer is a list of errors. Non-zero means the question could not be asked: no such target, no such query, a scope that cannot be assembled. Which projection was requested does not change the code.

## The server

The synchronous `lsp-server` crate: the main thread reads the protocol, one analysis thread owns the compiler, and requests cross on a channel. The compiler is single-threaded by construction — `Rc` spans, a thread-local prelude, a `RootSource` that is deliberately not `Send` — so a multi-threaded transport would add a channel with extra steps.

The server's documents are the overlay, and an edit schedules a new snapshot for the unit that document belongs to, coalesced while edits continue. Membership is the scope rule above, unchanged: the library whose index enumerates the document, the executable beside the manifest that it is, or no unit at all.

## What this changes in the compiler

- Spans on declaration labels in the surface tree — `TopLet`, `TopInduct`, `TopStruct`, `TopConcept`, and their cases and fields — following `TopMod.span`'s pattern of exclusion from `PartialEq`.
- `Error::innermost_span` and the goal batch made readable, and the `Context`'s witness table readable before it is discarded.
- A `curios-pipeline` entry point beside `typecheck_reporting` returning item records at a requested depth.
- `--print` removed: `curios/src/cli.rs:123`, `curios/src/pipeline.rs:92`, the payload rule and its test at `curios/tests/payload.rs:130`, the `usage.md` global-flags table, and the two doc comments that cite the flag (`curios/src/tests/codegen/ladder.rs:153`, `curios/src/tests/perimeter.rs:772`).

What it does not change: `compile_entrypoint`'s contract, the kernel, `Stage::NAMES`, anything below Ersd.

## Figures

None recorded here, by the rule that a figure lives beside the probe that reproduces it. Two things decided what is decided above, and the measurement test of the last milestone takes their shape.

- **A front-end snapshot costs an order of magnitude less than a compile.** Whole-unit re-elaboration per edit is therefore the starting position; incrementality inside a unit is bought when a figure says it must be.
- **The kernel's cost is elaboration's order**, read from `curios/src/tests/unfolding.rs`'s `scrutinee_refinement_measurements` and `curios-prelude-archive/src/restore.rs`'s `stored_prelude_measurements` rather than re-taken. Prelude items are answered by `globals` and not re-judged, so a user unit pays only for its own.

## Not in this landing

Stated so the boundary is deliberate rather than discovered.

- **Per-item recovery** in parse, lowering and elaboration, and with it any answer on a broken file beyond the first diagnostic.
- **Occurrence ids and per-occurrence types**, and with them `references`, `definition` at an arbitrary cursor, semantic tokens, and rename.
- **`scope`/completion**, which waits on recovery specifically: completion is asked while typing, which is to say on a broken file, always.
- **The binder table stored with a unit**, so `definition` reaches into a dependency. Worth a probe before it is designed: stored units already carry `Term` spans, and a `Definition`'s `type_` is a spanned term.
- **`search`/`workspaceSymbol`**, and the rule a set-scoped answer needs when the server has invalidated one member of the set.

## Milestones

Each lands with its fixtures in `curios/src/tests/`, the roadmap entry updated, and this file corrected where the code disagreed with it. A query is a fixture asserted twice — its rendering, as a reader would see it, and its `--json`, for shape.

1. **Crate, records, snapshot, scope resolution.** `diagnostics`, `modules`, `stage`; the one-shot transport, the rendering, `--json`, and the overlay exercised by `-`; `--print` retired; the `usage.md` section. Almost no compiler change: errors are already located and the module tree is already `RootSource`'s reads.
2. **Declaration label spans.** `symbols`, and `at`/`type` under selectors.
3. **Anchors under locations.** `at`/`type` at a written goal and at a declaration name.
4. **`witnesses`.** One read of the `Context`'s table before it is discarded.
5. **`wonder server`.** The transport, the overlay, membership, coalescing, diagnostics, hover, document symbols, formatting, and the measurement test.
