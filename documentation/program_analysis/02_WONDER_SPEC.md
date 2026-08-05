# `wonder` — Program Analysis Interface

This document specifies `curios wonder`, a read-only interface through which an LLM, script, or future editor integration can ask the Curios compiler for structured facts about a `.crs` program. The command name is intentionally whimsical; the underlying analysis model and response schema are transport-neutral, so the CLI may be renamed without redesigning the compiler-facing API.

The goal is not to replace text search or file reads. It is to expose facts the compiler uniquely knows, return the exact source associated with those facts when useful, and keep every answer tied to one coherent compilation snapshot.

Batched written-goal reports (`?`, located by source position) are a landed checked-front-end capability. `wonder` reports them through the ordinary diagnostic model while leaving their compiler semantics to that front end.

## Objective

An agent working on a Curios program should be able to determine, without reconstructing compiler state from prose or pretty-printed IR:

- Which files and embedded modules form the program.
- Which source declarations exist, where their exact bytes begin and end, and which symbols they introduce.
- Which declaration a name occurrence resolves to.
- What type elaboration assigned to a symbol.
- Which dependencies were written explicitly and which were inserted by lowering or elaboration.
- Which witnesses are registered, under which concept and rigid-head key, and where they came from.
- Which diagnostics and written goals prevent the program from becoming a checked compilation unit.

The interface is initially read-only. It never edits source, formats files, applies fixes, writes caches, or executes the program.

## Design principles

**Every answer is program-anchored.** Visibility, module resolution, qualified names, foreign declarations, elaboration, and the witness table are properties of a compilation. Every invocation therefore names an entrypoint; there is no implicit project selected from the current directory.

**One invocation observes one snapshot.** All source ranges, symbol identities, references, types, and diagnostics in a response come from the same set of source bytes and compiler configuration. Responses carry an analysis identity so a consumer can detect stale coordinates after an edit.

**Source structure and semantic symbols are different things.** A `use` declaration is a source item but declares no symbol. A mutual `rec` group is one source item but declares several symbols. An `induct` group declares types and constructors. Anonymous witnesses are registrations with source origins, not nameable source items. The public model preserves these distinctions instead of forcing everything into an item path.

**Source and semantic data may travel together, but are always tagged.** Exact source is returned as byte slices from retained input text. Elaborated renderings are explicitly marked semantic data. A symbol response may contain both because the association is valuable, but a consumer can never mistake a pretty-printed term for file content.

**JSON is normative.** The structured response is the contract. A human rendering is a convenience projection over the same query result, not a second source of semantics.

**Incomplete programs remain inspectable.** Parsing, resolution, lowering, and elaboration are separate availability levels. A type error does not erase source information that was already established. The response states the last completed phase and never fabricates data from a phase that did not finish.

**Public analysis types do not mirror internal Rust enums.** The schema is versioned independently of compiler implementation details. Internal refactors should not become breaking protocol changes by accident.

## Non-goals

The first implementation does not provide:

- Source editing or automated fixes.
- Formatting.
- Incremental parsing or general parser error recovery.
- A persistent daemon, language server, or MCP server.
- A stable identity for a declaration across arbitrary edits.
- A serialized form of the entire internal core term language.
- Runtime values, performance profiles, or execution traces.

The analysis library is deliberately reusable so persistent transports and the planned language server can be added without rebuilding the semantic index.

## Invocation and compilation identity

The CLI shape is:

```text
curios wonder <ENTRYPOINT> <QUERY> [QUERY OPTIONS]
```

`ENTRYPOINT` is parsed exactly like the input to `run` and `compile`. Its containing directory is the filesystem module root, its own module path is `/`, and the fixed `/sys`, `/syn`, and `/std` roots are attached through the ordinary prelude loader. Once Curios gains project manifests, a manifest may supply the resolver configuration, but the effective entrypoint and project identity must remain explicit in every response.

The Rust analysis API also accepts a caller-provided parsed entrypoint, root source, and logical input identity. This preserves filesystem-free embedders such as `curios-web` and focused tests; the `wonder` CLI always constructs that input from an explicit path.

An analysis identity is derived from:

- The compiler build identity and public analysis-schema version.
- The normalized entrypoint identity.
- Every loaded source's logical identity and content hash.
- Analysis-affecting configuration, including the reduction timeout.

Repeating a query with unchanged inputs produces the same analysis identity and deterministic snapshot-local IDs. Any source or relevant configuration change invalidates those IDs.

## Analysis model

### Sources

A source record describes one unit of input text. Its origin is one of:

- `file`: text read from a filesystem path.
- `embedded`: `std` or `syn` text embedded in the compiler, with a logical URI and module path.
- `generated`: declarations synthesized from compiler data, such as `/sys/Handle`; these have provenance but no verbatim source text.

Every textual source has a `source_id`, logical URI, content hash, byte length, and optional filesystem path. The source ID is the coordinate used by locations; paths are presentation and navigation metadata, not identity.

### Locations

A location contains a source ID and a half-open UTF-8 byte range:

```json
{
  "source": "source:entry",
  "range": { "start": 1042, "end": 1180 },
  "start": { "line": 37, "column": 5 }
}
```

Byte offsets are authoritative. Line and column are 1-based display derivations; columns count Unicode scalar values. Consumers performing edits use the byte range and verify the source hash before applying it.

Generated origins use an explicit generated-provenance object instead of a fictitious location.

### Modules

A module record has an absolute logical path such as `/`, `/std`, or `/std/Lst`, its visibility from its parent, its source origin when textual, and its source items in source order. Module identity is separate from the binding namespace because Curios permits a module and a binding to share a label.

### Source items

A source item is one contiguous top-level syntactic unit:

- `mod`
- `use`
- `let`
- a complete mutual `rec` group
- a complete mutual `induct` group
- `struct` or `record`
- `concept`
- `satisfy` witness declaration
- `foreign`

Each textual item has an opaque, deterministic snapshot-local `item_id`, its module, kind, visibility where applicable, full declaration range, and the symbols it introduces. When the syntax provides them, it also records exact ranges for contiguous leading comments, the declared name, written signature, body, and individual members of a mutual group.

The full declaration range begins at the first declaration token, including `pub`, and ends after the declaration's terminating token. Leading blank lines are excluded. A contiguous run of `--` comment lines immediately preceding the declaration with no intervening blank line is represented by a separate optional range; Curios does not otherwise assign those comments documentation semantics.

An item ID is not a language-level name. It can address anonymous witnesses and `use` declarations, and it remains valid only while the analysis identity remains unchanged.

### Semantic symbols

A semantic symbol is a named entity in the resolved program. Symbol kinds include modules, values, functions, type or concept constructors, data constructors, foreign bindings, and compiler-derived definitions such as concept method wrappers.

The canonical selector is a namespace plus an absolute path:

```json
{
  "namespace": "binding",
  "path": "/std/Lst/map"
}
```

The initial namespaces are `module` and `binding`, matching the language's actual namespace split. `kind` further describes the selected binding but is not part of lookup identity. CLI arguments use the compact spelling `module:/std/Lst` or `binding:/std/Lst/map`; JSON always uses the object form.

Every symbol records its origin item or generated provenance, visibility, declaring module, written signature range when one exists, and elaborated type when elaboration completed. A source item may introduce zero, one, or many symbols, while each source-declared symbol points back to exactly one origin item.

### References and dependencies

A reference is a source occurrence resolved to a semantic symbol. It records the occurrence location, selected symbol, and syntactic role. References are collected during name resolution before lowering erases surface structure.

Dependencies have two deliberately separate views:

- `source_dependencies`: resolved symbols explicitly referenced by the written declaration.
- `semantic_dependencies`: symbols referenced by the lowered or elaborated definition, including compiler-inserted operator methods, implicit machinery, constructors, and witnesses.

Both lists are deterministic and deduplicated, and each row states whether occurrence locations exist. Inserted semantic dependencies generally have provenance rather than a source occurrence.

### Witness registrations

A witness registration is an anonymous semantic record captured from the elaboration context before that context is discarded. It contains:

- The concept selector.
- The tuple of rigid input heads forming the witness key.
- The elaborated premise telescope.
- The elaborated full signature.
- The backing compiler definition.
- The declaring module and compilation root.
- Its source item or generated provenance.

Witness IDs are opaque and snapshot-local. They are not fabricated item paths.

### Types

A public type value initially has two textual forms:

```json
{
  "display": "Map(A)",
  "canonical": "/std/Map/Map(A)"
}
```

`display` is concise and scoped for reading. `canonical` uses fully qualified, collision-free global names and is the form used for composition. Neither field claims to be verbatim source. A structured type tree is deferred until a consumer demonstrates a need strong enough to justify stabilizing that larger schema.

### Diagnostics

Diagnostics are public records rather than serialized `curios_text::Error` or `curios_elab::Error` values. Every diagnostic has a stable code, kind, severity, compiler phase, message, optional primary location, and zero or more related locations. Variant-specific payloads carry structured facts such as inferred and expected types, witness keys, or written-goal scopes.

Written goals use diagnostic kind `goal`. A goal's identity is its source location — file, line, and column — the same coordinate every other diagnostic carries; no separate label exists.

## Analysis phases and status

An analysis records the last phase that completed:

1. `loaded`
2. `parsed`
3. `resolved`
4. `lowered`
5. `elaborated`
6. `zonked`

The overall program status is:

- `clean`: elaboration and zonking completed with no written goals or hard diagnostics; a checked program is available to the compilation pipeline.
- `incomplete`: elaboration completed and one or more written goals were collected before ordinary zonking; their scopes, expected types, and solutions are available, but no checked program is handed to erasure.
- `error`: a hard diagnostic prevented a clean or merely incomplete result.

Each query declares its minimum required phase. If that phase was not reached, the response contains no fabricated result and reports the phase-limiting diagnostic. For example, `item` can succeed after parsing while a symbol's elaborated type cannot be returned after a lowering error.

The initial parser and elaborator remain fail-fast for hard errors. The schema permits multiple diagnostics, but the first implementation does not claim general multi-error recovery. Written goals are the deliberate exception: the front end batches every reached goal today (as one error), and the typed incomplete outcome distinguishing `incomplete` from `error` is this specification's obligation to introduce.

## Query surface

### `overview`

Returns the analysis identity, entrypoint, status, phase reached, loaded sources, module summary, diagnostic counts, and aggregate counts for source items, symbols, references, and witnesses.

This is the first-contact query and the place an agent learns what further selectors are valid.

### `module <module-selector>`

Returns one module's metadata, child modules, source items in source order, and introduced symbols. `--public` filters the symbol view to the module's public interface but does not rewrite or reorder the underlying source-item list.

### `item <item-id>`

Returns the source item's structural metadata, introduced symbols, exact ranges, and exact declaration text when textual. `--with-leading-comments` expands the returned source slice to include the contiguous leading-comment range. Generated items return provenance and no `text` field.

### `symbol <symbol-selector>`

Returns the symbol's kind, visibility, declaring module, origin item, exact written signature and declaration text when available, elaborated type when available, and direct source and semantic dependencies.

This deliberately combines exact source with semantic facts in separately tagged fields; it is the primary high-value query for an agent inspecting a definition.

### `at <SOURCE>:<LINE>[:<COLUMN>]`

Resolves a source position to the narrowest known source item, name occurrence, reference target, and semantic symbol. The CLI parses line and column from the right so platform path prefixes remain valid. The JSON request form supplies source ID and position as separate fields.

Per-expression inferred types are deferred until the elaborator retains a source-to-elaborated-expression map. Their eventual addition extends this result rather than creating a separate coordinate system.

### `diagnostics`

Returns all diagnostics available from the completed phases. Goal diagnostics include their structured scope and expected type. A filter may select diagnostic kind, severity, phase, source, or owner symbol.

### `witnesses`

Returns witness registrations, filterable by concept selector, rigid head, declaring module, or compilation root. Results sort by concept selector, witness key, and declaring module.

### `references <symbol-selector>`

Returns source occurrences that resolve to the selected symbol, ordered by logical source and byte offset. Generated semantic uses are not source references and therefore do not appear here.

### `dependencies <symbol-selector>`

Returns explicit and semantic dependency sets separately. A reverse mode returns dependents. This query operates on resolved edges rather than textual spelling, so aliases and re-exports do not fragment the result.

### `snapshot`

Returns a bulk projection of the analysis indexes for consumers that prefer one large response to interactive queries. Root, module, phase, and record-kind filters keep the fixed prelude from dominating ordinary use. With no filters the snapshot contains the entry root only; including all fixed roots requires an explicit `--all-roots` request.

## Response contract

JSON is the default output of `wonder`. `--format human` selects the human projection.

In JSON format, every successful query request emits exactly one JSON object:

```json
{
  "schema": { "name": "curios.analysis.response", "version": 1 },
  "compiler_version": "...",
  "analysis": {
    "id": "...",
    "entrypoint": "app.crs",
    "status": "clean",
    "phase_reached": "zonked"
  },
  "request": { "operation": "symbol" },
  "result": {},
  "diagnostics": [],
  "request_errors": [],
  "page": null
}
```

The following laws apply:

- Breaking schema changes increment `schema.version`.
- Object key order has no semantic meaning, but the implementation serializes deterministically for stable tests and diffs.
- Arrays use source order unless the query documents another order.
- A missing phase-dependent value is represented by an explicit availability record, not by an ambiguous `null`.
- Optional semantic values for which `null` is meaningful, such as an unsolved goal's solution, use `null`.
- Large collections use `total`, `limit`, and `next_cursor`. Nothing is silently truncated.
- Cursors and opaque IDs are bound to `analysis.id` and rejected against a different snapshot.
- Exact source text appears only under a `source` field carrying its source ID, content hash, and byte range.
- Rendered compiler terms appear only under semantic fields such as `type`, `solution`, or `signature`.

A JSON-mode invocation that reaches command dispatch emits one envelope even when the request fails. In that case `analysis` and `result` may be unavailable, `request_errors` explains the invalid input, cursor, selector, I/O operation, serialization failure, or internal failure, and the process exits `1`. Argument errors rejected before dispatch use the CLI parser's ordinary stderr diagnostic.

The human projection may omit protocol metadata such as schema version and cursors, but it must preserve the substantive query result, diagnostics, ordering, and explicit truncation notices.

## Exit behavior

`wonder` distinguishes request execution from program validity:

- Exit `0`: the request was understood and a valid response was emitted, including when `analysis.status` is `incomplete` or `error`.
- Exit `1`: the request itself failed, for example because the entrypoint could not be opened, the query or selector was invalid, a cursor belonged to another snapshot, serialization failed, or the compiler encountered an internal error.

This keeps expected compiler diagnostics available to tool callers instead of turning them into transport failures.

## Architecture

The analysis substrate lives in a new pure `curios-analysis` crate rather than in the backend-owning `curios` CLI crate. It depends on `curios-abi`, `curios-base`, `curios-text`, and `curios-elab`, and owns the reusable load-through-zonk front end plus the public source and semantic indexes.

Its entry point accepts an `AnalysisInput` carrying a parsed entrypoint, `RootSource`, logical input identity, analysis configuration, and front-end observation callback. A path-based helper constructs that input for `wonder`; embedders can supply it directly without a filesystem.

`curios-pipeline` depends on `curios-analysis`. Its `compile_entrypoint` asks the analysis crate for a clean checked program and then continues through erasure, Ersd, continuations, and wasm. The fixed build-scoped prelude restoration path moves with the front end so checking and compilation use one implementation and one compiler-local artifact.

The front-end analysis entry point preserves the existing borrowed observation hooks for the text and lowered-core stages. `curios-pipeline::compile_entrypoint` forwards those observations into its `Stage` callback before continuing with the downstream stages, so moving ownership of the front end does not remove or retain whole IR dumps merely to support `--print`.

The central result is conceptually:

```text
AnalysisOutcome
  analysis identity and source hashes
  SourceIndex
  optional SemanticIndex
  diagnostics
  optional CheckedProgram
```

`CheckedProgram` is an internal Rust product containing the elaborated core module, entrypoint type, and harvested foreign store. It is available only for `clean` analyses and is not serialized as part of the public schema.

The query engine projects public response DTOs from `AnalysisOutcome`. The CLI's `wonder` subcommands are thin argument parsers over that engine. A future MCP server, language server, browser inspector, or NDJSON session reuses the same queries and DTOs rather than reaching into compiler internals.

## Required compiler substrate

### Resolved source graph

Module discovery and interface resolution must produce a reusable resolved-program value instead of remaining private temporary state inside `curios-text::into_core`. The value retains every loaded module, its logical path and root, source provenance, interface, and source order.

Lowering consumes this resolved graph, while analysis indexes it. Files are loaded and parsed once per analysis.

### Universal source origins

Every parsed top-level item needs one uniform origin record rather than ad hoc optional spans on selected payload variants. Mutual groups additionally retain member ranges. `let` and `rec` members retain exact written-signature and body ranges; named declarations retain name ranges; contiguous leading-comment attachment is recorded separately.

Synthetic declarations use generated provenance. Structural equality tests may continue ignoring source origins, but the analysis path treats origins as first-class data.

### Reference capture

Resolution records source occurrence to semantic-symbol edges before lowering replaces names with core variables or generated constructions. Lowering and elaboration separately record inserted semantic dependencies with provenance.

### Elaboration snapshot

Before the elaboration `Context` is discarded, analysis extracts witness registrations and written-goal reports into durable public-facing records. The elaborated core module alone is insufficient because it intentionally retains witness declaration names rather than the resolved witness table.

### Canonical rendering

The core printer gains or exposes a fully qualified rendering mode suitable for `type.canonical`. Existing concise diagnostic rendering supplies `type.display`. Both renderings operate on the same elaborated term.

## Performance and freshness

The first implementation is one-shot: each `wonder` process constructs one analysis and answers one query. High-value queries therefore return related facts together; `symbol` includes source, type, and direct dependencies instead of forcing three analyses.

The prelude remains cached per process, which benefits a bulk snapshot but not a sequence of separate CLI invocations. Once real usage shows repeated-query cost matters, a persistent `wonder serve` NDJSON mode or MCP transport may keep one immutable analysis in memory and answer many requests. The public query engine and snapshot-bound IDs make that an additive transport change.

No on-disk analysis cache is part of the first slice. Every response is tied to source hashes, so a future cache can be added without weakening freshness guarantees.

## Milestones

1. **Resolved source index.** Add the `curios-analysis` crate, explicit entrypoint-based analysis identity, reusable resolved source graph, universal item origins and ranges, source items versus symbol selectors, and the `overview`, `module`, `item`, and source-level `at` queries.
2. **Checked analysis.** Move the reusable load-through-zonk front end and prelude replay boundary into `curios-analysis`; add structured diagnostics, elaborated symbol types, witness snapshots, and the landed written-goal outcome; implement `symbol`, `diagnostics`, and `witnesses`.
3. **Semantic relationships.** Retain resolved reference edges and inserted dependency provenance; implement `references`, `dependencies`, canonical type rendering, and the filtered `snapshot` query.
4. **Persistent consumers.** Add a session transport when repeated-query measurements justify it, then build the planned language-server features over the same analysis and query APIs.

Each milestone must preserve the response envelope, analysis identity, phase availability, and source-versus-semantic distinction. Later milestones add facts; they do not reinterpret earlier ones.

## Retirement criteria

- Before this specification is deleted, the public command and response contracts are recorded in public CLI and analysis documentation, snapshot, phase, identity, diagnostic, and query invariants are recorded in `curios-analysis` and owning compiler module documentation and tests, remaining plans refer to the landed query API rather than this file, the roadmap subitem is a checked unlinked summary, and no reference to this filename remains.
