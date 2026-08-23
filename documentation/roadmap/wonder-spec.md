# A question about a program is answered by the compilation that would build it

## Status

Specification only; nothing is started. Supersedes the retired `wonder` draft (`git show 275a6153^:documentation/program_analysis/02_WONDER_SPEC.md`), keeping two of its findings — source and semantic data are tagged apart, and identities are bound to a snapshot — and reversing its two largest non-goals: parser recovery and a persistent transport are in scope, because they are what the headline objective turns on.

Every decision below was read from the code it names, and two were probed with the release binary of 2026-08-23; the probes are described under [Figures](#figures) and the figures themselves are owed to the measurement test of milestone 3. What remains argued rather than probed is listed under [The questions to answer](#the-questions-to-answer).

## Why it exists

Two consumers want to ask the compiler what it knows about a program, and neither can today. An agent editing Curios reconstructs scope, resolution and types from prose and `--print` dumps; an editor has no integration at all. Both want the same facts — what is declared where, what a name resolves to, what type elaboration assigned, which witnesses exist, what is wrong — and both want them about *the program as it currently is*, which is usually broken.

`curios wonder` is one engine under two transports: a one-shot query anchored to a target, and a language server anchored to a workspace. They are one subcommand so that the server is built *out of* the query engine rather than beside it; a feature with no query twin does not exist.

## What Curios already has

The engine is mostly a recording of work the compiler already does.

- **Every surface term and name carries a byte-range `Span` into a shared `Rc<Source>`**, and lowering stamps it onto the core term it constructs (`curios-utilities/src/span.rs`, `curios-text/src/into_core/lowerer.rs:492`). Spans survive elaboration, zonking and canonicalization because they sit on the wrapper, not the shared node (`curios-core/src/scope.rs:1141`), and they *archive*: `Term.span` is not skipped (`curios-core/src/term.rs:43`) and a `Span` carries its `Source` text, so every stored unit and the prelude image already hold the full source of what they were compiled from.
- **Diagnostics are located.** Text and elaboration errors wrap in `Located` with innermost-span rendering; written goals batch with span, scope and type (`curios-elab/src/error.rs:355`).
- **The front end stops cleanly after elaboration.** `typecheck_reporting` lowers and elaborates against a `Prefix` and returns the core module without touching Ersd or below (`curios-pipeline/src/compile.rs:93`).
- **Sources can be supplied instead of read.** `RootSource::supplied`/`insert_module` is how `curios-js` compiles with no filesystem (`curios-text/src/root_source.rs:83`).
- **Scope is decided, never probed.** `Governing`, `Target` and `order()` say which units a target is compiled against, and the store serves dependencies already built (`curios-package/src/govern.rs`, `run.rs`, `graph.rs`).
- **Locals have identity.** A binder and its occurrences share a `Free::Local(Mint)` (`curios-core/src/names.rs:139`); globals are `Global::Authored(Qualifier)` or a witness ordinal.
- **Each item is already bracketed.** `elaborate_module_item` sets the island, restores the budget and drains parked work per item (`curios-elab/src/elaborate/module.rs:905`); `solution_mark`/`rollback_solutions` unwind both unification stores and the reduction cache (`curios-elab/src/context.rs:1174`); `Context::assume` binds a name at a type with no body (`context.rs:537`); `take_parked` and `take_deferred_witnesses` discard without reporting. Declarations register before the item loop, so no item can leave one half-registered.
- **The kernel reports per item.** `recheck_module_verdicts` returns one `Verdict` per refused item and walks on past a refusal (`curios-cert/src/recheck.rs:104`).
- **Per-item import scope and a candidate pool exist.** `Imports` records what each definition could see (`curios-core/src/names.rs:290`); `suggest.rs` computes type-directed fits.
- **The prelude is restored once per thread**, and the formatter's parse product carries item spans and comments (`curios-prelude-archive/src/restore.rs:79`, `curios-text/src/module.rs:198`).

What it does not have: a definition-site span on any declaration or binder, a record of which occurrence resolved to what, a record of what type a subterm was given, any parse past the first syntax error, or any elaboration past the first failing item.

## Decisions

### One engine, two transports, and the transport is a flag

**Decided.** `curios wonder <QUERY> [ARGS] [TARGET]` answers one question about one target. The query comes first and is a closed vocabulary, each a subcommand with fixed arity; the target comes last and is optional, dispatched by `run`'s lexical law unchanged — absent, an identifier naming a declared executable, a `.crs` or path-shaped file, or `-` for standard input. `curios wonder --server` is the other transport: the Language Server Protocol on standard I/O for a workspace. The flag conflicts with every query; server-only flags require it; `--json` belongs to the query form and conflicts with `--server`, whose wire format is the protocol's and not a choice; an invocation naming neither a query nor the flag is refused; the flag sets never mix, and `usage.md` says so.

**Rationale.** Arity decides what follows a query, so the target slot needs no knowledge of the query vocabulary and `run`'s law is inherited rather than adapted: any executable may be named `symbols` or `stage`, and `curios wonder symbols symbols` asks the first of the second. One binary is the same argument the unit cache and the wasmtime pin already make — the server's answers come from the compiler that will compile the file, by construction. The server is a flag rather than a verb because every row of the query table has a record, a rendering, a `--json` and a fixture, and the transport has none of those; spelling it as a row would invite the question of what `serve --json` means.

**Rejected.** A separate `curios-lsp` binary — version drift between the server and the compiler, the class of defect the toolchain elsewhere makes unrepresentable. Target first — with an optional target, `wonder symbols` is ambiguous between an executable and a query, and requiring the target resolves it by removing a form of `run`'s law, which is the wrong thing to pay. A `serve` verb — parses, once the query is first, but is not a question.

### The zero-argument target means the governing package

**Decided.** An absent target means the governing package — its library and every executable it declares, each a unit in the snapshot — and not, as for `run` and `compile`, its default executable. The dispatch is `run`'s word for word; only the meaning of the absent form differs.

**Rationale.** `run` must pick one thing to run; `wonder` can ask about everything. A library-only package has no executable to default to, and it is exactly the package an editor lives in. Stated as a decision so the next reader does not "fix" it back to `run`'s reading.

**Rejected.** A fifth target form naming a library — breaks the law that makes the slot orthogonal. Defaulting to the default executable — unanswerable in a library-only package.

### The engine is a crate below the products, and it takes a scope

**Decided.** A new crate, `curios-wonder`, owns the analysis: it depends on `curios-pipeline`, `curios-text`, `curios-elab` and `curios-core`, and on nothing that reads a filesystem, speaks JSON, or names an LSP type. Like the pipeline it is handed a `Prefix` and an overlay of sources; deciding the scope stays the product's job. `curios` combines it with `curios-package` and the two transports, and is the workspace's only `lsp-server`, `lsp-types` and `serde_json` dependency.

**Rationale.** `curios-pipeline`'s law — the fold takes whatever scope it is given and cannot tell which unit is `/std` — is what lets `curios-js` exist. The same law here is what lets a browser editor ask for hover. The protocol crates are one external concern with one owner, and that owner is the product that has a terminal. `curios-analysis` is already the name of the shared checker rules, which is why the crate is named for the subcommand.

**Rejected.** Putting the engine in `curios` — unreachable from the browser and from filesystem-free tests. Putting the transport in the engine — an engine that names `lsp_types::Position` has chosen UTF-16 for every consumer.

### Sources are supplied, never assumed

**Decided.** The engine's input is a snapshot request: the scope, and an overlay `path → text` consulted before the disk for every module read. The one-shot transport builds the overlay from the target or from standard input; the server builds it from the documents the editor has open. The engine never opens a file the overlay does not route.

**Rationale.** An unsaved buffer is the normal case in an editor and the only case for standard input; one door for both means the one-shot mode is tested on exactly the path the server uses.

**Rejected.** Letting the engine read disk directly and the server patch around it — two read paths, one of which is only exercised interactively.

### Local units compile fresh; dependencies come from the store; a query never writes it

**Decided.** A snapshot compiles the units the user is editing — the package's own library and executables, and an umbrella's members — recording the tables below as it goes, and takes only *dependencies* from the store, already built. Nothing a query compiles is filed: the engine reads the store and never writes it, and `--print`'s rule that a stage dump "skips the get but still puts" (`curios/src/pipeline.rs:92`, `curios/tests/payload.rs:133`) dies with the flag. A dependency not in the store is a diagnostic on the manifest, never a fetch; `curate` remains the only network actor.

**Rationale.** The store addresses a unit by content, so a server that filed what it compiled would file a unit per keystroke. Local units are what the tables are about, and a front-end snapshot of one costs an order of magnitude less than a compile (see [Figures](#figures)).

**Rejected.** Serving local units from the store — they carry no tables, and they are the thing being edited. Filing what a query built — store growth proportional to typing.

### One snapshot, one identity, and every answer carries it

**Decided.** A snapshot is one compilation of one target against one scope over one overlay. Its identity is a digest of the compiler's identity, the scope's unit addresses, and every source's path and content. Every answer names the snapshot it came from; every snapshot-local identifier — an item, a witness, a reference — is valid only under that identity. A selector by absolute path (`binding:/std/Lst/map`, `module:/std/Lst`) is the only identity that crosses snapshots.

**Rationale.** A coordinate is meaningless without the text it indexes; an editor edits between a request and its reply. The store already addresses a unit by its mounts and its compiler, so the digest is the same vocabulary one level up.

**Rejected.** Stable declaration identity across arbitrary edits — a hard problem with no consumer here. Answering without identity — the retired draft's reason for recording this.

### Bytes are authoritative; the adapter owns UTF-16

**Decided.** A location is a source identity plus a half-open UTF-8 byte range, with 1-based line and scalar-counted column derived beside it, exactly as `Span::line_column` does. LSP's UTF-16 line/character exists only in `curios`'s server adapter, converted at the boundary in both directions. On the command line a location is spelled `path:line:column` — the diagnostic header's own form, with `STDIN_LABEL` (`curios/src/pipeline.rs:164`) as the path of the `-` form — so an answer's coordinates paste back as a question.

**Rationale.** The coordinate the compiler has is the byte; every other coordinate is a rendering. A consumer that edits verifies the source digest, then edits by bytes.

### Source structure and semantic facts are tagged apart

**Decided.** A record that carries verbatim source carries it as bytes sliced from the snapshot's text and says so; a record that carries an elaborated rendering — a type, a witness key, a canonical name — says that instead. A symbol record may carry both, because the association is the point, and a consumer can never mistake a pretty-printed term for file content. Types are rendered twice: `display`, scoped and short, and `canonical`, absolute and collision-free. A structured type tree is not offered until a consumer needs one.

**Rationale.** Kept verbatim from the retired draft; it was right.

### The unit of failure is the item, never the file

**Decided.** The engine always answers. A source item that fails to parse is recorded as an item of kind `unparsed` extending to the next item keyword — `pub`, `let`, `rec`, `induct`, `struct`, `concept`, `satisfy`, `foreign`, `mod`, `use` — **at the start of a line**, and parsing resumes there. An item that fails to elaborate is handled inside a mark: its written type elaborates first, and a witness registers on that type as today. If the type fails, the item is recorded as failed at phase `resolved` and nothing is assumed, so its name stays unbound. If the body fails, solutions are rolled back to the mark, parked and deferred goals are discarded, the universe transaction is finished, the name is **assumed** at the elaborated type — `Context::assume`, with a universe context generalized over the type alone — and a witness keeps the table entry it registered on its signature. The item is recorded as failed at phase `elaborated`, and elaboration continues. A `rec` group fails as a group, every member assumed at its written type. A diagnostic in a later item whose cause is an unbound or opaque name belonging to a failed item is folded into that item's record rather than reported at the consumer. Every record states the phase its item reached — `parsed`, `resolved`, `elaborated`, `certified` — and nothing is fabricated past it.

**Rationale.** This is the decision the headline rests on. An editor buffer is broken most of the time; an agent's program is broken halfway through every edit. A query that answers nothing on a broken file answers nothing most of the time. Both recovery points exist for language reasons. The resync anchor is the formatter's: every top-level item sits at column zero and every nested binding is indented, and `let` is both an item and a local form, so "the next `let`" would land inside the broken item's own body and shred the rest of it; formatted code recovers exactly, unformatted code recovers at its next formatted item. The opaque item is a composition of existing pieces rather than a mechanism: a top-level `let` *requires* its type, so an assumption at the written type is exactly the view every other item already has of it; the witness table entry is created from the signature before the body by design, so that a recursive witness resolves through its own entry (`curios-elab/src/elaborate/module.rs:613`), and a body failure leaves it in the state an opaque witness wants — a key, a type, a name, no body, still refusing a later overlapping key. A type failure leaves only the `mark_witness_declaration` flag, which gates registration and nothing resolution reads. Folding consequent errors is what keeps one broken signature one red mark.

**Rejected.** Answering from the last snapshot that succeeded — it lies about the current text, and coordinates drift silently. Expression-level recovery — fights the parser's commitment discipline, which was chosen for error quality; an unparsed item is an honest record, a guessed expression is a fabrication. A fabricated body for a failed item — would have to erase and be judged. Phases per *file*, the retired draft's position — a file that reaches `parsed` and stops is the common case in an editor and answers none of the questions asked.

### `certified` is a phase in both transports, and it is unit-wide

**Decided.** The kernel judges a unit only when every item in it reached `elaborated`; a unit with any failed item stops there, for every item. In the server, diagnostics are published at `elaborated` and amended at `certified`, so the editor's latency is elaboration's and the kernel's verdicts arrive as a second batch only when it refuses something. In the one-shot mode both phases complete before the answer.

**Rationale.** The compile path runs the kernel unconditionally in production, so a server that skips it answers for a compilation that does not exist — and a kernel refusal is the one diagnostic the editor cannot otherwise show and `run` will certainly show. The kernel already reports per item and walks past a refusal. It cannot judge a unit with a hole in it, because an opaque name has no definition for its dependents' `Globals`, and the kernel is not given axioms. Its recorded cost is the same order as elaboration's (`curios/src/tests/unfolding.rs:350`).

**Rejected.** Skipping the kernel in the server. Handing the kernel an assumption for an opaque item — an axiom mechanism in the trusted base, to serve a diagnostic. Judging only the items that do not depend on a failed one — deciding dependence outside the kernel, a second analysis of the thing the kernel exists to decide.

### Facts are recorded where the compiler decides them, never re-derived

**Decided.** Three tables are produced by the stages that already know their contents, as data beside the module: lowering records every resolved occurrence (`Span → Free`, with the syntactic role) and every binder's span (`Free → Span`, for globals and locals alike); elaboration records the type assigned to every spanned term at `elaborate`'s return, zonked at the item boundary; the witness table is read from the `Context` before it is discarded. The engine walks these tables. It resolves no name and types no term. The binder table is stored with a unit, so that `definition` reaches into a dependency the first time a recording compiler builds it.

**Rationale.** The `Intrinsic::signature` principle: one source of truth that consumers walk, so that what the engine says and what the compiler did cannot disagree. The alternative — walking the zonked core module and matching spans — meets elaboration's inserted implicit arguments, witnesses and `/syn` operator methods, which carry borrowed spans or none, and every filter written to skip them is a second resolver in disguise. Storing the binder table costs little because the spans and their sources are already in every stored unit.

**Rejected.** A resolver in the engine — drifts. Reading types back through the goal mechanism (`let _ : ? = f;`) — a report, not a record, and one goal per question.

### The query vocabulary is closed; each LSP method has exactly one query twin

**Decided.** Queries are a fixed set, each a subcommand with fixed arity, and a new fact is a new row rather than an option on an old one:

| Query | Answers | LSP twin |
| --- | --- | --- |
| `diagnostics` | every diagnostic and goal, by item, with the phase each item reached | `publishDiagnostics` |
| `symbols` | the items and the symbols they introduce, with ranges | `documentSymbol`, `workspaceSymbol` |
| `at <loc>` | the occurrence or binder at a location, its symbol, its type, its definition | `hover` |
| `definition <loc>` | the binder span of what is at a location | `definition` |
| `references <selector>` | every occurrence of a symbol, by role | `references`, `documentHighlight` |
| `type <loc>` | the type of the spanned term at a location, display and canonical | `hover` (expression part) |
| `scope <loc>` | the names visible at a location — binders, items, imports, with their spellings there | `completion` |
| `witnesses [<concept>]` | the witness table, keyed as elaboration keys it | none yet |
| `dependencies <selector>` | what a definition's written and elaborated forms reference, tagged | none yet |
| `modules` | the module tree, each with origin and source | none yet |
| `stage <name>` | the program's representation at one pipeline stage, reprinted — `Stage::NAMES`'s vocabulary verbatim | none |

Selectors carry their namespace prefix (`binding:`, `module:`). Formatting is not a query: the server's `formatting` is `curios format`'s machinery over the overlay, and exists because it is free. Semantic tokens are `references` over every symbol, projected by kind. `references` answers within the target's snapshot in the one-shot mode and across every unit the server holds in the other, because the occurrence table is per unit. `stage` is the one query that runs past elaboration, through the pipeline's observer, and costs a compile rather than a snapshot; `wasm-optm` exists only in `curios`, as today, because only the native product has Binaryen.

**Rationale.** The table is the contract and the test surface: a query is a fixture the corpus asserts twice — its rendering, as a reader would see it, and its `--json`, for shape — and the LSP method is an adapter over a record that already has both. A feature with no row is not implemented; a row with no fixture is not done.

**Rejected.** Exposing the LSP's own request vocabulary as the query language — UTF-16 coordinates and protocol-shaped records as the compiler's public face.

### `--print` is deleted

**Decided.** The global `--print` flag goes; `stage` is its replacement, its stage vocabulary and the `usage.md` table moving under `wonder`. The pipeline's observer and `Stage::NAMES` are untouched.

**Rationale.** The global flags split two ways: `--budget`, `--unit` and `--manifest` *parameterize* a compilation, and make sense on anything that compiles — `wonder` wants all three; `--print` *observes* one, which is a question, and the toolchain already answers questions-about-a-compilation with a subcommand (`profile`). Living at the root is what forces it to be the one flag that needs `require_equals` (`curios/src/cli.rs:118`). What is lost is composition — `run` no longer dumps while it runs — and it costs nothing, because the representations are a deterministic function of the sources and the budget, and asking twice answers the same.

**Rejected.** A subcommand-local `--print` on `run` and `compile` — fixes the habitat and keeps a second way to ask.

### The record is the answer; the terminal reads its rendering, and `--json` reads the record

**Decided.** Every query produces a record — `curios-wonder`'s own Rust types, plain data, deliberately distinct from `curios_core`'s and `curios_elab`'s internal enums so that a compiler refactor is never a protocol change by accident. A default invocation renders the record for a reader; `--json` emits the record itself; the server adapts it to `lsp-types`. All three are projections of the one record, and the rendering is computed *from the record* — never from the compiler beside it — so the three cannot disagree about a fact.

The rendering reuses what the compiler already renders with: a location is `Span::render_snippet`'s `--> path:line:column` header and caret, a type is laid out by `curios-print`'s width-aware algebra, and a diagnostic's message is the text `Error::format` produces on the compile path — so `wonder diagnostics` on a program reads as `curios run` would have reported it, with the per-item phase lines beside it. The record carries that message as text and the structured payload — the inferred and expected types, the witness key, the goal's scope — as data alongside, so `--json` loses nothing the rendering shows and the rendering invents nothing the record lacks.

Exit code 0 means the question was answered, including when the answer is a list of errors; a non-zero exit means the question could not be asked — no such target, no such query, a scope that cannot be assembled. Which projection was asked for does not change the code.

**Rationale.** Two kinds of consumer, and they announce themselves. A person at a terminal, or an agent reading one, reads a snippet with a caret better than a byte range in braces — the goal report already settled that for the compile path, and the default invocation is that consumer's. A script or an editor wants structure, and says so with a flag. Deciding by flag rather than by terminal is the `run` dispatch law: one command line means one thing, in a shell and in a pipeline alike. Rendering from the record is what keeps the rendering honest — it can only show what the JSON consumer also gets. A diagnostic is an answer, not a failure of the question; the goal report exits 2 on the compile path because there the question was "can this run," and here it is not.

**Rejected.** Probing for a terminal to choose the projection — a pipeline would see different output from a shell. JSON only — reversed, because the default invocation's consumer is the one who types it, and a question asked by hand wants an answer written for eyes. Rendering by a second walk over the compiler's own structures — two renderers for one fact. Re-exporting the compiler's types as the record — couples every internal rename to every consumer.

### The server is one analysis thread over an overlay

**Decided.** The transport is the synchronous `lsp-server` crate: the main thread reads the protocol, one analysis thread owns the compiler, and requests cross on a channel. The server's documents are the overlay; an edit schedules a new snapshot for the unit that document belongs to, coalesced while edits continue. A document belongs to the unit that declares it — the library whose `lib.crs` enumerates it, the executable beside the manifest that it is, or no unit at all, in which case it is standalone — decided from the manifest exactly as `run` decides it.

**Rationale.** The compiler is single-threaded by construction — `Rc` spans, a thread-local prelude, a `RootSource` that is deliberately `!Send` — so a multi-threaded transport would only add a channel with extra steps. The unit-membership rule is the package layout's own rule; the server adds no discovery.

**Rejected.** An async transport — nothing to await. Per-document analysis — a module is not a unit, and a module alone has no scope.

## What this changes in the compiler

Listed so the footprint is visible before the work is: every item is additive data or a recorded table, except the two recovery points and one deletion.

- Spans on declaration labels and binders in the surface tree — `TopLet`, `TopInduct`, `TopStruct`, `TopConcept`, cases, fields, `Pattern::Binder`, `FuncTypeParam`, `FuncParam` — following `TopMod.span`'s pattern of exclusion from `PartialEq`.
- The three recorded tables, produced by `into_core` and `elaborate` and handed out beside the lowered and elaborated module, the binder table stored with the unit; `Error::innermost_span` made readable.
- Item-level parse resync in the top-level loop, anchored at column zero, producing an `unparsed` item.
- The per-item mark in `elaborate_module_suffix`'s loop: rollback, discard, `assume`, continue — composed from `solution_mark`, `take_parked`, `take_deferred_witnesses`, `finish_universe_transaction` and `assume`, all existing.
- `--print` removed: `curios/src/cli.rs`, `curios/src/pipeline.rs`, the payload rule and its test at `curios/tests/payload.rs:133`, the `usage.md` global-flags table, and the two doc comments that cite the flag (`curios/src/tests/codegen/ladder.rs:153`, `curios/src/tests/perimeter.rs:772`).

What it does not change: `compile_entrypoint`'s contract, the kernel, anything below Ersd.

## Figures

None recorded here, by the rule that a figure lives beside the probe that reproduces it. Two things were probed on 2026-08-23 with the release binary and decided what is decided above; the measurement test of milestone 3 takes their shape and carries their numbers.

- **A front-end snapshot costs an order of magnitude less than a compile.** Method: prepend `let probe : {} = ?;` to a corpus program, so `run` elaborates every item and exits at the goal report before erasure; time it cold against the same program's full compile with a cold payload. On three programs from `programs/` the snapshot was a small fraction of the compile, the one-time prelude restore being most of it. Whole-unit re-elaboration per edit is therefore the starting position; incrementality inside a unit is bought when a figure says it must be.
- **The kernel's cost is elaboration's order.** Read from `curios/src/tests/unfolding.rs:350–386` and `curios-prelude-archive/src/restore.rs`'s `stored_prelude_measurements`, not re-taken: single-digit to low double-digit milliseconds per dozen definitions on the proof ladders, and seconds over the prelude's eleven hundred.

## The questions to answer

1. **What does `scope` offer beyond names?** Name-based completion from binders, items and `Imports` is the depth-one answer; type-directed completion is `suggest.rs`'s pool, and [goal suggestions are depth-one fits, not proof search](../design/toolchain/goal-suggestions-are-depth-one-fits-not-proof-search.md) bounds how far that may go.
2. **Where does the serialization derive live?** The records deriving `Serialize` puts `serde` in the engine; `curios-package` already carries it for TOML. The alternative — hand-written projections in `curios` — restates derivables, which the workspace refuses elsewhere.
3. **What does resync do at end of file with an unterminated `end`?** The anchor rule decides the common case; this one is fixed by fixture.
4. **A struct type-former whose body fails.** `elaborate_struct` rebuilds the registry telescopes after the body, so the struct's literals fail downstream as consequent errors; whether the fold rule above attributes them cleanly is a fixture for milestone 2.

## Milestones

Each lands with its fixtures in `curios/src/tests/`, the roadmap entry updated, and this file corrected where the code disagreed with it.

1. **Engine and one-shot transport over a clean program.** The crate, the snapshot request, the three tables, the label spans, `diagnostics`/`symbols`/`at`/`definition`/`type`/`modules`/`stage`, the rendering and `--json` with a fixture of each, `--print` retired, the `usage.md` section. A broken file yields a snapshot that stops at its first failure — honest, and not yet useful.
2. **Item-level recovery.** Column-zero resync and the per-item mark; `diagnostics` reports per item with phases; every milestone-1 query answers on a broken file. Opens with the fixture that settles the witness case: a witness whose body fails, a consumer that resolves it, and a second witness with the same key — expecting the consumer to type-check against the opaque witness and the duplicate to be refused.
3. **`--server`.** The transport, the overlay, unit membership, coalescing; diagnostics published at `elaborated` and amended at `certified`, hover, definition, document symbols, formatting; the measurement test.
4. **The rest of the table.** `references`, `scope`, `witnesses`, `dependencies`; semantic tokens and completion as their adapters.
