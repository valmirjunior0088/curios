# Questions file what they compile

Working specification for bringing `lint` and the `wonder` queries under [A command compiles only as far as its answer needs, and a project keeps what it compiled](../../design/tools/a-command-compiles-only-as-far-as-its-answer-needs-and-a-project-keeps-what-it-compiled.md): a question files, for a project, each unit it compiled whole from the text on disk, so the next command starts from the store rather than cold.

It is independent of every other spec. What a question compiles over a baseline stays unfiled, under [A stored unit is a baseline for an item-level recompile](../../design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md).

## What this builds on

- **The seam.** `Cache` (`curios-pipeline/src/compile.rs`) is `get`, `baseline` and `put(source, unit, followed)`. The fold asks `baseline` on a miss and compiles over what it answers (`compile_unit_over`), or whole where it answers `None`; `put` is handed the unit either way and is not told which.
- **The store's cache files.** `Verdicts`' `put` serializes the unit, writes its record and the unit as one file renamed into place (`replace`), and places it in the chain. It answers no baseline.
- **The engine's cache does not.** `ReadOnly` (`curios-wonder/src/diagnostics.rs`) believes a stored unit on a re-read through the overlay (`Verdicts::get_overlaid`), answers the nearest baseline — the session's (`Verdicts::kept`), then the slot's whatever its files now hold (`Verdicts::earlier`), then the scope's offer — and on `put` keeps the unit for the session and places it where something follows.
- **A record is what was read.** A unit's record lists every file its compilation read with a digest of the text parsed from it, taken at `RootSource::reads`, and a hit re-reads each (`unchanged`). A record of text the disk does not hold is a miss for every reader of the disk.
- **One compile.** A question's unit comes from the fold a build runs, `compile_unit`: lowered, elaborated, certified by the kernel and erased.
- **The contract.** Each command's `Contract` (`curios/src/contract.rs`) states how it reaches the store; `lint` and every `wonder` query read one and file nothing, which the engine holds whatever store it is handed.

## The gap

A question files nothing but the `compiler` memo. A dependency no build has filed is compiled in memory by every `lint` and every `wonder` invocation that reaches it, and again by every server session, so a question over a package nothing has built starts cold every time.

## Prior art

- **gopls** keeps what it derives from type-checking in "gopls' persistent, transactional, file-based key/value store", and names what that bought: "the fast restart, reduced memory consumption, and synergy across processes that were delivered by the v0.12 redesign" ([*Gopls: Implementation*](https://tip.golang.org/gopls/design/implementation)). Its release notes say the cache "is persisted across processes", so that "if you run two gopls instances, they work together synergistically" ([gopls v0.12](https://golang.org/s/gopls-v0.12)).
- **Lean's server builds what the open file imports through the build tool**: `lake setup-file` builds the workspace's modules among a file's imports for the server, so a dependency is filed as a build files it ([Lake](https://github.com/leanprover/lake/blob/master/README.md)).
- **rust-analyzer's diagnostics run the build tool in the build's own directory**, and contend with it: its `cargo check` holds the directory's lock, a `cargo build` beside it waits ("Blocking waiting for file lock on build directory"), and the remedy users reach for is a second target directory, which shares nothing ([users.rust-lang.org](https://users.rust-lang.org/t/neovim-vs-blocking-waiting-for-file-lock-on-build-directory/72188)).

Filing where a build files is what makes one command's work the next one's; what it must not cost is a lock two commands wait on, which a slot renamed into place does not take.

## Decisions

1. **What is filed.** A unit the question compiled whole whose record the disk confirms when it is filed: every file it read holds, on disk, the text it was read as. A unit compiled over a baseline is kept and placed, as today, until the baseline's filing rule changes.
2. **The disk decides, not the overlay.** The test is the record against the disk, the one a later `get` makes. A unit that read an open document whose text differs is not filed, and neither is one whose file changed while it compiled; a document open and unchanged files as any other. Filing either of the first two would be sound, a miss for every later reader, and would replace a slot that held the disk's unit with one nothing can use.
3. **`put` is told how the unit was compiled.** The fold knows whether it compiled whole or over a baseline, and says so at the seam, as it says `followed`. The engine's cache does not remember what its own `baseline` answered.
4. **Where.** The store a build files into: beside the governing manifest, or `CURIOS_CACHE`'s shared half. A loose file and standard input open no store.
5. **How far.** The judged unit. A question files no payload: its crates link no runtime.
6. **A one-shot question takes a baseline as a session does.** Over a slot whose files have moved, `lint` compiles the closure and files nothing, each time it is run, until a build files the unit. Compiling it whole in order to file it would cost a whole compile of the standard library to anyone editing it.

## Stages

1. **The seam says how.** `Cache::put` carries whether the unit was compiled whole; the fold passes it; `Verdicts` ignores it and `ReadOnly` does not yet read it. No behaviour moves, and `curios-pipeline`'s tests pass unchanged.
2. **The engine files.** The engine's cache files a unit compiled whole whose record the disk confirms, through the store it holds, and keeps and places the rest as `ReadOnly` does; its name and documentation say what it now does.
3. **The contracts.** `lint` and the `wonder` queries state the store access a build has, and `curios/src/contract/tests.rs` pins it.
4. **The server.** It files as every command does: each unit it compiles whole from disk text, the dependencies a session starts cold on among them. A keystroke files nothing. After a save, a compile over the session's baseline stays unfiled, and a whole compile of the unit files.

## Verification

- **One unit, whoever files it.** Over one package, the slot a `wonder diagnostics` files and the slot a build files hold the same bytes, unit for unit. This is what lets a build believe a question's unit, and it fails if a question's fold ever stops short of a build's.
- Over a package no build has filed, `wonder diagnostics` run twice: the second compiles no dependency, which its profile shows as no elaboration span for one, and `lint` after it compiles nothing already filed.
- A build after a question hits every unit the question filed, and files a payload over that chain.
- The server: a dependency it compiles is filed when it is compiled; keystrokes in an open document file nothing and leave the slot's bytes as they were; a saved document's unit compiled whole is filed, and one compiled over the session's baseline is not.
- A file rewritten while its unit compiles leaves the slot as it was.
- A loose file and standard input leave the store as they found it, and a store that cannot be written costs the reuse and never the answer (`Verdicts::refused`).
- A unit compiled over a baseline is not filed.
- [`curios-wonder`'s latency protocol](../../../curios-wonder/README.md#measuring-a-questions-latency), retaken cold and warm, naming the stage; the first compile of a session now serializes each unit it files, and the figure says what that costs.

## Open

- **How often a one-shot question compiles over a baseline it cannot file.** Decision 6 leaves `lint` recompiling a closure on every run between builds. The latency protocol counts, per invocation, the units compiled whole and the units compiled over a baseline; if the second dominates a workflow, the answer is the baseline's own filing rule, and its differential gate, not a rule here.

## Rejected

- **Filing unless an overlay differs.** It asks the overlay a question the record already answers against the disk, and misses the file that changed underneath a compile.
- **The cache remembering its own baseline answer** to know what it is later handed: state in the cache for a fact the fold holds.
- **A store of the server's own**, which is the second target directory: nothing a build files is the server's, and nothing the server compiles is a build's.
- **Compiling whole on save** so the saved text's unit is filed: a whole compile of the unit the author is editing, on every save, to spare the next build one.

## Retirement

`usage.md`'s surface table and its "Reusing what was already built" say what a question files, and the documentation that states today's rule follows the code: `curios-wonder`'s `//!` and `diagnostics.rs`'s `ReadOnly`, `curios/src/contract.rs`'s module documentation and store access, `curios/src/pipeline.rs`'s `documentation_of`, `curios-pipeline`'s `lib.rs`, `Cache::put` and `Cache::baseline` (`compile.rs`), and the tests that say it — `curios-pipeline/src/tests/baseline_tests.rs`, `curios/tests/payload.rs`, `curios-verdicts/src/verdicts/tests.rs`. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
