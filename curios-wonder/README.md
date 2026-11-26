# curios-wonder

What the compiler knows about a program, handed out as records: the `wonder` engine, which runs the compiler as far as a question needs and reads the answer off what it already decided, and the two transports that ask it — the command line (`ask`) and the language server (`server`) — with `check`, the command that turns what they report into an exit code. What each query means at the command line belongs to [usage.md](../documentation/usage.md)'s `wonder` and `check` sections; what a record carries, and how a transport converts it, belongs to the crate rustdoc.

## Design

### Under the native compiler, and free of what it links

**Decision.** This crate depends on the pipeline, the package crate and `curios-verdict`, and on nothing that links a back end: `cargo tree -p curios-wonder --edges normal` contains neither `curios-binaryen` nor `curios-native`.

**Rationale.** A question costs the compiler and nothing after it, so answering one needs no optimizer, code generator or launcher, and paying for all three on every build and test of the engine is what a module inside `curios` cost.

### The engine names no transport's types, and no product's

**Decision.** Nothing in the engine reads a file by a name it was not handed, encodes JSON, or spells an LSP type; a record is plain data over the compiler's coordinates — a `Span` of source identity and UTF-8 byte range — and a transport converts at its own edge. UTF-16 exists only in the server's adapter.

**Rationale.** Every rendering is computed from the record, which keeps the transports honest with each other: the command line reads `wonder` exactly as `curios run` would report the same program, because it is the same `Report` rendered.

### A question files the units it compiled from disk

**Decision.** Dependencies come from the cabinet; one that is not is compiled and filed, as every unit is where the disk holds every text its fold has read ([A command compiles only as far as its answer needs, and a project keeps what it compiled](../documentation/design/tooling/a-command-compiles-only-as-far-as-its-answer-needs-and-a-project-keeps-what-it-compiled.md)): each unit so far restored on a record the disk confirms, or compiled from text the disk holds, whole or over a baseline (`Overlaid`, `Verdicts::taken_on_disk`). A unit compiled from text an editor holds unsaved is not filed, nor any unit after it. Every unit is kept for the session as the baseline its next compile starts from (`Verdicts::keep`, `Verdicts::kept`), filed or not, and so is every unit the cabinet restored; a later hit on the bytes a kept unit was restored from or filed as is answered with it, in place of reading and decoding the object again. A unit that is not filed has no place in the cabinet: no object is addressed after another, so a unit that cannot read it is restored as though it were not there, and one that can is compiled over its baseline. Where the cabinet cannot be written the answer is what it would have been, and the engine hands the cabinet's refusal back beside it (`Diagnosed::unfiled`): the command line prints it once, and the server logs it once a session.

**Rationale.** Keeping every unit is what gives a kept unit the units it was compiled after: the ones the fold held before it. Keeping apart from filing is why the engine holds `curios-verdict`'s cabinet rather than a `dyn Cache`, whose trait cannot say the first without the second.

### A query reads the compiler's answer and computes none of its own

**Decision.** A query runs the compiler as far as its question needs and reads the answer off what the compiler decided: a diagnostic is the `Report` the compile path would have printed, a goal is the entry of the batch elaboration already builds. The engine resolves no name and types no term of its own.

**Rationale.** One source of truth that consumers walk, on the `Intrinsic::signature` principle, so what this crate says and what the compiler did cannot disagree.

### Whether the checkers admit a proof of `False` is answered as any question is

**Decision.** `--false` asks of a subject the question the soundness board asks of every witness, through the pipeline's put ([`curios-pipeline`](../curios-pipeline/README.md#a-term-put-at-false-is-answered-here)), and exits as every query does: 0 once the question is answered, whichever way the checkers went, the verdict the answer's first word.

**Rationale.** A query's exit says whether it was answered, and its answer is on standard output ([`wonder`](../documentation/usage.md#wonder)); a script reads `REFUSED` or `ADMITTED` there as it reads any answer.

**Rejected.** A search's exit, 0 where a proof was admitted, and a check's, 0 where both checkers refused, as `coqchk` exits: either makes `--false` the one query whose exit reports its answer.

### A change on disk is the editor's to watch, and it asks again about every open document

**Decision.** Where the client states `workspace.didChangeWatchedFiles.dynamicRegistration`, the server registers, once the session is initialized and before it reads a notification, one watcher over every module under the workspace, `**/*.crs`, and one over every manifest, `**/curios.toml`, and answers `workspace/didChangeWatchedFiles` as it answers a save: one check of every open document against the overlay as it stands. A client that does not watch on request is asked for nothing, and a save stays the gesture for the disk having moved. A `path` dependency outside the workspace folder, which neither glob reaches, is answered by the next save.

**Rationale.** A checkout, a rebase, another editor or a formatter run from a terminal moves the disk under the files no editor holds with no save, and what the editor shows is then what a check the disk has since moved under said, and a refusal published on a file no editor holds stays where it was published — the failure gopls's [#31553](https://github.com/golang/go/issues/31553) reports of version control. Both shipped clients watch on request, VS Code's language client (`FileSystemWatcherFeature`) and Zed ([#49778](https://github.com/zed-industries/zed/pull/49778)). Nothing a check writes matches either glob: a question files below the cabinet's `objects` family and beside its `compiler` memo, and the `sources` family, which only `curios pin` and a fetch write, holds dependency texts whose change a check should answer. What a check that filed takes away matches them where it is a tree no manifest pins any longer (`curios-verdict`'s `collect_project`), which costs one more check of the open documents, and that check finds nothing more to take. Asking again about a document nothing moved under costs what its reuse costs, timed at `5ba96f872` with the debug build, wall clock including the analyst's settle wait, as the median of seven runs with their span: 0.7 ms [0.6–0.8] for a loose program reading one module, 6.7 ms [6.1–7.2] for the twelve-module package below, opened on `M0.crs`, and 31.4 ms [31.0–32.8] for a copy of `curios-prelude-build/std` opened on `Nat.crs`. The measurement is the bench's `reopen_measurements` (`xbench/src/measurements/reopen.rs`), the figure its second reopen's.

**Rejected.** A watcher of the server's own, which [LSP 3.17](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles) recommends against ("getting file system watching on disk right is challenging", and a client usually starts more than one server), and which rust-analyzer keeps behind a setting whose default is the client's. Asking only about the open documents the changed paths reach, which needs reads no log holds: a refused unit is taken into no fold (`compile_units`, `curios-pipeline/src/compile.rs`), a file looked for and missing is no read (`RootSource::reads`), and the manifests and `mod` chains a placement walks are read by `Selection` into no log — where asking again about every open document costs what its reuse costs, as clangd revalidates every open file rather than the readers of the one changed ([D77847](https://github.com/llvm/llvm-project/commit/596b63ad4019e61030803789a1844a0f1aeb34db)). Registering each path dependency outside the workspace folder as a `RelativePattern` at its root, where the client states `relativePatternSupport`: the roots are the manifests' to state, so every manifest edit that adds or moves one would unregister and register again, for a directory the workspace does not hold and whose change the next save answers.

## Measuring a question's latency

Nothing that takes these figures is checked in: a driver and a fold are small, and a protocol stays readable where a script would need keeping.

**The binary.** `cargo xtask compiler launcher`, then `cargo build --release -p curios`: without `--profile` it measures wall clock, with `--profile <PATH>` it files a trace of every span. A rebuilt binary has a new digest, which no record in the cabinet names, so refile a package measured as built with the same binary first — `target/release/curios test <lib.crs>`.

**A session.** Start `target/release/curios [--profile <PATH>] wonder --server` and speak the protocol over its standard streams, each message JSON behind a `Content-Length` header. Send `initialize` with `workspaceFolders` naming the package root — a folder no manifest governs warms nothing — wait for its response, and send `initialized`. One fresh server per scenario, so the first check carries the one-time costs.

**The events.** `didOpen` with the full text, then `didChange` with the full text, every one distinct, since a change carrying the overlay the last check read measures what reusing that check costs rather than an edit. Time each notification from its write to the `publishDiagnostics` whose `uri` is that document, in wall-clock nanoseconds at both ends so a profile can be split by check. The analyst waits a tenth of its last check's cost, capped at `SETTLE`, before compiling, so wall clock includes that wait.

**The scenarios.** `curios-prelude-build/std/Nat.crs` opened, then keystrokes each appending `pub let _probe_N(a: Nat) -> Nat =` and `    a;` for a fresh `N`. A hub: `curios-prelude-build/std/Bool.crs` with its one `xor(b, true)` replaced by `xor(true, b)`, followed by keystrokes to `Nat.crs`, which the hub edit must stop costing. A package of a chosen size — a manifest and a `lib.crs` of `pub mod M0;` through `pub mod M11;`, declarations spread evenly and cycling a `Nat` sum, a `Nat` induction and a proof of `Eq()(n + 0, n)` by `Eq/refl()` — measured over a cabinet that holds it, over one that holds nothing, which the session's first check then files, and over one that cannot be written, a file where `.curios/cabinet/objects` would be. A burst: changes written a set interval apart, each placing a private unused declaration after a different number of blank lines, so the line its lint names says which text a publish answered. An edit that fails elaboration stops before the certifier and erasure, so its wall time is where elaboration decided.

**Flushing.** The recorder flushes every row as it is written, so a session's last edit is on disk when the edit is answered.

**Folding a stream by check.** The row kinds are `curios-profile/src/trace.rs`'s. `H` carries the wall-clock nanoseconds the stream's timestamps count from, which places a check's two stamps in stream time. Pair each `E` with the next `X` of the same span id, and name it through the `D` row its callsite index points at. A check's spans are those that start and end inside its window, and a span's own time is its duration less its direct children's, found by sorting spans by start, longest first on a tie, over a stack of spans still open; sum own time by callsite name, and by an `S` row's `group=` field per declaration. Keep the fold linear — one sort and a binary search for each window's first span — since a hub check is hundreds of thousands of spans.
