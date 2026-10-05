# curios-wonder

What the compiler knows about a program, handed out as records: the `wonder` engine, which runs the compiler as far as a question needs and reads the answer off what it already decided, and the two transports that ask it — the command line (`ask`) and the language server (`server`) — with the `lint` gate that turns one of those answers into an exit code. What each query means at the command line belongs to [usage.md](../documentation/usage.md)'s `wonder` and `lint` sections; what a record carries, and how a transport converts it, belongs to the crate rustdoc.

## Design

### Under the native compiler, and free of what it links

**Decision.** This crate depends on the pipeline, the package crate and `curios-verdicts`, and on nothing that links a back end: `cargo tree -p curios-wonder --edges normal` contains neither `curios-binaryen` nor `curios-runtime`. The one rung the driver cannot render, `wasm-optm`, is handed back as the emitted module for the transport that owns Binaryen to finish, through `wonder_stage`'s `finish` argument.

**Rationale.** A question costs the compiler and nothing after it, so answering one needs no optimizer, code generator or launcher, and paying for all three on every build and test of the engine is what a module inside `curios` cost. `curios-js` can reach the emitted module and not Binaryen, so taking the renderer as an argument states that fact at the crate boundary.

### The engine names no transport's types, and no product's

**Decision.** Nothing in the engine reads a file by a name it was not handed, encodes JSON, or spells an LSP type; a record is plain data over the compiler's coordinates — a `Span` of source identity and UTF-8 byte range — and a transport converts at its own edge. UTF-16 exists only in the server's adapter.

**Rationale.** Every rendering is computed from the record, which keeps the transports honest with each other: the command line reads `wonder diagnostics` exactly as `curios run` would report the same program, because it is the same `Report` rendered.

### A question files the units it compiled from disk

**Decision.** Dependencies come from the store; one that is not is compiled and filed, as every unit is where the disk holds every text its fold has read ([A command compiles only as far as its answer needs, and a project keeps what it compiled](../documentation/design/tools/a-command-compiles-only-as-far-as-its-answer-needs-and-a-project-keeps-what-it-compiled.md)): each unit so far restored on a record the disk confirms, or compiled from text the disk holds, whole or over a baseline (`Overlaid`, `Verdicts::taken_on_disk`). A unit compiled from text an editor holds unsaved is not filed, nor any unit after it. Every unit is kept for the session as the baseline its next compile starts from (`Verdicts::keep`, `Verdicts::kept`), filed or not, and so is every unit the store restored, which a later hit on the same slot is answered with in place of decoding it again. Every unit the fold compiles something after is placed (`Verdicts::place`), filed or not, since a slot is addressed after the units before it and a gap misses for the whole tail; a unit neither filed nor followed is not placed, since nothing addresses a slot after it. Where the store cannot be written the answer is what it would have been, and the engine hands the store's refusal back beside it (`Diagnosed::unfiled`): the command line prints it once, and the server logs it once a session.

**Rationale.** Filing only what the disk answers for is what keeps a server from filing a unit per keystroke, without starting every question cold, and what keeps a slot from holding a unit chained after one nothing on disk reproduces. Keeping every unit is what leaves the guard on a kept unit its evidence: the log of what each unit before it read. Placing apart from filing is why the engine holds `curios-verdicts`'s store rather than a `dyn Cache`, whose trait cannot say the first without the second.

## Measuring a question's latency

Nothing that takes these figures is checked in: a driver and a fold are small, and a protocol stays readable where a script would need keeping.

**The binary.** `cargo xtask runtime`, then `cargo build --release -p curios --features profile`: without `--profile` it measures wall clock, with `--profile <PATH>` it files one row per span. A rebuilt binary has a new digest, which moves every store slot, so refile a package measured as built with the same binary first — `target/release/curios test <lib.crs>`.

**A session.** Start `target/release/curios [--profile <PATH>] wonder server` and speak the protocol over its standard streams, each message JSON behind a `Content-Length` header. Send `initialize` with `workspaceFolders` naming the package root — a folder no manifest governs warms nothing — wait for its response, and send `initialized`. One fresh server per scenario, so the first check carries the one-time costs.

**The events.** `didOpen` with the full text, then `didChange` with the full text, every one distinct, since a change carrying the overlay the last check read publishes nothing. Time each notification from its write to the `publishDiagnostics` whose `uri` is that document, in wall-clock nanoseconds at both ends so a profile can be split by check. The analyst waits a tenth of its last check's cost, capped at `SETTLE`, before compiling, so wall clock includes that wait.

**The scenarios.** `curios-text/std/Nat.crs` opened, then keystrokes each appending `pub let _probe_N(a: Nat) -> Nat =` and `    a;` for a fresh `N`. A hub: `curios-text/std/Bool.crs` with its one `xor(b, true)` replaced by `xor(true, b)`, followed by keystrokes to `Nat.crs`, which the hub edit must stop costing. A package of a chosen size — a manifest and a `lib.crs` of `pub mod M0;` through `pub mod M11;`, declarations spread evenly and cycling a `Nat` sum, a `Nat` induction and a proof of `Eq()(n + 0, n)` by `Eq/refl()` — measured over a store that holds it, over one that holds nothing, which the session's first check then files, and over one that cannot be written, a file where `.curios/verdicts` would be. A burst: changes written a set interval apart, each placing a private unused declaration after a different number of blank lines, so the line its lint names says which text a publish answered. An edit that fails elaboration stops before the kernel and erasure, so its wall time is where elaboration decided.

**Flushing.** The recorder flushes every row as it is written, so a session's last edit is on disk when the edit is answered.

**Folding a stream by check.** The row kinds are `curios-profile/src/trace.rs`'s. `H` carries the wall-clock nanoseconds the stream's timestamps count from, which places a check's two stamps in stream time. Pair each `E` with the next `X` of the same span id, and name it through the `D` row its callsite index points at. A check's spans are those that start and end inside its window, and a span's own time is its duration less its direct children's, found by sorting spans by start, longest first on a tie, over a stack of spans still open; sum own time by callsite name, and by an `S` row's `group=` field per declaration. Keep the fold linear — one sort and a binary search for each window's first span — since a hub check is hundreds of thousands of spans.
