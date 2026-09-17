# curios-wonder

What the compiler knows about a program, handed out as records: the `wonder` engine, which runs the compiler as far as a question needs and reads the answer off what it already decided, and the two transports that ask it — the command line (`ask`) and the language server (`server`) — with the `lint` gate that turns one of those answers into an exit code. What each query means at the command line belongs to [usage.md](../documentation/usage.md)'s Asking about a program and Linting; what a record carries, and how a transport converts it, belongs to the crate rustdoc.

## Design

### Under the native compiler, and free of what it links

**Decision.** This crate depends on the pipeline, the package crate and `curios-verdicts`, and on nothing that links a back end: `cargo tree -p curios-wonder --edges normal` contains neither `curios-binaryen` nor `curios-runtime`. The one rung the driver cannot render, `wasm-optm`, is handed back as the emitted module for the transport that owns Binaryen to finish, through the `finish` argument of `wonder_stage`.

**Rationale.** A question costs the compiler and nothing after it, so a crate answering questions has no use for an optimizer, a code generator or a launcher — and paying for all three on every build and test of the engine is what the module inside `curios` cost. Handing the last rung back rather than rendering it is what the engine already did, since `curios-js` can reach the emitted module and not Binaryen; making the transport take the renderer as an argument states the same fact at the crate boundary.

**Rejected.** Keeping the engine in `curios` until a browser editor needed it: the split was owed to the native build's test cycle before any second consumer existed.

### The engine names no transport's types, and no product's

**Decision.** Nothing in the engine reads a file by a name it was not handed, encodes JSON, or spells an LSP type; a record is plain data over the compiler's coordinates — a `Span` of source identity and UTF-8 byte range — and a transport converts at its own edge. UTF-16 exists only in the server's adapter.

**Rationale.** Every rendering is computed from the record rather than from the compiler beside it, which is what keeps the two transports honest with each other: the command line reads `wonder diagnostics` exactly as `curios run` would have reported the same program, because it is the same `Report` rendered.

### A query never writes the store

**Decision.** Dependencies come from the store already built, and one that is not is compiled in memory and forgotten. The engine wraps the store it is handed in one that files nothing — while still placing every unit the fold compiles something after, through `Verdicts::place`, because a slot is addressed after the units before it and a chain with a gap in it misses for the whole tail. The unit a fold ends on is not placed: nothing addresses a slot after it, and placing it would serialize it whole.

**Rationale.** The store addresses a unit by content, and a server that filed what it checked would file a unit per keystroke. Placing without filing is why the engine holds `curios-verdicts`'s store rather than a `dyn Cache`: the trait has no way to say the first without the second.

## Measuring a question's latency

The figures this crate's documentation cites were taken this way, and nothing that takes them is checked in: a driver and a fold are small, and a protocol stays readable where a script would need keeping.

**The binary.** `cargo x runtime`, then `cargo build --release -p curios --features profile`. One binary serves both measurements: without `--profile` it is wall-clock, and with `--profile <PATH>` it files one row per span. A rebuilt binary has a new digest, which moves every store slot, so a package measured as built is refiled first with the same binary — `target/release/curios test <lib.crs>` — and a package never built is simply one nothing has been refiled for.

**A session.** Start `target/release/curios [--profile <PATH>] wonder server` and speak the protocol over its standard streams, each message JSON behind a `Content-Length` header. Send `initialize` with `workspaceFolders` naming the package root — a folder no manifest governs warms nothing — wait for its response, and send `initialized`. One fresh server per scenario, so the first check carries the one-time costs.

**The events.** `didOpen` with the full text, then `didChange` with the full text, every one of them *distinct*: a change or a save carrying the overlay the last check read publishes nothing, so a driver waiting on its answer waits forever. Time each notification from the moment it is written to the moment a `publishDiagnostics` arrives whose `uri` is that document, recording wall-clock nanoseconds at both ends so a profile can be split by check. The analyst waits a tenth of what its last check cost, capped at `SETTLE`, before it compiles, so wall-clock includes that wait and a profile attributes everything else.

**The scenarios.** `curios-prelude-archive/std/Nat.crs` opened, then keystrokes each appending `pub let _probe_N(a: Nat) -> Nat =` and `    a;` for a fresh `N`. A hub: `curios-prelude-archive/std/Bool.crs` with its one `xor(b, true)` replaced by `xor(true, b)`, followed by keystrokes to `Nat.crs`, which the hub edit must stop costing. A package of a chosen size: a manifest, a `lib.crs` of `pub mod M0;` through `pub mod M11;`, and declarations spread evenly over the twelve, cycling a `Nat` sum, a `Nat` induction and a proof of `Eq(n + 0, n)` by `Eq/refl()` — measured built and not. A burst: several changes written a set interval apart, each placing a private unused declaration after a different number of blank lines, so the line its lint names says which text a publish answered. The point at which elaboration has decided is measured without instrumentation: an edit that fails elaboration stops before the kernel and erasure, so its wall time is that point.

**Flushing.** The recorder flushes on the first row written a second or more after its last flush, so the closing rows of a session's last check are lost unless something follows them. End every profiled session with one more distinct edit after a pause of more than a second, and do not measure it.

**Folding a stream by check.** The row kinds are `curios-profile/src/trace.rs`'s. `H` carries the wall-clock nanoseconds the stream's timestamps count from, which is what places a check's two stamps in stream time. Pair each `E` with the next `X` of the same span id for a span's start and end, and name it through the `D` row its callsite index points at. A check's spans are those that start and end inside its window. A span's own time is its duration less its direct children's, found by sorting spans by start, longest first on a tie, and keeping a stack of the spans still open; sum own time by callsite name, and by an `S` row's `group=` field for per-declaration rows. Keep the fold linear — one sort, and a binary search for each window's first span — since a hub check alone is hundreds of thousands of spans.
