# A question costs what its edit reaches

## Status

**Measured, nothing built.** Three rounds of temporary instrumentation over the language server's diagnostics path, taken on `398181f9`, attribute every check to within 2 ms. Two groups of changes are refined enough to build: the cadence of the server, and the whole-unit work a question computes and never reads. Totality over the closure, incremental erasure and incremental lowering are not refined yet. This specification continues the one retired into [A stored unit is a baseline for an item-level recompile](../design/toolchain/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md), whose deliberately unspecified list named the last two as the next floors.

## Why it exists

An editor asks about a file on every edit, and again on every save. With the baseline recompile in place, the answer about `/std` costs 6.0 s in release when the edit reaches nothing, 21.8 s when it reaches a hub, and 8.7 s for the first check of a session. Most of the floor does not depend on what changed: every check lowers, diffs, classifies, erases, verifies and places the whole unit. A save then checks the text the last keystroke already checked, so a saved edit pays the floor twice.

## What was measured

End to end, from the event written to the server to the diagnostics it published for that document. The release column is one session running these events in this order, with each edit reverted before the next; each debug scenario ran in a fresh session, three times across three rounds.

| Event | Release | Debug, three rounds |
| --- | --- | --- |
| First check of a session, `/std/Nat` opened unedited | 8.7 s | 53.9–55.1 s |
| Save of `/std/Nat` unedited | 6.0 s | 23.8–24.8 s |
| A declaration appended to `/std/Nat`, and its save | 6.0 s | not profiled |
| The body of `/std/Bool/not` respelled | 21.8 s | 92.9–94.9 s |
| Save of that edit | 21.9 s | 92.9–94.9 s |

Warm checks ran about four times slower in debug than in release, and first checks more than six times slower, since the one-time digest of a much larger debug binary dominates them. The shares below are the finding, and the absolute debug figures are not.

**The floor.** Round three, the first save of `/std/Nat` unedited, 23.8 s in debug. The tests tail's rows overlap the erasure rows by a few hundred milliseconds, since its erasure passes through the same spans.

| Work | Span | Debug |
| --- | --- | --- |
| Erasing each of the unit's 1,974 items | `erase_item` | 7.21 s |
| Projecting the whole module out of universe levels before erasure | `erase_universe_project` | 2.31 s |
| Verifying the erased arena | `verify_module`, `verify_prefix` | 0.79 s |
| Classifying every definition's totality | `record_totality` | 3.32 s |
| Ordering every item | `lower_order_flat_items` | 1.35 s |
| Lowering every item, parsing already cached | `lower_process_items` | 1.19 s |
| Building the documentation record | `lower_documentation` | 0.72 s |
| Diffing lowered items against the baseline | `changed_names` | 1.65 s |
| The reverse closure over the baseline's graph | `reverse_closure` | 0.93 s |
| Placing the unit in the store: validate, serialize, digest | `cache_put` | 2.19 s |
| Elaborating and erasing the synthesized tests tail | `check_lowered`, `erase_checked` | 1.08 s |
| Closed reduction, positivity, recheck setup and the rest | | about 1.1 s |

**A hub edit.** Round three, the respelled `not`, 94.4 s in debug. The edit reaches 847 of the library's declarations.

| Work | Span | Debug |
| --- | --- | --- |
| Elaborating the 847 declarations | `declaration` | 32.81 s, of which 20.56 s their own |
| Finalization and the totality gates over them | `finalize_and_check` | 14.40 s |
| Totality classification | `record_totality` | 4.87 s |
| The kernel's recheck of the closure | `recheck_over` | 22.15 s, of which 13.94 s inside the kernel |
| Erasure, as on the floor | `erase_unit` | 11.23 s |
| Lowering, diff and placement, as on the floor | `into_core_unit`, `invalidated`, `cache_put` | 8.49 s |

**The first check of a session.** Round three, 53.9 s in debug. Each cost below is paid once per server thread, or once per binary.

| Work | Span | Debug |
| --- | --- | --- |
| Digesting the compiler binary for the store's identity | `cache_get` | 17.08 s |
| Parsing all 140 modules into the per-thread parse cache | `run_parser` | 11.22 s |
| Restoring and validating the archived prelude | `restore_archives` | 1.18 s |

The identity digest is skipped when `curios-prelude-archive/std/.curios/compiler` holds a stamp matching the running binary's size, modification time, device, inode and change time. A freshly built binary misses it once, and a debug binary is large enough that the miss is most of the 17 s.

**Erasure is concentrated in proofs.** In the floor's save, item erasure took 7,208 ms over 1,974 items with a median of 0.80 ms.

| Heaviest items | Share of item erasure |
| --- | --- |
| 1, `/std/BigNat/add/raw_assoc` at 1,369 ms | 19.0% |
| 10 | 31.6% |
| 100 | 59.9% |
| 1,000 | 96.0% |

After `raw_assoc` come `/std/BigNat/mul/trim_add_raw_trim_l`, `/std/Bytes/of_nat_injective`, `/std/BigNat/add/raw_succ_out_r` and `raw_succ_out_l`, `/std/BigNat/add/raw_trimmed`, `/std/Bytes/high_below`, `/std/BigNat/mul/trim_comm`, `/std/Tui/run` and `/std/BigNat/mul/trim_distrib_l`. The eight whose signatures were read, all but `raw_succ_out_l` and `/std/Tui/run`, state an `Eq` or a `Nat/Lt`; `/std/Tui/run` is a program. A proof erases to nothing, and the walk over its body is the cost.

## What is already built

- **The baseline recompile.** A question compiles the edited unit over the stored one: it lowers the unit whole, diffs it by declared name, elaborates and judges the reverse closure alone, and erases the unit whole.
- **A module is parsed once per path and text on a thread**, so only the first check of a session pays for parsing.
- **The server coalesces.** A burst of edits during one check costs one more check from the newest text.
- **The kernel skips what was judged.** Reused items are mounted into its globals, and it walks the closure.
- **The archived prelude is restored once per thread.**

## The design

In the order their return per effort ranks them.

### The server's cadence

**A check of text already checked is not run again.** The analyst keeps the overlay its last check read, and a job whose overlay equals it publishes nothing new. Equality is over every open document, because a question about a unit reads every file of it. VS Code's client resolves full text sync to a save notification without text, so a save arrives carrying nothing the overlay does not already hold.

**Diagnostics publish in two phases.** Elaboration's records publish first, and the kernel's and erasure's publish as a second, complete set for the same documents. A newer job arriving between the phases drops the second, as coalescing already drops a stale check. The first publish then precedes 10.8 s of erasure and verification on the floor's 23.8 s, and 33.4 s of recheck and erasure on the hub edit's 94.4 s.

**The analyst warms before the first document arrives.** On `initialize`, the analyst thread restores the archived prelude, settles the compiler identity for the workspace's store, and parses the modules of the package the workspace root governs. The thread owns the thread-local prelude and parse cache, so the warming happens there. That moves up to 29.5 s of debug time out of the first check.

### Whole-unit work a question never reads

**The documentation record.** Lowering builds a unit's documentation record whenever its source is documented. A question never reads it, and never files the unit that would carry it: 0.72 s in debug.

**Placement without a successor.** A question places each unit it compiles so the next unit's slot is addressed after it, and placing validates the stored identities, serializes the unit and digests the bytes. The last unit of a question has no successor, so a placement computed when a later unit first asks for its slot costs nothing there: 2.19 s in debug.

**The baseline's reverse graph.** The reverse closure walks the baseline's mention graph on every check, and the baseline is the same unit for a whole session. Kept beside the baseline, the graph is built once: 0.93 s in debug.

Together these are 3.84 s of the 23.8 s floor.

### Totality classification over the closure

Not refined yet. Classification runs over the reassembled module whole, inheriting only the scope's verdicts, while every reused item carries the verdict stamped by the walk that filed the baseline and the recompile already asserts those verdicts do not move. A reused item lies outside the reverse closure, so it mentions no changed name, and classifying the closure alone with the reused verdicts inherited is expected to decide what the whole module decides. The retired specification kept classification whole, judging it together with positivity and the witness-cycle check to cost under a second; alone it is an estimated 0.8 s in release at the debug ratio, but it is 14% of the floor, and 4.87 s in debug on a hub edit.

### Incremental erasure

Not refined yet. Erasure is the largest cost of the floor, 10.8 s of 23.8 s in debug, and it stays 11.2 s on a hub edit. The retired specification observed that the arena appends and tombstones, so erasing the closure onto the previous arena is structurally supported. Two measurements shape a design: the whole module's universe projection costs 2.3 s before any item is erased, and item erasure concentrates in proofs, a hundred items holding 60% of it.

### Incremental lowering

Not refined yet. Lowering and the diff cost 5.84 s of the floor in debug: ordering 1.35 s, lowering items 1.19 s, the documentation record 0.72 s, comparing items 1.65 s and the reverse closure 0.93 s. The record and the closure are taken up above, which leaves 4.19 s. Reusing the lowered items and order of modules whose text did not change, and comparing only the modules that did, removes most of it.

## What constrains any answer

- **The trusted base does not change.** A reused item stays judged by the walk that filed the baseline, and the argument remains [Cached verdicts](../soundness/admission-without-judgment/cached-verdicts.md) applied per item.
- **There is one fold.** `wonder diagnostics`, the server and every build compile through `curios-pipeline`, and the retired specification rejected building incrementality into the server as private state. The cadence belongs to the server; the rest belongs to the fold.
- **A unit with a successor is placed exactly as today,** because the successor's slot is addressed after it.
- **Every refusal is still reported.** Two phases change when a kernel or erasure refusal is published, never whether.
- **Erasure checks obligations while it walks,** so a proof that stops erasing at its type must not skip one.

## What has to be decided

- **How a check splits for two phases:** a pipeline result that carries the elaboration verdict before the recheck and erasure run, or two calls over one scope.
- **What overlay equality compares:** the documents' text, or the versions the editor sends with each change.
- **Where the baseline's reverse graph lives:** on the `Unit`, computed on first use, or with the `Cache` that offers the baseline.
- **Whether placement becomes lazy for every caller of the store,** or only for questions.
- **The argument for totality over the closure,** stated beside the recompile's equivalence argument.
- **Whether a proof's erasure can stop at its type.**

## Deliberately not specified

- The kernel's per-declaration cost on a hub edit, 13.94 s over 847 declarations in debug. Spans inside `curios-cert` are the next measurement if two phases are not enough.
- The cost of elaborating a declaration, 20.56 s of own time over the same 847.
- Narrowing the synthesized tests tail to edits that reach a test, 1.08 s of the floor.
- Keying the compiler identity by a build-time fingerprint instead of digesting the executable.
- Reparsing between the previous good parse's item spans, and a `Prop` body that invalidates nothing but itself, both carried from the retired specification.

## How to retake the measurements

Everything needed is in the tree except a small driver and a fold, and this section specifies both.

**The binaries.** `cargo x build` files the release compiler at `target/release/curios`. `cargo build -p curios --all-features` files a debug compiler at `target/debug/curios` with the `profile` feature on, which is what the `--profile` flag needs. Note the commit, since each binary measures the tree it was built from.

**A session.** From the repository root, start `target/debug/curios --profile <stream.tsv> wonder server`, or the release binary without `--profile`; `--profile` is a global flag and goes before the subcommand. Speak the language server protocol over its standard streams, each message JSON framed by a `Content-Length` header: send an `initialize` request with `rootUri` set to the repository and empty capabilities, wait for its response, then send `initialized`. Every edit lives in the editor overlay, and nothing on disk changes.

**The events.** `textDocument/didOpen` carries the document's full text; `textDocument/didChange` carries the full new text with the version incremented; `textDocument/didSave` carries no text. Time an event from the moment its notification is written to the moment a `textDocument/publishDiagnostics` arrives whose `uri` is that document, ignoring publishes for other documents. Record wall-clock nanoseconds at both ends, to split a profile stream by check.

**The scenarios.** Run each in a fresh server, so its first check carries the one-time costs and the checks after it are warm. For the floor, open `curios-prelude-archive/std/Nat.crs` unedited, then save it twice. For the hub, open `curios-prelude-archive/std/Bool.crs`, change its text by replacing the one occurrence of `xor(b, true)` with `xor(true, b)`, which respells the body of `not`, then save. For a leaf, append the two lines `pub let _latency_probe(a: Nat) -> Nat =` and `    a;` to `Nat.crs`'s text, then save. Compare warm checks only with warm checks.

**Cold and warm identity.** Keep `curios-prelude-archive/std/.curios/compiler` to measure a warm identity. Delete it to measure a cold one, which a rebuilt binary is on its first check anyway.

**Flushing.** The recorder flushes when it writes a row at least one second after its last flush, so the rows written in the last second before the server exits are lost, starting with the closing rows of the last check's outermost spans. End every session with one more, unmeasured save after a pause of more than a second, then send `shutdown` and `exit`.

**Folding a stream by check.** The row kinds are documented in `curios-profile/src/trace.rs`. `H` opens the stream with the wall-clock nanoseconds its timestamps count from. `D` names a callsite, `S` creates a span with its fields such as `group=`, and `E` and `X` enter and exit a span with the nanoseconds since the stream opened. Pair each `E` with the next `X` of the same span id; the difference is the span's duration, and its depth is the number of spans entered and not yet exited when it was entered. Subtract `H`'s stamp from a check's two wall-clock stamps to place the check in stream time. A check's spans are those that start and end inside it; its attributed time is the summed duration of the shallowest of them, and the rest of its wall time is unattributed. A span's own time is its duration less its direct children's, computed by sorting spans by start, longest first on ties, and keeping a stack of the spans still open. Aggregate by callsite name for the tables above, and by `group` for per-item rows. The fold must be linear: sort once, find a check's spans by binary search on their start, and compute a check's shallowest depth once. A hub session's stream is about 217 MB with some 464,000 spans in its hub check, and a fold that recomputed the minimum depth per span did not finish in six hours. `curios_profile::fold`, which `cargo x profile` uses, aggregates a whole stream and cannot split it by check.

**The spans an attribution needs.** Parsing, lowering, elaboration, the kernel's recheck, erasure's entry, verification and the prelude's restoration already carry spans. The rows above that name no span in the tree came from temporary `curios_profile::profile!` spans, added for the measurement and removed after it. To attribute a check again, wrap:

- in `curios-pipeline`, all of `check_with_units`; the cache's `get` and `put` calls inside `compile_units`; `recheck_over`; and inside `invalidated`, its `changed_names` and `reverse_closure` calls;
- in `curios-text`'s `into_core_unit_within`, the `process_items` calls for mounted modules, `order_flat_items`, the documentation record built from `source.documented()`, and `unused_declarations`;
- in `curios-elab`'s `erase_unit_within`, the `UniverseErased` projection of the module and the `erase_items` call, and inside `erase_items`' loop, a span grouped by the item's `describe()`.

The expression form `profile!("name" => expr)` expands to a plain block, so a `?` inside it returns from the enclosing function. A span in `curios-elab` or `curios-text` rebuilds the prelude archive, about four minutes in debug.

**Fidelity.** Across three debug rounds each event varied by under 5%, and every check left at most 2 ms unattributed. Attribute in debug, and report any change this specification drives with release end-to-end numbers from the same scenarios.
