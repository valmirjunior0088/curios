# Questions file what they compile

Working specification for bringing `lint` and the `wonder` queries under [A command compiles only as far as its answer needs, and a project keeps what it compiled](../../design/tools/a-command-compiles-only-as-far-as-its-answer-needs-and-a-project-keeps-what-it-compiled.md): a question files, for a project, each unit it compiled whole from the text on disk, so the next command starts from the store rather than cold.

**One unit, whoever files it.** The slot a question files holds the bytes a build files there, so over an unchanged disk the second command, whichever it is, compiles no unit. It is one rung of what the decision's goal stands on, that a command costs what changed since anything last compiled the project:

| Rung | States | Lands |
| --- | --- | --- |
| A unit is a function of what it was compiled from | the same bytes from any process | stage 0 here |
| One unit, whoever files it | a question's slot is a build's | stages 1 to 4 here |
| One unit, however it was compiled | whole or over a baseline, so what is compiled over one is filed | *Open* |
| A successor depends on what it read | an edit stops recompiling every unit after it | [One environment](../05-compilation/02-one-environment.md), [item tasks](../05-compilation/03-item-tasks.md) |

It needs no other spec, and lands the first rung itself: [item tasks](../05-compilation/03-item-tasks.md) holds a compilation to the same stored units for every number of workers, which is this rung with the schedule varied. What a question compiles over a baseline stays unfiled, under [A stored unit is a baseline for an item-level recompile](../../design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md).

## What this builds on

- **The seam.** `Cache` (`curios-pipeline/src/compile.rs`) is `get`, `baseline` and `put(source, unit, followed)`. The fold asks `baseline` on a miss and compiles over what it answers (`compile_unit_over`), or whole where it answers `None`; `put` is handed the unit either way and is not told which.
- **The store's cache files.** `Verdicts`' `put` serializes the unit, writes its record and the unit as one file renamed into place (`replace`), and places it in the chain. It answers no baseline.
- **The engine's cache does not.** `ReadOnly` (`curios-wonder/src/diagnostics.rs`) believes a stored unit on a re-read through the overlay (`Verdicts::get_overlaid`), answers the nearest baseline — the session's (`Verdicts::kept`), then the slot's whatever its files now hold (`Verdicts::earlier`), then the scope's offer — and on `put` keeps the unit for the session and places it where something follows.
- **A record is what was read, and what came before.** A unit's record lists every file its compilation read with a digest of the text parsed from it, taken at `RootSource::reads`, and a digest of what each predecessor contained; a hit re-reads each file (`unchanged`) and compares each predecessor (`chained`). A record of text the disk does not hold, or of a predecessor no reader will hold, is a miss for every reader of the disk.
- **A kept unit is guarded by a log.** `Verdicts::keep` adds what a unit read to the fold's log, and `Verdicts::kept` offers a kept unit only while the units before it read what they read when it was kept.
- **One compile.** A question's unit comes from the fold a build runs, `compile_unit`: lowered, elaborated, certified by the kernel and erased.
- **One fold per subject.** `Asked::every` (`curios-wonder/src/ask.rs`) asks about a package entire as its library and then each program it declares, each over a store handle and a fold of its own.
- **An entry is no unit.** A program's entry and the modules it reaches are checked on top of the fold (`Fold::check`); a build files what they compile to as a payload, and no question reads one.
- **The contract.** Each command's `Contract` (`curios/src/contract.rs`) states how it reaches the store; `lint` and every `wonder` query read one and file nothing, which the engine holds whatever store it is handed.

## The gap

**A question files nothing but the `compiler` memo.** A unit no build has filed is compiled in memory by every `lint` and every `wonder` invocation that reaches it, once for each subject of the invocation, and by every server session, which then compiles it over what it kept on every check that reaches it (`a_session_recompiles_what_nothing_was_filed_for`, `curios-wonder/src/tests/store_tests.rs`). Over a package and its path dependency that nothing has built, `wonder diagnostics` and then `lint` leave `.curios/` holding the memo alone.

**A unit is not a function of what it was compiled from.** Five builds of one text file five different units at one slot, under [*a unit's reproduction*](#the-measurements), counted at `87189c97d`. They read the same files and erase to the same arena; two of them differ in three of the standard library's 2,170 items, all in `/std/Str`, in the certifier's record, and in one table of the lowering:

- **A bound's proof follows a hash's order.** Two compilations write `/std/Str/At/onward` two valid proofs, one from `q`'s own bound and one from `p`'s and the hypothesis. The bound prover reads its guards from `Frames::visible_scrutinee_entries` (`curios-elab/src/context/frames.rs`), which walks each frame's `HashMap`: read from the code, and stage 0's loop is what says whether it is the only such order.
- **A global's re-export paths follow one.** `writable_paths` (`curios-text/src/into_core.rs`) pushes each path a module's `bindings` reach, a `HashMap`, onto the global's list.

A package of two one-declaration units is reproduced, across processes and across `run` and `test`, which is why no test has met it.

## Prior art

- **gopls** keeps what it derives from type-checking in "gopls' persistent, transactional, file-based key/value store", and names what that bought: "the fast restart, reduced memory consumption, and synergy across processes that were delivered by the v0.12 redesign" ([*Gopls: Implementation*](https://tip.golang.org/gopls/design/implementation)). "Since the cache is persisted across processes", its authors write, "if you run two gopls instances, they work together synergistically" ([*Scaling gopls for the growing Go ecosystem*](https://go.dev/blog/gopls-scalability)).
- **Lean's server builds what the open file imports through the build tool**: its file worker "Uses `lake setup-file` to compile dependencies on the fly and add them to `LEAN_PATH`" ([`SetupFile.lean`](https://github.com/leanprover/lean4/blob/master/src/Lean/Server/FileWorker/SetupFile.lean)), so a dependency is filed as a build files it.
- **rust-analyzer's diagnostics run the build tool in the build's own directory**, and contend with it. The remedy it documents is a directory of its own, which "prevents rust-analyzer’s cargo check and initial build-script and proc-macro building from locking the Cargo.lock at the expense of duplicating build artifacts" ([configuration](https://rust-analyzer.github.io/book/configuration.html)).
- **A store of values keyed by what they were built from is a store of constructive traces** ([*Build Systems à la Carte*](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems-a-la-carte.pdf), §4.2.3), and a slot's record is one: the hashes of its inputs, a predecessor's value among them. Such a store stops a rebuild early where a rebuilt value is unchanged; where a task is one that "can change the order in which unique variables are obtained from the supply, producing different but semantically identical results" (§6.3), the rebuilt value always differs and every successor misses.
- **Go's build identifies a step by its inputs and its result by its content**, and feeds the next step the second: "Separating action ID from content IDs is important for reproducible builds", since "because the content IDs converge, so too do the action IDs" ([`buildid.go`](https://go.googlesource.com/go/+/refs/heads/master/src/cmd/go/internal/work/buildid.go)). A slot and its record are that pair.
- **GHC took the same fault for the same reason.** Its goal is that, given one compiler, sources, flags and packages, GHC "should always produce the same interface files", and the first cause it names is that "The order of allocated Uniques is not stable across rebuilds" ([deterministic builds](https://gitlab.haskell.org/ghc/ghc/-/wikis/deterministic-builds)).
- **A cache that is believed checks that it may be.** rustc "expects that the output is the same as from a prior incremental compilation session", and since 1.52 "checks that the value is indeed as expected, rather than assuming so" ([Rust 1.52.1](https://blog.rust-lang.org/2021/05/10/Rust-1.52.1/)); Dune can "re-execute randomly chosen build rules and compare their results with those stored in the cache" ([Dune caches](https://dune.readthedocs.io/en/stable/reference/caches.html)).

Filing where a build files is what makes one command's work the next one's. What it must not cost is a lock two commands wait on, which a slot renamed into place does not take; and what it needs is that two commands filing one unit write one thing.

## Decisions

1. **What is filed.** A unit the question compiled whole, while its fold is the one a build would have run over the disk: every unit the fold has taken, this one included, was restored on a record the disk confirms or compiled whole from text the disk holds. The first unit taken any other way — compiled over a baseline, or from text the disk does not hold — ends filing for that fold; it and every unit after it are kept and placed, as today.
2. **The disk decides, not the overlay, for the whole fold.** The test is each record against the disk, the one a later `get` makes. It covers the units before a unit as it covers the unit, since a record names what each predecessor contained: a unit filed after a predecessor nothing on disk reproduces is a miss for every later reader, in the place of a slot that may have held the disk's unit. A document open and unchanged files as any other, and a file rewritten under a compile is caught with a document never saved.
3. **`put` is told how the unit was compiled.** The fold knows whether it compiled whole or over a baseline, and says so at the seam, as it says `followed`. The engine's cache does not remember what its own `baseline` answered.
4. **Every unit the fold takes is kept and logged, filed or not.** The session's baseline and the log a kept unit is guarded by are the fold's: a filed unit left out of the log would let a kept unit after it be offered once that predecessor had changed.
5. **Where.** The store a build files into: beside the governing manifest, or `CURIOS_CACHE`'s shared half. A loose file and standard input open no store.
6. **How far.** The judged unit. A question files no payload: its crates link no runtime.
7. **A one-shot question takes a baseline as a session does.** Over a slot whose files have moved, `lint` compiles the closure and files nothing, each time it is run, until a build files the unit. Compiling it whole in order to file it would cost a whole compile of the standard library to anyone editing it.
8. **A store that cannot be written is said.** A one-shot question prints the line a build prints, on standard error, once; the server tells its client once a session, in a `window/logMessage`. The answer and the exit are what they would have been.
9. **An order that reaches a unit is one the program states.** What is iterated into a term, a table or a report is iterated in the order of registration, declaration or label, never a hash's.

## Stages

0. **A unit is a function of what it was compiled from.** Scrutinee entries are enumerated in the order they were registered, a global's paths in the order of their labels, and whatever [*a unit's reproduction*](#the-measurements) still names after them is ordered the same way, until eight builds file one unit. Each order removed is held by a fixture compiled sixteen times in one process.
1. **The seam says how.** `Cache::put` carries whether the unit was compiled whole; the fold passes it; `Verdicts` ignores it and `ReadOnly` does not yet read it. No behaviour moves, and `curios-pipeline`'s tests assert what they asserted.
2. **The engine files, and the contracts say so.** The engine's cache keeps every unit, files through the store it holds the ones decision 1 names, and places the rest where something follows; its name and documentation say what it now does. `curios-verdicts` answers whether the disk holds what the fold has read. `lint` and the `wonder` queries state the store access a build has, and `curios/src/contract/tests.rs` pins it. The server files from this stage on, since it asks through the same engine.
3. **A question says what it could not file.** The engine hands the store's refusal back beside its answer; the command line prints it and the server logs it, once each.
4. **The server.** Its tests and its measurement: a dependency it compiles is filed when it is compiled, a keystroke files nothing, and a whole compile of a unit from disk text files.

## Verification

- **One unit, whoever files it.** Over one package, the slot a `wonder diagnostics` files and the slot a build files hold the same bytes, unit for unit. This is what lets a build believe a question's unit, and it fails if a question's fold ever stops short of a build's.
- [*A unit's reproduction*](#the-measurements): eight builds, one unit.
- Over a package no build has filed, `wonder diagnostics` run twice: the second compiles no unit, and `lint` after it compiles nothing already filed.
- A build after a question reports every unit the question filed as reused, and files a payload over that chain.
- A unit compiled whole after one compiled from unsaved text, or over a baseline, is not filed, and its slot holds the bytes it held.
- The server: a dependency it compiles is filed when it is compiled; keystrokes in an open document file nothing and leave every slot's bytes as they were; a unit compiled whole after a saved manifest edit moved its scope is filed, and one compiled over the session's baseline is not.
- A file rewritten while its unit compiles leaves the slot as it was.
- A kept unit is refused once a predecessor that could not be filed has changed, and what it would have hidden is reported.
- A loose file and standard input leave the store as they found it. A store that cannot be written costs the reuse and never the answer (`Verdicts::refused`): `lint` prints one line on standard error and exits as it would have, and the server logs once.
- [`curios-wonder`'s latency protocol](../../../curios-wonder/README.md#measuring-a-questions-latency), retaken cold and warm, naming the stage; the first compile of a session now serializes each unit it files, and the figure says what that costs.

## The measurements

Each is taken with a release build without `profile`, over a copy of `curios-text/std` outside the tree — a package named `std`, so a build compiles it whole and a question compiles it over the archived unit.

- **A unit's reproduction**, counted. `curios document --manifest <copy>/curios.toml`, with `<copy>/.curios/verdicts` removed before each run, and the one slot's digest taken after it. The digests are compared; a unit that differs is read back, each of its four parts and each item of its module serialized alone and compared, and a differing definition printed.
- **A hit against a recompile**, timed. `curios wonder diagnostics <copy>/Nat.crs`, three readings with the unit filed by the build above and three with `verdicts` removed, the text unchanged in both. At `87189c97d`, on a machine that was not quiet: 0.57 s filed (0.55 to 0.60) and 6.56 s unfiled (6.48 to 7.08).
- **Whole against over a baseline**, mixed. In one process, through a `Cache` that answers no hit: the copy compiled whole, one file edited, compiled whole twice more and once over the first unit, each timed; the two whole units compared part by part and item by item as the noise, then the whole one with the recompiled one. The leaf appends `pub let _probe_1(a: Nat) -> Nat = a;` to `Nat.crs`; the hub respells `Bool.crs`'s one `xor(b, true)` as `xor(true, b)`.

## Open

- **What is compiled over a baseline is never filed.** Decision 7 leaves a one-shot question recompiling a closure on every run between builds, and a question never files the package `std` at all, since the archived unit is always offered: 6.56 s a question where a filed unit answers in 0.57. Filing it is the third rung, and *whole against over a baseline* says how far a recompiled unit stands from the one a whole compile files, counted at `87189c97d`. For the leaf, two whole compiles differ in two items, and the recompiled unit differs from a whole one in 41, the reused items of `Nat.crs` each by the 38 bytes the edit added: a reused item carries the spans of the text it was elaborated from, so the unit holds both texts of the file, and is 1.0 MB larger of 29.7. Its erased arena differs at one size. For the hub, two whole compiles differ in one item and the recompiled unit in 13, the reused items of `Bool.crs` among them at one size each, the edit keeping the file's length; the arenas agree. Whether the gap can be closed here is decided once stage 0 has made the comparison clean.
- **A question about a program checks its entry every time.** An entry is no unit, so nothing a question compiles of it is filed and nothing a build filed of it answers one.

## Rejected

- **Filing unless an overlay differs.** It asks the overlay a question the record already answers against the disk, and misses the file that changed underneath a compile.
- **Filing whenever the disk confirms every read so far**, a recompiled predecessor among them. A later `lint` would then hit the units after a saved, unbuilt edit; and the store would hold slots no build reaches, chained after a unit only a recompile produces, with a filed verdict resting on one the baseline's decision withholds.
- **Holding the headliner to a fixture alone**, and filing the hash's order as a finding: the claim would be pinned where it already holds and false over the one library every program is compiled against.
- **The cache remembering its own baseline answer** to know what it is later handed: state in the cache for a fact the fold holds.
- **A store of the server's own**, which is the second target directory: nothing a build files is the server's, and nothing the server compiles is a build's.
- **Compiling whole on save** so the saved text's unit is filed: a whole compile of the unit the author is editing, on every save, to spare the next build one.
- **A question that stays silent about a store it cannot write.** Every invocation then compiles everything again, and it reads as the compiler being slow.

## Retirement

`usage.md`'s surface table and its "Reusing what was already built" say what a question files, and the documentation that states today's rule follows the code:

- `curios-wonder`: its `README.md`, its `//!`, `diagnostics.rs`'s `ReadOnly` and what `declared.rs`, `cost.rs`, `stage.rs` and `document.rs` say of their `cache`, `ask.rs` on status lines, and `server.rs`.
- `curios-verdicts`: its `README.md`'s decision on a disagreeing slot, and `verdicts.rs`'s `Session` and `place`.
- `curios-pipeline`: `lib.rs`, `Cache::put` and `Cache::baseline` (`compile.rs`).
- `curios`: `contract.rs`'s module documentation and store access, and `pipeline.rs`'s `documentation_of`.
- [Cached verdicts](../../design/soundness/admission/cached-verdicts.md), where a kept unit compiled whole is now filed, with the reproduction as its evidence; the design decision's rationale; and `curios-unit`'s `README.md`, whose claim about a unit's bytes gains its test.
- The tests that say it: `curios-pipeline/src/tests/baseline_tests.rs`, `curios-wonder/src/tests/store_tests.rs`, `curios-verdicts/src/verdicts/tests.rs`, `curios/tests/payload.rs` and `curios/tests/wonder.rs`.

What *Open* still holds becomes roadmap lines of its own, named from the code. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
