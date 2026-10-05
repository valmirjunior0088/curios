# Questions file what they compile

Working specification for bringing `lint` and the `wonder` queries under [A command compiles only as far as its answer needs, and a project keeps what it compiled](../../design/tools/a-command-compiles-only-as-far-as-its-answer-needs-and-a-project-keeps-what-it-compiled.md): a question files, for a project, each unit it compiled from the text on disk, so the next command starts from the store rather than cold.

**One unit, whoever files it.** The slot a question files holds the bytes a build files there, so over an unchanged disk the second command, whichever it is, compiles no unit. It is one rung of what the decision's goal stands on, that a command costs what changed since anything last compiled the project:

| Rung | States | Lands |
| --- | --- | --- |
| A unit is a function of what it was compiled from | the same bytes from any process | landed: [`curios-unit`](../../../curios-unit/README.md#a-unit-is-a-function-of-what-it-was-compiled-from) |
| One unit, whoever files it | a question's slot is a build's | landed for a unit compiled whole: [`curios-wonder`](../../../curios-wonder/README.md#a-question-files-the-units-it-compiled-from-disk); stages 3 and 4 here |
| One unit, however it was compiled | whole or over a baseline, so what is compiled over one is filed | stages 5 to 8 here |
| A successor depends on what it read | an edit stops recompiling every unit after it | [One environment](../05-compilation/02-one-environment.md), [item tasks](../05-compilation/03-item-tasks.md) |

Stages 3 and 4 need no other spec. Stages 5 to 8 need [One environment](../05-compilation/02-one-environment.md) to have made a declaration a function of what it reads: a declaration's identities counting from zero for it, and a recompile's invalidation and [Cached verdicts](../../design/soundness/admission/cached-verdicts.md)' per-item argument restated over recorded reads.

## What this builds on

- **The seam.** `Cache` (`curios-pipeline/src/compile.rs`) is `get`, `baseline` and `put(source, unit, followed)`. The fold asks `baseline` on a miss and compiles over what it answers (`compile_unit_over`), or whole where it answers `None`; `put` is handed the unit either way.
- **The store's cache files.** `Verdicts`' `put` serializes the unit, writes its record and the unit as one file renamed into place (`replace`), and places it in the chain. It answers no baseline.
- **The engine's cache files what a build would have.** `Overlaid` (`curios-wonder/src/diagnostics.rs`) believes a stored unit on a re-read through the overlay (`Verdicts::get_overlaid`) and answers the nearest baseline — the session's (`Verdicts::kept`), then the slot's whatever its files now hold (`Verdicts::earlier`), then the scope's offer. On `put` it keeps every unit, files it through the store while its fold is the one a build would have run over the disk, and otherwise places it where something follows. A fold leaves a build's path where the cache hands it a baseline, or where the disk does not hold what it has read (`Verdicts::taken_on_disk`).
- **A record is what was read, and what came before.** A unit's record lists every file its compilation read with a digest of the text parsed from it, taken at `RootSource::reads`, and a digest of what each predecessor contained; a hit re-reads each file (`unchanged`) and compares each predecessor (`chained`).
- **A kept unit is guarded by a log.** `Verdicts::keep` adds what a unit read to the fold's log, and `Verdicts::kept` offers a kept unit only while the units before it read what they read when it was kept.
- **One compile, and one unit from it.** A question's unit comes from the fold a build runs, `compile_unit`: lowered, elaborated, certified by the kernel and erased. Two compilations of one text store the same bytes ([`curios-unit`](../../../curios-unit/README.md#a-unit-is-a-function-of-what-it-was-compiled-from)), which [*a unit's reproduction*](#the-measurements) measures over the standard library.
- **A recompile reuses items as they stand.** `reassemble` (`curios-elab/src/elaborate/module.rs`) takes every item outside the closure from the baseline, with the spans it was elaborated under, and elaborates the closure in a context of its own; `credit_reused` (`curios-text/src/into_core.rs`) joins the baseline's credited binders to the closure's, and `Certification::extended` the baseline's record to the walk's.
- **One fold per subject.** `Asked::every` (`curios-wonder/src/ask.rs`) asks about a package entire as its library and then each program it declares, each over a store handle and a fold of its own.
- **An entry is no unit.** A program's entry and the modules it reaches are checked on top of the fold (`Fold::check`); a build files what they compile to as a payload, and no question reads one.
- **The contract.** Each command's `Contract` (`curios/src/contract.rs`) states how it reaches the store, and a question's access is a build's.

## The gap

**A store a question cannot write goes unsaid.** `Verdicts::refused` keeps why nothing was filed, and a build prints it (`curios/src/pipeline.rs`); `ask.rs` prints no status line and the server has no channel for one, so over a store nobody can write every question compiles everything again and says nothing.

**What is compiled over a baseline is never filed.** After a saved edit a one-shot question recompiles the closure on every run until a build files the unit, and a question never files the package `std` at all, since the archived unit is always offered: 6.56 s a question where a filed unit answers in 0.57, under [*a hit against a recompile*](#the-measurements).

**A unit compiled over a baseline is not the unit a whole compile stores.** Under [*whole against over a baseline*](#the-measurements), counted at `2d9290981`, two whole compiles of one text agree in every part, and a recompiled unit differs from a whole one in three ways, the rest following from them:

- **In definitions the edit never reached.** With one declaration appended to `Nat.crs`, a whole compile of the new text writes other terms for six definitions — `/std/Nat/le/mul_mono_r`, `/std/Nat/Divides/trans`, `add` and `mul`, `/std/Char/to_ascii_upper` and `/std/Str/At/to_after` — where the recompiled unit keeps the baseline's; for the hub, three of its closure are elaborated to other proofs than a whole compile gives them. A bound's proof and an inferred implicit's spelling follow the indices their locals were minted at ([Arithmetic: findings](../04-arithmetic/00-findings.md)), which count the declarations before them. The certifier's record differs for the two of those whose proofs read other lemmas, and the erased arena where an implicit that is kept at run time is spelled in another order.
- **In the positions of what it reused.** Each reused item of the edited file keeps the spans of the text it was elaborated from ([Compilation: findings](../05-compilation/00-findings.md)): 38 items for the leaf, and a unit 1.0 MB larger of 29.7, holding both texts of the file.
- **In the order of its credited binders.** For the hub the text stage's part differs at one size, every piece of it that can be read apart agreeing: a whole compile lists its credited binders in reading order, and a recompile lists the closure's and then the ones the baseline's proofs read (`credit`, `credit_reused`). Read from the code, and stage 6 is where it is confirmed.

Two more are read from the code, no probe meeting either: an item whose binder was renamed is reused under the old name, since the diff reads terms modulo hints (`Term::equal_modulo_metas`); and the record a recompile joins keeps the entry of a declaration the edit removed (`Certification::extended`).

Sharing made canonical as the prelude's image is (`Module::shared`) closes none of it: more items differ after it than before, since a structure's first occurrence gives it its spans.

**Nothing rests on a stored term's position.** A term's equality and hash read no span; the kernel calls `Term::span` nowhere and reports by name; a `Report` holds one position; and of the 84 places `curios-elab` reads a span outside its tests, every one reads the term being elaborated, zonked or erased, the surface term a goal was raised at, or the declaration's own type, domain, body or field. The one walk handed another compilation's term is erasure over a recompile's reassembled module, which would report at a reused item's stale span. The prelude's image already holds each structure at whichever position met it first.

## Prior art

- **gopls** keeps what it derives from type-checking in "gopls' persistent, transactional, file-based key/value store", and names what that bought: "the fast restart, reduced memory consumption, and synergy across processes that were delivered by the v0.12 redesign" ([*Gopls: Implementation*](https://tip.golang.org/gopls/design/implementation)). "Since the cache is persisted across processes", its authors write, "if you run two gopls instances, they work together synergistically" ([*Scaling gopls for the growing Go ecosystem*](https://go.dev/blog/gopls-scalability)).
- **Lean's server builds what the open file imports through the build tool**: its file worker "Uses `lake setup-file` to compile dependencies on the fly and add them to `LEAN_PATH`" ([`SetupFile.lean`](https://github.com/leanprover/lean4/blob/master/src/Lean/Server/FileWorker/SetupFile.lean)), so a dependency is filed as a build files it.
- **rust-analyzer's diagnostics run the build tool in the build's own directory**, and contend with it. The remedy it documents is a directory of its own, which "prevents rust-analyzer’s cargo check and initial build-script and proc-macro building from locking the Cargo.lock at the expense of duplicating build artifacts" ([configuration](https://rust-analyzer.github.io/book/configuration.html)).
- **A store of values keyed by what they were built from is a store of constructive traces** ([*Build Systems à la Carte*](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems-a-la-carte.pdf), §4.2.3), and a slot's record is one: the hashes of its inputs, a predecessor's value among them. Such a store stops a rebuild early where a rebuilt value is unchanged; where a task is one that "can change the order in which unique variables are obtained from the supply, producing different but semantically identical results" (§6.3), the rebuilt value always differs and every successor misses.
- **Go's build identifies a step by its inputs and its result by its content**, and feeds the next step the second: "Separating action ID from content IDs is important for reproducible builds", since "because the content IDs converge, so too do the action IDs" ([`buildid.go`](https://go.googlesource.com/go/+/refs/heads/master/src/cmd/go/internal/work/buildid.go)). A slot and its record are that pair.
- **GHC took the same fault for the same reason.** Its goal is that, given one compiler, sources, flags and packages, GHC "should always produce the same interface files", and the first cause it names is that "The order of allocated Uniques is not stable across rebuilds" ([deterministic builds](https://gitlab.haskell.org/ghc/ghc/-/wikis/deterministic-builds)).
- **A cache that is believed checks that it may be.** rustc "expects that the output is the same as from a prior incremental compilation session", and since 1.52 "checks that the value is indeed as expected, rather than assuming so" ([Rust 1.52.1](https://blog.rust-lang.org/2021/05/10/Rust-1.52.1/)); Dune can "re-execute randomly chosen build rules and compare their results with those stored in the cache" ([Dune caches](https://dune.readthedocs.io/en/stable/reference/caches.html)).

Filing where a build files is what makes one command's work the next one's. What it must not cost is a lock two commands wait on, which a slot renamed into place does not take; and what it needs is that two commands filing one unit write one thing, whichever way each compiled it.

## Decisions

1. **What is filed.** A unit the question compiled whole, while its fold is the one a build would have run over the disk: every unit the fold has taken, this one included, was restored on a record the disk confirms or compiled whole from text the disk holds. The first unit taken any other way — compiled over a baseline, or from text the disk does not hold — ends filing for that fold; it and every unit after it are kept and placed.
2. **The disk decides, not the overlay, for the whole fold.** The test is each record against the disk, the one a later `get` makes. It covers the units before a unit as it covers the unit, since a record names what each predecessor contained. A document open and unchanged files as any other, and a file rewritten under a compile is caught with a document never saved.
3. **The cache knows where its fold left a build's path.** That is where it hands out a baseline, since the fold compiles over the one it is handed, or where the disk does not hold what the fold has read: one fact for the fold, the cache's own, and the seam is as it was.
4. **Every unit the fold takes is kept and logged, filed or not.** The session's baseline and the log a kept unit is guarded by are the fold's: a filed unit left out of the log would let a kept unit after it be offered once that predecessor had changed.
5. **Where.** The store a build files into: beside the governing manifest, or `CURIOS_CACHE`'s shared half. A loose file and standard input open no store.
6. **How far.** The judged unit. A question files no payload: its crates link no runtime.
7. **A one-shot question takes a baseline as a session does.** Over a slot whose files have moved, `lint` compiles the closure. Compiling it whole in order to file it would cost a whole compile of the standard library to anyone editing it.
8. **A store that cannot be written is said.** A one-shot question prints the line a build prints, on standard error, once; the server tells its client once a session, in a `window/logMessage`. The answer and the exit are what they would have been.
9. **What is compiled over a baseline is filed once it is the unit a whole compile stores, and not before.** The claim is one over stored bytes, held by a gate, so that a filed recompile is no new thing to believe: decision 1 then reads without "whole", decision 3 goes with the fact it kept, and a question files whatever the disk confirms.

## Stages

Stage 2, the engine filing what a build would have and the contracts saying so, is landed.

3. **A question says what it could not file.** The engine hands the store's refusal back beside its answer; the command line prints it and the server logs it, once each.
4. **The server.** Its tests and its measurement: a dependency it compiles is filed when it is compiled, a keystroke files nothing, and a whole compile of a unit from disk text files.

After [One environment](../05-compilation/02-one-environment.md), over what it leaves:

5. **An elaborated item is stored without positions.** One function in `curios-unit` writes a unit's stored bytes: it rebuilds the elaborated module without spans, consed in a table of its own, and serializes the unit with that module in place. `Verdicts::placement` and the prelude's build script both call it, and the script's own consing becomes that call. In memory a module keeps its spans, which erasure reports at; a keystroke's unit is neither filed nor followed, so it is never serialized and pays nothing. An item whose binder was renamed is elaborated again without seeding the closure, since the diff reads terms modulo hints and a reused item would keep the old name. The edited file's text is then held once, and equal structures are one node however the unit was compiled.
6. **The parts that are not items agree.** A recompile credits its binders in one pass, in reading order as a whole compile does (`credit_reused` into `credit`); the record it joins holds the definitions the unit declares and no other, where `Certification::extended` keeps a removed declaration's; and whatever *whole against over a baseline* still names is made the same, each difference with its cause stated before it is removed.
7. **The gate.** `curios-pipeline/src/tests/incremental_tests.rs` holds each fixture's unit compiled over a baseline to the stored bytes of the unit compiled whole, where it holds their items to agree today, with fixtures for a declaration moved, a binder renamed and a declaration removed; and *whole against over a baseline* over the standard library, for its leaf and its hub, names no part that differs.
8. **A question files what it compiled over a baseline.** `Overlaid` keeps no account of having handed out a baseline and files whatever the disk confirms, and a question files the package `std`. [A stored unit is a baseline for an item-level recompile](../../design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md), [Cached verdicts](../../design/soundness/admission/cached-verdicts.md) and `curios-verdicts`' `README.md` say what is now filed, and why it may be.

## Verification

Held since stage 2, in `curios-wonder/src/tests/store_tests.rs` unless said otherwise:

- **One unit, whoever files it.** The slots a question files and the slots a build files hold the same bytes (`a_question_and_a_build_file_the_same_bytes`). This is what lets a build believe a question's unit, and it fails if a question's fold ever stops short of a build's.
- A question files what it compiled whole, and the next question and the next build reuse it (`a_question_files_what_it_compiled_whole_from_the_disk`); `run` after `wonder stage` reports the library reused and files a payload over it (`curios/tests/payload.rs`).
- A unit compiled after one from unsaved text, or after one compiled over a baseline, is not filed, and its slot holds the bytes it held.
- A file rewritten while its unit compiles leaves the store as it was.
- A kept unit is refused once a predecessor the store could not take has changed, and what it would have hidden is reported.
- [*A unit's reproduction*](#the-measurements): eight builds, one unit.

Still to hold:

- A store that cannot be written costs the reuse and never the answer: `lint` prints one line on standard error and exits as it would have, and the server logs once. A loose file and standard input leave the store as they found it.
- The server: a dependency it compiles is filed when it is compiled; keystrokes in an open document file nothing and leave every slot's bytes as they were; a unit compiled whole after a saved manifest edit moved its scope is filed, and one compiled over the session's baseline is not.
- [`curios-wonder`'s latency protocol](../../../curios-wonder/README.md#measuring-a-questions-latency), retaken cold and warm, naming the stage; the first compile of a session now serializes each unit it files, and the figure says what that costs.
- Stage 7's gate; and after stage 8, over a text saved since the store was filed, `lint` run twice: the second compiles no unit, and a build after it compiles none either.

## The measurements

Each is taken with a release build without `profile`, over a copy of `curios-text/std` outside the tree — a package named `std`, so a build compiles it whole and a question compiles it over the archived unit.

- **A unit's reproduction**, counted. `curios document --manifest <copy>/curios.toml`, with `<copy>/.curios/verdicts` removed before each run, and the one slot's digest taken after it. The digests are compared; a unit that differs is read back, each of its four parts and each item of its module serialized alone and compared, and a differing definition printed. `curios-pipeline`'s `std_unit_reproduction` takes it in one process.
- **A hit against a recompile**, timed. `curios wonder diagnostics <copy>/Nat.crs`, three readings with the unit filed by the build above and three with `verdicts` removed, the text unchanged in both. At `87189c97d`, on a machine that was not quiet: 0.57 s filed (0.55 to 0.60) and 6.56 s unfiled (6.48 to 7.08).
- **Whole against over a baseline**, counted. In one process, through a `Cache` that answers no hit: the copy compiled whole, one file edited, compiled whole twice more and once over the first unit. The two whole units are compared as the noise, then the whole one with the recompiled one: the text stage's part, the module, the arena and the certifier's record each serialized alone; each item serialized alone, and compared as terms, which reads neither spans nor hints; each definition's totality and reads; and the pieces of the text stage's part that can be read apart. The leaf appends `pub let _probe_1(a: Nat) -> Nat = a;` to `Nat.crs`; the hub respells `Bool.crs`'s one `xor(b, true)` as `xor(true, b)`.
- **What a stored unit costs**, timed. The copy's slot read back, and five rounds of: validating and deserializing its unit, serializing it, digesting the bytes, and consing its elaborated module in a table of its own (`Module::shared`). At `2d9290981`, on a machine that was not quiet, for a unit of 29.7 MB: 506 ms to read (429 to 560), 252 ms to serialize (206 to 256), 46 ms to digest (39 to 53) and 645 ms to cons (477 to 745), which takes the module from 14.08 MB to 13.68.

## Open

- **Whether a build takes a baseline once a recompile is the unit.** The baseline's decision keeps a build whole, trading its speed for trust in the closure; stage 7 is that trust earned for a filed unit, and whether a build then spends it is its own decision.
- **Whether stage 7's gate runs over the standard library on every run.** Compiling the library twice in a debug build takes four minutes, which is why `std_unit_reproduction` is run by hand; a step at the release profile would hold it at a build's cost.
- **Whether a recompile checks its closure against the certifier's reads as it runs.** `Certification` holds what judging each definition read, so a reused item whose reads meet what changed could send the unit to a whole compile. After one environment the invalidation is those reads, and the check would be the invalidation itself; decided then.
- **A question about a program checks its entry every time.** An entry is no unit, so nothing a question compiles of it is filed and nothing a build filed of it answers one.

## Rejected

- **Filing unless an overlay differs.** It asks the overlay a question the record already answers against the disk, and misses the file that changed underneath a compile.
- **Filing whenever the disk confirms every read so far, before a recompile is the unit**: the store would hold slots no build reaches, chained after a unit only a recompile produces, with a filed verdict resting on one the baseline's decision withholds.
- **Marking a slot as compiled over a baseline**, believed by a question and not by a build: the next question after a saved edit would hit, by a second kind of slot, a mistake in a closure kept across sessions until a build, and a mechanism stage 8 deletes.
- **Telling `put` how the unit was compiled**: the rule asks whether the fold was ever handed a baseline, which the cache that handed it knows.
- **Holding the headliner to a fixture alone**, and filing a hash's order as a finding: the claim would be pinned where it already held and false over the one library every program is compiled against.
- **Canonical sharing as what makes a recompile the unit**: it orders what is equal and leaves what differs.
- **Giving a reused item the new lowering's positions** as it is reassembled: an elaborated item and its lowered one are two trees of different shape, and no walk hands one the other's spans.
- **Dropping positions where a unit is assembled**, in memory too: the pass costs 645 ms on the standard library's module where a leaf recompile takes 1.3 s, so every keystroke would pay half again for a unit it never stores.
- **A table shared with the lowered module**, as the prelude's image has: the lowered module keeps its spans, and an elaborated node equal to a lowered one as a term would take it, span and all.
- **A field adapter on the unit's module** in place of a function: `Via` asks its proxy for the stand-in once to serialize and once to resolve, which runs the pass twice a write.
- **A store of the server's own**, which is the second target directory: nothing a build files is the server's, and nothing the server compiles is a build's.
- **Compiling whole on save** so the saved text's unit is filed: a whole compile of the unit the author is editing, on every save, to spare the next build one.
- **A question that stays silent about a store it cannot write.** Every invocation then compiles everything again, and it reads as the compiler being slow.

## Retirement

Done when a question files whatever the disk confirms and says so where it cannot. What stages 3, 4 and 8 change follows the code as it lands: `curios-wonder`'s `ask.rs` on status lines and `server.rs`; `usage.md`'s "Reusing what was already built"; the design decision's rationale and what it rejects; [A stored unit is a baseline for an item-level recompile](../../design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md), [Cached verdicts](../../design/soundness/admission/cached-verdicts.md) and `curios-verdicts`' `README.md` on what is compiled over a baseline; and the finding on a reused item's positions, which stage 5 closes. What *Open* still holds becomes roadmap lines of its own, named from the code. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
