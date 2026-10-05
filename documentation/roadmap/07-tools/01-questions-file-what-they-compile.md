# Questions file what they compile

Working specification for bringing `lint` and the `wonder` queries under [A command compiles only as far as its answer needs, and a project keeps what it compiled](../../design/tools/a-command-compiles-only-as-far-as-its-answer-needs-and-a-project-keeps-what-it-compiled.md): a question files, for a project, each unit it compiled from the text on disk, so the next command starts from the store rather than cold.

**One unit, whoever files it.** The slot a question files holds the bytes a build files there, so over an unchanged disk the second command, whichever it is, compiles no unit. It is one rung of what the decision's goal stands on, that a command costs what changed since anything last compiled the project:

| Rung | States | Lands |
| --- | --- | --- |
| A unit is a function of what it was compiled from | the same bytes from any process | landed: [`curios-unit`](../../../curios-unit/README.md#a-unit-is-a-function-of-what-it-was-compiled-from) |
| One unit, whoever files it | a question's slot is a build's | landed for a unit compiled whole: [`curios-wonder`](../../../curios-wonder/README.md#a-question-files-the-units-it-compiled-from-disk) |
| One unit, however it was compiled | whole or over a baseline, so what is compiled over one is filed | stages 7 and 8 here |
| A successor depends on what it read | an edit stops recompiling every unit after it | [One environment](../05-compilation/02-one-environment.md), [item tasks](../05-compilation/03-item-tasks.md) |

Stage 7's gate over the standard library needs [One environment](../05-compilation/02-one-environment.md) to have made a declaration a function of what it reads: its identities counting from zero for it, without which a proof follows the declarations before it, and a goal answered the same whenever it is asked, without which a recompile settles a universe level a whole compile leaves open. Stage 8 follows its restating of [Cached verdicts](../../design/soundness/admission/cached-verdicts.md)' per-item argument over recorded reads, on top of which it changes who may file.

## What this builds on

- **The seam.** `Cache` (`curios-pipeline/src/compile.rs`) is `get`, `baseline` and `put(source, unit, followed)`. The fold asks `baseline` on a miss and compiles over what it answers (`compile_unit_over`), or whole where it answers `None`; `put` is handed the unit either way.
- **The store's cache files.** `Verdicts`' `put` serializes the unit, writes its record and the unit as one file renamed into place (`replace`), and places it in the chain. It answers no baseline.
- **The engine's cache files what a build would have.** `Overlaid` (`curios-wonder/src/diagnostics.rs`) believes a stored unit on a re-read through the overlay (`Verdicts::get_overlaid`) and answers the nearest baseline — the session's (`Verdicts::kept`), then the slot's whatever its files now hold (`Verdicts::earlier`), then the scope's offer. On `put` it keeps every unit, files it through the store while its fold is the one a build would have run over the disk, and otherwise places it where something follows. A fold leaves a build's path where the cache hands it a baseline, or where the disk does not hold what it has read (`Verdicts::taken_on_disk`). Where the store cannot be written the engine hands its refusal back beside the answer (`Diagnosed::unfiled`), which the command line prints once and the server logs once a session.
- **A record is what was read, and what came before.** A unit's record lists every file its compilation read with a digest of the text parsed from it, taken at `RootSource::reads`, and a digest of what each predecessor contained; a hit re-reads each file (`unchanged`) and compares each predecessor (`chained`).
- **A kept unit is guarded by a log.** `Verdicts::keep` adds what a unit read to the fold's log, and `Verdicts::kept` offers a kept unit only while the units before it read what they read when it was kept.
- **One compile, and one unit from it.** A question's unit comes from the fold a build runs, `compile_unit`: lowered, elaborated, certified by the kernel and erased. Two compilations of one text store the same bytes ([`curios-unit`](../../../curios-unit/README.md#a-unit-is-a-function-of-what-it-was-compiled-from)), which [*a unit's reproduction*](#the-measurements) measures over the standard library.
- **A consed module is one node for each structure as it is spelled, under no position.** `curios_core::Sharing` builds each node over its canonical children and looks it up under the names its scopes bind and the very nodes it holds, so no structure takes another's parameter names and nothing consed says where it was written. The prelude's elaborated modules are archived so, and its lowered ones as they were built, since a lint and a report are located in them, and a slot through `Unit::stored` ([`curios-unit`](../../../curios-unit/README.md#a-stored-unit-says-what-its-items-mean-not-where-they-were-written-or-how-they-were-come-by)).
- **A recompile reuses items as they stand.** `reassemble` (`curios-elab/src/elaborate/module.rs`) takes every item outside the closure from the baseline, with the spans it was elaborated under, and elaborates the closure in a context of its own; `credit_reused` (`curios-text/src/into_core.rs`) joins the baseline's credited binders to the closure's, and `Certification::extended` the baseline's record to the walk's.
- **One fold per subject.** `Asked::every` (`curios-wonder/src/ask.rs`) asks about a package entire as its library and then each program it declares, each over a store handle and a fold of its own.
- **An entry is no unit.** A program's entry and the modules it reaches are checked on top of the fold (`Fold::check`); a build files what they compile to as a payload, and no question reads one.
- **The contract.** Each command's `Contract` (`curios/src/contract.rs`) states how it reaches the store, and a question's access is a build's.

## The gap

**What is compiled over a baseline is never filed.** After a saved edit a one-shot question recompiles the closure on every run until a build files the unit, and a question never files the package `std` at all, since the archived unit is always offered: 6.56 s a question where a filed unit answers in 0.57, under [*a hit against a recompile*](#the-measurements).

**A unit compiled over a baseline is not the unit a whole compile stores.** Under [*whole against over a baseline*](#the-measurements), counted at `2d9290981`, two whole compiles of one text agree in every part, and a recompiled unit differs from a whole one in definitions the edit never reached, the rest following from them; the readings were taken while a reused item's positions stood between the two as well, and are retaken with the probe.

- **In definitions the edit never reached.** With one declaration appended to `Nat.crs`, a whole compile of the new text writes other terms for six definitions — `/std/Nat/le/mul_mono_r`, `/std/Nat/Divides/trans`, `add` and `mul`, `/std/Char/to_ascii_upper` and `/std/Str/At/to_after` — where the recompiled unit keeps the baseline's; for the hub, three of its closure are elaborated to other proofs than a whole compile gives them. A bound's proof and an inferred implicit's spelling follow the indices their locals were minted at ([Arithmetic: findings](../04-arithmetic/00-findings.md)), which count the declarations before them. The certifier's record differs for the two of those whose proofs read other lemmas, and the erased arena where an implicit that is kept at run time is spelled in another order.

**Nothing rests on a stored term's position.** A term's equality and hash read no span; the kernel calls `Term::span` nowhere and reports by name; a `Report` holds one position; and of the 84 places `curios-elab` reads a span outside its tests, every one reads the term being elaborated, zonked or erased, the surface term a goal was raised at, or the declaration's own type, domain, body or field. The one walk handed another compilation's term is erasure over a recompile's reassembled module, which would report at a reused item's stale span where that item is a session's kept one; a stored unit holds none, on a term or on a universe constraint.

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

Stages 2 to 6 are landed, and the fixtures' half of stage 7:

- The engine files what a build would have and the contracts say so, a question says what it could not file, and the server is held to it by its tests and measured.
- **An elaborated item is stored without positions, under the names it was written with.** A slot is written by `Unit::stored`, its elaborated module hash-consed, as the prelude's image is. A universe constraint carries no position where it is stored or instantiated: one instantiated from a scheme is written with none, since where the scheme's declaration raised it is that declaration's and the error it closes is located by the term being elaborated, as every error is on its way out, naming the declaration and binder the constraint came from. The recompile's diff reads binder names, since a dependent's elaborated term carries the names of a signature it was handed and a reused one would keep the old.
- The parts of a unit that are not items are stored as a whole compile stores them: its credited binders a set, ordered by declaration and place whichever pass credited them, and the record a recompile joins holding the definitions the new text declares and no other.
- `curios-pipeline/src/tests/incremental_tests.rs` holds each fixture's unit compiled over a baseline to the stored bytes of the unit compiled whole.

After [One environment](../05-compilation/02-one-environment.md):

7. **The gate, over the standard library.** *Whole against over a baseline*, for its leaf and its hub, names no part that differs: whatever it still names is made the same first, each difference with its cause stated before it is removed.
8. **A question files what it compiled over a baseline.** `Overlaid` keeps no account of having handed out a baseline and files whatever the disk confirms, and a question files the package `std`. [A stored unit is a baseline for an item-level recompile](../../design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md), [Cached verdicts](../../design/soundness/admission/cached-verdicts.md) and `curios-verdicts`' `README.md` say what is now filed, and why it may be.

## Verification

Held since stage 2, in `curios-wonder/src/tests/store_tests.rs` unless said otherwise:

- **One unit, whoever files it.** The slots a question files and the slots a build files hold the same bytes (`a_question_and_a_build_file_the_same_bytes`). This is what lets a build believe a question's unit, and it fails if a question's fold ever stops short of a build's.
- A question files what it compiled whole, and the next question and the next build reuse it (`a_question_files_what_it_compiled_whole_from_the_disk`); `run` after `wonder stage` reports the library reused and files a payload over it (`curios/tests/payload.rs`).
- A unit compiled after one from unsaved text, or after one compiled over a baseline, is not filed, and its slot holds the bytes it held.
- A file rewritten while its unit compiles leaves the store as it was.
- A kept unit is refused once a predecessor the store could not take has changed, and what it would have hidden is reported.
- [*A unit's reproduction*](#the-measurements): eight builds, one unit.
- A store that cannot be written costs the reuse and never the answer: the engine names the refusal beside its answer, `lint` prints one line on standard error and reports and exits as it would have (`curios/tests/lint.rs`), and the server logs once and publishes as it would have (`curios/tests/wonder.rs`).
- A question about a loose file or standard input writes nothing beside it (`a_loose_file_and_standard_input_leave_no_store`, `curios/tests/wonder.rs`).
- A session files a dependency when it compiles it and nothing for a keystroke, every slot holding the bytes it held (`a_session_files_a_dependency_and_no_keystroke`); a unit it compiles whole after a saved manifest edit moved its scope is filed where the new scope addresses it (`a_unit_compiled_whole_in_a_new_scope_is_filed_there`); and over the wire an opened document leaves its library in the store, an edit leaves the store as it was, and a build afterwards reuses the unit (`the_server_files_a_unit_from_the_disk_and_nothing_for_a_keystroke`, `curios/tests/wonder.rs`).
- [*A session's first check*](#the-measurements): 0.65 s from a store holding the unit where compiling it takes 5.3 s, what filing costs that compile inside the readings' spread.
- Over a baseline as whole, in `curios-pipeline/src/tests/incremental_tests.rs`: a unit's credited binders are stored alike whichever pass credited them (`credited_binders_are_stored_alike_whichever_pass_credited_them`), and the record holds no entry for a declaration the text dropped or renamed (`a_removed_item_is_gone_and_the_rest_is_reused`, `a_renamed_item_leaves_no_entry_under_its_old_name`).
- A consed term is one node for each structure as it is spelled, built over its canonical children, under no position (`curios-core/src/term/consing_tests.rs`), and a prelude signature is reported under the names it was written with (`curios-pipeline`'s `a_prelude_signature_is_reported_under_the_names_it_was_written_with`).
- A unit compiled over a baseline is stored as a whole compile stores it, byte for byte, in every fixture of `curios-pipeline/src/tests/incremental_tests.rs` that compiles one both ways (`assert_stored_alike`): nothing changed, a body edited, a declaration removed or moved, a polymorphic item inserted, a struct edited, a field's mark or a family's variance moved, every item changed, and a parameter renamed, the rename reaching the dependent that was handed its signature. A renamed binder is another text to the diff (`a_renamed_binder_is_another_text_modulo_metas`, `curios-core`).
- A constraint instantiated from a scheme carries no position (`a_constraint_instantiated_from_a_scheme_carries_no_position`, `curios-elab`), and a consed universe context none (`a_universe_context_is_consed_without_its_positions`, `curios-core`).

Still to hold:

- Stage 7's gate over the standard library; and after stage 8, over a text saved since the store was filed, `lint` run twice: the second compiles no unit, and a build after it compiles none either.

## The measurements

Each is taken with a release build without `profile`, over a copy of `curios-text/std` outside the tree — a package named `std`, so a build compiles it whole and a question compiles it over the archived unit.

- **A unit's reproduction**, counted. `curios document --manifest <copy>/curios.toml`, with `<copy>/.curios/verdicts` removed before each run, and the one slot's digest taken after it. The digests are compared; a unit that differs is read back, each of its four parts and each item of its module serialized alone and compared, and a differing definition printed. `curios-pipeline`'s `std_unit_reproduction` takes it in one process.
- **A hit against a recompile**, timed. `curios wonder diagnostics <copy>/Nat.crs`, three readings with the unit filed by the build above and three with `verdicts` removed, the text unchanged in both. At `87189c97d`, on a machine that was not quiet: 0.57 s filed (0.55 to 0.60) and 6.56 s unfiled (6.48 to 7.08).
- **Whole against over a baseline**, counted. In one process, through a `Cache` that answers no hit: the copy compiled whole, one file edited, compiled whole twice more and once over the first unit. The two whole units are compared as the noise, then the whole one with the recompiled one: the text stage's part, the module, the arena and the certifier's record each serialized alone; each item serialized alone, and compared as terms, which reads neither spans nor hints; each definition's totality and reads; and the pieces of the text stage's part that can be read apart. The leaf appends `pub let _probe_1(a: Nat) -> Nat = a;` to `Nat.crs`; the hub respells `Bool.crs`'s one `xor(b, true)` as `xor(true, b)`.
- **What a stored unit costs**, timed. The copy's slot read back, and five rounds of: validating and deserializing its unit, serializing it, digesting the bytes, and consing its elaborated module in a table of its own (`Module::shared`). At `2d9290981`, on a machine that was not quiet, for a unit of 29.7 MB: 506 ms to read (429 to 560), 252 ms to serialize (206 to 256) and 46 ms to digest (39 to 53). A slot is stored through the consing since, and the pass timed at that commit shared whole repeated terms alone, so what consing costs, what it leaves of a slot and what reading one back then costs are retaken with the probe.
- **A session's first check**, timed. [`curios-wonder`'s latency protocol](../../../curios-wonder/README.md#measuring-a-questions-latency) over its sized package at 720 declarations: `M0.crs` opened with the text on disk, then two changes each appending a declaration, in four kinds of session. One is the compiler at `2d9290981`, which files nothing, over a copy of the package of its own, so that neither compiler rewrites the other's `compiler` memo. Three are the compiler at `3ec7b4dad`: with `.curios/verdicts` removed first, with the store as that session left it, and with a file where `.curios/verdicts` would be. Five rounds of the four in that order, after one unmeasured session of each compiler. On a machine that was not quiet, in seconds, each a median with its span:

  | | Files nothing | Store empty | Store holding the unit | Store unwritable |
  | --- | --- | --- | --- | --- |
  | open | 6.50 (4.17 to 7.66) | 5.34 (3.88 to 7.32) | 0.65 (0.37 to 0.99) | 6.09 (4.07 to 6.78) |
  | first change | 1.15 (0.87 to 1.45) | 1.34 (0.89 to 1.50) | 2.58 (1.39 to 2.99) | 1.11 (0.79 to 1.53) |
  | second change | 0.94 (0.55 to 1.47) | 1.24 (0.70 to 1.25) | 1.22 (0.78 to 1.47) | 0.92 (0.75 to 1.41) |

  The first change after an open the store answered reads the slot a second time, as its baseline ([Tools: findings](00-findings.md)).

## Open

- **Whether a build takes a baseline once a recompile is the unit.** The baseline's decision keeps a build whole, trading its speed for trust in the closure; stage 7 is that trust earned for a filed unit, and whether a build then spends it is its own decision.
- **Whether stage 7's gate runs over the standard library on every run.** Compiling the library twice in a debug build takes four minutes, which is why `std_unit_reproduction` is run by hand; a step at the release profile would hold it at a build's cost.
- **Whether a recompile checks its closure against the certifier's reads as it runs.** `Certification` holds what judging each definition read, so a reused item whose reads meet what changed could send the unit to a whole compile. After one environment the invalidation is those reads, and the check would be the invalidation itself; decided then.
- **A question about a program checks its entry every time.** An entry is no unit, so nothing a question compiles of it is filed and nothing a build filed of it answers one.
- **What a session's kept unit does with the positions it holds.** Stage 5 drops them where a unit is stored, so a unit read back from a slot has none and a unit a session keeps has the ones it was elaborated with, a reused item's ageing with every keystroke above it ([Compilation: findings](../05-compilation/00-findings.md)).

## Rejected

- **Filing unless an overlay differs.** It asks the overlay a question the record already answers against the disk, and misses the file that changed underneath a compile.
- **Filing whenever the disk confirms every read so far, before a recompile is the unit**: the store would hold slots no build reaches, chained after a unit only a recompile produces, with a filed verdict resting on one the baseline's decision withholds.
- **Marking a slot as compiled over a baseline**, believed by a question and not by a build: the next question after a saved edit would hit, by a second kind of slot, a mistake in a closure kept across sessions until a build, and a mechanism stage 8 deletes.
- **Telling `put` how the unit was compiled**: the rule asks whether the fold was ever handed a baseline, which the cache that handed it knows.
- **Holding the headliner to a fixture alone**, and filing a hash's order as a finding: the claim would be pinned where it already held and false over the one library every program is compiled against.
- **Consing alone as what makes a recompile the unit**: it makes one node of what is spelled alike and leaves what the two compilations concluded differently.
- **Consing a stored unit up to α**: a signature is then reported under the parameter names of another declaration.
- **Keeping a stored constraint's position**: a reused item's is the old text's, and stage 7 could not hold.
- **Dropping a constraint's position in the stored form and nowhere else**: a scheme elaborated in this compile would keep the position its declaration raised it at and one read from a slot would have none, so a universe inconsistency would be located by how its unit was compiled.
- **Stamping a constraint with the position of the use that instantiated its scheme**: the solver is reached through six helpers with thirty callers, eight of them in conversion and erasure where no written occurrence is in hand, for a position the term being elaborated already gives an error on its way out.
- **Giving a reused item the new lowering's positions** as it is reassembled: an elaborated item and its lowered one are two trees of different shape, and no walk hands one the other's spans.
- **Dropping positions where a unit is assembled**, in memory too: the pass builds a node for every node of the module, where a leaf recompile of the standard library takes 1.3 s, so every keystroke would pay it for a unit it never stores.
- **A table shared with the lowered module**: the lowered module keeps its positions, and a consed node holds none, so the two share nothing a table could keep.
- **A field adapter on the unit's module** in place of a function: `Via` asks its proxy for the stand-in once to serialize and once to resolve, which runs the pass twice a write.
- **A store of the server's own**, which is the second target directory: nothing a build files is the server's, and nothing the server compiles is a build's.
- **Compiling whole on save** so the saved text's unit is filed: a whole compile of the unit the author is editing, on every save, to spare the next build one.
- **A question that stays silent about a store it cannot write.** Every invocation then compiles everything again, and it reads as the compiler being slow.

## Retirement

Done when a question files whatever the disk confirms and says so where it cannot. What stage 8 changes follows the code as it lands: `usage.md`'s "Reusing what was already built"; the design decision's rationale and what it rejects; [A stored unit is a baseline for an item-level recompile](../../design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md), [Cached verdicts](../../design/soundness/admission/cached-verdicts.md) and `curios-verdicts`' `README.md` on what is compiled over a baseline; and the [finding](../05-compilation/00-findings.md) on a reused item's positions, which stage 5 closes for a unit read back from a slot. What *Open* still holds becomes roadmap lines of its own, named from the code. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
