# A profile is a fact about the program, not about the machine

## Status

**Question 0 has landed, and so has the removal.** `wonder cost` reports what became of each declaration; `curios profile` is gone and profiling is a property of the build. What remains unrefined is questions 1 and 2, which is what keeps this specification open — when one of them refines, this file becomes `profiling/01`, `02` and `03`, and the thesis under [The hinge](#the-hinge) graduates to `documentation/design/toolchain/` where a landed item's cross-cutting rationale belongs.

Nine peer toolchains were surveyed for what they profile and in what units; where one is precedent it is cited where it applies.

## Why it exists

`curios-profile` profiles the compiler. Nothing profiles a Curios program. A user whose program is slow, and an author whose prelude build moved from 22.8s to ~600s, have the same recourse: an external sampler, a wall-clock flame graph over `func/N$hint` symbols, and a guess.

That recourse is unusable for the question being asked, on three counts. It measures the machine, so no two runs agree and nothing is diffable. It reports where *time* went, when every cliff in the roadmap is about what a spelling *builds*. And it cannot see the compilation, so the most common cause — an optimization that did not fire — is invisible in the one artifact that could name it.

Meanwhile the compiler already prices every construction it performs, in a unit no target can move, and already computes what each declaration spent. It publishes the maximum, unnamed.

## The hinge

`curios-core`'s `cost`: "charging before allocating is the rule that makes the bound mean anything — a charge taken afterwards is a report, not a limit."

A profile is that report. The budget is the cost model used to **refuse**; a profile is the same model used to **explain**. This specification adds no second price list and introduces no unit the budget does not already denominate.

## The three questions

They are separate because their answers come from different places, cost differently, and are useful independently. A user asks them in this order, and they land in this order.

**0. Which cliff am I on?** Did the optimization fire? Answered from the compilation alone — no execution, no counters, no perturbation. `curios-cont`'s optimizer already decides inlining, contification, specialization, worker/wrapper splitting and window virtualization by name, and discards every decision.

**1. What did checking it cost?** `Consumption` per declaration — units and peak depth, in the budget's own unit, machine-independent, reproducible, and priced identically by both checkers by design.

**2. How often did it happen?** The compiler knows the price of every site; one execution supplies the counts. Counts, never durations: a count is a fact about the program and its input, and a duration is a fact about the machine.

## The unit is the declaration, and that is free

Curios has no loop syntax. Every repetition is a recursive `let`, so the declaration boundary the budget already restores at is also the loop boundary a profile needs. Nothing is annotated, nothing is marked, and no build mode differs from the one that ships.

This is what makes late attribution unnecessary rather than merely deferred. GHC needed `-fprof-late` because cost centres inserted before the optimizer changed the program being measured; MLton, Erlang and Racket each pay a version of the same tax. A profile with no annotations perturbs nothing, so the program profiled is the program that runs.

The residue is honest and is reported as such: after Ersd and Cont a source declaration may have been inlined away, split, cloned per call pattern, or contified. The report says which. **A declaration that vanished is the most useful row in the table**, not a gap in it.

## What question 0 reports, and what it refuses to

**Fate** — what became of the declaration: survives as a function, inlined into some number of sites, contified, specialized some number of ways, worker/wrapper split, pruned unreachable, folded away. Each is a named pass in `curios-cont`'s `cps/optimize.rs` that already decides it.

**Carried cost** — for whatever survived: allocation sites and whether any sits inside a recursive component, calls that stayed indirect, host calls, whether its scalars are unboxed locals, and whether it is a tail loop.

**Not in the report:** any sentence containing *should*, *expensive*, or *consider*. A row saying "allocates one per iteration; calls through an unknown function" is a fact the author acts on. A row saying that this is bad is a heuristic, and heuristics are what drag in levels, categories and suppression — the apparatus [a lint is an exact finding read off the compilation](../design/toolchain/a-lint-is-an-exact-finding-read-off-the-compilation.md) spent a paragraph refusing.

A judgment is admissible only where a shape is exactly a cliff with no counterexample — an indirect call inside a recursive component is the candidate, since [that cliff is still open](known-function-specialization-spec.md). That is a **lint**, held to the same always-on, nothing-to-configure bar, and argued separately rather than smuggled in here.

## Reproducibility is the deliverable

Same program, same input, same profile — on every host, in the playground, in continuous integration, next year. This is not a side effect; it is why the units are the budget's. A profile that reproduces can be committed, and a regression becomes a diff rather than a judgment.

Precedent, and where it stops. Lean's heartbeats are deterministic but count small-object allocations, so a few large objects are cheap in them; Rocq measures allocation volume in machine words, which cannot cross native and wasm32. Curios's logical unit resolves both, and [a reduction step costs what it builds](../design/toolchain/a-reduction-step-costs-what-it-builds.md) records why.

## Where time is admitted

As a falsifier, never as an instrument. The ledger says where the work is; a sampler says where the time went. They are expected to agree, and a **disagreement is the finding** — most usefully where the ledger is blind by construction, which is collection behavior. The existing perf-map path under `curios-runtime`'s `profile` feature is that sampler; it needs a documented recipe and no code.

## What is removed

`curios profile` — the subcommand, its `--out` and `--cap` flags, its dispatch arm and `summarize()`. It profiles the *compiler*, so a `Mode` variant for it in the user's command enum is a category error, and the boundary this specification draws makes that plain: **`wonder cost` answers about the program, `cargo x profile` answers about the compiler, and nothing in `curios` answers about the compiler.**

It is replaced by nothing. Under `profile`, `curios`'s `main` installs a process-lifetime subscriber beside the `#[global_allocator]` already there — two `#[cfg]` items, no command surface. A `tracing` global default composes with a thread-local one, so the two callers that need a scoped capture — `curios-prelude-archive`'s build script and `curios`'s fixpoint probe — keep working unchanged.

**The stream is filed, not piped.** `.artifacts/profile.tsv` beside the crate that wrote it, because a hung run is the case profiling exists for and a piped stream dies with the interrupt — already recorded in `curios-profile`'s README and in the prelude build script's own comment — and because the output is read after the run that made it, which is CLAUDE.md's rule for the directory. It also puts both callers on one mechanism, so `Destination::Rotating` stops being machinery for a single caller.

**The path is derived, spelled once, and never quoted in prose.** `concat!(env!("CARGO_MANIFEST_DIR"), "/.artifacts/profile.tsv")` — the pattern `xtask`'s `root()` already uses — behind a `curios-profile` macro whose `env!` expands at the caller's site, exactly as `profile!` expands `$crate::tracing::…` and asks nothing of the caller. Documents name the convention rather than the literal, and the run announces the real path on exit as the build script already does. `.gitignore`'s `.artifacts/` entry is unanchored, so nothing is added there.

`cargo x profile` keeps its name, its arguments and its default subject; it drops the `profile` argument it passes through, gains the fold, and derives the path the way it derives every other path. **Every subcommand becomes profileable** — `run`, `test`, `document`, a package build — where today only a synthetic one-file compile is. The removal is a capability gain.

**Cost, stated.** `curios-profile`'s README says there is "no environment-variable switch, no process-global subscriber, and no metrics API". That sentence is amended. Its rationale survives — "a measurement is already specified at its call sites, and a second, out-of-band specification could only disagree with the first" — because a compile-time feature and a derived path are not a second specification. The README already grants a permanently installed process-global for the `log` facade where that facade forces one; this is the same accommodation for the same reason.

**Condition.** `env!` bakes in the builder's tree, so a profile build is meaningful only in the checkout that produced it. That is acceptable because profiling is only ever run with this repository in hand, and the feature gate is what guarantees the path never reaches a shipped binary.

**Landed**, first and on its own commit, as it said it would. One thing it turned up that the plan did not have: `install` may not announce where it wrote. `curios/tests/lint.rs` asserts that a lint run narrates nothing on stderr, and under `--all-features` every spawned `curios` would have narrated. The announcement moved to `cargo x profile`, which derives the same path — a better place regardless, since the reader is what should name what it read.

## The routes for question 2

**Emit counters into the compiled program.** Curios owns the emitter, so a per-site counter is a lowering-level rewrite under a build mode, read out at exit. Deterministic and complete. It costs a second emission path, and a program measurably slower than the one shipped.

**Instrument through Binaryen.** `--log-execution` exists and Curios links the library. It costs widening `curios-binaryen`'s interface, which is one `optimize` entry point rather than a pass runner, and a host call per function entry — not small on a wasm-GC program.

**Sample, and multiply by the price list.** Cheapest, and the only route that is not deterministic, which forfeits the property this specification exists for.

**Do not answer question 2 yet.** Ship 0 and 1 and find out how often a count is still wanted. Both are cheaper, neither needs a run, and 0 is believed to answer most of what sends a user to a profiler.

## What is certain

- **A hint traces to exactly one source declaration, and it is the qualified one.** Erasure names every function it lifts after the declaration it descends from, and derives `owner/n` for an anonymous one — `curios-elab`'s `Lowering::derived_hint` states it and names a profile as the reader that pays for an absent hint. The name survives the optimizer: `curios-cont`'s clone and inline paths both copy `debug_name` onto the copy. Measured on `programs/parse_digits.crs`, the rows read `/std/Str/fold`, `/std/Str/step`, `/std/Str/trim_bounds/1`.
- **Fate needs no instrumentation, because the driver already observes both sides.** `curios-pipeline` emits `Stage::Cont` and `Stage::ContOptm` around one call to the optimizer, once per compilation, so a consumer that counts declarations on each side learns every fate from the difference. No pass records anything, and the program measured is the program that ships. This is what closed the question of which decisions are per-site and which per-pass: for a fate, neither.
- **What a difference cannot separate is inlining from pruning.** Both remove a name, so both report as absorbed. Separating them needs the passes to say which, and nothing here is worth that.
- **All three outcomes occur.** On `programs/hello_world.crs`, 36 of 43 rows are absorbed, 6 survived, and `io/pure` is specialized 7 ways; on `programs/parse_digits.crs` the same declaration is specialized 20 ways. A report is dominated by absorption, which is the finding: most of what a program names costs nothing of its own.
- **No span survives below Core.** Neither `curios-ersd` nor `curios-cont` mentions one, which is why the unit of attribution is the declaration and not a position. Identity survives instead, as [one naming scheme for compiler identities](../design/toolchain/one-naming-scheme-for-compiler-identities.md) states.
- **Curios has no loop syntax.** No `for`, `while` or `loop` in [syntax.md](../syntax.md), so every repetition is a recursive `let` and per-declaration attribution is per-loop attribution.
- **A test run shares one stream.** Under `--all-features` every `curios` the integration suite spawns installs the recorder and files the same path, so what is left there is many processes interleaved. Nothing reads it and a failed write is dropped, so the suite is unaffected — recorded in `curios-profile`'s README rather than engineered around.

Still owed, by question 1:

- **The per-declaration ledger exists and is discarded.** `curios-elab`'s `Context::consumed` computes the units spent and the peak depth reached, and `curios-cert`'s `Spend::consumed` computes the kernel's. `heaviest_declaration` folds by `heavier_of` and keeps no name. *Owes: that the value is live at every budget-restoring boundary, not only at module end.*
- **Cost has a taxonomy, already declined for this use.** `Cost`'s `Category` names eight rows with display strings, and the `Cost` handed to `spend` already carries one — it is what builds the refusal. `cost`'s module documentation states that dominance "is deliberately not promised" and would need "cumulative per-category accounting". The accumulator is one indexed add on a path that already branches, subtracts and stores. **It belongs in `curios-elab`'s `Context` alone**: a user's compile time is spent in the elaborator, and the kernel's copy is a separate question with a different justification — the cost-parity invariant has been violated three times, and was found by bisection each time. *Owes: the measured cost of the accumulator on the elaborator's hot path.*
- **Guest sampling works and is undocumented.** `curios-runtime`'s shared engine selects a perf map under `profile`, and `curios-wasm`'s writer emits the full name section. *Owes: one recipe.*

## What has to be decided

- **Whether question 0 alone is the deliverable.** It has landed; the question is now whether using it makes 1 and 2 wanted, which is answered by living with it rather than by arguing about it.
- **Whether carried cost joins fate.** A row says what became of a declaration and not yet what the survivors carry — allocation sites, calls that stayed indirect, membership of a recursive component. `curios`'s codegen census already computes a version of this over a fixed corpus; what of it is corpus-specific is the open part.
- **Whether the elaborator's per-category accumulator is affordable** at its measured cost, and whether the kernel's follows it.
- **Whether an indirect call inside a recursive component is a lint**, held to the exactness bar the lint decision sets. It is the one shape that is a cliff with no counterexample, and the report deliberately does not say so.
- **Whether counts ever arrive**, and by which of the four routes.

## Deliberately not specified

Wall-clock sampling as an instrument rather than a falsifier. Any viewer, interface, or format that needs a tool to read. Environment-variable configuration of anything. Source-position attribution below Core, which needs spans that do not exist and is a campaign of its own. What the collector costs, which is [The survivors are what cost](collector-economics-spec.md)'s. Per-fiber attribution under `/std/Async`, which needs a decision about what a fiber's cost means before it needs a mechanism.
