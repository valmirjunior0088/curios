# Cached user modules

The fixed prelude reaches every stage as an *environment* rather than as items copied into the unit being compiled: `Globals` at the certifier, `Established` at elaboration, `Resumed` at erasure, `Scoped` at the lowerer. Nothing about those types says "prelude" — they say "what is already in scope" — so caching a *user* module's elaborated Core and its verdict is the extension the shape was built for. The environment is assembled from N modules rather than one, and there is no seam to identify because there was never a prefix.

This was the last milestone of the prelude-environment specification, now retired; everything it established is recorded in [DESIGN.md](../../DESIGN.md), "A module is a compilation unit, and the prelude is an environment", and in the crate documentation of `curios-cert`, `curios-analysis`, `curios-elab`, `curios-text` and `curios-prelude-archive`.

## What is already true

Every stage takes a scope it does not own, and each of those constructors already accepts *a* module rather than *the* prelude. Widening them to many is a change of arity, not of design — which is the whole return on the work that preceded this.

`check_definition` and `check_rec_group` return before their `define` step, so an item enters the environment whether or not it checked. The environment is therefore a pure function of the module, which is what makes a cached one replayable.

## What is not

**Identities must survive splicing independently elaborated modules.** Binder, universe, metavariable and witness identities are monotonic counters seeded from watermarks. Two modules elaborated separately mint from overlapping ranges, so composing them can alias — and an aliased binder silently identifies two terms that differ. [SOUNDNESS.md](../../SOUNDNESS.md)'s *Binder identity* row is graded **argued**, with a single positive control. That grade is adequate for one prelude whose floor is carried and combined by maximum; it is not adequate for N modules composed pairwise. **This row must be defended better before any module is cached**, and that is the first task here, not a later one.

**Verdicts must be keyed on the terms and the certifier, not on the file.** GHC fingerprints each declaration's interface and recompiles what actually depends on what changed; Cargo's granularity is the crate, which is what made a kernel edit re-elaborate the standard library until the crate split. A cache keyed on anything coarser than the exact terms plus the certifier's own version reintroduces that failure at a level where it admits rather than merely costs time.

**Anything reasoning over "every name the program has" becomes a scope question.** Retiring the splice broke two such sites — strict positivity's declaration set, and `build_shorten`'s abbreviation table, which also starved `nominal_plicities` beside it. **Both were found by a test rather than by inspection**, and a third is not ruled out. Before caching multiplies what "in scope" can mean, search for `module_symbols`, `nominal_plicities`, bare `items.iter()` and registry iteration, and decide each one deliberately.

## Out of scope

- **Whole-module passes never cache.** Strict positivity over the complete declaration set, declaration sizing, and witness coherence are program-wide by definition — a coherence violation is only visible where two modules meet. They re-run at link. That bounds the win without removing it, since per-item typing is the expensive part.
- **Parallelising elaboration.** The shared monotonic counters are a serialization point by design.
- **Making the archive a stable interchange format.** It stays scoped to one compiler build.

## Deferred, with its reasoning

**Parallel per-item certification**, which was M6. Split the walk into a serial define-all phase and a parallel check-all phase, one `Kernel` per item over a shared read-only environment, verdicts sorted by item index for determinism. Per-item kernels make the data independent and settle binder identity without arithmetic: each is seeded at the same derived floor, above every identity in the module, so two workers minting the same index never share a scope. A shared counter is ruled out — nondeterministic under work stealing, and the archive must stay byte-reproducible. Any parallelism must be feature-gated native-only, because `curios-web` compiles `curios-pipeline` and therefore `curios-cert` to `wasm32-unknown-unknown`, which has no threads.

**Parked on 2026-08-09, on measurement.** After the crate split, certification is the whole cost of a kernel-edit rebuild rather than 18% of a full one, so parallelising it would act on all of that loop — but the loop is ~100 s, and the win is perhaps 60–70 s of it. Against that: per-item kernels, a native-only feature gate, a memo-cost measurement, determinism obligations, and **concurrency inside the trusted base**, where *parallel verdicts equal serial verdicts* becomes something to prove rather than assume.

The same day's profiling put 469 s of a ~570 s prelude build in elaboration, of which 204 s is universe finalization — and 63% of *that* is one declaration, `/std/Async/block_on`, whose constraint graph is three orders of magnitude larger than any other. That work sits outside the trusted base and may be contained inside `finalize`. M6 is worth revisiting if the kernel loop ever hurts again; it is not worth adding threads to the certifier while the larger cost is elsewhere and cheaper.

## Retirement criteria

Before this specification is deleted: *Binder identity* is defended beyond **argued** in [SOUNDNESS.md](../../SOUNDNESS.md), with the composition of independently elaborated modules probed rather than reasoned about; the cache's keying discipline — terms plus certifier version, never a path or a timestamp — is recorded wherever the cache lives; and the audit of "every name the program has" sites is carried out and its result recorded, so the next person does not rediscover it from a failing diagnostic test.
