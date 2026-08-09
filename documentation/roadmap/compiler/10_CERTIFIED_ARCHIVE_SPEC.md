# Certified archives — separating the image from the verdict

This document specifies splitting the fixed prelude's build product in two — the elaborated *image*, and the kernel *verdict* over it — so that each is invalidated by what it actually depends on; then the two capabilities that split makes reachable, parallel certification and cached user modules. It is an umbrella: the first part is a refactor with a measurable payoff, the last is a language capability that does not exist and is not otherwise on the roadmap.

## Problem

`curios-prelude`'s build script does two separable things in one product. It elaborates `/std` and `/syn` into the Text/Core/Ersd image and serializes it — needing `curios-text`, `curios-elab`, `curios-ersd` — and it runs `recheck_module_verdicts` over the result and fails the build on any refusal, needing `curios-cert`. Cargo's invalidation granularity is the build script, so a change to **either** dependency set re-runs **both** halves.

The cost lands on exactly the workflow the trusted base needs most. A soundness hunt edits `curios-cert` continuously, and every edit re-elaborates the entire standard library to produce a byte-identical image. Measured on this workspace: `target/debug/.fingerprint` holds **13 distinct build-script fingerprints**, each with its own 7.3 MiB archive, and the only fingerprint inputs that vary are `deps`, `features`, and `rustflags` — `rustc`, `profile` and `target` never do. The `features` and `rustflags` axes are the local gate colliding with itself and are fixed in `CLAUDE.md` without code. The `deps` axis is this document.

## Constraints, verified

- `curios-cert` is used by the build script in exactly **two** places: `recheck_module_verdicts`, and `derived_binder_floor`. It is a `[build-dependencies]` entry only — the `curios-prelude` *library* does not depend on it at all.
- `recheck_module_verdicts` **produces nothing that enters the image**. It panics or returns; its result is not serialized. The image is a function of (`.crs` sources, `curios-text`, `curios-elab`, `curios-ersd`, `curios-core`); the verdict is a function of (the image's `core`, `curios-cert`). The two are disjoint.
- `derived_binder_floor` is a walk computing a maximum over `Free::local_index()`. It reaches no `Kernel` and no `Env`, so by `curios-cert`'s own stated criterion — a property of the data is read once, a question that needs a procedure is answered twice — it belongs in `curios-core`, exactly as `UniverseContext::is_closed` does. Moving it is justified independently of this document and is a prerequisite for it.
- Caching *inside* the existing build script cannot work. Cargo rebuilds the script binary whenever any build-dependency changes, and from inside the script there is no input that distinguishes "`curios-elab` changed" from "`curios-cert` changed". Recorded so it is not re-attempted.
- The invariant that must survive is stated in `curios-cert/src/recheck.rs` and relied upon by [SOUNDNESS.md](../../SOUNDNESS.md)'s *Prefix identification* row: an archive that exists is one whose every item the kernel accepted, which is what lets `compile_entrypoint` define the archived prefix without judging it.
- `curios-web` compiles `curios-pipeline`, and therefore `curios-cert`, to `wasm32-unknown-unknown`. That target has no threads, so any parallelism added to the certifier must be feature-gated native-only, with the sequential path remaining the one wasm32 builds.

## Prior art

Coq/Rocq ships this split as `-vos`/`-vok`. `coqc -vos` produces a `.vos` carrying everything except opaque proofs; `coqc -vok` checks the proofs and emits a `.vok` with **empty contents**, a placeholder whose existence means the file compiled. The design below is that shape, with the verdict carried by a crate's successful build rather than a marker file.

Two lessons transfer. Coq's stage 2 parallelises across files — `make vos` then `make -j vok` — because checking one file's proofs depends on other files' *statements* only, which is the same independence the design below exploits per item. And Coq's documented cost, that `-vos`/`-vok` typechecks every definition twice because stage 2 re-reads source, is one this design does **not** pay: the verdict half reads the serialized image, so elaboration happens once.

Lean's `.olean` takes the other branch — imports are trusted, and re-verification is an opt-in external pass (`lean4checker`) that replays declarations through the kernel. That is a coherent posture and it is not this project's: here the compile path runs the kernel on every build. Its documented weakness is instructive anyway, since `lean4checker` reads `.olean` files without validating their format; the archive's schema, source fingerprint, and bytecheck on restore are what stand in that place here.

GHC's recompilation avoidance is the model for the user-module step: fingerprint each interface **and each declaration within it**, record the fingerprints of everything a module used, and recompile only when what you actually depend on changed — rather than when the file did.

## Design

**M1 — move `derived_binder_floor` to `curios-core`.** Independently justified, and it leaves `recheck_module_verdicts` as the build script's only use of `curios-cert`.

**M2 — measure before splitting.** The payoff is `elaborate / (elaborate + recheck + erase + serialize)`. The build script already runs under `curios_profile::capture` behind its `profile` feature and writes `OUT_DIR/profile.tsv` with per-span timings and allocation columns, so the ratio is one build away. If the walk dominates, M3 buys a crate and little else, and this document should say so and stop.

**M3 — the split.** A new crate `curios-archive` owns elaboration and the image, with build-dependencies `curios-text`, `curios-elab`, `curios-ersd`, `curios-core` and no certifier. `curios-prelude` **keeps its name, its public API, and every downstream dependency**, and gains a build script whose only job is to restore the image and run `recheck_module_verdicts`, with `curios-archive` and `curios-cert` as its build-dependencies. Nothing downstream of `curios-prelude` changes.

Naming: the repository already calls this object the archive — `PreludeArchive`, `SCHEMA`, `src/archive.rs`, and the `archive` feature on five crates — so `curios-archive` names the thing owned, as `base`, `core`, `cert` and `text` do. Putting the new name on the *lower* half is what keeps `curios-prelude` meaning what it has always meant: the prelude you are allowed to use.

The verdict is the certifying crate's successful build, exactly as `.vok`'s existence is Coq's. Nothing can reach the prelude except through it, so the invariant holds by construction rather than by convention — which is what rules out the cheaper alternative of moving certification into a test, where an archive could exist, be used, and never have been certified.

One property improves rather than degrades. Today the walk certifies `core` **before** it is hash-consed and serialized, so what is certified is not literally what is stored; restoration covers the gap with schema, fingerprint and bytecheck. After the split the kernel walks the restored image, so it certifies the bytes that will actually be used.

**M4 — parallel certification.** `recheck_module_verdicts`'s own documentation establishes the precondition: `check_definition` and `check_rec_group` both return before their `define` step, so every item enters the environment whether or not it checked, and each item is judged against exactly what a fully-passing walk would give it. The environment is therefore a pure function of the module. Split the walk into a serial define-all phase and a parallel check-all phase, one `Kernel` per item over a shared read-only environment, verdicts sorted by item index for determinism.

Per-item kernels are what make the data independent, and they solve binder identity without arithmetic: each is seeded at the same derived floor, which is above every identity in the module, and two workers minting the same index never share a scope. A shared counter would be nondeterministic under work stealing and is ruled out — the archive must stay byte-reproducible and minted identities appear in diagnostics.

The cost to measure first is memo sharing, and it is already measurable: `recheck_module_verdicts_uncached` exists for the memo-parity test, and its slowdown against the cached walk is the ceiling on what per-item kernels lose.

**M5 — cached user modules.** The generalization: cache a user module's elaborated Core and its verdict, and reuse both when nothing it depends on has changed. Not on the roadmap today — the archived-prelude item is explicitly build-scoped, with production compilations elaborating only the user suffix.

Four things must land first, and three of them are perimeter work rather than build work.

*Binder identity* must be hardened. The prelude works because there is exactly one prefix seeding every floor; N independently elaborated modules each mint into their own space, and splicing them requires renumbering, where a collision is a capture. That row is graded *argued* with a single positive control, and `validate_universes` records two demonstrated capture bugs from the analogous universe renumbering.

*Prefix identification* must become an identity rather than a length. Today `checked_from` is one index and "everything below is the archive" is true by construction; with a set of cached modules it becomes "these items are those certified items", and the cheap re-derivation that row proposes stops being optional.

*Interface fingerprints* must be per declaration, following GHC, so a module is re-elaborated when what it depends on changed rather than when a file did. This is the granularity Cargo cannot express and the reason the `deps` axis has 13 values today.

*Verdicts must be keyed on (the exact terms × the certifier version).* M3 is the first, smallest, fully controlled instance of exactly this discipline, which is the argument for doing it deliberately now rather than inventing it under pressure later.

The whole-module passes never cache and re-run at link: `recheck_module_suffix` already runs positivity over the whole spliced declaration set because a new declaration can reach an old one, declaration sizing and the entrypoint check are the same shape, and witness coherence is program-wide by definition — a violation is only visible where two modules meet. That bounds the win without removing it, since per-item typing is the expensive part.

## Out of scope

- Making the archive a stable interchange format. It stays scoped to one compiler build; this document changes *what* scopes it, not that it is scoped.
- Parallelising elaboration. Shared monotonic `Entropy` for binders, metavariables, universes and witnesses is a serialization point by design, and it is the premise *Binder identity* is about.
- Parallelising conversion within one item, which is a recursive walk over a shared history and budget rather than data.
- Weakening the compile path's second opinion. `recheck_module_suffix` continues to judge the user suffix on every compile.

## Rejected

- **Caching inside the existing build script.** Impossible for the reason under Constraints: the script cannot tell which build-dependency invalidated it.
- **Certification as a test.** Cheapest, and it demotes the invariant from a build-time impossibility to a convention — an archive could exist, be compiled against, and never have been certified. This is Lean's posture and it is deliberately not this project's.
- **A single crate with two build scripts.** Cargo permits one build script per package; two products with different dependency sets means two packages.
- **Naming the certifying crate.** Putting the new name on the certifying half churns every downstream dependency and leaves consumers importing the prelude from something that does not sound like the prelude.

## Tests

- M1: `derived_binder_floor`'s existing coverage moves with it; no behavioral change is expected and none should appear.
- M3: an empty-cache build and a cache hit, following the `curios-binaryen` precedent — plus the case this exists for, a `curios-cert`-only edit that re-runs certification and **not** elaboration, asserted by the build script's own emitted warning appearing once rather than twice.
- M3: a corrupted or stale image must fail certification rather than be skipped — the fingerprint discipline is the whole safety argument.
- M4: the parallel walk's verdicts must equal the serial walk's, item for item and in order, over the whole prelude; and `kernel_memo_parity`'s property must survive per-item kernels.
- M5: deferred to its own specification when M1–M4 have landed and the perimeter rows it depends on are better defended.

## Retirement criteria

- Before this specification is deleted: the image/verdict boundary and its keying discipline are recorded in `curios-archive`'s and `curios-prelude`'s crate documentation; the invariant's new mechanism is restated in `curios-cert/src/recheck.rs` where the old one is stated today; [SOUNDNESS.md](../../SOUNDNESS.md)'s *Prefix identification* row is updated to describe what the split establishes; the cross-cutting decision is recorded in [DESIGN.md](../../DESIGN.md); and M5, if still pending, is carried out to a specification of its own rather than left inside this one.
