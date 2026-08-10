# A project names what it depends on, and a unit is compiled once

## Two objectives, and why they are one document

**A program can depend on code it does not contain, without vendoring it.** That decomposes into how a dependency is *named*, how it is *located*, how the compilation is *ordered*, and what the boundary *means*.

**A unit compiled by one compilation can be consumed by another**, so that depending on N packages does not cost N elaborations per build.

These were specified as two phases of one compilation-units document, in that order, and separating them is what showed the order was wrong. The second is not merely less urgent than the first — it is **downstream** of it. A cache has to live somewhere, and where it lives, how it is laid out, and whether it is shared between projects are all functions of what a project *is*, which is the first objective's question. The earlier document called storage "ordinary engineering", which is true of the mechanism and false of the location.

They are one document because the coupling runs both ways. The first is unusable at scale without the second — packages ship source, so every consumer elaborates every dependency — and the second cannot be designed before the first. Two documents would each have to state the other's constraints.

## What is already in place

The compiler no longer knows structurally that there is *a prelude* and *a program*. A compilation is a set of units folded over a dependency order, and this specification builds on that rather than restating it. The landed vocabulary, which every section below names rather than re-derives:

| Thing | What it is |
| --- | --- |
| `curios_unit::Unit` | One compiled unit: its resolution state, its elaborated `Module`, its erased arena, and its binder floor |
| `curios_unit::Scope` | Every predecessor, borrowed in dependency order — never merged, because merging copies the standard library into every compilation |
| `curios_base::Mount` | A claimed prefix and its `RootKind`; lookup is longest-match, and mount sets are pairwise disjoint |
| `curios_text::UnitSource` | The seam a unit arrives through: the entry with its loader, or a mounted tree |
| `curios_pipeline::compile_units` | The fold — each source compiled against the base and everything before it, judged by the kernel between elaboration and erasure |
| `curios_core::validate_stored_identities` | The refusal at the one seam a unit is written today |
| `--unit <PREFIX>=<PATH>` | Repeatable, argument order *is* dependency order |

Three properties that matter here, all of them enforced rather than assumed. The orphan rule fires between two ordinary units and not between two modules of one unit, which is what makes the unit boundary the place coherence is decided. Two units claiming one prefix is refused at mount, naming both claimants, and an entry `mod foo` beside a unit at `/foo` is that same refusal. Every unit's `foreign` rows reach the store the driver returns, unioned, disjoint by mount.

## Prior art

**Coq** binds a logical prefix to a physical directory with `-Q dir Lib`, which is where mounts come from. It does not key coherence on the prefix, which is why it can let the *consumer* choose the name and this design cannot — see below.

**Lean** stores a pre-elaborated environment per module and unions imports before checking, with `lean4checker` as a separate re-checker over the result. Its trust posture — a stored verdict believed on the strength of the file it came in — is the one this specification declines to inherit silently, and states as an entry instead.

**GHC** splits its scope in two: a `HomePackageTable` for what is being built now and an `ExternalPackageState` for what is already built. That split arrives here as the *provenance* of a unit — cached or live — not as a second scope type. It also fingerprints each declaration's interface, which is the granularity argument the key section rests on.

**rustc** assigns crate numbers per compilation and therefore needs `cnum_map` to remap them on load. Every identity decision below is downstream of refusing that.

**OCaml with Dune** wraps a library by prefixing every module with the library name, precisely because top-level compilation-unit names must be unique at link. Prefix-as-identity, arrived at independently. Its *packed modules* alternative is the shape the erased artifact deliberately takes.

**Cargo** supplies four things to the prelude's cache today — storage, key, invalidation, enforcement — and this specification is largely the story of replacing them one at a time.

## The order, which is not the order these were written in

**C1 → C2 → B1 → B3 → B4 → B6.** The identities are kept from the document these came from, because they are referenced by commits and by the task list, and because renumbering an identity to match a position is the mistake this whole design exists to refuse.

- **C1** — the manifest, and the resolver behind its source column. Nothing depends on it; everything else does.
- **C2** — dependency order and the conflict refusal. Needs a manifest to have something to conflict.
- **B1** — a witness is identified by its mount. Independent of both, and a precondition for storing more than one unit.
- **B3** — what replaces Cargo, and what the compiler starts believing. Needs C1, because the store's location is a fact about projects.
- **B4** — the erased artifact is keyed on the prefix, not on the unit. Needs B3's key.
- **B6** — what never caches, and what only looks like it. An audit; last because it is about what B3 and B4 built.

## Decided: the package boundary

### A package names itself

> **A package chooses its own mount prefix, and every consumer refers to it by that name.**

The reason is version coexistence being declined, reached from the other side. If a *consumer* chose the prefix, then when packages `X` and `Y` both depend on `D`, each mounts `D` where it likes, `D` compiles twice under two prefixes, and its types become two nominally distinct families spelled the same. `Show(D/Foo)` through `X` and `Show(D/Foo)` through `Y` are then different keys, so the orphan rule never fires between them and they silently fail to interoperate — the exact failure that decision exists to prevent, arriving through the prefix rather than through the version. Package-chosen naming is what makes a diamond *share* instead of duplicate.

B1 sharpens it: a mount would scope witness identities too, so a prefix becomes load-bearing identity throughout a stored unit rather than a spelling convenience.

**The cost, stated rather than discovered.** Two unrelated packages that each call themselves `/json` are permanently incompatible, and no consumer can repair it. That is rustc's position before namespacing; it is survivable with no ecosystem, and both escapes are additive — a namespace convention inside the canonical name, or reference-level aliasing, which `Context::insert_scope` already supports.

**This is not what `--unit` does today.** `--unit <PREFIX>=<PATH>` lets the *consumer* pick the prefix for any source, which is exactly the duplication this decision forbids. Benign while there is one consumer and no ecosystem, and closed by C1 when the manifest supplies the name. Recorded so C1 does not rediscover it as a surprise.

**Rejected: Coq's `-Q dir Lib`, where the consumer names the library.** It is where this design's mounts come from and it is right for Coq, which does not key coherence on the prefix. Here it duplicates every shared dependency.

### A dependency is pinned exactly, so there is nothing to solve

> **A dependency names a canonical name and one exact revision. There is no requirement language.**

No ranges, so no solver. No lockfile, because a manifest of exact pins is one. The order the fold consumes is a topological sort over the declared dependencies, and a cycle is refused.

**The revision identifier is opaque to the compiler.** It is compared for equality and never interpreted, so the compiler needs no notion of a registry, a version scheme, or a hash. A resolver turns an identifier into bytes; that is the entire contract, and it is what makes fetching genuinely separable rather than nominally so.

`--unit <prefix>=<path>` survives underneath all of this as the already-resolved form: it names a mount and a location, which is what a manifest entry becomes once its revision has been resolved to bytes.

**The cost.** Exact pinning conflicts on any difference, including compatible ones, so a deep graph grows annoying sooner than Cargo's would. The named successor is Go's minimal version selection, which needs only a total order on revisions rather than a constraint language, and which *produces* the pins this design already consumes — an additive layer rather than a redesign.

### A conflict is a refusal, and this specification owns it

Declining coexistence is what makes a version conflict a refusal rather than a resolution, and under exact pins the refusal is a comparison: two units in the graph pin different revisions of one canonical name. It names both dependents and both revisions, before any of them elaborates.

It cannot wait. Unowned, a conflict reaches the compiler as an unbound name, or as a nominal conversion failure raised by the certifier — which holds no span for the other revision. That is the diagnostic class declining coexistence was meant to avoid.

### What a manifest holds, and what it cannot

- The unit's **canonical name**, which is its mount. Absent for the entry, which owns the empty prefix precisely because it is the unit with no successors.
- Its **dependencies**: a canonical name, an exact revision, and a source, each.
- Nothing else. No privilege tier and no second prefix, per the two decisions below.

**A manifest is optional.** A program depending on nothing needs none, and compiling a bare `.crs` file keeps working exactly as it does today: no manifest means no dependencies, not a missing file.

The format is a Rust-side choice with no reuse available from the standard library: `/std`'s TOML codec is a guest library and cannot serve the compiler driver.

### Decided: a package is ordinary, and the manifest cannot say otherwise

`RootKind` fuses two powers into one tier: may reference an internal root, and — through the orphan rule's "the declaring root is unprivileged" condition — is exempt from the refusal that stops two authors colliding on one `(concept, key)` pair. Depending on someone else's code needs neither. A manifest that may declare its own tier hands a package the power to exempt itself from the one rule the unit boundary exists to enforce, spelled in the file the package's own author writes.

So a mounted package is `Ordinary`, the manifest has no tier field, and the privileged tier stays the compiler's own. **Reinstate if** a package is ever wanted that must reference an internal root — and split the tier in two before doing it, because reach and exemption are different powers and only the first would be being asked for.

### Decided: one prefix per manifest

The prelude mounts three because `/syn` and `/std` are mutually dependent and no order exists over them. Nothing else has that shape. `Unit`'s mounts are already a list, so the day something does, lifting this is additive and nothing archived changes. Refusing it now is what keeps a package's name and its mount the same word.

### A source is a resolver, not a path

`RootSource` is the whole logical-to-physical mapping today, and it is one optional directory: qualifier `a/b/c` reads `base/a/b/c.crs`. Generalizing it to one base per mount is small. What must not follow the directory into the format is the assumption that a source *is* a directory — `curios-web` compiles with no filesystem at all and supplies every module body inline, so a manifest whose source column can only spell a path excludes a shipped product. The column names a resolver; the filesystem is one of them.

### Packages ship source, not artifacts

The prelude archive is build-scoped and deliberately not an interchange format; generalizing "one artifact per package" would quietly make it one. Rust ships source and rebuilds, which keeps that constraint honest and keeps the caching half about *local* caching rather than distribution. It also means every consumer elaborates every dependency, which is the coupling that puts caching in this document rather than in a later one.

### What this does not touch

**The prelude is not a package.** Every compilation depends on it implicitly, no manifest names it, and its privileged mounts stay the compiler's own — which is *a package is ordinary*, seen from the other side.

**No surface syntax changes.** `use /foo/Bar` already reaches a mounted prefix, so [SYNTAX.md](../../SYNTAX.md) is untouched and no `.crs` file spells a dependency. A package boundary is invisible to the grammar and visible only to coherence, visibility, and the mount table.

## Open: two decisions this specification must make and has not

Both were raised and deliberately left unsettled. They are recorded here as questions with their alternatives, because writing down a decision nobody made is worse than an acknowledged gap.

### Is a manifest named, or discovered?

Everything C1 and C2 need works with a manifest that is *named*: parse it, resolve each dependency to bytes, topologically sort, refuse a cycle, refuse a conflict. Discovery is a separate, purely additive layer — and it is the thing that makes "is this file part of a project?" a question at all.

- **Implicit, walking up from the file or the working directory** — Cargo's rule. A project works from anywhere inside it. The cost is the scratch file: a throwaway `.crs` inside a project directory becomes a project member without saying so, compiled against its dependencies and refused by its conflicts. "Standalone" then depends on where a file happens to sit.
- **Explicit, a flag or an argument.** A bare file is always a bare file, wherever it sits; a project costs one flag, or a wrapper grown later. Never surprising, more typing.

Until this is decided, **every file is standalone** and nothing on disk is implicitly part of anything, which is also the state the compiler is in today.

### Where does a store live?

B3 settles the key and the trust. It does not settle the location, and the two are not independent: content-derived keys are what make a store shareable between projects at all, and a path-keyed one could only ever be local.

- **Project-local**, beside whatever a project turns out to be.
- **Shared and content-addressed**, so two projects depending on one package at one revision pay for it once — the diamond argument, applied to build time rather than to coherence.
- **Under the existing `target/`**, which this repository already treats as a hand-pruned cache that survives `cargo clean` only by never being given it.

This wants deciding against a real dependency graph rather than against the single hand-passed unit `--unit` allows today, which is the reason the caching half is sequenced after C1 rather than before it.

## The caching half

**It does not introduce verdict caching. It removes Cargo from underneath the one that already exists.** The prelude is a cached unit today — `verdicts_from` skips an item every one of whose declared names the environment already answers for, so the archive's items are never re-judged on the compile path, and what makes that sound is that the only crate handing the image out is one whose build script walked it with the kernel first. Cargo supplies four things there: storage (`OUT_DIR` and `include_bytes!`), the key (a schema constant and a source fingerprint), invalidation (the build script's own dependency graph), and enforcement (a crate that does not compile). A unit that is not a crate has none of them. Three are engineering. The fourth is a change to what the compiler believes, and it is stated in B3 rather than inherited.

### The rule a stored unit is checked against

> **A unit may be stored only if it carries no positional identity.**

A positional identity is one meaningful solely in the compilation that assigned it. Storing one is how rustc came to need `cnum_map`, and it is the one property that decides whether a stored unit is portable. Measured against the stored prelude — 1079 items, 1094 definitions, release build:

| Identity | In a stored unit | Established by |
| --- | --- | --- |
| Term metavariable | none — zonking substitutes every solution and refuses an unsolved hole | `validate_stored_identities`, on the value the archive build serializes |
| Universe metavariable | none — a level holding one is not closed over its declaration's parameters | `validate_bound_universes`, which names it in as many words |
| Free local binder | none — `derived_binder_floor` over items *and* registries is **0**, against a lowering watermark of 6684 | `validate_stored_identities` |
| Witness | **75, densely 0..74, 34 of them referenced from terms** | nothing; B1 |

**The precondition this rests on today, stated because nothing checks it:** at most one unit in a compilation is restored from storage. Dense witness identities are safe only because there is exactly one storer and it always sits first. A second stored unit would land on `0..74` and silently rebind coherence entries. B1 is what removes the precondition; until then it is load-bearing and unenforced, and enforcing it needs a `Unit` to know whether it was stored or elaborated — GHC's home/external split, which belongs with the store that could violate it rather than ahead of it.

Of the four monotonic counters — metavariable, binder, witness, universe — exactly one mints an identity that reaches a stored unit. The other three leave watermarks, which combine by maximum and cannot alias. This makes **no claim on [SOUNDNESS.md](../../SOUNDNESS.md)'s *Binder identity* row**: that row is about a checker's own fresh mints aliasing identities in a live scope, which is a within-compilation property.

The fold changes shape, and this is the whole of it:

```rust
// curios-pipeline, with this half in place. Compare `compile_units` today.
let mut units: Vec<Unit> = Vec::new();
let mut globals = Globals::default();

for source in sources {                              // dependency order
    let unit = match store.unit(source.key()) {      // key: its content, and the certifier
        Some(unit) => unit,                          // already judged; nothing re-runs
        None => {
            let elaborated = elaborate_unit(Scope::over(&units), source, budget)?;
            judge(&elaborated, &globals)?;           // curios-cert
            store.put_unit(source.key(), elaborated)? // refuses a positional identity
        }
    };
    globals.mount(&unit);
    units.push(unit);
}

// One erased artifact for the whole prefix, keyed on the ordered set above — never per unit.
let prefix = store
    .prefix(units.keys())
    .unwrap_or_else(|| store.put_prefix(units.keys(), erase_prefix(&units, budget)?))?;

// The entry is what you are editing, so it is never cached: it erases onto the prefix, as today.
let ersd = erase_onto(prefix, &entry, budget)?;
```

### B1 — a witness is identified by its mount

`Global::Witness(WitnessId)` is minted from one program-global counter, and it is the only name in a stored unit that carries no prefix. Two units elaborated in separate compilations both mint from zero, and `curios-core`'s own note states the consequence: *"aliasing one would silently rebind a coherence-table entry."* That admits rather than crashes, and the prelude's 75 dense identities are exactly what a second unit would land on.

The identity gains its declaring mount. The production surface is three files — the mint in `curios-text`'s `into_core`, the counter beside `fresh_binder`, and the variant with its `Display` in `curios-core` — and at the mint site the declaring mount is one lookup away on the same context. The archive schema bumps.

**This does not contradict the note that warns about it.** That note refuses a bare per-module *ordinal*, on the grounds that two modules' `witness#0` would alias. A pair — mount and ordinal — is disjoint by the same argument mount disjointness already carries everywhere else.

**It is also what makes a unit cacheable at all, which is the stronger reason.** A witness identity is minted from a counter seeded at `witness_floor`, so the same package takes ids 75 and up when compiled after the prelude and 0 and up when compiled alone — different bytes from identical source. Everything else in the table is already position-independent, so the witness counter is the *only* thing tying a stored unit to where it sat, and a per-mount ordinal is what lets B3's key be content-derived rather than content-and-position. Record the consequence so nobody preserves it: `PreparedPrelude::witness_floor` becomes vestigial once each mount numbers its own.

*Must not change:* what any program means. A witness is anonymous and reached only through resolution, so scoping its identity renames nothing a programmer wrote.

*Verified by:* the full gate over a corpus that runs identically, and the prelude re-certifying at 0 refusals against the bumped schema.

**Rejected: renumbering witnesses as a unit is restored.** `cnum_map`, refused again.

### B3 — what replaces Cargo, and what the compiler starts believing

Storage and invalidation are ordinary engineering as *mechanisms*; where the store lives is [an open decision above](#where-does-a-store-live). The key and the enforcement are one question — *what makes a cached verdict unforgeable and unstale* — and answering it turns the verdict from a build artifact into a recorded claim, which is exactly what `curios-prelude`'s documentation says the present design is not.

That is not a reason to refuse it. It is a reason to write it down. **A cached verdict is a rule that admits, so it earns an entry in [SOUNDNESS.md](../../SOUNDNESS.md) — its assumption, its grade, and the evidence behind it — and no unit's verdict is cached before that entry exists.**

The key must say *these terms, this certifier*, never a path and never a timestamp. There are two ways to get a key wrong and they have different consequences. An **over-broad** key invalidates more than it must and costs time: Cargo's granularity is the crate, which is what made a kernel edit re-elaborate the standard library until `curios-analysis` was split out, and GHC avoids it by fingerprinting each declaration's interface. An **imprecise** key — a path, a timestamp, a number someone must remember to bump — fails to invalidate when it should, and a verdict that survives the change it should not have survived *admits*. Only the second is a soundness question, and it is the one this decision is about.

The terms half is a content fingerprint. For the certifier half the mechanism already exists one crate over: the prelude's source fingerprint is a build script hashing authored sources into an `env!`, and the same over `curios-cert` and `curios-analysis` yields a certifier fingerprint that is *derived* rather than remembered. The archive's hand-bumped schema constant is what the alternative looks like — a number describing a layout and nothing about the kernel's decisions — and a key that must be remembered is one that eventually is not. **State the limit beside the mechanism:** a source fingerprint moves when those sources move, and a dependency bump changes what the certifier decides without touching them, so either the key covers that closure or it is conservative by construction.

### B4 — the erased artifact is keyed on the prefix, not on the unit

Re-erasing one unit costs **608 ms**, measured over the stored prelude in release, against a ~680 ms release compile of a one-line program. So a dependant cannot re-erase its predecessors per compile.

It does not follow that each unit's erased form is stored on its own. `curios_ersd::Module` is five arenas plus five positional `Vec`s, its `Environment` maps a name to bindings holding arena atoms, and two independently erased units both number from zero — so per-unit erased artifacts need a relocation pass, which is `cnum_map` once more.

Store the erased artifact against the **ordered set of predecessors** instead. That is today's mechanism unchanged, because the prelude *is* that set while there is one unit, and `Resumed` already borrows a core per unit and threads exactly one arena. Core and verdict cache per unit, where elaboration's cost is; the erased prefix caches per dependency set. Two artifacts, two keys, both content-derived. Adding a dependency pays one erasure; compiling under an unchanged set pays none.

**This is what keeps "there is no link step at the erased level" true.** That holds unconditionally while every unit is erased in one process in dependency order, which is where the compiler is today. Under caching it holds only because of the decision above; per-unit erased artifacts would invent one.

**It also makes one field of `curios_unit::Unit` provisional**, which its own documentation already records: the erased half moves off the unit and onto the prefix.

### B6 — what never caches, and what only looks like it

**Genuinely program-wide:** witness coherence and the visibility fixed point. A coherence violation is only visible where two units meet, and `Audiences::compute` runs over the union of scope and unit. Neither is decidable inside a unit, so neither caches.

**Stable under extension, and so cacheable exactly when the key already covers the predecessors:** strict positivity over the declaration set, declaration sizing, and concept-registry validation. Mounts are disjoint and units are ordered, so nothing later can add a constructor to an earlier unit's inductive or a field to its structure — an earlier unit's answer cannot be falsified by what comes after it. Decide each rather than the group, and move any of them into the paragraph above if it turns out to read something a successor can change.

Either way the win is bounded rather than removed, because per-item typing is the expensive part.

### Out of the caching half

- **Restoring lazily.** Idris 2 stores a TTC entry as a blob and deserializes on first lookup. Measured in release, the whole Curios image — bytecheck, plus deserializing the prepared Text state, the Core and the erased prefix — restores in **34.4 ms**, and the erased clone taken per compile is 1.4 ms. The analogy holds and the lever does not.
- **Incrementality *within* a unit.** A different objective, recorded in the appendix.

## Out of scope

- **Parallelising elaboration.** The shared monotonic counters are a serialization point by design.
- **A third visibility level.** Package-privacy is subtree containment, which the audience model already expresses. `Audiences` computes who-can-see-what as sets of subtree roots, and a package is a subtree — Rust needed `pub(crate)` because its module tree and crate boundary are different things, and here they coincide.
- **Making the prelude archive a stable interchange format.**
- **Version coexistence**, per the decision above.
- **Selecting versions, and fetching.** A dependency is pinned exactly, so this document owns the conflict *refusal* and chooses nothing; minimal version selection and a fetcher are additive layers after it.

## Tests

- **The diamond:** two packages depending on one package at the same revision compile it once, and a witness declared in it resolves identically through both. This is what consumer-chosen prefixes would silently have duplicated.
- **The conflict:** two dependents pinning different revisions of one canonical name is refused naming both dependents and both revisions, before any of the three elaborates.
- **The cycle:** a dependency cycle is refused; and a manifest declaring a prefix another manifest in the graph already claims is the mount collision already diagnosed at mount.
- **The bare file:** a `.crs` file with no manifest compiles exactly as it does today. No manifest means no dependencies, not a missing file.
- **The resolver:** a source that is not a directory resolves, so the format cannot quietly assume a filesystem — the property that keeps `curios-web` compiling.
- **The storage check** refuses an unscoped witness, once B1 gives "scoped" a meaning. It already refuses a free local and a metavariable of either kind.
- **Two units elaborated in separate compilations**, each declaring witnesses, resolve to their own — the collision B1 removes, written as the fixture that would have caught it.
- **A cached unit and a freshly elaborated one produce the same program**, and changing either half of the key — the terms or the certifier — invalidates.

## Retirement criteria

Before this specification is deleted: a manifest names a unit and its exact dependencies and is parsed by the driver; a source column names a resolver rather than a path, with the filesystem as one implementation and `curios-web` still compiling; dependency order comes from declared dependencies rather than from argument position; a cycle and a revision conflict are each refused with a diagnostic naming both parties; no stored unit carries a positional identity, witnesses included, and the check enforcing that runs at every seam a unit is written; every cached verdict carries its [SOUNDNESS.md](../../SOUNDNESS.md) entry with a grade and evidence; the store's location and layout are decided and documented where the store lives; and the manifest-discovery question above is answered rather than deferred again.

**The appendix is not deleted with this file.** Its measurements are the only record of how they were taken, and its findings outlive the work that turned them up.

## Appendix — measurements and adjacent findings

### Measurements

Every figure this document leans on, with its date, its **profile**, and how to retake it. Two items in the predecessor document were designed against unattributed numbers and both were wrong: a 471 ms eager restore that is 34.4 ms, and parallel certification's estimated 60–70 s win over an operation that takes 11.8 s. A number in prose with no method decays quietly and is then designed against, which is what this section exists to stop.

Taken **2026-08-09**, **release** profile, over the stored prelude. The probe was a throwaway test in `curios-prelude-archive` and is **not in-tree**, so retaking these means writing it again — `with_prelude` for the restore, `Prelude::ersd` for the clone, `erase_unit` over `Prelude::core` for the erasure, and `recheck_module_verdicts` from a default `Globals` for the certification.

| What | Measured |
| --- | --- |
| Cold restore — bytecheck, then deserializing the prepared Text state, the Core and the erased prefix | 34.4 ms |
| Erased-prefix clone, taken once per compile | 1.4 ms |
| Re-erasing one whole unit over the stored Core | 608 ms |
| Certifying one whole unit — `recheck_module_verdicts` from an empty environment | 11.8 s, 0 refusals |

Shape of the stored prelude, same run: 1079 items and 1094 definitions; 75 witnesses at identities 0..74 with no gaps, 34 of them referenced from terms; 31 inductives, 46 structures, 14 concepts; `derived_binder_floor` **0**, against a lowering watermark of 6684.

**Landing the probe is itself an item.** These figures are cited by this document and by work outside it, and the only thing keeping them honest is a test nobody has written. In-tree they become `cargo test`-retakeable and every reader cites rather than copies.

**Inherited, undated, profile unrecorded.** Kept because they are load-bearing elsewhere, labelled because nothing here can check them: 469 s of a ~570 s prelude build in elaboration, and 204 s of that in universe finalization. Cargo builds a build script in the profile of the build that triggers it, so a dev iteration loop and a release measurement are not comparable — which is why profile is part of the method above and not a footnote.

### Findings whose triggers fire inside this specification

**The `O(scope)` per-compile prologues.** Erasure projects the whole predecessor Core and re-seeds the elaboration context with every one of its definitions, and `Globals::of` copies every registry and builds a map of every definition — both on every compile, today, with one predecessor. Read from the code and **not measured**. Recorded because this specification multiplies each by the number of dependencies, and because measuring before designing is what removed three items from the caching half.

**Parallel per-item certification.** Split the certifier's walk into a serial define-all phase and a parallel check-all phase, one `Kernel` per item over a shared read-only environment, verdicts sorted by item index for determinism. Per-item kernels settle binder identity without arithmetic: each is seeded at the same derived floor, above every identity in the module. A shared counter is ruled out — nondeterministic under work stealing, and the archive must stay byte-reproducible. Any parallelism must be feature-gated native-only, because `curios-web` compiles `curios-cert` to `wasm32-unknown-unknown`, which has no threads.

*Declined on measurement, not merely parked.* The original estimate cannot be right, because certifying a whole unit takes 11.8 s and nothing can save 60 s of it. What the measurement changes is not the size of the prize but who pays: B3 caches a verdict against its terms and its certifier, so a dependency is certified once when it is stored and never again while both hold. Spending concurrency **inside the trusted base** — where *parallel verdicts equal serial verdicts* becomes something to prove — to speed up a once-per-dependency cost is the wrong trade. **Revisit if** first-build latency for a dependency, or a compiler upgrade re-certifying every cached dependency at once, becomes the complaint; and try narrowing what an upgrade invalidates before reaching for threads, since that is sequential and outside the trusted base.

**Incrementality within a unit.** Not declined on the merits — a different objective. This document needs a unit reused whole or recompiled whole; per-declaration fingerprinting, which is GHC's model, answers a question about editing your own code that nothing here asks.
