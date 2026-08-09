# A compilation is units folded over a dependency order

The compiler structurally knows that there is a prelude and there is a program. That knowledge is smeared across eleven public names, one crate-graph edge, a closed four-variant enum, and two functions that are one function. This specifies removing it, so that *which unit is the standard library* and *which is the one you asked for* become data rather than structure — and user packages become reachable without a second mechanism.

## The centre of gravity

> **There is one operation — compile a unit against a scope — and a compilation is that operation folded over a dependency order.**
>
> **A unit is defined by what it provides to its successors, not by what it is.**

That second sentence is the one that does the work, and half of it is already true. `Globals`, `Established`, `Resumed`, `Scoped` and `Declarations` each name what a stage *receives*, and none mentions the prelude — they were built that way by [the prelude-environment work](../../DESIGN.md), which stopped at the types. The functions and crates still say `prelude`, and that is the remaining half.

**The test to apply to any change made under this specification:** if it does not fall out of *one operation, folded, over units defined by what they provide*, it is a separate change and wants its own argument.

## The smear, as it is written today

Eleven public names say *prelude* where they mean *a scope*:

```text
into_core_with_prelude          prepare_prelude            PreparedPrelude
elaborate_and_zonk_with_prelude erase_prelude_prefix       ErasedPrelude
erase_module_with_prelude       PreludeModules
```

And the structural form of the same thing, in `curios-pipeline/Cargo.toml`:

```toml
curios-prelude = { workspace = true }
```

The general compile driver depends on the specific standard library. Every other symptom follows from that.

## Prior art

Four systems made different bets, and the differences are the useful part.

**Coq** binds *logical* paths to *physical* directories (`-Q dir Lib`, `-R` recursively), so a package is a **name** mapped to a location rather than a directory that happens to be somewhere. That is the model this specification takes for roots, and `RootKind`'s existing privilege tier is already the `-Q`/`-R` distinction in everything but name.

**Lean** loads dependencies' `.olean` files and constructs *"a pre-environment that contains the union of the dependencies' environments"* — `Established` with N inputs, reached independently. It also splits a serialized environment into private, public and server parts, and does not propagate extension state across imports unless a persistent extension is registered: two answers to *what crosses a boundary*, decided per item rather than wholesale. Its trust posture is **not** taken — imports are believed and re-verification is an opt-in external pass, where Curios runs the kernel on the compile path and, since the crate split, can afford to.

**Idris 2** stores a TTC entry as *"either a Binary blob or a processed definition"* and deserializes only on first lookup, because *"converting Binary to the definition is fairly costly and often definitions in an imported file are never used"*. Curios restores its whole prelude eagerly, measured at 471 ms. That is Phase B's lever, and it composes with N units where eager restoration multiplies.

**Agda** stores interface files under `_build/VERSION` so that switching versions does not discard them. Curios keys its archive on a schema plus a source fingerprint instead, which is the same idea without the path.

## Phase A — many units, one process

No caching, no artifacts, no manifest. N units lowered, elaborated, certified and erased in dependency order, each becoming scope for the next. This exercises every seam with machinery that already exists.

### The structural change

**`curios-pipeline` stops depending on `curios-prelude`.** It gains one operation — compile a unit against a scope — and the fold over a dependency order. Who supplies the first units becomes the caller's business: `curios` and `curios-web` pass the prelude in as data, exactly as they already pass `SYNTAX`, which is a parameter at the lowering and imported at the pipeline only out of convenience.

The inversion is cycle-free and is what makes the rest possible rather than cosmetic: `curios-prelude-archive` can then build its unit *through* `curios-pipeline` instead of alongside it, and the prelude's build script stops being a second code path.

### `Unit`

One type replaces four half-artifacts. Today a unit's contents are split across `PreparedPrelude` (resolution state, lowered Core, four watermarks), `PreludeArchive` (that, plus the *elaborated* Core, plus `body_type`, plus `ersd`), `ErasedPrelude`, and a `ForeignStore` that reaches no successor at all.

Defined by what a successor needs: the resolution table and public interface, the elaborated `Module`, the erased arena, the four watermarks, the foreign rows. `PreludeArchive` is then its serialized form — which is what Phase B caches, and why the prelude's archive stops being a mechanism and becomes an instance.

It lives in `curios-pipeline`, with the fold.

### The vices to delete rather than extend

**`RootId::of_segment` re-derives a root from a name string.** That is the exact thing `RootId` exists to prevent — its own module documentation says the type is *"the handle every other stage compares by equality instead of re-deriving 'which root does this belong to' from a qualified-name string"* — and it survives at three call sites only because `_ => Entry` is correct while one user root exists. With N packages it is unsalvageable: it cannot distinguish a package root from a module inside the entry. It goes, and `Qualifier::root_segment` goes with it, since its only two callers feed it.

**`RootId::Entry` does double duty.** `Resolved::for_prelude` builds the synthetic compilation root — the empty qualifier that owns every mount — as `ModuleInfo::new(RootId::Entry)`, because there is no "no root". A value standing in for something it is not, benign only while there is one of the thing: the same shape as the `checked_from` index the previous specification removed. `Entry` becomes an ordinary index, and the synthetic root gets a representation of its own.

**`prepare_prelude` and `into_core_with_prelude` are one function written twice.** They differ in whether a prior scope exists and where four counters start. They merge, which is the same *delete the second spelling* move that gave `curios-core` one recursion form where it had two.

### What already composes, and costs nothing

**Identity.** Four monotonic counters — metavariable, binder, witness, universe — each seeded from its predecessor's final count, and `Entropy::seed` only raises. A topological order seeds each unit from the running maximum, which is exactly what prelude→entry does today. **The `Binder identity` work belongs to Phase B, not here**, and only because independently elaborated units are combined later.

**Coherence.** The orphan rule is already written for this world, in the future tense: it *"only bites an ordinary root — the entry program today, an untrusted external package once one exists"*, with privileged roots exempt because sys/syn/std are *"one coordinated standard library, not independent unrelated packages"*. Two units registering the same key collide when a third puts both in scope, which is the correct link-time behaviour and needs no new machinery.

**Erasure.** `Resumed` restores an arena and erases a unit's items onto it. N units compose by threading it; there is no link step to invent at the erased level.

**Visibility, which was expected to be the sweep and is not.** `Audiences` computes who-can-see-what as *sets of subtree roots* — *"a declaration is visible to consumer `C` when `C` lies within any of them"* — and *"`pub` inside a private module reaches exactly that module's audience and no further"*. A package is a subtree, so a package's internals under a non-`pub` submodule are already package-scoped. Rust needed `pub(crate)` because its module tree and crate boundary are different things; here they coincide. Visibility stays a `bool` on two axes (`vis_pub` for the name, `rep_pub` for the representation) and gains no third level.

### The one real gap

`ForeignStore` has no merge, and `register` **panics** on a duplicate — *"registering a duplicate is a construction bug and panics"*. With two independent packages a duplicate import name stops being a construction bug and becomes a *program* error, which by this repository's own rule must be a diagnostic. Worse, the identity is the flat wasm import string, which is externally meaningful and cannot be package-qualified arbitrarily: it is the one namespace two packages can collide in that neither owns. Merging, diagnosing, and deciding whether import names carry a package are all Phase A work.

### Decided: a unit's name is its path prefix

Coq's answer, adopted deliberately rather than inherited.

`Global::Authored(Qualifier)` **is** the identity — there is no package field, and adding one means an indirection in the single type the kernel, the registries, positivity and the coherence table all compare by equality.

**Version coexistence is declined**, and the reason is specific to this language. Two versions would declare two distinct *nominal* types spelled identically. In Rust that is already the ecosystem's worst diagnostic class; here it lands worse, because conversion is nominal, the kernel compares `Global`s, and the refusal surfaces from the certifier, which holds no source span for the other version. Coherence fares worse still: `Show(Bar_v1)` and `Show(Bar_v2)` are *different keys*, so the orphan rule never fires — two instances coexisting and silently failing to interoperate, which is the collision that rule exists to prevent arriving through the one door it does not watch.

Two units claiming one prefix is an error **diagnosed at mount**, where the registry knows both.

**Aliasing stays reachable and costs nothing.** `Context::insert_scope` already maps a local string to a qualifier, so reference-name and declaration-name are distinct mechanisms that merely coincide today. If ergonomics later want `use /foo as bar`, that is a scope feature and no `Global` changes.

And it is the cheaper mistake: prefix-as-identity can gain an indirection later; starting with the indirection pays for coexistence nobody has asked for.

## Phase B — cached units

With units in place, caching a unit's elaborated Core and its verdict is the extension the shape was built for: the scope is assembled from N cached units rather than recompiled, and there is no seam to identify because there was never a prefix.

**Identities must survive combining independently elaborated units.** The counters are monotonic and seeded from watermarks; two units elaborated in *separate processes* mint from overlapping ranges, and an aliased binder silently identifies two terms that differ. [SOUNDNESS.md](../../SOUNDNESS.md)'s *Binder identity* row is graded **argued**, with one positive control — adequate for a single carried floor combined by maximum, not for N units composed pairwise. **This row must be defended before any unit is cached**, and it is the first task of this phase rather than a later one.

**Verdicts must be keyed on the terms and the certifier, never on a path or a timestamp.** GHC fingerprints each declaration's interface and recompiles what actually depends on what changed; Cargo's granularity is the crate, which is what made a kernel edit re-elaborate the standard library until `curios-analysis` was split out. A coarser key reintroduces that failure at a level where it *admits* rather than merely costing time.

**Anything reasoning over "every name the program has" becomes a scope question.** Retiring the splice broke two such sites — strict positivity's declaration set, and `build_shorten`'s abbreviation table, which also starved `nominal_plicities` beside it. Both were found by a test rather than by inspection. Before caching multiplies what "in scope" can mean, search for `module_symbols`, `nominal_plicities`, bare `items.iter()` and registry iteration, and decide each deliberately.

**Lazy entries are the lever.** Idris 2's blob-until-first-lookup is the direct answer to the 471 ms eager restore, and it matters more with N units than with one. It also argues against a Lean-style split artifact: if entries are lazy, separate public and private files are unnecessary to avoid paying for private ones.

**Whole-unit passes never cache and re-run at link:** strict positivity over the complete declaration set, declaration sizing, concept-registry validation, and witness coherence — program-wide by definition, since a coherence violation is only visible where two units meet. That bounds the win without removing it, because per-item typing is the expensive part.

### Deferred here, with its reasoning: parallel per-item certification

Split the certifier's walk into a serial define-all phase and a parallel check-all phase, one `Kernel` per item over a shared read-only environment, verdicts sorted by item index for determinism. Per-item kernels settle binder identity without arithmetic: each is seeded at the same derived floor, above every identity in the module, so two workers minting the same index never share a scope. A shared counter is ruled out — nondeterministic under work stealing, and the archive must stay byte-reproducible. Any parallelism must be feature-gated native-only, because `curios-web` compiles `curios-cert` to `wasm32-unknown-unknown`, which has no threads.

**Parked on 2026-08-09, on measurement.** After the crate split, certification is the whole cost of a kernel-edit rebuild rather than 18% of a full one — but that loop is ~100 s and the win is perhaps 60–70 s, against per-item kernels, a feature gate, a memo-cost measurement, determinism obligations, and **concurrency inside the trusted base**, where *parallel verdicts equal serial verdicts* becomes something to prove. The same day's profiling put 469 s of a ~570 s prelude build in elaboration, 204 s of it in universe finalization, and **63% of that in one declaration** — `/std/Async/block_on`, whose constraint graph is three orders of magnitude larger than any other. That sits outside the trusted base and may be contained inside `finalize`. Revisit if the kernel loop hurts again.

## Phase C — the manifest

Sketched only; it wants its own specification once A and B have landed.

A manifest maps a **name** to a **source**, and the name becomes the unit's path prefix — Coq's `-Q dir Lib`, which is the model Phase A already adopts for the root registry. It declares a unit's dependencies, which is what supplies the fold's topological order, and its privilege tier, which is what the orphan rule already reads.

**Packages ship source, not artifacts.** The archive is build-scoped and deliberately not an interchange format; generalizing "one artifact per package" would quietly make it one. Rust ships source and rebuilds, which keeps that constraint honest and keeps Phase B about *local* caching rather than distribution.

Open, and genuinely undecided: whether the prelude is one unit with three roots or three units, and whether a manifest can mount a privileged root at all or that tier stays reserved for the compiler's own.

## Out of scope

- **Parallelising elaboration.** The shared monotonic counters are a serialization point by design.
- **A third visibility level.** Package-privacy is subtree containment, which the audience model already expresses.
- **Making the archive a stable interchange format.**
- **Version coexistence**, per the decision above.

## Tests

- Phase A: a program of N units compiles to a module identical to the same program written as one unit with `mod` declarations — the property that says the unit boundary is not semantic.
- Phase A: two units mounting the same prefix is diagnosed, not a panic and not a silent shadow.
- Phase A: two units declaring the same foreign import name is diagnosed at link, with both declaring sites named.
- Phase A: a unit's `pub` item exposing a dependency's non-`pub` type is refused by the existing audit, with no new rule.
- Phase A: the orphan rule fires across two ordinary units and stays exempt across privileged ones.
- Phase B: as its section states, `Binder identity` probed before anything is cached.

## Retirement criteria

Before this specification is deleted: `curios-pipeline` names no crate that is specific to the standard library; `of_segment` and `root_segment` are gone and no stage derives a root from a name; the fold and the `Unit` boundary are recorded in `curios-pipeline`'s crate documentation, and the root registry's logical-to-physical mapping in `curios-text`'s; *Binder identity* is defended beyond **argued** if any part of Phase B has landed; and Phase C, if still pending, is carried out to a specification of its own.
