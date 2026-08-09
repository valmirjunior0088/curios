# A compilation is units folded over a dependency order

The compiler structurally knows that there is a prelude and there is a program. That knowledge is smeared across eleven public names, three spellings of one lowering, two of one erasure, one crate-graph edge, a closed four-variant enum, and a stamp that caches a value it could take the first segment of. This specifies removing it, so that *which unit is the standard library* and *which is the one you asked for* become data rather than structure — and user packages become reachable without a second mechanism.

## The centre of gravity

> **A scope is what earlier units established. Every stage takes a scope and a unit, and a compilation is each stage folded over a dependency order.**
>
> **A unit is defined by what it provides to its successors, not by what it is.**

That second sentence is the one that does the work, and half of it is already true. `Globals`, `Established`, `Resumed`, `Scoped`, `NominalScope` and `Declarations` each name what a stage *receives*, and none mentions the prelude — they were built that way by [the prelude-environment work](../../DESIGN.md), which stopped at the types. The functions and crates still say `prelude`, and that is the remaining half.

**It is not one operation.** An earlier draft said so and it was wrong twice over. The stages are ordered *lower → elaborate → judge → erase*, and judgment sits between elaboration and erasure so that a module the kernel refuses never reaches erasure's budget; that ordering is a property of the sequence, not of any single call. And the producer of the prelude image runs the sequence *without* the judge, because `curios-prelude-archive` deliberately does not depend on `curios-cert` — [certification is a separate crate's build script](../../../curios-prelude-archive/README.md), and folding it into one operation would either reinstate that dependency or hide it behind a parameter. So: separate calls, one fold, judgment interleaved by the driver.

**The test to apply to any change made under this specification:** if it does not fall out of *stages folded over units defined by what they provide*, it is a separate change and wants its own argument.

## The smear, as it is written today

Eleven public names say *prelude* where they mean *a scope*:

```text
into_core_with_prelude          prepare_prelude            PreparedPrelude
elaborate_and_zonk_with_prelude erase_prelude_prefix       ErasedPrelude
erase_module_with_prelude       PreludeModules
```

Three of them are one lowering: `into_core`, `prepare_prelude`, `into_core_with_prelude`. They differ in whether a prior scope exists, whether the unit's items sit at the empty qualifier or under mounted roots, and where four counters start. `into_core` is public but reaches no production caller — only `curios-text`'s own tests — so it is a third spelling maintained for a test harness.

Two more are one erasure: `erase_prelude_prefix` and `erase_module_with_prelude`. They differ in whether a scope exists and whether an entrypoint body is sealed at the end.

And the structural form of the same thing, in `curios-pipeline/Cargo.toml`:

```toml
curios-prelude = { workspace = true }
```

The general compile driver depends on the specific standard library. Every other symptom follows from that.

## Prior art

Eight systems, and the differences are the useful part. Four were already recorded; four were added because they answer questions this specification actually has.

**Coq** binds *logical* paths to *physical* directories (`-Q dir Lib`, `-R` recursively), so a package is a **name** mapped to a location rather than a directory that happens to be somewhere. That is the model this specification takes for mounts, and `RootKind`'s privilege tier is already the `-Q`/`-R` distinction in everything but name.

**Lean** loads dependencies' `.olean` files and constructs a pre-environment containing the union of the dependencies' environments — `Established` with N inputs, reached independently. It also splits a serialized environment into private, public and server parts, and does not propagate extension state across imports unless a persistent extension is registered: two answers to *what crosses a boundary*, decided per item rather than wholesale. Its trust posture is **not** taken — imports are believed and re-verification is an opt-in external pass (`lean4checker`), where Curios runs the kernel on the compile path and, since the crate split, can afford to.

**Idris 2** stores a TTC entry as either a binary blob or a processed definition and deserializes only on first lookup, because converting binary to a definition is costly and imported definitions are often never used. Curios restores its whole prelude eagerly, measured at 471 ms. That is Phase B's lever, and it composes with N units where eager restoration multiplies.

**Agda** stores interface files under `_build/VERSION` so that switching versions does not discard them. Curios keys its archive on a schema plus a source fingerprint instead, which is the same idea without the path.

**rustc** is the direct warning. Crate `B`'s metadata refers to crate `C` by *B's own* `CrateNum`, so when `A` loads `B` it must translate through a `cnum_map` in `B`'s `CrateMetadata`. That table exists because unit identity is a per-unit index. Curios's `Global::Authored(Qualifier)` is a global path and pays nothing for this — and the one place the same mistake is currently available is `RootId`, which the roots-redesign sketch proposed making a `Vec` index. **Do not.** An index stamped into an archived declaration is a `cnum_map` waiting to be written; see *Decided: a root is a prefix* below.

**GHC** splits its scope in two: a `HomePackageTable` per home unit for what is being built now (collected across units in a `HomeUnitGraph`), and a global `ExternalPackageState` for what is already built. Curios has one tier in Phase A because every unit is compiled in one process. The split arrives with Phase B, and it is a **provenance** question — cached or live — not a new scope type. Nothing in Phase A should anticipate it.

**OCaml with Dune** wraps a library by prefixing every module with the library name, precisely because top-level compilation-unit names must be unique at link, and gives the short names back through module aliases plus `-open`. That is prefix-as-identity and `Context::insert_scope` aliasing, arrived at independently. Its *packed modules* alternative carries the warning for Phase B: referring to a single module links the whole pack, which is the eager-restore cost in another shape.

**Swift** made the opposite choice on collisions: a local or imported declaration that collides with a module name resolves by precedence, consulting the declaration and falling back to qualified lookup. It then needed dedicated `::` module-selector syntax in 6.3 to say what was previously unsayable. That is the cost of resolving a mount collision instead of refusing it, and it is why *two units claiming one prefix is an error* below is an error rather than a precedence rule.

## What a unit is, and what a root is

**A unit mounts a set of prefixes, not one.** The prelude is one unit with three mounts — `/sys`, `/syn`, `/std` — and it cannot be three units, because `/syn` and `/std` are mutually dependent: `syn.crs`, `syn/Str.crs`, `syn/Eql.crs` and `syn/Char.crs` all `use /std/…`, and `std/Str.crs` uses `/syn/{True, False}`. There is no dependency order over the three. This was recorded as open for Phase C; it is closed, by looking.

**A mount carries a kind.** `/sys` is `Internal`, `/syn` and `/std` are `Privileged`, a package and the entry are `Ordinary`. Kinds differ *within* the prelude unit, so kind belongs to the mount and not to the unit.

**Exactly one unit mounts the empty prefix, and that unit is the entry.** Its modules keep the names they have today (`mod foo` is `/foo`), no program's spelling changes, and no error message moves. This is the one asymmetry in the model and it is a definition rather than a special case: a unit is the entry when it has no successors, and having no successors is what lets it own the root namespace.

**Mount sets are pairwise disjoint, and that disjointness is load-bearing three times over.** It is what makes `Scoped`'s "the entry's own shadows the base" unreachable rather than merely stated. It is what makes `Globals`, `Established` and the elaboration registries mount without collision. And it is what makes foreign import names disjoint for free: an `ffi` row's name is the declaration's fully qualified name with a leading `/`, so unit `/a`'s rows are `/a/…` and unit `/b`'s are `/b/…`.

## Decided

### A root is a prefix, and the `root` stamps are deleted

`RootId` is a closed four-variant enum stamped onto `Definition`, `RecDefinition`, `InductDecl`, `StructDecl` and `ConceptDecl`, and archived with all of them. `Definition::root`'s own documentation says what it is: *"`island`'s leading segment, precomputed once by `into_core` … so `Context::set_island` (and, downstream, the orphan-rule check) never has to re-derive it from `island` itself."* Taking a qualifier's first segment is `O(1)`; the cache buys nothing and costs the archive a stamp whose meaning depends on the compilation that wrote it.

The stamps go. What replaces them:

- **Identity** is the mount prefix, read off the name — `Global::Authored(q)`'s first segment, or for a `Global::Witness`, its `Definition::island`'s first segment. Nothing is stored and nothing can drift from the name it describes.
- **Kind** comes from the scope's mount table, which `Unit` carries and the fold assembles. `curios-text` already has the table (`ModuleInfo::root` becomes `ModuleInfo`'s mount lookup); `curios-elab`'s `Context` gains it as a field, seeded from the scope, which is what `root_of_head` and the orphan rule read.
- **An intrinsic head has no prefix** and answers `None`, which is privileged and equal to no authored root. That preserves today's verdicts exactly: `RootId::Sys` was never *matched* by an ordinary consumer, it only failed to match, and `None` fails to match identically.

This also deletes `RootId::of_segment` and `Qualifier::root_segment`. `of_segment` re-derives a root from a name string, which is the exact thing `RootId` exists to prevent, and it survives at two call sites — `interface.rs:376` and `interface.rs:400`, both building a synthetic constructor or concept-method namespace — where the correct answer is the *parent's* mount, already in the table one lookup away. `root_segment`'s only callers are those two.

**Rejected: making `RootId` a stable interned name and keeping the stamps.** It fixes the archive-portability half and leaves the redundancy: a field that restates the first segment of a name held beside it. Kind still needs the mount table at every site that asks, so the stamp saves one `O(1)` derivation and nothing else.

**Rejected: making `RootId` an index into the compilation's root list**, as the July roots sketch proposed. That is rustc's `cnum_map` reinvented — an identity meaningful only in the compilation that assigned it, stamped into an artifact another compilation reads. It survives Phase A only by an unwritten invariant that seeded roots always sort first, and Phase B breaks it.

### Two units claiming one prefix is diagnosed at mount

Not shadowed, not resolved by precedence. The registry knows both, so it names both. Swift's module selectors are what precedence eventually costs.

### The orphan rule requires distinguishable ordinary roots

This was previously listed as composing free. It does not. `register_witness` refuses when the declaring root is unprivileged, differs from the concept's root, and matches no key head's root. Every ordinary root today is the single value `RootId::Entry`, so two packages compare equal and the rule goes inert *exactly between the units it exists to separate* — two unrelated packages could each `satisfy` the same `(concept, key)` pair and collide unfixably at link, which is the outcome the rule exists to prevent.

Deleting the stamps fixes it as a side effect rather than as a separate rule: two packages have different prefixes, so they compare unequal. That is why the root decision is the **first** milestone and not a cleanup.

### Version coexistence is declined

Unchanged, and the reason is specific to this language. Two versions would declare two distinct *nominal* types spelled identically. In Rust that is already the ecosystem's worst diagnostic class; here it lands worse, because conversion is nominal, the kernel compares `Global`s, and the refusal surfaces from the certifier, which holds no source span for the other version. Coherence fares worse still: `Show(Bar_v1)` and `Show(Bar_v2)` are *different keys*, so the orphan rule never fires — two instances coexisting and silently failing to interoperate, which is the collision that rule exists to prevent arriving through the one door it does not watch.

Aliasing stays reachable and costs nothing: `Context::insert_scope` already maps a local string to a qualifier, so reference-name and declaration-name are distinct mechanisms that merely coincide today. That is OCaml's `-open` over a wrapped library, and it needs no `Global` to change.

### The scope's N predecessors are borrowed, not merged

One decision, applied uniformly, so no site invents its own answer:

| Holder | Today | Phase A | Why |
| --- | --- | --- | --- |
| `Scoped<'a, V>` (text) | `base: Option<&'a BTreeMap>` | `base: &'a [&'a BTreeMap]` | Borrowed; merging would copy every mount's resolution table per compilation |
| `NominalScope<'a>` (text) | `base: Option<&'a Module>` | `base: &'a [&'a Module]` | Same; the alias walk asks each in turn |
| `Established<'a>` (elab) | `module: Option<&'a Module>` | `modules: &'a [&'a Module]` | Its own documentation already names this as the intended extension; stays `Copy` |
| `Resumed<'a>` (elab) | `core: &'a Module` + one arena | `cores: &'a [&'a Module]` + one arena | The cores are borrowed per unit; the **arena is threaded**, because each erasure resumes over the last |
| `Globals` (cert) | `of(module, floor)` | `of` plus `mount(&Unit)` | It already *owns* copied registries, so accumulation is its shape; mounts are disjoint, so `mount` asserts rather than diagnoses |
| `Declarations<'a>` (analysis) | `base: Option<Registries>` | **unchanged** | Its base is whatever `Globals`/`Context` accumulated, which is already one pair of maps |
| `Context` (elab) | seeded per scope | **unchanged** | Accumulates by construction; `Established::seed_*` runs once per scope module |
| `ForeignStore` (abi) | per-compilation, discarded for the prelude | carried on `Unit`, unioned by the fold | See *The real gaps* |

`Established::nothing()`, `Scoped::default()` and `NominalScope::new(None, …)` all become the empty slice, which is the from-scratch case with no second implementation.

**Rejected: merging predecessors into one `Module` and keeping every holder single-based.** It would leave four types untouched and is wrong for the reason the splice was wrong: it copies the standard library into a per-compilation value, which is the cost the prelude-environment work removed.

### The CLI names units positionally

Phase A adds one repeatable flag, `--unit <prefix>=<path>`, whose order *is* the dependency order. No resolution, no versions, no lockfile, no transitive discovery — those are Phase C. This exists so Phase A is exercisable from a shell rather than only from a test, and so the manifest, when it arrives, has something to desugar into.

## The API, stated so it is not invented twice

A new crate, `curios-unit`, owns the unit and the two stages that do not judge.

```rust
// curios-unit

/// One prefix a unit claims, and the privilege tier that prefix carries.
pub struct Mount { pub prefix: Qualifier, pub kind: RootKind }

/// What one unit provides to its successors. `PreludeArchive` becomes this type's serialized form.
pub struct Unit {
    mounts: Vec<Mount>,
    table: BTreeMap<Qualifier, ModuleInfo>,   // resolution
    public: BTreeMap<Qualifier, PublicInterface>,
    core: curios_core::Module,                 // elaborated and zonked
    ersd: ErasedUnit,                          // arena + environment
    foreigns: ForeignStore,
    metavariable_floor: usize,
    binder_floor: usize,
    witness_floor: usize,
    universe_floor: usize,
}

/// The units already compiled, in dependency order.
pub struct Scope<'a> { units: &'a [Unit] }

/// The source of a unit that has not been compiled: its mounts and its parsed module tree,
/// plus — for the entry alone — the entrypoint's optional type and its tail.
pub struct UnitSource { /* mounts, modules, entrypoint: Option<Entrypoint> */ }

pub fn elaborate_unit(scope: Scope<'_>, source: &UnitSource, budget: u64)
    -> Result<Elaborated, Error>;          // lower + elaborate + zonk

pub fn erase_unit(scope: Scope<'_>, elaborated: Elaborated, budget: u64)
    -> Result<Unit, Error>;
```

And the fold, in `curios-pipeline`:

```rust
let mut units: Vec<Unit> = restored;                  // the prelude arrives as data
let mut globals = Globals::default();
for unit in &units { globals.mount(unit); }

for source in sources {
    let elaborated = elaborate_unit(Scope::over(&units), source, budget)?;
    judge(&elaborated, &globals)?;                    // curios-cert
    let unit = erase_unit(Scope::over(&units), elaborated, budget)?;
    globals.mount(&unit);
    units.push(unit);
}
```

Two mechanical facts that will otherwise be discovered at the borrow checker:

- `Scope::over(&units)` is rebuilt each iteration rather than held across the loop, because `units.push` needs the vector back. It is a slice borrow, so rebuilding is free.
- `Globals` is threaded by value instead, because it owns its maps. Rebuilding it per iteration would be `O(N × total items)`; mounting incrementally is `O(total items)` once. `mount` runs before any walk, so the memo-invalidation coupling `Globals::insert` carries does not apply and `mount` asserts disjointness instead of reporting an overwrite.

### Why a new crate, and the rule that keeps it honest

`curios-pipeline` cannot hold `Unit`: it depends on `curios-cert`, so a build script reaching it re-runs on every kernel edit — which is the 469-second regression `curios-analysis` was split out to fix, arriving through a different door. `curios-prelude-archive`'s build script must construct a `Unit`, so `Unit` must live below the kernel.

`curios-unit` depends on `curios-text` (hence `curios-elab`, `curios-ersd`) and never on `curios-cert`. The rule is checkable, and it is the same rule the archive crate already states for itself:

```sh
cargo tree -p curios-unit --edges normal      # must not contain curios-cert
cargo tree -p curios-prelude-archive --edges build   # must not contain curios-cert
```

It also removes a cycle that the "build the prelude through `curios-pipeline`" phrasing would have created: `curios-pipeline` needs `curios-prelude` as a **dev**-dependency for its 97 tests, and with the archive building through `curios-unit` instead there is no edge back at all.

## The milestones

Five, in order. Each is a separate commit or run of commits, each clears the full gate on its own, and A1 through A4 must not change what the compiler decides about any program.

### A1 — A root is a mounted prefix

Delete `RootId`, `RootId::of_segment` and `Qualifier::root_segment`. Delete the `root` stamps from `Definition`, `RecDefinition`, `InductDecl`, `StructDecl` and `ConceptDecl`, and bump the archive schema. Introduce `Mount`, and the mount table on `curios-text`'s resolution and on `curios-elab`'s `Context`. `RootId::Entry` stops standing in for the synthetic compilation root: the empty qualifier's `ModuleInfo` and `PublicInterface` become scope-owned, built by the mount registration rather than by any unit's own scan.

*Must not change:* any verdict. The compilation still has exactly the four roots it has today, so every kind lookup returns what the stamp returned.

*Verified by:* the full gate, plus the existing orphan-rule and internal-root-privilege fixtures, plus a new one asserting an ordinary root still cannot reach `/sys`.

*Moves together with the deletion, and cannot be split from it:* the two `of_segment` call sites, `Context::root()`'s twelve write sites in `into_core.rs`, `zonk.rs`'s two copies, `established.rs`'s replay, `root_of_head`, and the orphan-rule comparison.

### A2 — The scope holds N predecessors

`Scoped`, `NominalScope`, `Established` and `Resumed` take slices per the table above; `Globals` gains `mount`. Nothing yet passes more than one.

*Must not change:* any verdict. Every caller passes a one-element slice.

*Verified by:* the full gate. This milestone is where a mistake is cheapest, because the one-element case is the whole corpus.

### A3 — `Unit`, `Scope`, and one spelling per stage

Create `curios-unit`. `PreparedPrelude`, `ErasedPrelude` and `PreludeArchive`'s payload collapse into `Unit`. Collapse the three lowerings into one, the two erasures into one, and give `elaborate_and_zonk_with_prelude` an `Established` rather than a `&Module`. `curios-prelude-archive`'s build script calls `elaborate_unit` + `erase_unit`; `curios-prelude`'s build script still judges the restored unit.

*Must not change:* any verdict, and not the archive's determinism — `build.rs` already serializes twice and compares, and that assertion stays.

*Verified by:* the full gate, plus `cargo tree` on both edge rules above, plus the empirical check that a `curios-cert` edit rebuilds `curios-cert` and `curios-prelude` and nothing else.

### A4 — The driver stops naming the standard library

`curios-pipeline` drops `curios-prelude` from `[dependencies]` and picks it up in `[dev-dependencies]`. `compile_entrypoint` takes a scope. `curios` and `curios-web` restore the prelude unit and pass it, exactly as they already pass nothing and let the pipeline import `SYNTAX`.

*Must not change:* any verdict, and not the public shape of `curios::compile`/`run`, which keeps supplying the prelude itself.

*Verified by:* the full gate, plus `cargo tree -p curios-pipeline --edges normal` not containing `curios-prelude`, plus `make curios/web`.

### A5 — More than one unit

The `--unit` flag, the mount-collision diagnostic, the foreign-store union, the forward-reference diagnostic, and the orphan rule firing between two ordinary units. This is the first milestone that changes what the compiler accepts, and it only adds refusals.

*Verified by:* the tests below.

## What composes free, and costs nothing

**Identity.** Four monotonic counters — metavariable, binder, witness, universe — each seeded from its predecessor's final count, and `Entropy::seed` only raises. A topological order seeds each unit from the running maximum, which is exactly what prelude→entry does today. **The `Binder identity` work belongs to Phase B**, and only because independently elaborated units are combined later.

**Erasure.** `Resumed` restores an arena and erases a unit's items onto it; N units compose by threading it, and there is no link step to invent at the erased level.

**Visibility, which was expected to be the sweep and is not.** `Audiences` computes who-can-see-what as *sets of subtree roots* — a declaration is visible to consumer `C` when `C` lies within any of them — and `pub` inside a private module reaches exactly that module's audience and no further. A package is a subtree, so a package's internals under a non-`pub` submodule are already package-scoped. Rust needed `pub(crate)` because its module tree and crate boundary are different things; here they coincide. Visibility stays a `bool` on two axes (`vis_pub` for the name, `rep_pub` for the representation) and gains no third level.

**The certifier's item selection.** `verdicts_from` already decides what to judge by asking `Globals::in_scope` per declared name, reads no length, and requires no item to sit anywhere in particular. N units need nothing from it.

## The real gaps

**Foreign rows reach no successor.** `prepare_prelude` builds a `ForeignStore`, passes it to `process_items`, and never returns it — vacuously fine today, because the prelude declares no `foreign` items and uses the `sys` tier instead, and a real defect the moment a *package* declares one. `Unit` carries its rows and the fold unions them. What does **not** need solving is the collision the earlier draft feared: `register`'s panic on a duplicate import name stays a construction bug, because prefixes are disjoint and an `ffi` name is the fully qualified declaration name. The one shape that could collide — a package mounted at `/foo` and an entry module `mod foo` — is the mount collision A5 already refuses.

**The entrypoint asymmetry is unrepresented.** `curios_core::Module::body` is a non-optional `Term`, so `prepare_prelude` stores `Nat::Zero` and the prelude's build certifies a dummy entrypoint. That is `RootId::Entry`'s double duty in another place: a value standing in for "there is none". Make `Module::body` and the entrypoint's `type_` optional together, with the invariant that exactly one unit in a compilation has them. Six sites read it and no more: `recheck.rs`'s `check_entrypoint` call and its two universe residue/escape checks, `seal_entry` in erasure, `reach.rs`'s entrypoint annotation walk, `suggest.rs`'s goal collection, and `curios_core::Module`'s free-variable walk.

**A forward reference deserves its own diagnostic.** With the order supplied by the caller, a unit naming a later unit's prefix fails as an ordinary unbound name. The mount table knows every prefix before any unit compiles, so the driver can upgrade that refusal to name the ordering: *`/b` is mounted after `/a`; a unit may only reference units before it*. This is a diagnostic refinement over an existing refusal, not a change to resolution.

**The visibility fixed point is already whole-scope, per compilation.** `Audiences::compute` iterates `Scoped::iter` — the union of scope and unit — and runs to a fixed point, and `audit_public_exposures` calls it on every compile. That is `O(everything in scope)` today and becomes `O(all units)`. It is correct, it is not new, and it is the whole-scope pass most worth measuring before Phase B decides what to cache. Measure it; do not assume it.

## Phase B — cached units

With units in place, caching a unit's elaborated Core and its verdict is the extension the shape was built for: the scope is assembled from N cached units rather than recompiled, and there is no seam to identify because there was never a prefix. GHC's home/external split arrives here as the provenance of a unit, not as a second scope type.

**Identities must survive combining independently elaborated units.** The counters are monotonic and seeded from watermarks; two units elaborated in *separate processes* mint from overlapping ranges, and an aliased binder silently identifies two terms that differ. [SOUNDNESS.md](../../SOUNDNESS.md)'s *Binder identity* row is graded **argued**, with one positive control — adequate for a single carried floor combined by maximum, not for N units composed pairwise. **This row must be defended before any unit is cached**, and it is the first task of this phase rather than a later one.

**Verdicts must be keyed on the terms and the certifier, never on a path or a timestamp.** GHC fingerprints each declaration's interface and recompiles what actually depends on what changed; Cargo's granularity is the crate, which is what made a kernel edit re-elaborate the standard library until `curios-analysis` was split out. A coarser key reintroduces that failure at a level where it *admits* rather than merely costing time.

**Anything reasoning over "every name the program has" becomes a scope question.** Retiring the splice broke two such sites — strict positivity's declaration set, and `build_shorten`'s abbreviation table, which also starved `nominal_plicities` beside it. Both were found by a test rather than by inspection. Before caching multiplies what "in scope" can mean, search for `module_symbols`, `nominal_plicities`, bare `items.iter()` and registry iteration, and decide each deliberately.

**Lazy entries are the lever.** Idris 2's blob-until-first-lookup is the direct answer to the 471 ms eager restore, and it matters more with N units than with one. It also argues against a Lean-style split artifact, and against OCaml's packed modules for the same reason: if entries are lazy, nothing pays for what it does not name.

**Whole-unit passes never cache and re-run at link:** strict positivity over the complete declaration set, declaration sizing, concept-registry validation, witness coherence, and the visibility fixed point — program-wide by definition, since a coherence violation is only visible where two units meet. That bounds the win without removing it, because per-item typing is the expensive part.

### Deferred here, with its reasoning: parallel per-item certification

Split the certifier's walk into a serial define-all phase and a parallel check-all phase, one `Kernel` per item over a shared read-only environment, verdicts sorted by item index for determinism. Per-item kernels settle binder identity without arithmetic: each is seeded at the same derived floor, above every identity in the module, so two workers minting the same index never share a scope. A shared counter is ruled out — nondeterministic under work stealing, and the archive must stay byte-reproducible. Any parallelism must be feature-gated native-only, because `curios-web` compiles `curios-cert` to `wasm32-unknown-unknown`, which has no threads.

**Parked on 2026-08-09, on measurement.** After the crate split, certification is the whole cost of a kernel-edit rebuild rather than 18% of a full one — but that loop is ~100 s and the win is perhaps 60–70 s, against per-item kernels, a feature gate, a memo-cost measurement, determinism obligations, and **concurrency inside the trusted base**, where *parallel verdicts equal serial verdicts* becomes something to prove. The same day's profiling put 469 s of a ~570 s prelude build in elaboration, 204 s of it in universe finalization, and **63% of that in one declaration** — `/std/Async/block_on`, whose constraint graph is three orders of magnitude larger than any other. That sits outside the trusted base and may be contained inside `finalize`. Revisit if the kernel loop hurts again.

## Phase C — the manifest

Sketched only; it wants its own specification once A and B have landed.

A manifest maps a **name** to a **source**, and the name becomes a mount — Coq's `-Q dir Lib`, which is the model Phase A already adopts. It declares a unit's dependencies, which is what supplies the fold's order in place of `--unit`'s positional one, and its privilege tier, which is what the mount table already reads.

**Packages ship source, not artifacts.** The archive is build-scoped and deliberately not an interchange format; generalizing "one artifact per package" would quietly make it one. Rust ships source and rebuilds, which keeps that constraint honest and keeps Phase B about *local* caching rather than distribution.

Open, and genuinely undecided: whether a manifest may mount a privileged root at all, or whether that tier stays reserved for the compiler's own; and whether a package may mount more than one prefix, which the prelude needs and which nothing else has asked for.

## Out of scope

- **Parallelising elaboration.** The shared monotonic counters are a serialization point by design.
- **A third visibility level.** Package-privacy is subtree containment, which the audience model already expresses.
- **Making the archive a stable interchange format.**
- **Version coexistence**, per the decision above.
- **Dependency resolution, lockfiles, and fetching.** Phase C, and probably its own specification after that.

## Tests

The property an earlier draft asserted here — that N units compile identically to the same program written as one unit with `mod` declarations — is **false**, and stating it inverted the point. The orphan rule *only bites an ordinary root*, so a witness in unit `A` for a concept in unit `B` is refused across units and accepted across modules of one unit. The unit boundary is exactly where coherence is enforced; a test asserting it is invisible would assert the rule away.

- **A1:** an ordinary root still cannot reference `/sys`, and a `/std` witness for a `/syn` concept at a `/sys` type is still exempt. Both already have fixtures; they must survive the stamp deletion unchanged.
- **A5, the boundary is semantic:** a witness declared in ordinary unit `A` for a concept declared in ordinary unit `B` over a type declared in ordinary unit `C` is refused as an orphan; the same three declarations as three `mod`s of one unit are accepted. The pair is the test — either half alone proves nothing.
- **A5, the boundary is otherwise invisible:** a program whose units declare no concepts and no witnesses compiles to the same wasm as the same declarations written as `mod`s. This is the salvageable half of the false property, scoped to where it holds.
- **A5:** two units mounting the same prefix is diagnosed naming both, not a panic and not a silent shadow.
- **A5:** an entry module `mod foo` alongside a unit mounted at `/foo` is the same diagnostic, which is also what keeps foreign import names disjoint.
- **A5:** two units each declaring a `foreign` row surface both in the store `compile_entrypoint` returns, under their own qualified names, and an embedder can bind both.
- **A5:** a unit referencing a prefix mounted after it is refused by the ordering diagnostic, not by a bare unbound name.
- **A5:** a unit's `pub` item exposing a dependency's non-`pub` type is refused by the existing audit, with no new rule.
- **Phase B:** as its section states, `Binder identity` probed before anything is cached.

## Retirement criteria

Before this specification is deleted: `curios-pipeline` names no crate specific to the standard library outside `[dev-dependencies]`; `RootId`, `of_segment` and `root_segment` are gone and no stage derives or stores a root beside the name that determines it; `Unit` is the only artifact a unit produces and `PreludeArchive` is its serialized form; the fold and the `Unit` boundary are recorded in `curios-pipeline`'s and `curios-unit`'s crate documentation, and the mount table's logical-to-physical mapping in `curios-text`'s; *Binder identity* is defended beyond **argued** if any part of Phase B has landed; and Phase C, if still pending, is carried out to a specification of its own.
