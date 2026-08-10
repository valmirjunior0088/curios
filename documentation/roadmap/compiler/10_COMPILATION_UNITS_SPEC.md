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

**Idris 2** stores a TTC entry as either a binary blob or a processed definition and deserializes only on first lookup, because converting binary to a definition is costly and imported definitions are often never used. Curios restores its whole prelude eagerly and, on the strength of a 471 ms figure, this specification once made that a phase item of its own. Re-measured in release it is **34.4 ms** for the whole image and 1.4 ms for the erased clone taken per compile, so the analogy holds and the lever does not; see [Out of the caching half](../tooling/01_PROJECTS_SPEC.md#out-of-the-caching-half).

**Agda** stores interface files under `_build/VERSION` so that switching versions does not discard them. Curios keys its archive on a schema plus a source fingerprint instead, which is the same idea without the path.

**rustc** is the direct warning. Crate `B`'s metadata refers to crate `C` by *B's own* `CrateNum`, so when `A` loads `B` it must translate through a `cnum_map` in `B`'s `CrateMetadata`. That table exists because unit identity is a per-unit index. Curios's `Global::Authored(Qualifier)` is a global path and pays nothing for this — and the one place the same mistake is currently available is `RootId`, which the roots-redesign sketch proposed making a `Vec` index. **Do not.** An index stamped into an archived declaration is a `cnum_map` waiting to be written; see *Decided: a root is a prefix* below.

**GHC** splits its scope in two: a `HomePackageTable` per home unit for what is being built now (collected across units in a `HomeUnitGraph`), and a global `ExternalPackageState` for what is already built. Curios has one tier in Phase A because every unit is compiled in one process. The split arrives with [the caching half](../tooling/01_PROJECTS_SPEC.md#the-caching-half), and it is a **provenance** question — cached or live — not a new scope type. Nothing in Phase A should anticipate it.

**OCaml with Dune** wraps a library by prefixing every module with the library name, precisely because top-level compilation-unit names must be unique at link, and gives the short names back through module aliases plus `-open`. That is prefix-as-identity and `Context::insert_scope` aliasing, arrived at independently. Its *packed modules* alternative is the shape a cached erased prefix deliberately takes — referring to one module brings the whole pack — which is affordable here only because the pack is restored in 34.4 ms and is keyed on the dependency set that produced it.

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

**Rejected: making `RootId` an index into the compilation's root list**, as the July roots sketch proposed. That is rustc's `cnum_map` reinvented — an identity meaningful only in the compilation that assigned it, stamped into an artifact another compilation reads. It survives Phase A only by an unwritten invariant that seeded roots always sort first, and storing a unit breaks it.

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
| `Scoped<'a, V>` (text) | `base: Option<&'a BTreeMap>` | `bases: &'a [&'a BTreeMap]` | Borrowed; merging would copy every mount's resolution table per compilation |
| `NominalScope<'a>` (text) | `base: Option<&'a Module>` | `bases: &'a [&'a Module]` | Same; the alias walk asks each in turn |
| `Established<'a>` (elab) | `module: Option<&'a Module>` | `modules: &'a [&'a Module]` | Its own documentation already names this as the intended extension; stays `Copy` |
| `Resumed<'a>` (elab) | `core: &'a Module` + one arena | `cores: &'a [&'a Module]` + one arena | The cores are borrowed per unit; the **arena is threaded**, because each erasure resumes over the last |
| `Globals` (cert) | `of(module, floor)` | `of` plus `mount(module, floor)` | It already *owns* copied registries, so accumulation is its shape; mounts are disjoint, so `mount` asserts rather than diagnoses |
| `Declarations<'a>` (analysis) | `base: Option<Registries>` | **unchanged** | Its base is whatever `Globals`/`Context` accumulated, which is already one pair of maps |
| `Context` (elab) | seeded per scope | **unchanged** | Accumulates by construction; `Established::seed_*` runs once per scope module |
| `ForeignStore` (abi) | per-compilation, discarded for the prelude | carried on `Unit`, unioned by the fold | See *The real gaps* |

`Established::nothing()`, `Scoped::default()` and `NominalScope::new(&[], …)` all become the empty slice, which is the from-scratch case with no second implementation. Reading latest-first is what makes the nearest half win, which is the same shadowing rule stated in each type and unreachable while mount sets stay disjoint.

**A one-element slice needs something to borrow from.** Every caller that had one predecessor now writes `from_ref(&base)` over a *bound* reference rather than `&[&base]`, because the latter is a temporary that dies at the end of the statement. It is a small thing and it is where all four call sites first failed to compile.

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
///
/// One opaque artifact per stage, not their fields flattened: `curios-text`'s resolution tables
/// and `curios-elab`'s arena stay private to those crates, so what this type adds is the pairing —
/// the halves describe *one* unit, and nothing else says so. The foreign rows and the four
/// watermarks ride inside the lowered half, which is what produced them.
pub struct Unit {
    text: curios_text::PreparedPrelude,   // mounts, resolution, interfaces, foreigns, watermarks
    core: curios_core::Module,            // elaborated and zonked
    ersd: curios_elab::ErasedUnit,        // arena + environment
    binder_floor: usize,                  // derived over `core` by the walk that built this
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

**One field is provisional, and A3 landed it knowing so.** `ersd` reads as one erased artifact per unit, which is right while every unit is erased in one process in dependency order. The deferred [B4](../tooling/01_PROJECTS_SPEC.md#b4--the-erased-artifact-is-keyed-on-the-prefix-not-on-the-unit) decides that a *stored* erased artifact belongs to the ordered set of predecessors instead, because per-unit arenas would need a relocation pass. With one unit the two readings coincide — the prelude is the prefix — so A3 looks correct either way and its gate cannot tell them apart. It is written so the erased half can move off `Unit` without disturbing the rest.

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

**Where this stands.** All five have landed and Phase A is on `main`. Phase B has since been narrowed to [the two milestones that check something today](#phase-b--the-invariant-half); the four that only pay off once bytes are stored moved out with the whole package boundary, into [the projects specification](../tooling/01_PROJECTS_SPEC.md). Each milestone below records what actually landed under it, including where the implementation differed from what this section specified — a milestone whose entry says nothing landed exactly as written.

### A1 — A root is a mounted prefix

**Landed**, in two commits: deleting `of_segment` turned out to be separable from deleting the stamps, and gating it alone is cheap where the stamp deletion costs a prelude rebuild.

Delete `RootId`, `RootId::of_segment` and `Qualifier::root_segment`. Delete the `root` stamps from `Definition`, `RecDefinition`, `InductDecl`, `StructDecl` and `ConceptDecl`, and bump the archive schema. Introduce `Mount`, and the mount table on `curios-text`'s resolution and on `curios-elab`'s `Context`. `RootId::Entry` stops standing in for the synthetic compilation root: the empty qualifier's `ModuleInfo` and `PublicInterface` become scope-owned, built by the mount registration rather than by any unit's own scan.

*Must not change:* any verdict. The compilation still has exactly the four roots it has today, so every kind lookup returns what the stamp returned.

*Verified by:* the full gate, plus the existing orphan-rule and internal-root-privilege fixtures, plus a new one asserting an ordinary root still cannot reach `/sys`.

*Moves together with the deletion, and cannot be split from it:* the two `of_segment` call sites, `Context::root()`'s twelve write sites in `into_core.rs`, `zonk.rs`'s two copies, `established.rs`'s replay, `root_of_head`, and the orphan-rule comparison.

**Five differences between what landed and the paragraphs above.** Each is the specification being corrected by the implementation, not the other way round.

- **There was a sixth stamp.** `curios-elab`'s own `Witness` carried a `root` that was written at registration and read nowhere — the orphan rule used the parameter beside it. It is gone too.
- **The mount list lives on `curios_core::Module`**, one per module, not threaded separately. `Established` and the elaborator's `Program` both need kind lookups and `Module` is already what crosses that boundary carrying `induct_decls`, `concepts`, `witnesses` and `binder_floor` — so `Unit` carries the mounts by carrying the module. The kernel reads neither, so `Globals` is untouched.
- **`Module::mounts` is the unit's own claim, not the compilation's.** Lowering an entry against a scope needs every mount for privilege checks and records only `[empty → Ordinary]` on the module. Conflating the two would have made every entry module claim `/sys`, `/syn` and `/std`.
- **`ModuleInfo` lost `root` outright**, so the parent-inherited root the first commit introduced was deleted by the second. The synthetic root is scope-owned on the prelude path as specified; on the entry path the empty qualifier's `ModuleInfo` *is* the entry's own scan, which is not an exception but the definition of the unit that mounts the empty prefix.
- **`root_of_head` became `mount_of_head`**, returning `Option<&Mount>`. `None` means "claimed by no authored mount", which no declaring mount equals — exactly what the `RootId::Sys` fallback achieved by never being matched.

### A2 — The scope holds N predecessors

**Landed.** 1645 tests pass, none fail.

`Scoped`, `NominalScope`, `Established` and `Resumed` take slices per the table above; `Globals` gains `mount`. Nothing yet passes more than one.

*Must not change:* any verdict. Every caller passes a one-element slice.

*Verified by:* the full gate. This milestone is where a mistake is cheapest, because the one-element case is the whole corpus.

**Three differences between what landed and the table above.**

- **`Scoped::iter` needed a rule the table did not state.** `get` is a lookup and stops at the first hit; `iter` yields *every* entry in scope and so must drop what a nearer half shadows — the unit's own, then each base against every later base. The two now answer by one rule, which is what the table's "borrowed, not merged" costs and what a merged map would have hidden.
- **`Globals::mount` takes a module and a floor, not a `&Unit`.** `Unit` does not exist until A3, and inventing it early would have made A2 depend on the milestone after it.
- **`Established::of` became `Established::over`**, and `Resumed::projected_core` became `projected_cores`, because both now take the plural. `Established::nothing()` survives as the empty slice, which is the from-scratch case with no second implementation — as specified.

### A3 — `Unit`, `Scope`, and one spelling per stage

**Landed**, in three commits, because it turned out to be three ideas rather than one: the entrypoint and the erasure collapse, the crate and the artifact, and the lowering collapse.

Create `curios-unit`. `PreparedPrelude`, `ErasedPrelude` and `PreludeArchive`'s payload collapse into `Unit`. Collapse the three lowerings into one, the two erasures into one, and give `elaborate_and_zonk_with_prelude` an `Established` rather than a `&Module`. `curios-prelude-archive`'s build script calls `elaborate_unit` + `erase_unit`; `curios-prelude`'s build script still judges the restored unit.

**Being the entry means having an entrypoint, and that turned out to be the erasure collapse rather than a separate cleanup.** `Module::body` is now `Option<Term>`; `erase_prelude_prefix` and `erase_module_with_prelude` are one `erase_unit`, which seals an entry when the unit has one and leaves the arena open when it does not. The two spellings differed by that condition and by nothing else, so making the asymmetry representable *is* what merged them. `ErasedPrelude` becomes `ErasedUnit` and gains `Default`, which is the empty scope — sound because `ErsdBuilder::resume` over an empty module reindexes nothing and yields exactly a fresh builder, so "erase the first unit" and "erase a later one" are one call.

Three consequences worth recording:

- **`erase_unit` asserts that a body and its expected type arrive together.** Nothing previously stopped a caller pairing one with the other's absence.
- **The archive's `body_type` is deleted.** It was computed by `build.rs`, stored, exposed through an accessor, and read by nothing — an entrypoint type for a unit with no entrypoint. Elaboration's body type is now `Option<Term>` all the way out, which is what made the dead field visible.
- **The kernel no longer certifies a dummy.** `check_entrypoint` runs only where there is a body, so the prelude's `Nat::Zero` stopped being judged because it stopped existing.

**`curios-unit` exists and the restored prelude *is* a `Unit`.** Both edge rules were checked rather than assumed: neither `cargo tree -p curios-unit --edges normal` nor `cargo tree -p curios-prelude-archive --edges build` contains `curios-cert`. Three corrections to the API block above, all forced by encapsulation:

- **`Unit` composes one opaque artifact per stage**, as [the API block](#the-api-stated-so-it-is-not-invented-twice) now shows. `ModuleInfo` and `PublicInterface` are `pub(super)` inside `curios-text`; holding them on `Unit` directly would have exported a resolver's internals for no consumer.
- **`Scope` hands each stage a slice of *that stage's* type** — `Vec<&PreparedPrelude>` to the lowerer, `Vec<&Module>` to elaboration and the kernel — and each builds its own view. It cannot construct a `Scoped` itself, for the same reason.
- **The arena is one value on `Scope`, not one per unit**, mirroring `Resumed`: `Scope::arena` returns the last unit's, because each erasure resumes over what the previous one produced. An empty scope yields `ErasedUnit::default()`.

**And the foreign rows stopped being discarded.** `PreparedPrelude` now carries what `prepare_prelude` collects, so `Unit::foreigns` delegates rather than storing an empty store the build never computed. That was going to be an A5 gap; landing it here was cheaper than writing a stand-in and deleting it later.

**Three lowerings became one, and three interface resolutions with them.** `into_core_unit` takes a [`UnitSource`] — an entrypoint with its loader, or a set of already-parsed modules under the prefixes they claim — and a scope, and every difference between the three walks is one of its arguments. `interface::resolve_unit` collapses the same way: the three differed only in which items were seeded at which prefix. The old names survive as four-line adapters, which is what lets a later milestone delete them without touching the walk.

Two rules the merge had to state that no single walk had needed:

- **The counters seed from the maximum across every predecessor**, not from one. With a single scope unit the two readings coincide, which is why nothing had to choose before.
- **The universe seed table is the scope's, concatenated in dependency order.** Each unit allocated above the last, so concatenation keeps every `UniverseMetaId` at its own index — a merge that sorted or deduplicated would not.

The collapse also reported its own completeness: `Resolved::new`, `Resolved::for_entrypoint` and a `from_ref` import went dead the moment the three walks became one, and deleting them is the whole of what the dead-code warning asked for.

*Must not change:* any verdict, and not the archive's determinism — `build.rs` already serializes twice and compares, and that assertion stays.

*Verified by:* the full gate, plus `cargo tree` on both edge rules above, plus the empirical check that a `curios-cert` edit rebuilds `curios-cert` and `curios-prelude` and nothing else.

### A4 — The driver stops naming the standard library

**Landed.** All three edge rules check clean, and `make curios/web` passes.

`curios-pipeline` drops `curios-prelude` from `[dependencies]` and picks it up in `[dev-dependencies]`. `compile_entrypoint` takes a scope. `curios` and `curios-web` restore the prelude unit and pass it, exactly as they already pass nothing and let the pipeline import `SYNTAX`.

**A product names its standard library; the driver has no way to.** `curios` gained `compile_with_prelude`, `typecheck_with_prelude` and `recheck_with_prelude` — every CLI path, embedder helper and integration fixture goes through them, so one place answers "what does a Curios program get for free". `curios-web` does the same inline, and `curios-pipeline`'s own 97 fixtures go through a `compile_fixture` helper backed by the dev-dependency. `SYNTAX` became a parameter beside the scope for the same reason it always should have been: the driver knows *that* a registry is needed and not *which*.

Two things A4 forced that were scheduled elsewhere:

- **`format_with` is N-ary in both checkers.** `Option<&Module>` became `&[&Module]` in `curios-elab` and `curios-cert` alike, because the driver has no single module to hand it. That is [B5](#b5--every-all-the-names-in-the-program-site-becomes-a-scope-question)'s site, and it arrived here rather than there.
- **`Scope::units()` exists because `curios-unit` cannot build a `Globals`.** The kernel's environment needs each unit's module *and* its carried binder floor together, and the crate is defined to stay below `curios-cert` — so the driver iterates and mounts. The boundary held under the first real pull on it.

*Must not change:* any verdict, and not the public shape of `curios::compile`/`run`, which keeps supplying the prelude itself.

*Verified by:* the full gate, plus `cargo tree -p curios-pipeline --edges normal` not containing `curios-prelude`, plus `make curios/web`.

### A5 — More than one unit

**Landed**, in three commits: the fold, the refusals and the union, then the flag. `curios --unit lib=lib.crs run main.crs` compiles and runs a two-unit program.

The `--unit` flag, the mount-collision diagnostic, the foreign-store union, the forward-reference diagnostic, and the orphan rule firing between two ordinary units. This is the first milestone that changes what the compiler accepts, and it only adds refusals.

*Verified by:* the tests below.

**Writing the tests found a real defect, which is the argument for writing them.** A mounted unit could not see `/std`: `Resolved::for_mounted` rebuilt the synthetic compilation root's children from its *own* mounts, and because that write lands in the unit's own layer it shadowed the scope's — so the standard library disappeared from every unit except the entry. The root belongs to the scope, as stated above; only half of that was implemented. Nothing but a second unit could have shown it.

**Two corrections to earlier milestones fell out of the same tests:**

- **A3c's universe-seed concatenation was wrong.** A module carries the *cumulative* seed table from index zero — `universe_floor` is asserted equal to its length — so the scope's table is the last unit's, already containing every earlier one. Concatenating counted each predecessor once per successor. The assertion caught it on the first three-unit compile.
- **The mount check has to run before discovery.** Left after it, `insert_child` reports the collision first as a duplicate declaration, which names the label but not what else claimed it.

**The `ffi` namespace needs no new rule**, as predicted: an import name is its declaration's fully qualified name, so mount disjointness makes the union disjoint, and `ForeignStore::absorb` asserts rather than diagnoses. The one shape that could collide — a mounted `/foo` beside an entry's own `mod foo` — is the mount collision, refused upstream.

**A mounted unit's tree is materialized eagerly**, by `curios`'s `load_unit`, because discovery of a unit in scope has no loader to reach: `curios-web` supplies every body inline and compiles with no file system at all. The walk lives at the one boundary that does have one.

## What composes free, and costs nothing

**Identity.** Four monotonic counters — metavariable, binder, witness, universe — each seeded from its predecessor's final count, and `Entropy::seed` only raises. A topological order seeds each unit from the running maximum, which is exactly what prelude→entry does today. That is the whole of it *within* one compilation. Across compilations only the witness counter matters, for the reason the deferred [B1](../tooling/01_PROJECTS_SPEC.md#b1--a-witness-is-identified-by-its-mount) gives; the other three are measured against a stored unit under [the rule the storage check is written against](#the-rule-the-storage-check-is-written-against), and none of them reaches one.

**Erasure.** `Resumed` restores an arena and erases a unit's items onto it; N units compose by threading it, and there is no link step to invent at the erased level — *while every unit is erased in one process in dependency order*, which is Phase A and is what the compiler does today. The deferred [B4](../tooling/01_PROJECTS_SPEC.md#b4--the-erased-artifact-is-keyed-on-the-prefix-not-on-the-unit) is what would keep it true once erased artifacts are stored.

**Visibility, which was expected to be the sweep and is not.** `Audiences` computes who-can-see-what as *sets of subtree roots* — a declaration is visible to consumer `C` when `C` lies within any of them — and `pub` inside a private module reaches exactly that module's audience and no further. A package is a subtree, so a package's internals under a non-`pub` submodule are already package-scoped. Rust needed `pub(crate)` because its module tree and crate boundary are different things; here they coincide. Visibility stays a `bool` on two axes (`vis_pub` for the name, `rep_pub` for the representation) and gains no third level.

**The certifier's item selection.** `verdicts_from` already decides what to judge by asking `Globals::in_scope` per declared name, reads no length, and requires no item to sit anywhere in particular. N units need nothing from it.

## The real gaps

**Foreign rows reach no successor.** `prepare_prelude` builds a `ForeignStore`, passes it to `process_items`, and never returns it — vacuously fine today, because the prelude declares no `foreign` items and uses the `sys` tier instead, and a real defect the moment a *package* declares one. `Unit` carries its rows and the fold unions them. What does **not** need solving is the collision the earlier draft feared: `register`'s panic on a duplicate import name stays a construction bug, because prefixes are disjoint and an `ffi` name is the fully qualified declaration name. The one shape that could collide — a package mounted at `/foo` and an entry module `mod foo` — is the mount collision A5 already refuses.

**The entrypoint asymmetry is unrepresented.** `curios_core::Module::body` is a non-optional `Term`, so `prepare_prelude` stores `Nat::Zero` and the prelude's build certifies a dummy entrypoint. That is `RootId::Entry`'s double duty in another place: a value standing in for "there is none". Make `Module::body` and the entrypoint's `type_` optional together, with the invariant that exactly one unit in a compilation has them. Six sites read it and no more: `recheck.rs`'s `check_entrypoint` call and its two universe residue/escape checks, `seal_entry` in erasure, `reach.rs`'s entrypoint annotation walk, `suggest.rs`'s goal collection, and `curios_core::Module`'s free-variable walk.

**A forward reference deserves its own diagnostic.** With the order supplied by the caller, a unit naming a later unit's prefix fails as an ordinary unbound name. The mount table knows every prefix before any unit compiles, so the driver can upgrade that refusal to name the ordering: *`/b` is mounted after `/a`; a unit may only reference units before it*. This is a diagnostic refinement over an existing refusal, not a change to resolution.

**The visibility fixed point is already whole-scope, per compilation.** `Audiences::compute` iterates `Scoped::iter` — the union of scope and unit — and runs to a fixed point, and `audit_public_exposures` calls it on every compile. That is `O(everything in scope)` today and becomes `O(all units)`. It is correct, it is not new, and it is the whole-scope pass most worth measuring before anything decides what to cache. Measure it; do not assume it.

## Phase B — the invariant half

**The objective is that what a unit is trusted to carry is checked where the unit is produced, rather than assumed by whoever later reads it.** Two milestones, B2 and B5. The numbering has gaps because the four that are missing are deferred rather than renumbered — an identity is not a position, which is the argument this specification makes about everything else it names.

**What was deferred, and why.** This phase was specified as *cached units*: a unit compiled by one compilation consumed by another, so that depending on N packages does not cost N elaborations per build. That objective is intact and its specification survives whole, in [the projects specification](../tooling/01_PROJECTS_SPEC.md#the-caching-half). It is deferred because a cache before a package boundary has almost nothing to cache. The prelude is the only unit a compilation does not author, and Cargo already caches it; every other unit is one the programmer named on the command line and is presently editing. Designing a store's location, layout and invalidation against that case is designing against a one-package guess, and the store's shape is the part this specification is least able to settle without a real dependency graph in front of it. C1 and C2 need no cache in order to be correct — uncached, they re-elaborate every dependency per build, which is a cost statement rather than a correctness one, and it is the same statement [Packages ship source](../tooling/01_PROJECTS_SPEC.md#packages-ship-source-not-artifacts) already makes about that work at scale.

**What survives is what more than one unit made checkable now.** [A5](#a5--more-than-one-unit) put N units in a compilation, and two properties that were vacuous under a single predecessor stopped being vacuous: what an archive is trusted to carry, and what "all the names in the program" means. Neither waits on storage, and neither is verdict-preserving by construction, so each states its own obligation rather than inheriting one from [the milestones](#the-milestones).

### The rule the storage check is written against

> **A unit may be stored only if it carries no positional identity.**

A positional identity is one meaningful solely in the compilation that assigned it. Storing one is how rustc came to need `cnum_map`, it is why [the `root` stamps are deleted](#a-root-is-a-prefix-and-the-root-stamps-are-deleted), and it is the one property that decides whether a stored unit is portable. Measured against the stored prelude — 1079 items, 1094 definitions, release build:

| Identity | In a stored unit | Established by |
| --- | --- | --- |
| Term metavariable | none — zonking substitutes every solution and refuses an unsolved hole | zonk's contract, not a check on the archived value |
| Universe metavariable | none — a level holding one is not closed over its declaration's parameters, and `validate_bound_universes` refuses it by that name | `validate_universes`, asserted on the value `build.rs` serializes |
| Free local binder | none — `derived_binder_floor` over items *and* registries is **0**, against a lowering watermark of 6684 | nothing yet; B2 |
| Witness | **75, densely 0..74, 34 of them referenced from terms** | nothing; [deferred B1](../tooling/01_PROJECTS_SPEC.md#b1--a-witness-is-identified-by-its-mount) |

Two consequences, both stated because an earlier draft assumed otherwise. Of the four monotonic counters under [What composes free](#what-composes-free-and-costs-nothing), exactly one mints an identity that reaches a stored unit — the witness counter, which the deferred B1 scopes; the other three leave watermarks, which combine by maximum and cannot alias. And this phase makes **no claim on [SOUNDNESS.md](../../SOUNDNESS.md)'s *Binder identity* row**: that row is about a checker's own fresh mints aliasing identities in a live scope, which is a within-compilation property, and nothing a stored unit carries participates in it.

**B2 and B5 are independent of each other, and B2 is deliberately short of its third row.** Only the witness row waits on scoped witness identities, which is deferred; a check that refuses two of the three today and gains the third when a witness carries its mount is a documented gap rather than a broken precondition, and the alternative — waiting — leaves the two rows nothing watches unwatched for the sake of symmetry.

### B2 — the rule is checked where a unit is stored

**Landed.** `curios_core::validate_stored_identities` refuses a free local and a surviving metavariable, called from `curios-prelude-archive`'s build script beside `validate_universes`. Four tests: three refusals and one control.

The three `none` rows above are an observation about today's output, and the rule needs them as an invariant. Exactly **one** of them is asserted where an archive is written — `validate_universes`, on the value `build.rs` serializes. The other two are contracts of the passes that produced the value rather than checks on it: zonk refuses an unsolved hole, and nothing whatever watches free locals. `derived_binder_floor` exists precisely because the number a module carries is untrusted; `recheck.rs` says of `Module::binder_floor`, "which nothing checks".

So the rule becomes one function, called at the one seam where a unit is stored — which today is `curios-prelude-archive`'s build script, joining the universe check already standing there. A later change that begins leaving identities in stored output then fails at the boundary that cares, instead of aliasing silently in whatever compilation restores two such units together.

The witness row is the same function's next refusal, and it arrives with the [deferred B1](../tooling/01_PROJECTS_SPEC.md#b1--a-witness-is-identified-by-its-mount). Writing the function now is what makes that a line rather than a second search for the seam.

*Its obligation:* the refusals are the test. A check that only ever meets conforming input asserts nothing, so each refusal needs a unit built to trip it; the prelude passing is the control, not the evidence.

**Three differences between what landed and the paragraphs above.**

- **The rule is two calls at that seam, not one, and the paragraphs above overstated its reach.** "A metavariable of either kind" was wrong about what needed writing: `validate_bound_universes` already refuses an unsolved *universe* metavariable and names it in as many words, at this very seam. Restating it would be a second implementation of one predicate rather than a second opinion about it — the standing `UniverseContext::is_closed` records, in the same crate, for the same reason. So the new function refuses the two rows nothing watches, and the seam calls both.
- **The positions are enumerated once and read twice.** The check needed the same list `derived_binder_floor` walks, and that walk's own contract is that *"deciding a field cannot matter is the reasoning this walk exists to replace"* — which a second copy of the list would defeat on the day a position is added to one and not the other. `module_positions` is now that list, offering each position to a collector; the floor takes the highest local index, the check takes the first identity it may not carry. Behaviour-preserving for the floor, and the reason the entrypoint — the position belonging to no declared name, and so the one a hand-written list drops — has a test of its own.
- **It lives in `curios-core`, beside the floor, not in `curios-unit`.** The alternative reading is that "what a stored unit may carry" belongs to the crate that owns `Unit`. Against it: the walk it shares is here, `curios-unit` is not a build-dependency of the archive crate and would have to become one for a single function, and every future store depends on `curios-core` already. It describes rather than judges by this module's own rule — whether a node is a metavariable, and whether a variable is local, are properties of the representation — and it refuses rather than reports only because an identity, unlike a bound, has no safe direction to degrade in.

### B5 — every "all the names in the program" site becomes a scope question

**Landed, and it found no behaviour to change.** Fourteen sites audited, every one already scope-correct. What it did find is one defect repeated four times, and it is the kind an audit is the only way to catch: **a pass that is N-ary in code and single-predecessor in its stated reason.**

`reach.rs` skipped an out-of-module name because it was "a replayed prelude definition, whose own closure was settled when the archive was built"; `classify_module` treated an unknown name as total because "every `Global` a zonked module mentions is defined either in it or in the prelude"; `check_positivity` called its boundary sound because "prelude items cannot mention user code"; `Coverage`'s own documentation described `Partial` as "the user suffix while the prelude prefix was analyzed". Each rule is correct for N predecessors and each argument was written for one. A reader auditing whether a `--unit` package is safe here would have found an argument about the archive, and the general reason — a predecessor cannot mention a successor, because mounts are disjoint and the fold is a dependency order — was nowhere stated. All four now give it.

The one site that does *not* union its scope is `curios_core::Module`'s own `Display`, and it stays that way, decided rather than inherited. A value printing itself has no scope to be handed without ceasing to be `Display`; no ambiguity can follow, because `build_shorten` records only names that shorten and `Spelling::symbol` falls back to the full path, so a name from outside the unit prints qualified rather than misleadingly short; and a compiler-facing dump is *better* for it, since a qualified `/std/Str/concat` beside a bare `append` says which unit each came from. Both checkers' `format_with` — the sites a programmer reads — took `&[&Module]` in [A4](#a4--the-driver-stops-naming-the-standard-library).

**One thing found and not fixed here.** `suggest.rs`'s pools are bounded by what a program already mentions, which is a suggestion-quality question rather than a scope-correctness one, and which the unit boundary sharpens rather than creates. It is recorded in full as a ROADMAP item extending the landed goal-suggestions entry, with the cheap correction and the real question stated separately.

Retiring the splice broke two such sites — strict positivity's declaration set, and `build_shorten`'s abbreviation table, which also starved `nominal_plicities` beside it. Both were found by a test rather than by inspection, which is why this is an audit and not a fix: the failure mode is a site nobody thought to look at. [A5](#a5--more-than-one-unit) has already multiplied what "in scope" can mean, so search for `module_symbols`, `nominal_plicities`, bare `items.iter()` and registry iteration, and decide each deliberately.

A first pass says the diagnostic sites in `curios-cert` and `curios-elab` already union the scope's symbols with the module's — [A4](#a4--the-driver-stops-naming-the-standard-library) made `format_with` N-ary in both for exactly this reason, arriving there rather than here — and that the one which does not is `curios_core::Module`'s own `Display`, shortening against its own symbols alone. Decide whether that is correct for a value printing itself, rather than inheriting it as an answer.

*Its obligation:* every site the audit reaches is recorded with its decision. A site left unchanged because the audit judged it correct is a different outcome from one the audit never reached, and only the record tells them apart.

## Out of scope

- **Parallelising elaboration.** The shared monotonic counters are a serialization point by design.
- **A third visibility level.** Package-privacy is subtree containment, which the audience model already expresses.
- **Making the archive a stable interchange format.**
- **Everything a project is** — manifests, dependency pins, conflict refusal, and the cache that makes depending on a package affordable. Specified in [A project names what it depends on, and a unit is compiled once](../tooling/01_PROJECTS_SPEC.md), which took the deferred caching phase and the whole package boundary out of this document. What stays here is the compiler no longer knowing structurally that there is a prelude and a program; what a *project* is builds on that and is not it.

## Tests

The property an earlier draft asserted here — that N units compile identically to the same program written as one unit with `mod` declarations — is **false**, and stating it inverted the point. The orphan rule *only bites an ordinary root*, so a witness in unit `A` for a concept in unit `B` is refused across units and accepted across modules of one unit. The unit boundary is exactly where coherence is enforced; a test asserting it is invisible would assert the rule away.

- **A1:** an ordinary root still cannot reference `/sys`, and a `/std` witness for a `/syn` concept at a `/sys` type is still exempt. Both already have fixtures; they must survive the stamp deletion unchanged.
- **A5, the boundary is semantic:** a witness declared in ordinary unit `A` for a concept declared in ordinary unit `B` over a type declared in ordinary unit `C` is refused as an orphan; the same three declarations as three `mod`s of one unit are accepted. The pair is the test — either half alone proves nothing.
- **A5, the boundary is otherwise invisible:** a program whose units declare no concepts and no witnesses compiles to the same wasm as the same declarations written as `mod`s. This is the salvageable half of the false property, scoped to where it holds.
- **A5:** two units mounting the same prefix is diagnosed naming both, not a panic and not a silent shadow.
- **A5:** an entry module `mod foo` alongside a unit mounted at `/foo` is the same diagnostic, which is also what keeps foreign import names disjoint.
- **A5:** two units each declaring a `foreign` row surface both in the store `compile_entrypoint` returns, under their own qualified names, and an embedder can bind both.
- **A5:** a unit's `pub` item exposing a dependency's non-`pub` type is refused by the existing audit, with no new rule.
- **B2:** the storage check refuses a unit carrying a free local and one carrying a metavariable of either kind, and accepts the prelude, whose derived binder floor is 0. The refusals are the test; a check that only ever meets conforming input asserts nothing.
- **B5:** whatever the audit changes carries the test for the site it changed, and a site it judges already correct carries none — the record is the deliverable there, not a fixture.

**One test listed here was never written, and the feature under it was never built.** A unit naming a prefix mounted after it still fails as an ordinary unbound name rather than by an ordering diagnostic. [A5](#a5--more-than-one-unit) claimed it landed and it did not; the decision, recorded at `curios/src/compile.rs`, is that a positional order has little to say beyond "that name is not in scope yet", and declared dependencies remove the situation rather than improve the message. It is the projects specification's to own or to drop, and not a debt of this one.

## Retirement criteria

Before this specification is deleted: `curios-pipeline` names no crate specific to the standard library outside `[dev-dependencies]`; `RootId`, `of_segment` and `root_segment` are gone and no stage derives or stores a root beside the name that determines it; `Unit` is the only artifact a unit produces and `PreludeArchive` is its serialized form; **the fold and the `Unit` boundary are recorded in `curios-pipeline`'s and `curios-unit`'s crate documentation, and the mount table's logical-to-physical mapping in `curios-text`'s**; the storage check runs at every seam a unit is written, refusing the two identity classes nothing watches today; and every site B5's audit reached carries its decision.

Two of those are **unmet**, and they are the whole of the remaining work. `curios-pipeline`'s crate documentation still says the driver restores the prelude from `curios-prelude`'s archive, which is the coupling A4 removed and is now false about its own crate; and it records the fold nowhere. `curios-text`'s says lowering resolves against *the prelude's* names, singular, and does not mention the mount table at all.

**Deletion is the last step, not the first.** [ROADMAP.md](../../ROADMAP.md)'s own protocol governs it: transfer every durable contract and invariant to its owning source, module or crate documentation and tests; record design rationale and rejected alternatives in [DESIGN.md](../../DESIGN.md) when cross-cutting or in the owning crate's `README.md` when crate-scoped; update remaining specifications to depend on the landed API; replace the linked checkbox with a checked plain-text summary; verify nothing still references the filename; then delete. `DESIGN.md`'s "A module is a compilation unit, and the prelude is an environment" is the existing owner of this decision and is where the cross-cutting half lands — its closing sentence still describes as *what remains* the thing this document delivered, and still links here.
