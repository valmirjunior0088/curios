# The prelude is an environment, not a prefix

This document specifies removing the fixed prelude's *splice*: today every user compilation copies all 1052 prelude items into its own `Module` and then carries apparatus to ignore them again. Three stages hold that apparatus, each states the same contract in prose, and nothing checks any of it. The end state is the one Coq, GHC and Lean all reached independently — imports populate an environment, and a module contains only its own declarations.

An earlier draft of this specification led with build invalidation, splitting the archive's keying so a `curios-cert` edit would stop re-elaborating the standard library. That remains worth doing and is M5 below, but it was a symptom taken for the disease: keying is awkward *because* the prelude is spliced, and a design that hardened the splice would have made the awkwardness permanent by making it comfortable.

## Problem

`curios-text`'s `into_core_with_prelude` builds the module as `prepared.core.items.clone()` extended with the entry's own items. The prelude is therefore materialized into every compilation, and every consumer past that point needs a way to say *ignore the first 1052 of these*.

Three stages carry that apparatus, and all three state the same unchecked caller contract:

- **The certifier.** `verdicts_from` takes `checked_from` as `prefix.module.items.len()` — a length off the archive — applies it as an index into the module it was handed, and defines everything below it into the kernel's environment at its declared type without typing it. `Prefix`'s own documentation says *"its items are `module.items[..prefix.module.items.len()]` of the module being judged"*, which is a contract on the caller expressed in prose. [SOUNDNESS.md](../../SOUNDNESS.md)'s *Prefix identification* row records it.
- **Erasure.** `erase_module_with_prelude` states it outright: *"`module` must be it extended in place, its items the prelude's own followed by the user's"*. Same hazard, one stage over, and with no perimeter row of its own.
- **Elaboration.** `elaborate_module_suffix` takes `prefix: Option<&Module>` beside the module it elaborates, for the same reason.

The apparatus itself is the cost. `Prefix` and its three provenance queries (`declares_induct`, `declares_struct`, `declares_concept`); prefix parameters on `dependency_order`, `partial_definitions` and `derived_binder_floor_beyond`; four `recheck_*` entry points where there is one judgment; `erase_prelude_prefix` on the erasure side; and the index escaping the certifier entirely, since `typecheck_reporting` returns `(Module, usize, Vec<String>)` — a positional detail of the kernel's walk sitting in `curios-pipeline`'s public signature and in the test harness.

`Module` shows why it invites this. Its registries are `BTreeMap<Global, _>` and its witness set a `BTreeSet<Global>` — content-keyed, order-free, mergeable — while `items` and `universe_seeds` are `Vec` and `binder_floor` is a watermark. Half the struct is a set and half is a list, and `Prefix` is the adapter between them: `declares_induct` queries the map side, `checked_from` indexes the vec side. A structure that needs a companion object to be interpretable is reporting that it serves two roles — here, the *unit of compilation* and the *complete program*.

## Constraints, verified

- **The environment-seeding loop already exists, three times, and runs on every compile.** `erase_module_with_prelude` walks `prelude.items` calling `context.define_assuming_scheme`; `verdicts_from` walks `module.items[..checked_from]` calling `kernel.define`; elaboration does the same for its prefix. Each stage *already builds an environment from the prelude* and then **additionally** requires those items to be physically at the front of the module it processes. The splice is redundant with the seeding, which is what makes this a deletion rather than a redesign.
- `Kernel::define(&mut self, name: &Free, type_: &Term, value: &Term, universes: &UniverseContext)` is a plain method. Nothing structurally requires the prelude to be *inside* a module for the kernel to know it.
- Per-item provenance already exists. `Definition` carries `root: RootId` and `island: Qualifier`, the prelude is rooted `Sys`/`Syn`/`Std` through `PreludeModules::insert_root`, user items are `RootId::Entry`, and `into_core` already tests `root != RootId::Entry`. The seam is derivable from the data without any length.
- `Definition::totality` is the shape to copy: a per-item verdict, carried in the data, inherited across the archive, read by name. Nobody passes a `TotalityPrefix` with a length, and the asymmetry between it and `checked_from` is the whole finding.
- The vocabulary is already here. `curios-cert`'s `Env`/`Judge` seam exists to draw exactly this line between what a shared analysis asks and what each checker supplies; `positivity`'s `Coverage::Complete`/`Partial` already distinguishes "the whole declaration set" from "a registry read"; and `UniverseErased::<Module>::project_extending` already types the prelude-extended-in-place relation.
- The whole-module passes need the *complete declaration set*, not the items list: `check_positivity` runs over the spliced registries at `Coverage::Complete` precisely because a new declaration can reach an old one, and declaration sizing is the same shape. Those read the maps, which are already merged by name — so they are unaffected by removing the items splice, and this is the load-bearing distinction the design below rests on.
- `check_definition` and `check_rec_group` both return **before** their `define` step, so an item is defined whether or not it checked. The environment is therefore a pure function of the module, which is what M6 needs.

## Prior art

Three mature systems reached the same shape independently, and **none of them splices**.

Coq's `Require` loads a compiled library into the global environment; the file being checked contains only its own vernacular. GHC loads `.hi` interface files into the type environment, fingerprinting each interface *and each declaration within it*, and recompiles only when what a module actually depends on changed. Lean's checker is described in the words of this design: it *"replays the environment in a module, starting from the environment provided by its imports"*.

Coq additionally ships the image/verdict split this document's M5 describes, as `-vos`/`-vok`: `-vos` produces everything except opaque proofs, `-vok` checks the proofs and emits a file with **empty contents** whose existence means the file compiled. Its documented cost — typechecking every definition twice, because stage two re-reads source — is one M5 does not pay, since the verdict half reads the serialized image.

Lean takes the opposite branch on trust: imports are believed, and re-verification is an opt-in external pass. That is a coherent posture and deliberately not this project's, where the compile path runs the kernel on every build. Its documented weakness is instructive regardless — `lean4checker` reads `.olean` files without validating their format — and the archive's schema, source fingerprint and bytecheck on restore are what stand in that place here.

## Design

Each stage takes an **environment** plus the unit's own items. The archive is that environment's serialized form, which is what `.vo`, `.olean` and `.hi` already are — so the archive is not discarded, it finally plays the role it was shaped for.

The house method is the one [SOUNDNESS.md](../../SOUNDNESS.md) records for the recursive-member defect: *closed by deleting the second spelling rather than by adding a second check*, which gave `curios-core` one recursion form where it had two. Here the two spellings are the compilation unit and the spliced complete program, and `Prefix` is the second check reconciling them. The apparatus goes because the thing that made it necessary goes.

### The environment is `Globals`, promoted

There is no new type. `curios-cert`'s `kernel::globals::Globals` already *is* this — its own documentation says so: *"What is in scope beyond the walk: top-level definitions, and the nominal registry… the one component that exists to answer with something other than the term in hand, which is exactly what a definition store is for."* It is `pub(super)`, so what the design needs is a promotion and two fields, not a structure.

It must not be called `Env`. That name is taken in this crate by the analysis-facing *trait* — `force`, `assumption`, `fresh` — which says what a shared analysis may ask of whichever checker runs it. That is behaviour; this is data. Giving both one word would bury the seam's whole point, and `Globals` also avoids inventing a fourth term beside `Scope` (`curios-core`) and `Context` (`curios-elab`).

Two fields make it absorb `Prefix` entirely. A `binder_floor`, stored as the maximum of the archive's carried value and the walk's own derivation — a floor is a bound rather than a verdict, so widening can only cost freshness, which is the discipline `recheck.rs` already applies and `derived_binder_floor` (now in `curios-core`) already supplies. And a set of concept *names*: `declares_induct` and `declares_struct` are already answerable from the maps `Globals` holds, and `declares_concept` was the only query with no home. With both, `Prefix` deletes rather than lingering.

```rust
pub struct Globals {
    definitions: HashMap<Free, Definition>,
    inducts: HashMap<Global, InductDecl>,
    structs: HashMap<Global, StructDecl>,
    concepts: HashSet<Global>,
    binder_floor: usize,
}

impl Globals {
    /// Everything `module` puts in scope: its definitions at their declared types with their real bodies, and its nominal registry.
    pub fn of(module: &Module, carried: usize) -> Self { … }
}
```

`recheck_module(module, budget, globals)` is then the single entry point, and a whole-module walk is `Globals::default()` — which is what `recheck_module_verdicts` means today, so four entry points become one.

### M1a — the certifier skips by identity rather than position

`verdicts_from` seeds from `Globals` and judges every item in `module.items` **whose declared names are not already in scope**. That deletes `checked_from`, `Prefix`, and the prefix parameters on `dependency_order`, `partial_definitions` and `derived_binder_floor_beyond`, and takes the index out of `curios-pipeline`'s public signature — without touching the producer.

One dependency has to move with it. `derived_binder_floor_beyond` skips the prefix's *declarations* by name but its *items* by index, and it lives in `curios-core`, which cannot name `Globals`. It takes a membership predicate instead — `derived_binder_floor_outside(module, in_scope: impl Fn(&Global) -> bool)` — which makes the floor derivation identity-based like everything else here, and is the last thing holding a positional read in that function.

Doing identity first is what makes the rest safe. It is semantics-preserving, since a `Global` is unique within a module and a user item cannot reuse a prelude name; it is independently gateable; and it removes the positional claim [SOUNDNESS.md](../../SOUNDNESS.md)'s *Prefix identification* row is about, ahead of the change that removes the position itself.

### M1b — the producer stops splicing

`into_core_with_prelude` returns the unit's items alone, and elaboration and erasure stop expecting the prelude at the front of what they process. All three consumers already *receive* the prelude separately — `recheck_module_suffix(module, budget, prefix)`, `elaborate_module_suffix(context, prefix, module, …)`, `erase_module_with_prelude(context, prelude, module, …)` — so this is one producer change plus one deletion in each, with no new channel to build.

After M1a this is a pure optimisation: 1052 items stop being cloned per compilation and nothing changes about what is judged.

**These two are not independent, and an earlier draft of this document wrongly said the milestones below were.** M1b's producer change and the consumers' skip-logic must move together or the kernel double-defines; M1a is what makes that a mechanical step rather than a coordinated redesign. M2 onward genuinely can follow separately.

### M2 — registries as base plus additions

The provenance queries `Prefix::declares_*` exist to recover, per lookup, what a merged map threw away. Replace the merge with a base environment the unit adds to. `Coverage::Complete`/`Partial` already names the distinction the whole-module passes need, and those passes continue to see the complete set.

### M2b — the lowerer names its scope

Found while doing M2, and the reason M2 read as smaller than it was. `into_core_with_prelude` does not consult the prelude; it *rebuilds* it, once per namespace, on every compile:

```rust
table:      prepared.table.clone().into_iter().collect(),   // module graph
public:     prepared.public.clone().into_iter().collect(),  // public interfaces
induct_decls = prepared.core.induct_decls.clone();          // and struct_decls, concepts, witnesses
```

Seven collections, one move: clone the prelude's, extend with the entry's own, hand the merged thing forward. That is the splice, in a different container — and `PreparedPrelude` is already the Text-stage environment, consumed field by field instead of being asked.

M2 removed four of the seven, because checking showed only one consumer ever needed the prelude's entries and it now asks for them: `audit_public_exposures` walks alias edges until one lands on something nominal — which may be a prelude type — and then reads that declaration's telescopes. It was correct only because somebody upstream had concatenated the prelude into the map it searched, with nothing stating that requirement; `NominalScope` states it. The dependency sort, the other supposed reason for merging, never needed it at all: `node_reference_names` looks a declaration up only for names an item itself declares.

What remains is `table` and `public`, extended in place by `resolve` and `resolve_with_prelude`. Those are the module graph and the interface map — real scope, and the same unstated relation. They were left out of M2 deliberately: they are name resolution and the public-interface audit, the riskiest code in the lowerer, and they have nothing to do with which declarations a `Module` carries. `universe_seeds` is *not* in scope here — it is an index space, covered by the watermark exclusion below.

The shape is the one `Globals` has: `PreparedPrelude` gains lookup, the lowerer resolves own-then-base, and nothing is copied to make a query answerable.

### M3 — elaboration names its environment

M1b removes elaboration's positional assumption; what remains is that it still takes a bare `prefix: Option<&Module>` and re-seeds a context from it by hand. Give it the environment type its own stage deserves, so the prelude arrives as scope rather than as a module that happens to be consulted.

### M4 — erasure names its environment

The same for `erase_module_with_prelude`, whose prose contract M1b discharges: with the prelude arriving as scope, `erase_prelude_prefix` retires and the caller guarantee has nothing left to state. Erasure is the stage closest to correct already, since it restores an archived erased prefix and erases only the suffix, so this is the smallest of the four.

### M5 — split the archive's keying (`curios-prelude-archive`)

Unblocked by the above rather than blocking it. `curios-prelude`'s build script does two things in one product: it elaborates and serializes the image, needing `curios-text`/`curios-elab`/`curios-ersd`; and it runs `recheck_module_verdicts`, needing `curios-cert`. Cargo's granularity is the build script, so either dependency set re-runs both halves — measured at 13 distinct build-script fingerprints, each with its own 7.3 MiB archive, with `deps`, `features` and `rustflags` the only varying inputs.

**Those two sets are not disjoint, and this document asserted they were.** `curios-elab` depends on `curios-cert`, so the elaborating half already reached the certifier transitively. Splitting the crate was therefore necessary and *not sufficient*, which the split's own experiment showed: with `curios-prelude-archive` carrying no `curios-cert` build-dependency, touching `curios-cert/src/lib.rs` still produced

```text
Compiling curios-prelude-archive
warning: fixed prelude hash-consed to 23964 distinct structures    ← re-elaborated anyway
Compiling curios-prelude
warning: fixed prelude certified: 1079 items accepted by the kernel
```

What closes it is splitting `curios-cert` in turn. `curios-elab` needs the *shared analyses* — the `Env`/`Judge` seam, inversion, positivity, totality, satisfiability — and needs no kernel: every production use of `Kernel`, `carries_information` and `satisfiable` in that crate is in a test. So the analyses become `curios-analysis`, `curios-cert` keeps the kernel and the module walk, and `curios-elab` takes `curios-cert` as a **dev**-dependency, which does not propagate. `cargo tree -p curios-prelude-archive --edges build` then contains no `curios-cert` at all, and the re-elaboration cannot recur. See [DESIGN.md](../../DESIGN.md), "An independent kernel re-checks what the elaborator accepts".

A new crate `curios-prelude-archive` owns the authored `/sys`, `/syn` and `/std` sources, their elaboration, and the serialized image, with no certifier dependency. It composes with the rkyv facade rather than competing with it: [`curios-archive`](11_ARCHIVE_FACADE_SPEC.md) owns *archiving* as a capability, and this is *an archive, of the prelude* — the two are a leaf below `curios-base` and a crate above `curios-ersd` respectively, which is also why they cannot be one crate. `curios-prelude` **keeps its name, its public API and every downstream dependency**, and gains a build script that restores the image and certifies it. The verdict is that crate's successful build, exactly as `.vok`'s existence is Coq's — nothing can reach the prelude except through it, so the invariant *an archive that exists is one whose every item the kernel accepted* holds by construction rather than by convention.

One property improves: today the walk certifies `core` **before** hash-consing and serialization, so what is certified is not literally what is stored. After the split the kernel walks the restored image.

**Measured, 2026-08-09.** The build script's own `OUT_DIR/profile.tsv`, from a debug `--all-features` build:

```text
469215 ms  elaborate_and_zonk_module
  4516 ms  erase_prelude_prefix
  1223 ms  zonk_module
   536 ms  prepare_prelude
    52 ms  positivity_vectors
```

Elaboration is 469 s of a prelude build that takes roughly 570 s wall, which bounds everything else — certification included — at about 100 s. The decision rule this paragraph used to state was *if the walk dominates, M5 buys a crate and little else*; it does not dominate, and is outweighed at least four to one. A `curios-cert`-only edit currently pays 469 s of elaboration for nothing, which is the cost this milestone removes. One caveat on the figure: `recheck_module_verdicts` carries no `profile_span!`, so certification is bounded by subtraction from wall clock rather than read directly.

### M6 — parallel certification

Split the walk into a serial define-all phase and a parallel check-all phase, one `Kernel` per item over a shared read-only environment, verdicts sorted by item index for determinism. Per-item kernels make the data independent and settle binder identity without arithmetic: each is seeded at the same derived floor, above every identity in the module, and two workers minting the same index never share a scope. A shared counter is ruled out — nondeterministic under work stealing, and the archive must stay byte-reproducible.

This is Coq's `make -j vok`, reached independently. Any parallelism must be feature-gated native-only, because `curios-web` compiles `curios-pipeline` and therefore `curios-cert` to `wasm32-unknown-unknown`, which has no threads. Measure the memo cost first: `recheck_module_verdicts_uncached` exists for the memo-parity test, and its slowdown is the ceiling on what per-item kernels lose.

### M7 — cached user modules

With the environment in place, caching a module's elaborated Core and its verdict is the natural extension: the environment is built from N cached modules rather than one, and there is no seam to identify because there was never a prefix. Two things remain, and both are perimeter work: identities must survive splicing independently elaborated modules, which is what *Binder identity* is about and why it must be defended better than *argued* with one control first; and verdicts must be keyed on the exact terms and the certifier version, following GHC's per-declaration interface fingerprints rather than Cargo's per-crate granularity.

Whole-module passes never cache and re-run at link: positivity over the complete declaration set, declaration sizing, and witness coherence, which is program-wide by definition since a violation is only visible where two modules meet. That bounds the win without removing it, since per-item typing is the expensive part.

## Out of scope

- **The identity-space watermarks** — `binder_floor`, `universe_floor`, `metavar_floor`, `witness_floor`. These partition *allocation spaces*, which is a problem a watermark is a reasonable answer to, and they are a separate concern from the items splice. `Entropy::seed` only raises, so combining is `max` and widening is always safe; that property is load-bearing for `derived_binder_floor` and is not to be lost by accident.
- **Parallelising elaboration.** The shared monotonic counters are a serialization point by design.
- **Making the archive a stable interchange format.** It stays scoped to one compiler build.
- **Weakening the compile path's second opinion.** The kernel continues to judge the user's items on every compile.

## Rejected

- **Hardening `checked_from` with an identity check.** The obvious local fix — compare the prefix items against the archive's and fall back to judging everything on a mismatch — makes a spliced, positionally delimited module *safe to live with*. That is how a wart becomes permanent: it stops hurting, so nobody removes it. *Prefix identification* is left standing as the recorded symptom, to be deleted by M1 rather than patched.
- **Moving `checked_from` onto `Module`.** It would become the elaborator's claim, and the kernel already refuses to believe `Module::binder_floor` for exactly that reason.
- **Certification as a test.** Cheapest route to M5's build-time win, and it demotes the invariant from a build-time impossibility to a convention: an archive could exist, be compiled against, and never have been certified. Lean's posture, deliberately not this project's.
- **Caching inside the existing build script.** Impossible: Cargo rebuilds the script binary whenever any build-dependency changes, and from inside the script no input distinguishes which one did.
- **Naming the certifying crate rather than the image crate.** Putting the new name on the certifying half churns every downstream dependency and leaves consumers importing the prelude from something that does not sound like the prelude.

## Tests

- M1: the kernel's verdicts over the whole corpus are unchanged, item for item, against the spliced walk it replaces — the migration's only real risk is a definition that silently stops being in scope.
- M1: an environment lookup that misses refuses, with its own diagnostic, rather than panicking or skipping.
- M2–M4: each stage's prose contract becomes an assertion or disappears; `erase_module_with_prelude`'s caller guarantee in particular has no test today.
- M2b: a public entry aliasing a *prelude* nominal type is audited identically whether or not anything merged the registries — the property `exposed_nominal` rested on and nothing stated.
- M5: an empty-cache build and a cache hit, following the `curios-binaryen` precedent — plus the case it exists for, a `curios-cert`-only edit that re-runs certification and **not** elaboration, asserted by the build script's own warning appearing once rather than twice. That test is what caught the crate split being insufficient on its own, and it is worth keeping executable rather than leaving as a claim. A stale or corrupted image must fail certification rather than be skipped.
- M6: parallel verdicts equal serial verdicts, item for item and in order, over the whole prelude; `kernel_memo_parity`'s property survives per-item kernels.
- M7: deferred to its own specification once M1–M6 have landed.

## Retirement criteria

- Before this specification is deleted: the environment boundary is recorded in `curios-cert`'s, `curios-elab`'s and `curios-text`'s crate documentation, replacing the prose caller contracts it removes; the cross-cutting decision — that a module is a compilation unit and the prelude an environment — is recorded in [DESIGN.md](../../DESIGN.md); [SOUNDNESS.md](../../SOUNDNESS.md)'s *Prefix identification* row is deleted with the mechanism it describes rather than re-graded, and erasure's matching contract is deleted with it rather than added as a row; the `curios-prelude-archive` keying discipline is recorded in that crate's documentation; and M7, if still pending, is carried out to a specification of its own.
