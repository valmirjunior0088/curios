# Caching the elaborated/erased prelude, and de-smelling the prelude-replay fork

Working implementation specification. Like [BIG_FLT_SPEC.md](BIG_FLT_SPEC.md) and [NUMERIC_REPRESENTATION_SPEC.md](NUMERIC_REPRESENTATION_SPEC.md), this file is a working reference for one arc of work, not permanent documentation: fold its durable conclusions into [AGENTS.md](../AGENTS.md) and [ROADMAP.md](ROADMAP.md) as they land, and delete it once the arc is finished. It assumes the architecture in AGENTS.md (the `text → core → ersd → cont → wasm` pipeline, the crate layering, the slim-launcher boundary) and does not restate it.

## Motivation

Every invocation of the compiler recomputes the `sys`/`syn`/`std` prelude — parse, lower, elaborate, zonk, erase — before it touches a single line of the user's program. The prelude is a fixed, program-independent prefix of *every* Curios program: with the reachability prune gone from `into_core`, every program lowers the identical prelude, and since prelude items depend only on each other they always topologically sort ahead of the user items. Its type-checking result does not depend on the user code. Recomputing it per compile is pure waste.

Today that waste is only partially avoided, and only within a single process. The elaborated-and-zonked prelude is cached in a thread-local `PRELUDE` (`curios-pipeline/src/lib.rs:66`), built once per thread by `build_prelude` and replayed into each compile's fresh context. That cache dies with the process, so the CLI — which runs one process per compile — pays the full elaboration on every `curios run`/`curios compile`. It is also per-thread, so a `cargo test` run with N worker threads elaborates the prelude N times. And two of the four prelude phases are not cached at all even within a process.

The goal of this work is to compute the fixed prefix **zero times per user compile**: the prelude is elaborated (and erased) once, out of band, and every subsequent compile restores that result instead of recomputing it.

This is not a performance-tuning exercise and carries no benchmark target. It is the removal of redundant recomputation of an invariant. It is one of two prongs against the same std-cost pain: the [numeric representation](NUMERIC_REPRESENTATION_SPEC.md) work makes the prefix *cheap to compute*; this work makes it *not recomputed at all*. The two are complementary — the numeric fix shrinks every artifact this work caches and deletes the `RecId` serialization hazard (see Sequencing).

## What actually re-runs, per compile

The prelude passes through four phases, cached at different granularities:

| Phase | Where | Cached today | This work |
| --- | --- | --- | --- |
| Parse | `STD_MODULES`/`SYN_MODULES` thread-locals (`curios-text/src/prelude.rs:727,751`) | per-thread | left as-is |
| Lower (`into_core`) | not cached — only the topological permutation is (`into_core.rs:1135`) | no | **L**, deferred |
| Elaborate + zonk | `PRELUDE` thread-local (`curios-pipeline/src/lib.rs:66`) | per-thread | **E**, cached cross-process |
| Erase | not cached — `erase_module` runs over the whole module (`curios-pipeline/src/lib.rs:203`, `curios-core/src/erase.rs:1381`) | no | **R**, cached cross-process |

Within one process (the test suite), the repeated-*every-compile* costs are lower-of-prelude and erase-of-prelude; across processes (the CLI), add parse and elaborate. There is also pure waste in the cached path: `into_core` lowers the *whole* prelude+user program every compile, and `elaborate_and_zonk_with_prelude` then discards the prelude portion with `module.items.iter().skip(prelude.items.len())` (`curios-core/src/elaborate/module.rs:556`). The lowered prelude terms are built and thrown away on every compile — this is what **L** removes, and it is deferred (see The artifact).

## Design keystones

- **The prelude is a restorable `Context` prefix, not a special elaboration mode.** Elaboration is a fold that threads a `Context` through items; the prelude is a prefix of that fold. Skipping its recomputation is `Context` *restoration*, a preparation concern — not a second elaboration entry point. This is Stage 0.
- **Out of band, embedded, never committed.** The artifact is regenerated on every build from the current source tree by a `build.rs`, then `include_bytes!`'d. This collapses the format-compatibility surface to nothing: the generator and the consumer are the same crate compiled in the same build, so serde layout can never desync them. There is no persisted-format version to maintain.
- **One artifact, sectioned per phase.** The cacheable phases (E, R) are cleanly extractable but genuinely coupled; a single atomic, internally-sectioned blob captures that honestly while still giving per-phase structure and per-phase correctness oracles.
- **The declarative `Module` is the artifact, not raw `Context` state.** A zonked prelude has no parked constraints and no unsolved metavariables, so its post-prelude `Context` is fully determined by the declarative module (definitions + registries + witness set). Serializing the module and replaying it into a fresh `Context` is robust; serializing mutable elaboration state is not.
- **Keep the slim launcher slim.** `serde` must stay out of `curios-abi` and `curios-rt`. Two containment rules (see Serialization) enforce this.

## Stage 0 — unify the prelude-replay fork (independent refactor)

`elaborate_and_zonk_with_prelude` (`curios-core/src/elaborate/module.rs:472`) is a near-copy of `elaborate_module` (`module.rs:338`) plus `zonk_module`, with three things wedged in:

- its user-item loop (`555-569`) is line-for-line the regular loop (`385-397`) plus a `skip(prelude.items.len())`;
- its registry seeding is *more* complex, not less (`482-517`): it seeds from the prelude and from the user module's keys-minus-prelude, with explicit dedup, because `into_core` lowered the whole program so the user module's registries redundantly contain the prelude's;
- its registry pullback (`578+`) is the regular pullback (`411-435`) restricted to user keys, with `zonk` folded in (only the user suffix is zonked; the prelude is already meta-free).

The genuinely new part is only the replay step (`526-547`): `define_assuming` / `define_rec_members` / `register_witness` that advances the context through the prelude *without checking*. Everything around it is a specialized clone. The fork is on the wrong axis — the replay is a `Context`-preparation concern implemented as an elaboration-control-flow fork.

Collapse it into a seam:

- **`prepare_context_from_prelude(context, &prelude)`** — the one shared replay primitive: register the prelude's registries (inductives/structures/concepts), `define_assuming` each `Let` and `define_rec_members` each `Rec` group, `register_witness` each prelude witness, and seed the entropy/metavar floors. It restores the post-prelude `Context` exactly. Sound for the reason stated in `elaborate_and_zonk_with_prelude`'s own doc: the prelude is program-independent, top-level definitions are excluded from a metavariable's Γ (`Context::identity_snapshot`), and a zonked prelude is meta-free, so a user item elaborates against the identical local context it would under a from-scratch `elaborate_module`.
- **One parameterized fold** for `elaborate_module`, and the **symmetric treatment for `erase_module`** (`curios-core/src/erase.rs:1362`): each folds user items onto whatever `Context` it is handed, fresh or restored, from a given `start_index`.

Both flows then become the same flow: **prepare context (fresh | restored) → normal fold on top.** The `skip` survives as the `start_index` parameter (and disappears entirely once **L** lands); `zonk` becomes "zonk the user suffix" via the same prefix-skip; the registry dedup collapses to "seed whatever is not already registered."

Two reasons Stage 0 is load-bearing for the rest:

1. It removes the duplication instead of building the cache on top of it. The serialized artifact then feeds the clean seam (`prepare_context_from_prelude`), not a bespoke elaboration entry point.
2. It pre-empts cloning the smell into `erase`. `erase_module` has no `_with_prelude` variant today; **R** would otherwise invite an `erase_module_with_prelude` — a second copy of the fork. The unification gives `erase` the same shape reusing the same primitive.

Stage 0 is a valid refactor independent of any serialization, and should land first.

## The crate: `curios-prelude`

A new crate holding the artifact and the restore logic, built out of band like `curios/runtime` but via a `build.rs` rather than a `make` target (the reason `runtime` uses an isolated build — keeping Cranelift/Binaryen out of the slim launcher — does not apply; the blob is inert data).

- **`build.rs`** — declares `[build-dependencies]` on `curios-core`, `curios-text`, `curios-ersd`, `curios-abi`; builds the prelude from source (Stage 0's from-scratch path over the trivial `"0"` entrypoint, exactly as `build_prelude` does today), serializes the sectioned artifact, and writes it to `OUT_DIR/prelude.bin`. Emits `rerun-if-changed` on the embedded `.crs` sources as belt-and-suspenders over the transitive `curios-text` rebuild trigger. Elaboration failure surfaces as a build-script panic with the formatted diagnostic.
- **lib** — `include_bytes!(concat!(env!("OUT_DIR"), "/prelude.bin"))`, `deserialize`, the from-source builders (retained for the round-trip oracle and as a fallback), and the restore/splice logic layered on Stage 0's `prepare_context_from_prelude`.
- **`curios-pipeline`** depends on `curios-prelude`; its `PRELUDE` becomes `deserialize(blob)` instead of `build_prelude()`, and its erase step gains the restore-and-splice path (see The artifact).

No dependency cycle: `curios-core`/`text`/`ersd` never depend on `curios-prelude`, and `[build-dependencies]` are resolved as a graph separate from the crate's normal dependencies. Cargo runs the build script before compiling the lib, so the `OUT_DIR` blob always exists when `include_bytes!` reads it.

Staleness is airtight via cargo: any change to the `.crs` sources or to the elaborator (`core`/`text`/`ersd`/`abi`, all build-dependencies) reruns the build script and regenerates the blob; no-op rebuilds skip it (cached). The elaboration cost is paid only when the prelude or the elaborator changes — exactly when the blob must regenerate anyway.

Bonus: with `PRELUDE = deserialize(blob)`, the test suite elaborates the whole prelude **once at build time** (the `build.rs`) instead of once per test-worker thread.

## The artifact — one sectioned bundle

```
struct PreludeArtifact {
    elaborated: core::Module,   // E
    erased:     ersd::Module,   // R (items)
    // provenance
}
```

Each phase extracts cleanly — E is the zonked module `build_prelude` already produces; R is `erase_module` over that same prelude, whose `items` are the erased prefix — but E and R are genuinely coupled in both directions:

- **R is generated from E.** `erase` takes the elaborated module as input; there is no erased prelude without the elaborated prelude.
- **R is consumed with E.** Erasing the user items re-runs `infer` and delta-reduces through prelude *definitions* (`curios-core/src/erase.rs:1399,1420`) and reads the prelude *registries* (`erase.rs:1368`). So the erase context must be seeded from E — via the same `prepare_context_from_prelude` primitive, into the erase context — before the cached erased items are spliced and the user items erased.

A single atomic, internally-sectioned blob encodes this coupling by construction: there is no way to consume R without E. Per-phase typed fields keep the structure (each with its own restore entry point and its own oracle); the atomic blob and single `build.rs` provenance keep them in lockstep.

**R's restore path**, concretely: fresh erase `Context` → `prepare_context_from_prelude(from E)` → prepend the cached erased ersd items → erase only the user items (via Stage 0's `start_index` fold over `erase_module`). The per-program ersd prune/optimize (`curios_ersd::optimize`) runs afterward over the whole spliced module exactly as today, so its output is unchanged.

**L is not a third section.** The residual `into_core` prefix-skip needs a resolution/interface table — qualified names, the module tree, the `pub use` re-export graph — only partly a projection of E, and partly structure E discards when it flattens. It is a separate, more-invasive `into_core` change with its own open artifact shape, deferred out of this arc.

## Serialization

- **Format: `postcard`** (already present transitively in `Cargo.lock`). Its varint integer encoding is width-independent, which is mandatory here: `curios-js` (wasm32) → `curios-pipeline` → `curios-prelude`, so the host-built blob is `include_bytes!`'d into a 32-bit target, and any native-width `usize` (de Bruijn indices, scope arities, collection lengths) would corrupt across the host→wasm boundary. `postcard` is also `no_std`/wasm-friendly and compact.
- **Sharing:** serialize the `Rc<Subterm>` DAG flat and **re-intern on load** via `Term`'s existing structural `Hash`/`Eq` (`curios-core/src/term.rs:800,806`) — a load-time hash-conser rebuilds the shared `Rc`s. Binary size (the blob is `include_bytes!`'d into the compiler) is the only reason to preserve sharing *in the format itself*; deferred until it bites.
- **The two containment rules that keep the slim launcher slim.** `curios-rt` depends on `num-bigint` (its handle table, `curios-rt/src/table.rs:12`) and on `curios-abi` (a pure leaf with zero dependencies); both are verified. Naively serializing the bignums or `RootId` would leak `serde` into `curios-rt`, which AGENTS.md guards. Route around both:
  1. **`BigUint`/`BigInt`** (in `Nat::Succ`, `curios-core/src/nat.rs:11`; and `Int`, `curios-base/src/int.rs:15`) → `to_bytes_le`/`from_bytes_le` byte-vector adapters. **Do not** enable `num-bigint/serde` — feature unification would pull `serde` into every num-bigint user, including `curios-rt`. The adapter is idiomatic here; `curios-rt` already uses `to_bytes_le` (`curios-rt/src/host.rs:40`).
  2. **`RootId`** (`curios-abi/src/root_id.rs:17`, carried on `Definition` and the registry entries) → serde `remote` derive, defined outside `curios-abi`. **Do not** derive `serde` on `curios-abi` — it would dirty the pure leaf and leak `serde` into `curios-rt`.
  - Everything else in the transitive closure — `Term`/`Subterm`/`Prim`/`Scope`/`Telescope`/`Var`/`Atom`/`Qualifier`/`Flt` (a `u32` bit pattern, `curios-base/src/flt.rs:13`)/`Bin` (`Vec<u8>`)/the registries — lives in `core`/`base`/`ersd`, none of which feed `curios-rt`, so plain `#[derive]`s are safe there.
- **Skips and drops** (none can affect the oracle; see Correctness):
  - `OnceCell` `hash`/`reach`/`transient` (`curios-core/src/term.rs:22-24`) → skip; rebuilt lazily on first access.
  - `Span { Rc<Source> }` (`curios-base/src/span.rs:43`) → drop (`span: None`). Spans are identity-irrelevant — hash and equality look only at the inner `Subterm` (`term.rs:18,816`), so a spanned build-from-source module and a span-`None` deserialized one compare equal — and dropping them sidesteps serializing the embedded source text entirely. Prelude-internal diagnostics lose snippets, which is acceptable since a malformed prelude is a `panic!` compiler invariant.
  - `Prim::Foreign(Arc<ForeignFunction>, _)` (`curios-core/src/prim.rs:130`) → serialize `(namespace, name)`, re-resolve the `Arc` from `sys_io()` on load. `ForeignFunction::eq` is `(namespace, name)`-only by design — its doc states the intent verbatim: "makes rows from different stores with the same content compare equal, so a cached prelude term matches a freshly minted one" (`curios-abi/src/host.rs`).
  - `Metavar`/`Infix`/`NumLit` — cannot occur in a zonked, fully-elaborated prelude (`zonk` rejects unsolved holes; `elaborate_infix`/`elaborate_numlit` consume the transient nodes). Assert their absence at serialize time as an invariant guard.
  - `RecId` + entropy reseed — deleted by the numeric fix; moot when this work is sequenced after it (see Sequencing).

## Correctness

- **E — round-trip oracle (a complete proof, not a smoke test).** After Stage 0, the consuming path is unchanged, production-tested code; E only swaps the *source* of the cached module from `build_prelude()` to `deserialize(blob)`. `curios_core::Module` derives `PartialEq` structurally all the way down, so `deserialize(serialize(build_from_source())) == build_from_source()` proves E cannot change compilation behavior. Span-drop and Foreign re-resolution both pass this oracle for the reasons above.
- **R — equivalence oracle (bounded).** R introduces a genuinely new path (there is no `erase_module_with_prelude` today; erase re-erases the whole prelude every compile). Its oracle: `erase_with_prelude(prelude ++ user)` produces the same `ersd::Module` as `erase_module(prelude ++ user)`, for a representative corpus of user programs.
- **Risk asymmetry, stated plainly.** E is a source-swap behind a complete oracle — near-zero risk. R is a new code path bounded by its equivalence oracle. They are not equally safe and must not be presented as such.

## Build topology and the done bar

- **One empirical check to run, not assert.** With zero workspace features and host == target, resolver-2 *should* share the `core`/`text`/`ersd` units between the build-dependency graph and the normal graph, so editing `curios-core` recompiles it once. Confirm it: touch `curios-core`, run `cargo build -p curios`, and verify `curios-core` compiles once, not twice. Cross-compiled builds (the `wasm32` CI job) do compile the elaborator once for the host to run the build script — inherent and CI-only. **Fallback if the native double-compile turns out real and painful:** the out-of-band generator-bin form (a bin that shares the normal-graph compile, written to a gitignored path and `include_bytes!`'d, exactly like `curios/runtime`).
- **New done-bar line:** `cargo tree -p curios-rt | grep serde` must be empty. The existing isolated `cargo build --package curios-rt` slim check backs it up — a change can pass `--workspace --all-features` and still leak `serde` into the launcher.
- **AGENTS.md cleanup:** remove the stale `RUST_MIN_STACK` gotcha. The `.cargo/config.toml` value is already deleted; `rec` lowering is now iterative, so elaboration no longer has the deep-recursion stack demand that gotcha documented, and the `build.rs` runs elaboration on the ordinary main-thread stack with no special handling.

## Staging and ordering

1. **Numeric representation fix first.** It removes `RecId` from the serialization surface (and its entropy-reseed hazard), shrinks every artifact, and avoids building serde against a representation that is about to be deleted.
2. **Stage 0 — de-smell.** Unify `elaborate_module` / `elaborate_and_zonk_with_prelude` behind `prepare_context_from_prelude` + one parameterized fold, and give `erase_module` the symmetric shape. Independent, and de-risks everything after it.
3. **E.** The `curios-prelude` crate + `build.rs`, the elaborated-core section, and the elaborate restore path. Guard with the round-trip oracle.
4. **R.** The erased-ersd section and the erase restore-and-splice path, reusing Stage 0's primitive. Guard with the equivalence oracle.
5. **L, later.** Teach `into_core` to lower only the user program on top of a cached prelude interface — a separate change with its own artifact, out of this arc.

## Goals and non-goals

Goals:

- Compute the fixed prelude prefix zero times per user compile, across processes.
- Remove the `elaborate_and_zonk_with_prelude` fork (Stage 0) and pre-empt its erase twin.
- Keep `serde` out of `curios-abi` and `curios-rt`.

Non-goals:

- **L** (the `into_core` prefix-skip) — deferred; residual lowering waste remains after this arc.
- Caching any program-dependent stage (cont, wasm, the ersd prune/optimize) — those fold the whole program together and are not a fixed prefix.
- Committing the artifact — it is a build product, regenerated every build.
- Any benchmark target — the success criterion is "the fixed prefix is computed zero times per user compile," not a speed number.

## Background facts (verified against the tree)

- The prelude is elaborated once per thread by `build_prelude` and replayed by `elaborate_and_zonk_with_prelude`; the replay is already "cheap map inserts, no checking" (`curios-pipeline/src/lib.rs:66`, `curios-core/src/elaborate/module.rs:472,520`).
- `erase_module` walks items in order, defining each into the context so later items delta-reduce through it, exactly like `elaborate_module` (`curios-core/src/erase.rs:1381,1399,1420`).
- `curios_core::Module` and its `Item`/`Definition` derive `PartialEq`; `Term`'s `PartialEq` is structural and span-blind (`curios-core/src/module.rs:19,46,62`; `curios-core/src/term.rs:806,816`).
- The numeric leaf types: `Nat = enum { Zero, Succ(BigUint, Term) }` (`curios-core/src/nat.rs:10`), `Int = { BigInt }` (`curios-base/src/int.rs:15`), `Flt = { bits: u32 }` (`curios-base/src/flt.rs:13`), `Bin = Vec<u8>` (`curios-core/src/prim.rs:103`).
- `ForeignFunction::eq`/`hash` key on `(namespace, name)` only, deliberately so cached and freshly-minted rows compare equal (`curios-abi/src/host.rs`).
- `curios-abi` has zero dependencies; `curios-rt` depends on `curios-abi`, `wasmtime` (runtime-only), and `num-bigint`, and not on `curios-base`/`core`/`ersd` (`curios-rt/Cargo.toml`).
- The workspace uses `resolver = "2"`, edition 2024, and has no `[features]` in any crate; `num-bigint` is pulled with no features enabled (`Cargo.toml`).
- The launcher precedent: `curios/runtime` is built out of band by `make curios/runtime` and `include_bytes!`'d via a `CARGO_MANIFEST_DIR` path, failing the build loudly if absent (`Makefile`, `curios/src/bundle.rs:20`).

## Open questions

- **Crate name.** `curios-prelude` is the working name; confirm or replace.
- **Native build-dep double-compile.** Confirmed by reasoning, pending the one empirical check above; the out-of-band bin is the clean fallback.
- **L's interface artifact.** Left fully open — its shape (names + module tree + re-export graph) is scoped only when L is picked up.
