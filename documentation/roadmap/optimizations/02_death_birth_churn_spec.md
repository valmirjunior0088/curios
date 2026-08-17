# A pure program rebuilds what an impure one would mutate

## Status

This specification defines a measurement program — two benchmark workloads and a static census — and the lever gate any mechanism addressing allocation churn must pass. It is deliberately mechanism-blocking: no milestone below changes a representation, an encoding, or the runtime, and the levers in M2 are admitted or refused on M0's and M1's numbers alone.

Nothing is started.

## The question under measurement

Purity makes death-birth overlap the sanctioned idiom rather than a code smell: `T { ..base, f = x }` is how a field changes, a `/std/Map` insert rebuilds a spine whose predecessor dies with the operation, and the surface language teaches both as the obvious spelling. The emitted aggregates are immutable on an engine-owned heap, so every such step allocates, and the dead predecessor's only cost is its share of the collections the live set forces.

The question is what that idiom costs a regular user writing obvious code, measured against the one harness contestant whose compiler exploits the overlap, and where in real Curios code the overlap concentrates. Both numbers exist before any lever is picked, because the levers below move different costs and only measurement says which cost is the one being paid.

Not in scope: in-flight representation, which is the sibling specification's subject ([01_variant_width_spec.md](01_variant_width_spec.md)), and any reuse mechanism ahead of the M2 gate.

## Evidence

The trees campaign measured the semi-space collector's share of the allocation workload and recorded it beside its probes; the landed encoding decision ([A variant collapses when nothing needs to distinguish it](../../design/toolchain/a-variant-collapses-when-nothing-needs-to-distinguish-it.md)) then removed the leaf half of that population outright. What survives — interior nodes, records, spines — is exactly the death-birth population, and neither landed campaign can reach it: the value-lifetime work erases identities never observed, the encoding work erases identities that fit an immediate, and a record that persists between iterations or a spine that lives in a map is kept, boxed, and reborn each step.

The TOML decoder is the in-corpus spine-churn consumer: table construction rebuilds `/std/Map` spines the way any user counting words would.

The substrate facts that shape the levers: bare arrays are the only mutable storage the compiler emits, and rope leaves are deliberately shared under suffix-window virtualization — so an array is the one place a reuse write is representable today, and a rope leaf is the one place dynamic uniqueness would usually answer no by design.

## Existing substrate

The benchmark harness (`curios-benchmarks/README.md` owns its mechanics) already carries every contestant this specification needs, one number per workload, and its Lean 4 column is compiled under [Perceus reference counting with reset and reuse](https://lean-lang.org/doc/reference/latest/Run-Time-Code/Reference-Counting/) — so the Curios-to-Lean ratio on a death-birth workload is a direct price of what dynamic reuse would buy, and the imperative columns price the purity tax as users feel it, because their obvious spelling mutates.

The census precedent is `aggregate_flow_census` in `curios/src/tests/codegen/census.rs`: a whole-corpus classification over optimized CPS with its figures beside it. The death-birth classifier is its sibling instrument.

The engine seam is `curios-runtime`'s single wasmtime pin and its collector selection; the artifact-side guards in `curios/src/bundle.rs` are unaffected by anything here because nothing here changes the launcher.

## Adopted precedents

[GHC's generational economics](https://wiki.haskell.org/GHC/Memory_Management) supply lever A's model: a bump-allocated nursery where dead young objects cost nothing, with immutability an asset because old data never points at young data. Delegating churn to the engine is also the entire strategy of the WasmGC mainstream — [V8's porting guidance](https://v8.dev/blog/wasm-gc-porting) tells toolchains to reuse the host collector, and dart2wasm and Kotlin/Wasm ship no reuse machinery of their own. [Wasmtime's collectors](https://docs.wasmtime.dev/api/wasmtime/enum.Collector.html) have no nursery today, which is the native-side gap this specification prices; the browser side already lives under a generational collector.

[Mercury's compile-time structure reuse](https://mercurylang.org/documentation/papers/CW2004_03_mazur.pdf) and [Futhark's array-scoped uniqueness](https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html) supply lever B's model: the pairing of a death with a same-shape birth can be static with no runtime fact, and uniqueness pays for itself when scoped to the one substrate whose copies are O(n) — which for Curios is also the one substrate whose fields are already mutable.

[Clojure's transients](https://clojure.org/reference/transients) name the shape lever B recognizes — a builder threaded linearly through a batch and frozen at the end — recognized here by the optimizer rather than trusted from the user, because an unenforced linear discipline is a soundness hole a dependently typed language cannot culturally absorb.

[OCaml's tail-modulo-cons](https://v2.ocaml.org/releases/4.14/htmlman/tail_mod_cons.html) names lever C's constructor-tail shape, and its choice to be annotation-driven rather than implicit is the precedent for honesty about applicability.

[Perceus](https://dl.acm.org/doi/10.1145/3453483.3454032) is the comparator, not an adopted mechanism — see the refusals below for why its precondition is not purchasable here at a price worth paying.

## Milestones

The spine is `M0 → M1 → M2`. M0 and M1 are independent and may land in either order; M2 reads both.

### M0 — The workloads

- Add two workload directories beside `lcg/` and `trees/`, each with the per-contestant sources the harness's layout prescribes.

- `churn`: a record of about six fields threaded through N LCG-fed steps, two fields updated per step via spread, printing one field modulo a prime. The purest record-update signal — the imperative contestants mutate a struct, Lean's structure update gets reuse, Curios reconstructs — with the minimal algorithmic confound.

- `spines`: N LCG-keyed inserts into `/std/Map` followed by a fold, printing modulo a prime. Adds the live-set-under-churn dimension, and its header records the confound plainly: it also compares map algorithms across contestants, so it orients rather than proves.

- Both take their iteration counts from runtime input, per the harness's discipline that keeps a closed program from const-folding away, and every contestant must print the same number.

- Deliverable: the workloads in the harness and their first capture in a results file; every figure lives there, none here.

### M1 — The census

- Add the death-birth classifier beside the aggregate-flow census: over optimized CPS, a value of some layout whose last use is in scope with a construction of matching layout, reported per substrate — records and tuples, arrays, map spines — over the corpus, `/std`, and the M0 workloads.

- Record the figures beside an ignored probe carrying the command, the date, and what it last printed, per the repository's measurement discipline.

### M2 — The lever gate

- Levers are admitted or refused on M0's and M1's numbers; each admitted lever becomes its own design decision with its own evidence, and this specification retires when every lever is dispositioned.

- Lever A — the engine: a wasmtime pin bump and heap sizing now, a generational collector upstream as the horizon. Admission: the churn class — not one workload — is collection-bound. An engine knob was already refused once as a single-benchmark overfit; it enters here only as the class-wide answer the WasmGC mainstream ships.

- Lever B — static array-scoped reuse: optimizer-proven non-escaping, linearly threaded array builders reuse their dying predecessor in place, over the already-mutable array substrate only. Admission: M1 shows the builder population in real code, and M0 shows the gap is copy-bound where arrays churn.

- Lever C — construction avoidance: spread coalescing (several updates in one body fold to one construction) and constructor-tail rewrites. Admission: M1 shows multi-spread bodies or constructor-tail recursions at any nontrivial frequency, because the rewrites are cheap.

- Evidence that would stop the campaign: `churn` showing Curios within noise of Lean — nothing for reuse to buy — or M1 showing the death-birth population rare outside the workloads themselves. Reaching it is a result.

## Refused mechanisms

Recorded 2026-08-17, so the gate does not relitigate them.

A per-object reference count: on an engine-owned heap a count can free nothing, so counts are pure overhead purchased solely for a uniqueness test — dup/drop traffic on every reference duplication, payload fields losing immutability and gaining exactly the write-barrier cost of the generational engines lever A converges on, and an undercount silently corrupts a shared value in place, which makes count discipline a permanent obligation on every future `curios-cont` pass. Perceus's counts pay for themselves by *being* the memory manager; on top of a GC they are only the test.

A sticky one-bit sharing flag: the cheaper half of the same lane — no decrements, static death-birth pairing with a dynamic aliasing answer — refused with it, because it shares the mutable header, the mut-ification, and the misplaced-dup soundness class, and its coverage advantage over lever B is confined to shapes M1 can measure before anyone pays for them.

Regions and arenas: inexpressible on WasmGC — the engine owns the heap and nothing frees wholesale.

Owning allocation — reference counting in linear memory: a different compilation strategy, not an optimization of this pipeline; it forfeits the engine collector and the browser story to buy the Perceus precondition.
