# A pure program rebuilds what an impure one would mutate

## Status

This specification defines a measurement program — benchmark workloads and a static census — and the lever gate any mechanism addressing allocation churn must pass. It is deliberately mechanism-blocking: no milestone below changes a representation, an encoding, or the runtime, and the levers in M2 are admitted or refused on M0's and M1's numbers alone.

M0 is complete in the harness as of 2026-08-17, and the census landed the same day. The gate is decided: lever A — the engine — is admitted, and levers B and C are refused on the census's numbers. What remains is sequenced: run 08 as the pre-lever baseline, then the admitted engine campaign, whose design decision this specification retires into.

## The question under measurement

Purity makes death-birth overlap the sanctioned idiom rather than a code smell: `T { ..base, f = x }` is how a field changes, a `/std/Map` insert rebuilds a spine whose predecessor dies with the operation, and the surface language teaches both as the obvious spelling. The emitted aggregates are immutable on an engine-owned heap, so every such step allocates, and the dead predecessor's only cost is its share of the collections the live set forces.

The question is what that idiom costs a regular user writing obvious code, measured against the one harness contestant whose compiler exploits the overlap, and where in real Curios code the overlap concentrates. Both numbers exist before any lever is picked, because the levers below move different costs and only measurement says which cost is the one being paid.

Not in scope: in-flight representation, which the sibling campaign settled and retired as [A variant travels as the fields of its widest constructor](../design/toolchain/a-variant-travels-as-the-fields-of-its-widest-constructor.md), and any reuse mechanism ahead of the M2 gate.

## Evidence

The trees campaign measured the semi-space collector's share of the allocation workload and recorded it beside its probes; the landed encoding decision ([A variant collapses when nothing needs to distinguish it](../design/toolchain/a-variant-collapses-when-nothing-needs-to-distinguish-it.md)) then removed the leaf half of that population outright. What survives — interior nodes, records, spines — is exactly the death-birth population, and neither landed campaign can reach it: the value-lifetime work erases identities never observed, the encoding work erases identities that fit an immediate, and a record that persists between iterations or a spine that lives in a map is kept, boxed, and reborn each step.

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

- The substrate workload exists: `chain`, landed 2026-08-17 beside `lcg/` and `trees/`, rebuilds a cons spine K times with nothing surviving the step that replaces it. It prices death-birth churn against every contestant's allocator and nothing more — on this shape the imperative contestants' obvious spelling also rebuilds cell by cell, so `chain` compares memory-management strategies, not purity against mutation. Its first capture is pending.

- The record workload exists: `churn`, landed 2026-08-17, threads a six-field record through N LCG-fed steps, two fields updated per step via spread, printing one field modulo a prime — the purest record-update signal, with the minimal algorithmic confound: the imperative contestants mutate a struct in place and allocate nothing, and Lean's structure update gets reuse. The landing answered part of its question before any table: the emitted Curios loop allocates nothing either, the threaded record travelling as fields through the landed in-flight campaigns — pinned by `churn_threaded_record_allocates_nothing` in `curios/src/tests/codegen/churn.rs` — so the capture prices the erased spelling against the mutation floor, and the record-update tax lives only where a record rests, which is `spines`' and the census's territory.

- The map workload exists: `spines`, landed 2026-08-17, drives N LCG-keyed inserts into `/std/Map` followed by a fold, printing modulo a prime. It adds the live-set-under-churn dimension — `chain` keeps almost nothing alive and `trees` keeps everything, and a collector's economics live between — and its header records two confounds plainly: it compares map algorithms across contestants, and `/std/Map` deliberately has no `Key(Nat)`, so LCG keys enter through `Bytes/of_nat` at the boundary — a per-insert cost of a few divisions and table reads no int-keyed hash map quite mirrors. It orients rather than proves.

- No M0 workload churns the array substrate, and by decision none joins yet: the rope amortizes idiomatic accumulation into O(1) nodes with one force-time fill, so a synthetic builder workload would price a shape no measurement yet shows users writing. Lever B's copy-bound evidence therefore comes from M1's census, and a builder workload joins M0 only if the census finds the population first.

- All take their iteration counts from runtime input, per the harness's discipline that keeps a closed program from const-folding away, and every contestant must print the same number.

- Deliverable: the workloads in the harness and their first capture in a results file; every figure lives there, none here. The capture is sequenced last deliberately — after the census and the gate's verdicts, whose admission questions the in-repo probes already answer with deterministic collection counts, and before any admitted lever's implementation lands, so run 08 is the clean pre-lever baseline the across-runs tables need.

### M1 — The census

- The classifier exists: `death_birth_census`, landed 2026-08-17 beside the aggregate-flow census in `curios/src/tests/codegen/census.rs` — over optimized CPS, a construction beside a co-resident value of matching width whose every use takes it apart, reported per substrate over the corpus, the M0 workloads, and a TOML-decoding driver, with the figures, the command, and their reading beside the probe.

- What the measurement decided, for the gate to read: the population is pervasive rather than rare, so the stop-evidence clause does not fire; it is entirely the cross-frame shape — the dying value always arrives as a parameter taken apart where the matching birth happens, never from the constructing function's own sites, so a reuse mechanism keyed to intra-function allocation would see none of it; the map-spine and TOML concentrations sit where the evidence predicted; and the linearly threaded builder population lever B's admission asks after exists, at over half of all rope extends.

### M2 — The lever gate

- Levers are admitted or refused on M0's and M1's numbers; each admitted lever becomes its own design decision with its own evidence, and this specification retires when every lever is dispositioned.

- Lever A — the engine — is **admitted**, 2026-08-17. The class is collection-bound: both members spend about two thirds of their churn on collections under the heap the engine's grow-only-on-overflow policy parks within a doubling of the live set — `chain` over a fixed ~320 KB chain, `spines` over a map growing toward its plateau, each recorded beside its probe (`chain_collection_decomposition` and `spines_collection_decomposition` in `curios/src/tests/codegen/churn.rs`) — and `churn` left the class when its loop stopped allocating. The census closes the argument from the other side: every located pair is cross-frame, so no intra-function mechanism reaches the population the engine does. The admitted campaign is a wasmtime pin bump past 47 and a heap-sizing *policy* — right-sizing recovered about 3× on both members, a maximal pre-grow handed half of chain's win back to cold pages while tying on spines, and no single static initial size serves a small-live churner and a large-live tree at once — with an upstream generational collector as the horizon. An engine knob was refused once as a single-benchmark overfit; this admission is the class-wide answer the WasmGC mainstream ships. This specification is that campaign's living record and retires into its design decision.

- Lever B — static array-scoped reuse — is **refused**, 2026-08-17. The builder population exists — 136 of 256 rope extends are linearly threaded — but its churn is not copy-bound: the rope already amortizes a linear builder to one O(1) node per extend and a single force-time fill, so an in-place write over the mutable array substrate would relocate node allocations rather than remove copies, and node churn is the class the admitted engine lever answers. Reinstate condition: copy-boundness observed in real code — repeated force-fills on builder chains, the alternating append-and-read hazard the rope's own documentation names.

- Lever C — construction avoidance — is **refused**, 2026-08-17. The shapes its rewrites erase are measured at zero: a multi-spread body's intermediate construction would pair intra-function, and the census found zero constructed-width pairs in the whole corpus, because construct-then-match folding and field travel dissolve those flows before CPS settles — `churn_threaded_record_allocates_nothing` pins the strongest case. Constructor-tail recursion is not the corpus's spelling — its walks are accumulator-first — and the rewrite is not cheap either, honestly costed: a tail-modulo-cons write needs the mutable tuple field the emitted representation deliberately lacks. Reinstate condition: constructed-width pairs rising from zero, or a counted constructor-tail population.

- Neither stop fired: the census shows the population pervasive rather than rare, and `churn`'s half of the clause dissolved when its loop stopped allocating — nothing was left for the Lean comparison to gate.

## Refused mechanisms

Recorded 2026-08-17, so the gate does not relitigate them.

A per-object reference count: on an engine-owned heap a count can free nothing, so counts are pure overhead purchased solely for a uniqueness test — dup/drop traffic on every reference duplication, payload fields losing immutability and gaining exactly the write-barrier cost of the generational engines lever A converges on, and an undercount silently corrupts a shared value in place, which makes count discipline a permanent obligation on every future `curios-cont` pass. Perceus's counts pay for themselves by *being* the memory manager; on top of a GC they are only the test.

A sticky one-bit sharing flag: the cheaper half of the same lane — no decrements, static death-birth pairing with a dynamic aliasing answer — refused with it, because it shares the mutable header, the mut-ification, and the misplaced-dup soundness class, and its coverage advantage over lever B is confined to shapes M1 can measure before anyone pays for them.

Regions and arenas: inexpressible on WasmGC — the engine owns the heap and nothing frees wholesale.

Owning allocation — reference counting in linear memory: a different compilation strategy, not an optimization of this pipeline; it forfeits the engine collector and the browser story to buy the Perceus precondition.
