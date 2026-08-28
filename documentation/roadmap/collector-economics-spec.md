# The survivors are what cost

## Status

Researched, not designed. This specification records what is certain — read from the pinned engine's sources and its governing RFC, or established by comparing the same emitted module under two engines — and stops there. The mechanism question is settled below and the route question is open; every detail past the route is untouched, awaiting whoever picks this up. Nothing is started.

## Why it exists

[The heap is sized ahead of its churn](../design/toolchain/the-heap-is-sized-ahead-of-its-churn.md) closed the churn campaign by sizing the semi-space, and its limits paragraph is this specification's charter: a constant cannot serve a small-live churner and a large-live tree at once, and no engine hook chooses per program.

The native engine's collector charges for what survives. A semi-space copier recopies its entire live set on every collection, and grows only when a single allocation still does not fit after one — so a program whose live set is large pays for that live set again at every cycle, and a constant initial size can only move where the paying starts. The browser half of this toolchain already runs under a generational, mark-compacting collector, so this is a native-only gap and closing it is a parity question rather than a new capability.

## The mechanism, settled

This specification was opened as a generational nursery, on the reading that the churn campaign's remaining cost was young objects being collected as though they might survive. Running the same emitted module under both the native engine and V8 does not support that aim, and the direction of the result is not marginal.

- On the death-birth churn workloads the sizing decision already closed most of the distance to V8. What is left is a fraction, not a multiple.
- On an all-live tree the distance *is* a multiple, and it appears exactly at the knee where the live set outgrows the sized heap: below it the two engines are near parity, above it the native per-node cost roughly doubles and stays doubled while V8's does not rise. Recursion depth barely moves across that sweep, so the step is the collector's, not the call path's.
- **A nursery cannot close that gap.** Every node of an all-live structure survives, so every one would promote; a nursery adds a copy on the way out rather than removing one. The mechanisms that do close it are the growth policy — sizing the heap to a multiple of the live set after each collection, rather than to the first allocation that did not fit — and an old generation that does not recopy what it already holds.
- The nursery keeps a smaller claim, on the churn class alone, where the remaining distance is a fraction. It is a later refinement, not the campaign.

The comparison is reproducible without new machinery, and reproducing it is the right first act of picking this up: `curios-js` compiles a source and its harness runs the emitted module under Node, against the same source built as a native executable. Two cautions belong with any retake. Normalize by an allocation-free control, so the engine's constant tax — a different code generator, and a V8 arm that never sees Binaryen — is cancelled rather than attributed to the collector. And keep the workload's input arriving through the host boundary on both arms, or the erased optimizer folds the program to its answer.

## What is certain about the engine

Read at the wasmtime 47.0.3 pin.

- The collector inventory is a null collector, deferred reference counting, and a semi-space copier. No nursery, no generations. The copier has an in-wasm bump-allocation fast path, grows only when a single post-collection allocation cannot fit, never shrinks, and exposes size knobs alone — initial size, reservation, reservation for growth, guard size — none of which is a policy.
- A collector is *two* implementations, not one: a runtime half behind `GcRuntime`/`GcHeap`, and a compiler half behind `GcCompiler` that emits the inline allocation path and the barriers into compiled code. Three worked siblings exist to read from, and the two halves must agree.
- The barrier hooks are already threaded through the compiler half at the sites a generational or old-space design needs. The write barrier is invoked from the struct-and-array field store path, the copying collector implements it as a no-op, and the reference-counting collector implements it for real — so whether a non-trivial barrier is expressible in this framework is answered, and answered yes. This is the decisive integration fact on the engine's side.
- The collector's identity travels in the `.cwasm` compatibility stamp and is checked on deserialization with a named error, as the heap's initial size already is. Any engine-side change here therefore travels under the workspace's single-pin invariant with no new mechanism, and an artifact cannot be run by an engine that would collect it differently.
- **Upstream refuses embedder-supplied collectors by design.** The traits are not exported from the user-facing crate, and the accepted RFC states the intent directly: the crate exposes a handful of built-in collectors and will not let an embedder bring its own. The escape hatch the RFC names instead is a further built-in collector, gated on demand for one.
- The engine's maintainers state that the collector has been built for correctness rather than performance, and the announced roadmap is component-model integration. There is no tracking issue for a collector with a nursery or a generational old space.

## What is certain about Curios

- The write-barrier surface is small and enumerable. This pipeline's emitted mutable stores are a rope node's memoization, `Cell` writes, and the back-patching of a recursive closure shell; every tuple field is emitted immutable, so a record or variant can never point old-to-young after birth. GHC's "immutability is an asset" is, here, a handful of known sites — all compiler-controlled, none user-extensible — which is why a barrier's cost on Curios programs would be near zero and why that is an unusually strong argument for this design in this compiler.
- Emitted modules declare no linear memory at all. Every value, and every host-ABI crossing, is a GC reference.
- The population is the collector's, by census: half of all constructions stand beside a co-resident dying value of matching width, and every such pair is cross-frame, so no intra-function mechanism competes with a collector-side cure.
- The moving-collector obligations are already met on the main path, and have been since the copying collector arrived.
- The browser product is the standing proof that this emitted module runs, and runs well, under a generational collector. It is evidence for the whole campaign and the reason the gap is native-only.

## The three routes

Where a better collector comes from is the open question. The mechanism above does not choose between these, and each is a different kind of commitment.

**Upstream.** What waiting buys is not a hook — that is refused — but a further built-in collector selectable by one configuration call, in which case Curios's entire integration is that call. The RFC's gate is demand, and Curios is a WasmGC producer with a measured workload corpus and a reproducible cross-engine comparison. Filing that is nearly free and is the only lever that moves this route's date. It is not a plan with a date otherwise.

**A carried patch, or a fork.** The seam is well factored, three siblings are templates, the barrier hooks exist, and the pin is one row — so a `[patch]` or a git dependency is a one-row change, and the single-pin invariant already guarantees the precompiler and the launcher cannot disagree. The costs are equally nameable: rebase burden against a fast-moving engine; a moving collector is soundness-critical, and a defect in one is host memory unsafety rather than a wrong answer; and the blast radius reaches Cranelift, so the isolated slim-launcher build and the bundle guards must be re-checked rather than assumed. Growth policy is by far the smaller of the two patches — no barrier, no header change, no compiler half at all — which makes it both the cheaper win and the honest probe of whether upstream will take a collector-economics patch before a larger one is written against that hope.

**A different engine.** V8 has what is missing, and the browser product already proves the emitted module runs under it, so this route buys the mechanism by deletion rather than by implementation. Its costs are structural rather than incremental: the native product's Cranelift precompilation, the `.cwasm` format, the slim launcher and the bundle format are all built on the current engine, and the JavaScript harness's host boundary is deliberately shallow — stdin at EOF, filesystem and network refused — so the native host operations are a gap to be written, not a port. This is the only route that changes the product rather than the engine under it, and it should be weighed as a product decision, not as a collector one.

## Parked, with reasons

Recorded so they are not relitigated.

**A nursery built over the copier from inside the guest.** Refuted, not merely hard. A nursery needs two powers — choosing where an allocation lands, and tracing a subset of the heap — and WasmGC withholds both from the guest deliberately. No instruction converts a reference to an integer, so no guest-side bump pointer, card table or location-keyed remembered set can exist; nothing triggers, observes or scopes a collection; nothing frees. What remains buildable is an object pool reused through mutable fields, which is the inversion of a nursery rather than an approximation of one: pooled objects are permanently reachable, so the engine recopies the whole pool at every collection, and the scheme additionally needs the mutable tuple fields the representation withholds and a uniqueness test whose two candidate mechanisms the churn campaign already refused at its gate.

**The generational nursery as this campaign's aim.** Demoted, not refused, for the reason the settled mechanism gives: it does not touch an all-live heap, which is where the measured gap is. It keeps its smaller claim on the churn class and composes with the growth policy rather than competing with it, so it belongs after the growth policy has landed and been measured, never before.

**Owning allocation in linear memory.** Refused, and more expensively than the churn campaign's one-line rejection records. Since no emitted module declares a memory today, this is not adding an allocator: it is introducing linear memory, re-laying-out every value representation, rewriting the host ABI's lift and lower on both the native and the JavaScript sides, and writing and maintaining a collector including its root finding — which on wasm means a shadow stack, a pervasive change to continuation emission and a cost on every call. It is also the only option that makes the browser back end worse in order to make the native one better, since the browser half would forfeit the collector it already has.

## Deliberately not specified

Which route is taken; the growth policy's multiple, its floor and its interaction with the sized initial heap; whether an old generation earns a representation of its own or a growth factor suffices; where barriers would be emitted, if a design needs any; how the host ABI's flat-array crossings interact with evacuation; and the measurement program that would gate any of it. That refinement begins when this specification is picked up.
