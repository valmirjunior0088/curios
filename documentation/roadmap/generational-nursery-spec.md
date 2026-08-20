# A young value dies free

## Status

Deliberately unrefined. This specification records only what the death-birth churn campaign established for certain — measured on probes that survive in the tree, or read from the pinned engine's source — and stops there. The design space is untouched, awaiting whoever picks this up. Nothing is started.

## Why it exists

[The heap is sized ahead of its churn](../design/toolchain/the-heap-is-sized-ahead-of-its-churn.md) closed the churn campaign by sizing the semi-space, and its limits paragraph is this specification's charter: a constant cannot serve a small-live churner and a large-live tree at once, an all-live workload's survivors are still recopied on every collection, and a heap large enough to make collections rare is too large to stay cache-hot. A generational nursery is the mechanism the measured economics point at — a small hot region where young objects are born, whose dead cost nothing at collection, whose survivors are evacuated once — and it does not exist in the native engine.

## Known for certain

- The engine's collector inventory, read at the 47.0.3 pin: deferred reference counting and a semi-space copier; no nursery, no generations. The copier has an in-wasm bump-allocation fast path; the heap grows only when a single post-collection allocation cannot fit (`collect_and_maybe_grow_gc_heap`), never shrinks, and exposes exactly three knobs — initial size, reservation, guard size. The initial-size tunable is baked into the `.cwasm` compatibility stamp, so any engine-side change here travels under the single-pin invariant automatically.

- The class economics, measured: two thirds of churn cost was collection recopying under the parked heap, cured for small live sets by the sizing decision, whose three-arm table is the baseline any nursery claim must beat. `trees` gained only 3% from the same constant because an all-live structure outgrows it — its survivors are recopied on every collection, which is exactly the cost a nursery's one-time promotion removes.

- The cache dimension, measured: `chain` ran 1.9× faster in a 16 MiB heap than a 256 MiB one at near-identical collection counts — recycling a small region is materially faster than sweeping a large cold one — which is the nursery's second economics, independent of collection frequency. The effect vanishes on cache-scattered walks (`spines` tied its two pre-grown arms), so it prices hot loops specifically.

- The write-barrier surface is small and enumerable — the decisive integration fact. A generational design must track old-to-young pointers, and this pipeline's emitted mutable stores are exactly three: a rope node's memoization (the `cache` written once and the children nulled), `Cell` writes, and the back-patching of a recursive closure shell. Every tuple field is emitted `Const` (`tuple_field_mutability` in `curios-cont`), so a record or variant can never point old-to-young after birth. GHC's "immutability is an asset" is, here, three known sites a barrier must cover — all compiler-controlled, none user-extensible.

- The population is the collector's, by census: 509 of 1 168 constructions stand beside a co-resident dying value of matching width, and every pair is cross-frame (`death_birth_census`), so no intra-function mechanism competes with a collector-side cure.

- The integration seam is one row: `curios-runtime`'s single wasmtime pin. The browser half already runs under V8's generational collector, so this is a native-only gap, and the moving-collector obligations are already met on the main path (`Rooted<AnyRef>` throughout since the copying collector arrived).

- Survivor-proportional growth is the smaller sibling: growing to a multiple of the live set after each collection would cure `trees`' recopying without a nursery, is equally absent upstream, and would compose with rather than replace this specification's subject.

## Deliberately not specified

Where the nursery lives — an upstream wasmtime contribution, a carried patch, or a fork; nursery size and promotion policy; whether barriers are emitted by `curios-cont` at the three sites or live engine-side; how the host ABI's flat-array crossings interact with evacuation; and the measurement program that would gate any of it. That brainstorming begins when this specification is picked up.
