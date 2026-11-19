# A callee is weighed by what it emits

**Not refined yet.** The inliner admits a callee by counting its live nodes, a `Nat` or `Int` operation one of them, though its emitted form carries a fallback many times its size. This specification reserves weighing a callee by what it emits. It is not an implementation plan: its first step is a sweep and a timing deciding the weight.

## What is wrong today

`inline_known_calls` (`curios-cps/src/optimize/inline.rs`) admits a callee with several call sites only where its extent holds no more than `MULTI_SITE_INLINE_LIMIT` nodes (`curios-cps/src/optimize.rs`), and each node counts one. A `Nat` or `Int` operation is one node, emitted by `curios-emit` as an `i31` fast path beside a call into the big-number helpers, so a callee of a few arithmetic operations is admitted at a weight that understates what each copy adds. On a prototype over a fork of sixteen fields, which [`Map` is a trie forking sixteen ways, and its shape is its API](../../../curios-prelude/README.md#map-is-a-trie-forking-sixteen-ways-and-its-shape-is-its-api) rejects, an insert each arm of which called two small helpers held in each arm its own copy of the helpers' digit reads with their fallbacks, and its program spent 14.5 s in `emit` against 1.1 s over the map of `8ed961f9c`.

Counted at `5b3028ed8` over the corpus (protocol below): weighing each `Nat` and `Int` operation of a callee's extent as five nodes shrinks the emitted text of 26 of the 28 programs of `programs/` by 2.8 to 11.3%, most by about 6% (`flt_hot_loop` 11.3%, `parse_bindless` 10.5%), and leaves `hello_world` and `dependent_vectors` as they are; weighing each as seventeen grows every program by 1.2 to 9%. The weight moves what `/std`'s functions every program links are copied into, not one program's own.

## Protocol

A variant of `inline_known_calls`, switched by an environment variable and off by default, adds `extra` to a callee's extent size for each `Node::LetIntrinsic` whose operation is a `Nat` or an `Int` one, refuses the callee where the sum passes the limit the call is held to, and charges the copy budget by the sum. With the variant off, every program's emitted text is byte-equal to the compiler's. For each `.crs` of `programs/` and its directories, the bytes of `curios wonder --stage wasm <file>`, counted, at each weight.

## Refinement

- **The weight.** A sweep of `extra` from 0 through 16, counted as above and over `/std`'s test program, and per operation kind where an addition's fallback and a division's part, read off each intrinsic's emitted form (`curios-emit`'s fast paths and their helpers).
- **What it costs at run time.** The six xbench workloads timed at the two best weights against today's, interleaved, seven rounds, median and span, on a quiet machine: a call an inlined fast path saved comes back where a copy is refused.
- **The limit beside it.** Whether `MULTI_SITE_INLINE_LIMIT` and `SCC_CLONE_NODE_LIMIT` move with the weight, and whether the cloner, which copies a group by the same count, takes the same weight.

## Verification and retirement

Once refined, its stages state their checks; the weight lands stated in `curios-cps`'s README beside the limit it is read against, the figures named by their commit, and this file is deleted.
