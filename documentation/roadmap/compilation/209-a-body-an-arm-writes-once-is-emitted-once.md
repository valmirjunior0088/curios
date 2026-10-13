# A body an arm writes once is emitted once

**Not refined yet.** This specification reserves the place where two leaves of one match that run the same body come to run one copy of it. It is not an implementation plan, and no design is chosen. It waits on [typed patterns](../surface/200-typed-patterns.md), which makes the copies the compiler's own, and on the count named under *Refinement*.

## What is missing

**Nothing finds a body written twice.** A body a reader writes in two arms is emitted in both, through every rung to the optimized module, and the continuation optimizer (`curios-cont/src/optimize/`) holds no pass that takes two identical bodies for one. Today the reader chooses: a body bound once as a local function and called from both arms stays one where it is large, and is copied back into both where the inliner takes it. Once patterns are typed the choice is the compiler's: a row reached at several leaves is written at each as an instance of one elaborated body, since Core has nothing that shares a body among arms.

## What is known

- **What a copy weighs.** Counted, at `6f2028e9b`, by the release compiler. A program reads `n` from standard input as `programs/lcg/lcg.crs` does and prints `pick(half(n), third(n), n)`, where `half(n)` is `Option/some(n)` at an even `n` and `Option/none()` otherwise, `third(n)` the same at a multiple of three, and `pick(a, b, n)` matches `a` and under each of its arms `b`: `x + y` at `some(x)` and `some(y)`, `y` at `none()` and `some(y)`, and one body at `none()` of `b` under both arms of `a`. In the copied spelling that body is written in both arms; in the shared one it is `let shared: () -> Nat = () => …;` above the match, called from both. The small body is `((n * 3 + 424243) % 1000003 * (n + 11) % 65537 + n * n % 9973) * 7 % 100003`. The large one is the sum, for `i` from 1 to 40, of `((n * 3 + 424243) % 1000003 * (n + 13 * i) % 65537)`, taken `% 100003`. Each program is reprinted by `wonder stage <rung> -` and the bytes of its standard output counted.

  | Body | Spelling | `cont-optm` | `wasm-optm` |
  | --- | --- | --- | --- |
  | small | copied | 130,037 | 558,714 |
  | small | shared | 130,037 | 558,714 |
  | large | copied | 160,521 | 991,516 |
  | large | shared | 143,509 | 678,022 |

  The two spellings print one answer, `64888` and `70407` at an input of `5`.
- **The inliner already decides when sharing pays.** The small body bound once is copied into both callers and the two spellings come to one size; the large one stays one function. So what is missing is the other direction alone, from two copies to one.
- **Where no body rests on a case.** After erasure the types and the proofs are gone, and two instances of one row differ by the names they bind and nothing else: a body shared there needs no equation the kernel holds, which is why the sharing is not Core's ([typed patterns](../surface/200-typed-patterns.md), *A leaf holds the body with the leaf's values in its variables' places*).
- **Peers share at the leaf.** A decision tree's "primary drawback is potential code size explosion", and "decision trees produced by the real compiler are implemented as dags with maximal sharing" (Maranget, "Compiling Pattern Matching to Good Decision Trees", ML Workshop 2008, §1 and §4), where a leaf is an action's number and a body is written once by construction.

## Refinement

- **What two bodies are held equal by**: structure up to the binders an instance renames, and at which rung — the erased term, where a match is still a tree, or the continuation graph, where a body is a continuation with parameters.
- **Whether the inliner's rule decides the merge**, so that a body it would copy back is never merged first.
- **The count this waits on.** Over `/std` once its hand-nested matches are flattened ([typed patterns](../surface/200-typed-patterns.md), stage 6): how many rows reach more than one leaf, and what their instances weigh at `cont-optm`.
- **Check.** The table above, the copied spelling coming to the shared one's size at both rungs for the large body, and unmoved for the small.
