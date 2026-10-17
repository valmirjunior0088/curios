# A stuck fold keeps what its walk found

**Not refined yet.** This specification reserves a stuck operation's fold taken again from what the operations beneath it found, where each reducer walks every node beneath it again today. It is not an implementation plan. It is taken after [An atom is ranked by where its binder stands](100-an-atom-is-ranked-by-where-its-binder-stands.md), which changes the same normal forms.

## What is missing

Each reducer's `refolded` (`curios-elab/src/reduce.rs`, `curios-cert/src/kernel/whnf.rs`) hands out a stuck operation's atoms (`atoms_within`, `curios-analysis/src/classify.rs`) and takes its fold again over its nodes (`refold`), each a walk of every node beneath the operation. A sum of `n` operations is `n` stuck sums, one inside the next, so one reduction of it walks its nodes `n` times over, and a comparison reduces it more than once.

Over `x1 … xn, y1 … yn: Nat`, `Eq()(Nat/and(x1, y1) + … + Nat/and(xn, yn), Nat/and(xn, yn) + … + Nat/and(x1, y1))`, the summands reversed and none respelled, closes by `Eq/refl()` in 0.46 s [0.38–1.07] at `n = 64`, 1.92 s [1.25–3.10] at 128 and 8.35 s [5.47–11.12] at 256, and with each summand's operands swapped as well in 0.95 s [0.89–1.18], 2.50 s [2.33–2.83] and 10.75 s [10.72–10.98]: timed, the median and span of three rounds of `run -` through the release compiler on top of `0db77a9a9`, the two programs and the three sizes interleaved, on a machine under load. Two of three backtraces taken under gdb at `n = 256` stand in a walk under `refolded`, and the third in the elaboration of the infix chain.

## Refinement

- **What is kept.** What a stuck operation's walk found, its atoms and the spelling of its nodes, kept with its reduct, so the operation above reads its own operands and joins what they kept in place of walking them again. Where it is kept in each reducer, and how long it lives beside the reduct tables, is not read yet.
- **Check**: the `summed` rows of `curios`'s `nested_operation_measurements`, whose cost per summand is then flat.
