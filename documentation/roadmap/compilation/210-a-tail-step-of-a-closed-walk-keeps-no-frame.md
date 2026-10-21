# A tail step of a closed walk keeps no frame

**Not refined yet.** This specification reserves running a closed recursive walk in the machine at a stack that does not grow with the walk. It is not an implementation plan: it waits on the design of a tail frame named under *Refinement*.

## What is missing

`curios_core::Machine` (`curios-core/src/machine.rs`), which both checkers run a closed term on, keeps every step of a walk on its stack until the walk's value comes back. A call and a `match` each file their value under a `Frame::Memo`, and a recursive member's body runs under a `Frame::RecGuard`; the recursive call in an arm's tail is evaluated above all of them, so a walk of `n` steps ends holding about `6n` frames, and `push` charges `machine_frame()` at every new peak. The charge is honest — the frames are held — and removable: a tail step's frames all answer with the value of the step after them.

Measured at `997cec045` (counted, the protocol under *What is known*), per element of a walk over a 200-element literal, across every charge the elaborator takes in the run:

| Walk | Units per element | Beta | Memo entries | Frame peaks | Transitions | Other |
| --- | --- | --- | --- | --- | --- | --- |
| `Bytes/fold(b, 0, (b, n) => n + 1)` | 257 | 104 | 39 | 37 | 31 | 46 |
| `Str/fold(s, 0, (c, n) => n)` | 248 | 110 | 45 | 49 | 28 | 16 |

`/std/Fmt/parse` is such a walk with its own step on top: a `Fmt/render` call's least budget grows by about 390 units a character of its format (`fmt1_200` to `fmt1_400`), of which the frame peaks are a tenth, the memo entries and the step's `let` destructuring a sixth each, and the betas two fifths.

The frame peaks are the gap. The betas are construction — a step opens its parameters, a lifted `go` carrying what it closes over — and the memo entries are kept on purpose, below.

## What is known

- **The protocol.** A scratch copy is patched so that `Reducer::spend`, the elaborator's `Context::spend` and `Spend::spend` are `#[track_caller]`, and `Spend::spend` notes `site = Location::caller()`, `units` and `category` through `curios_profile::note!`. It is built `--release --features profile`; each probe runs as `curios --profile <stream> wonder diagnostics <probe>`, and the stream's `V` rows are summed by site. A probe is `let a: Eq()(<walk>, <its value>) = Eq/refl();` over a 200-character literal, so the walk runs in the machine during elaboration; a walk's figure per element is its difference from the probe `Eq()(1, 1)`, over 200. The least budgets are the bisected `--budget` at which `wonder diagnostics` stops reporting exhaustion.
- **What the library pays at a peak.** Counted at `f0712f8e4`, by counters a scratch copy kept at `push` and took at each budget sample over `cargo xtask profile-prelude elaboration` and `certification`, a new peak charging 6 units. Elaboration reaches 154,575 peaks, 927,450 units, 1.55% of its 60,010,479; 399,750 of them, 0.67%, stand above a `Frame::Memo` or a `Frame::RecGuard`. Certification reaches 87,727 peaks, 526,362 units, 2.53% of its 20,801,136; 269,220 of them, 1.29%, stand above one. That is past the hundredth a half's units this specification was to be deleted under, in certification.
- **The memo entries stay.** `Frame::Head` files every call, and `Frame::Memo` every `match`, because a projected result asks one call once per component: with the recursive arm left unfiled, a fold returning a pair costs exponentially more than its single-value twin (`Frame::Head`'s comment), and an arm projecting its hypothesis `k` times runs the fold `k` times (`eval`'s comment on `match`). A tail frame can carry several keys; it cannot drop them.
- **An outer guard of the same group passes what an inner one returned**, read from `Frame::RecGuard` and not yet tested: an inner guard returns either its folded call, an application the neutral test does not match, or a value that is not both neutral and naming the group, so the outer guard returns its value unchanged — save a member of no parameters, whose folded spelling is a projection.
- **Prior art.** GHC's runtime squeezes a run of adjacent update frames into one, all the closures updated with the one value ([`rts/ThreadPaused.c`](https://gitlab.haskell.org/normalcoder/ghc/-/blob/d3e069a3f8238c01df235ec7aff5c946843e8a21/rts/ThreadPaused.c), [#20795](https://gitlab.haskell.org/ghc/ghc/-/issues/20795)). Clinger states the space property a loop needs of an implementation ([Proper tail recursion and space efficiency](https://static.aminer.org/pdf/20170130/pdfs/pldi/jdpipmac2xyouvhf5lqzui8dswk1sonr.pdf), PLDI 1998). Lean 4's kernel files a `whnf` only under the term asked, stepping its unfolding loop without filing what it passes ([`type_checker.cpp`](https://github.com/leanprover/lean4/blob/master/src/kernel/type_checker.cpp)).

## Refinement

- **The design it waits on.** A tail frame: a `Frame::Memo` that carries the keys of the frames it absorbs, and a `Frame::RecGuard` not pushed above one of its own group. Both checkers run this machine, so a change moves both by one rule; `tests::runaways`, `runaway_measurements` and the parity measurements are retaken, and the prelude's image digests are compared at one path before and after.
