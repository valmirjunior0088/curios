# A case refinement is keyed at the cheap spelling first

## Status

Deliberately unrefined as a *plan*, but the cause is located and the cure already exists on the other side of the two-checker seam. The table below is reproduced by `curios`' `scrutinee_refinement_measurements`; the profiler attributions come from the command at the foot of this document, and the one figure that is a single unrepeated reading says so where it stands. Nothing is started, and this touches the trusted base, so it wants review before it wants speed.

## Why it exists

A web of fourteen combinator definitions refuses to compile:

```crs
let Pred: Type = (x: Nat) -> Bool;
let both(p: Pred, q: Pred) -> Pred = (x) => p(x) && q(x);
let anyof(p: Pred, q: Pred) -> Pred = (x) => p(x) || q(x);
let base: Pred = (x) => x % 2 == 0;
let other: Pred = (x) => x % 3 == 0;

let r0: Pred = both(base, anyof(other, base));
let r1: Pred = both(r0, anyof(base, r0));
-- … thirteen of these compile; the fourteenth does not
let top: Pred = r13;

let probe(n: Nat) -> Str =
    match top(n): (_) => Str | true => "y" | false => "n" end;
```

```
the kernel refused /probe: the kernel's reduction budget ran out
```

Nothing here is exotic. It is a web of small combinator definitions consumed by a match — which is what a user writes when they build anything out of composable pieces.

## The trigger, isolated

The same web under two independent variations: how often a rule names the one before it, and whether the web's value reaches a match scrutinee.

| definitions | named twice, not scrutinized | named once per rule, scrutinized | named twice, scrutinized |
| --- | --- | --- | --- |
| 8 | 0.28 s | 0.26 s | 0.29 s |
| 10 | 0.26 s | 0.28 s | 0.42 s |
| 12 | 0.26 s | 0.28 s | 1.21 s |
| 13 | 0.26 s | 0.30 s | 2.60 s |
| 14 | 0.27 s | 0.31 s | **refused** |
| 20 | 0.27 s | 0.99 s | **refused** |

Both conditions are necessary and neither is sufficient. A web nothing scrutinizes is flat however it fans out. A web that *is* scrutinized costs what its fan-out is: the middle column still names each definition twice across the chain — once as the previous rule, once as the older one — and grows accordingly, just far more slowly than naming it twice within a single rule.

Wrapping the carrier in a nominal `struct` does not help. Neither does an intrinsic at the use site — `top(n) && true` is free — so a strict operand is not the trigger; the scrutinee is.

## Where it comes from

`curios-cert/src/kernel/infer/eliminate.rs`, in `assume_case_value`:

```rust
let stuck = kernel.reduce(scrutinee.clone()).unwrap_or_else(|_| scrutinee.clone());
kernel.refine(stuck, value);
```

A scrutinee that is not a bare variable is fully weak-head reduced to get a key for the case refinement, **once per arm**. Two things then compound:

- `top(n)` mentions the local `n`, and `Memos::storable` admits a whnf entry only for a *local-free* term — correctly, since a key must be scope-independent. So neither the scrutinee nor any sub-term of its reduction can be memoized, and a web where each definition is named twice unfolds `2^n`.
- The reduction's own failure is swallowed by `unwrap_or_else`, so exhausting the budget here surfaces as a refusal on whatever judgment runs next, with a message that names a budget rather than a cause.

## The elaborator already fixed this

`curios-elab/src/reduce.rs` documents the same defect in the same words and the cure it shipped:

> The *cheap* refinement key: metavariable solutions materialized and universe instances erased, with every argument left exactly as written.
>
> `canonical_scrutinee` additionally reduces each argument … A guard over an expensive operand then pays for the very computation it was written to avoid … Both sites therefore key on this form first and escalate to the canonical one only on a miss, which is a strict superset: every occurrence that matched before still matches, and **the ones that used to spend the declaration's whole budget on the way in now match without reducing anything**.

So the cure is not a new idea to be designed. It is a two-tier key — cheap spelling first, canonical form only on a miss — that one checker has and the other does not.

## Known for certain

- **The two checkers price the same terms 221× apart, and the gap is exponential.** Under `--features profile` at thirteen definitions, `recheck` is **2 881 ms of a 3 061 ms compile — 94.1%** — allocating 6 955 MB across 74.8 M allocations, against `elaborate_and_zonk`'s **13 ms** and 134 k allocations.

- **It is latent in shipped code, not active.** The prelude certifies, and the kernel costs 8 ms on a real `Toml/decode` compile. The elaborator's own note records that real code hit the elaborator-side version, so the shape is not synthetic — the kernel simply has not met it yet.

- **The elaborator's escalation is a strict superset**, by its own argument, which is what makes porting it rule-preserving rather than a weakening of the trusted base. That property is the reason this is a defect rather than a design question, and it is also the thing a reviewer should check first.

- **One thing is not explained** *(a one-off reading, not covered by the probe)*. With combinator bodies written as a stuck `match` instead of `&&`, the cost steps from 0.03 s at seventeen definitions to 2.81 s at eighteen and stays flat to forty, insensitive to `--budget` from 7.5 M to 60 M. Same call site. A step function that a budget does not move is not the mechanism above, and it is recorded rather than guessed at — including that it is one reading, which is what the predecessor to the sibling specification got wrong.

## What this costs the parity claim

[A reduction step costs what it builds](../../design/toolchain/a-reduction-step-costs-what-it-builds.md) states that cost parity between the two checkers is a property to measure rather than assume, and counts the times it has failed. This is the third, and the first that is exponential rather than a multiple. Its measurement belongs beside `str_literal_cost_measurements` and `kernel_memo_charge_measurements`, which that decision already names as what parity-by-measurement means here.

## Deliberately not specified

Whether the cheap key should be shared with the elaborator through `curios-core` rather than written twice — the two-checker discipline says the rules are written separately on purpose, and a *key* is arguably representation rather than judgment, which is the same question `reduce_closed` answered one way and each checker's strategy answers the other. Whether the swallowed `unwrap_or_else` should become a diagnostic. And the eighteen-definition step above.

## How to take the figures

```sh
cargo test --release --package curios --lib -- --ignored --nocapture scrutinee_refinement_measurements
make curios/profile CURIOS_PROFILE_SOURCE=<a thirteen-definition web>
```

Both taken **2026-08-21**, **release**, `aarch64-apple-darwin`.
