# An index inversion reduces its actuals, and both checkers pay for it

## Status

Located and measured, unstarted, and deliberately unrefined as a plan. The table below is reproduced by `curios`' `scrutinee_refinement_measurements` and `scrutinee_retention_measurements`. What separates this from its predecessor is that it is **not a parity gap**: the elaborator grows at the same rate as the kernel and is only cheaper by a constant, so neither side's rule can be borrowed to fix the other.

## Why it exists

An ordinary equality proof over combinator definitions costs exponentially in the number of definitions:

```crs
let Pred: Type = (x: Nat) -> Bool;
let both(p: Pred, q: Pred) -> Pred = (x) => p(x) && q(x);
let anyof(p: Pred, q: Pred) -> Pred = (x) => p(x) || q(x);
let base: Pred = (x) => x % 2 == 0;
let other: Pred = (x) => x % 3 == 0;

let r0: Pred = both(base, anyof(other, base));
let r1: Pred = both(r0, anyof(r0, r0));
-- … eleven more
let top: Pred = r12;

let probe(n: Nat, e: Eq(top(n), true)) -> Str =
    match e: (_, _, _) => Str | refl(@z) => "y" end;
```

| definitions | 8 | 10 | 12 | 13 |
| --- | --- | --- | --- | --- |
| compile | 0.11 s | 0.28 s | 1.25 s | 2.89 s |
| elaborator's heaviest declaration, units | 16 279 | 18 689 | 21 099 | 22 304 |
| kernel's retention, units | 3 962 757 | 22 197 227 | 128 490 445 | 309 948 607 |

About ×2.4 per definition on the wall clock and on retention alike. Thirteen definitions consume 31% of the compilation's whole retention allowance; fifteen would exhaust it.

## Where it comes from

No case refinement is involved, which is what makes this a second defect rather than the one the two-tier refinement key closed — `curios-cert`'s `Scope::refine` carries that account. The scrutinee is `e`, a bare variable, so `assume_case_value` takes its variable branch and reduces nothing.

The reduction comes from the elimination rule itself. `refl`'s index targets are `(z, z)`; the actuals are `(top(n), true)`. `invert_indices` pins `z := top(n)` and then has to decide `true ~ top(n)`, which it puts to `Judge::convert_at` — and conversion reduces `top(n)` in full. Each definition names the one before it twice, `top(n)` mentions a local, and a local-bearing term is memoized by neither checker's tables, so the web unfolds `2^n`.

Both checkers drive the *same* `curios-analysis` inverter behind the `Judge` seam, and both reduce with their own strategies underneath it. That is why both grow.

## Known for certain

- **Both checkers are exponential, at the same rate.** At 13 definitions `recheck` is 2 738 ms and `elaborate_and_zonk` is 105 ms, against 39 ms and 9 ms at 8 — ×2.4 and ×2.2 respectively. The kernel is 26× the constant, which is an ordinary difference between two evaluators; the *exponent* is shared.

- **It is untouched by the two-tier refinement key.** The same programs cost the same wall clock before and after that commit, to the hundredth of a second, while the scrutinized shape went from refusing at fourteen definitions to flat at forty. That is the measurement which says the two are separate defects rather than one seen twice.

- **The shape is ordinary.** `Eq`, `refl`, and a definition built from smaller ones are the first three things a user reaches for when they want to *prove* something about code they already wrote.

## Deliberately not specified

Whether the cure is at the inverter (do not put a decided index to conversion when one side is a constructor and the other is a stuck application — a clash is what the arm needs, and reduction is one way to look for it rather than the only one), at conversion (a positive result cache keyed on the local context, which the kernel does not have at all and the elaborator has only for `closed()` terms), or under both (a reduction memo that admits local-bearing terms, which is what `curios-elab`'s reduction cache already does and `curios-cert`'s `Memos` refuses — see `documentation/soundness/what-the-kernel-consults/the-evaluation-memo.md` for why it refuses).

The third is the largest and would also close [The retention allowance is a compilation's, and one declaration can spend it](05-kernel-retention-accounting-spec.md); it is also the one that changes the trusted base's evaluation strategy, so it wants its own decision rather than a line here.

## How to take the figures

```sh
cargo test --release --package curios --lib -- --ignored --nocapture scrutinee_refinement_measurements
cargo test --release --package curios --lib -- --ignored --nocapture scrutinee_retention_measurements
```

Both taken **2026-08-21**, **release**, `aarch64-apple-darwin`.
