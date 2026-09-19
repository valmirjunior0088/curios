# A refused law is lifted as a row moving, at every carrier

## Status

Not refined yet. The refused half of `curios/src/tests/laws.rs` was read row by row against the tree, and what that found is below: a decision made at the comparison needs no normal form, and the refused half is itself incomplete by the grid's own rule. Every row has a mechanism within reach. The shift by a literal count, parity, the position inside a window and the map by the identity have landed and left this file; De Morgan and absorption are not started. The rows are independent of one another, so this file is a set of lifts under one acceptance bar and not a sequence.

## Why it exists

The grid keeps its refused rows "so the refused set is a record rather than a rumor, and so that taking one later is a row moving, not a test appearing". That record is what gets quoted when someone asks where conversion's algebra ends, so the reason beside a row has to be the one that actually holds the row back. For every row here that reason is a missing mechanism and never an inadmissible equation.

Each lift is an equation beneath both checkers. `reduce::intrinsic` and `spine` are `curios-core`'s, so a wrong `Equal` admits by [the perimeter's shared route](../design/language/the-soundness-perimeter.md) and a wrong `Clash` by the vacuous-elimination route [Intrinsic fold laws and the free-monoid peel](../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md) records. A lift is therefore priced here as an addition to the trusted base, with the evidence it owes, and never as a convenience.

## What was found

Taken **2026-09-19** at `c1fc7d16`, through the line the grid itself reads: each claim stated as a written goal, held when the report carries `? ≈ Eq/refl()` and refused when it does not.

| Row | Today | Recorded reason | Does the reason stand |
| --- | --- | --- | --- |
| `Eq(Bool/not(b && c), Bool/not(b) \|\| Bool/not(c))` and `Eq(b \|\| (b && c), b)` | refused | "need a normal form past the leaf set" | Yes for a *rewriting* normal form; a comparison-time decision needs none |

**The refused half is incomplete by the grid's own rule.** "State a row at every carrier, and state it first." Refused today and stated nowhere: the duals of De Morgan and absorption, and distribution of `&&` over `||`.

## The acceptance every lift shares

A row is lifted when all of these hold, and the spec's per-row sections add only what is particular to the row.

- **The rows are stated first, on the refused side, at every carrier the rule will reach** — the twins above and a near-miss control beside each, a claim the lift must *leave* refused. The change that follows is then a refactor with an oracle, and `every_row_is_on_the_side_the_compiler_puts_it` names every row the rule moved, wanted or not.
- **The row moves from `refused` to `held`**, where `every_held_law_closes_by_refl` puts it to both checkers as an `Eq/refl()` proof.
- **The value grid holds it.** A fold law is a shape in `reduce::intrinsic::laws_tests`' `every_open_fold_law_preserves_the_value_at_every_closed_instantiation`, pinned to its stated reduct so a law that stops firing fails. A peel verdict is a shape in that carrier's `every_*_peel_verdict_holds_at_every_closed_instantiation` under the verdict's own obligation, with the tally moved, and the near-miss control beside it.
- **One mutation is run and caught**, and the entry names the mutation and the instantiation that caught it, so it can be run again.
- **Whatever the rule builds is charged before it is built**, in the price list's own units, and a count or a cap is the theory's and never the host's.
- **The rule sits where the two checkers cannot disagree about it**: in `curios-core`, and where it is probe-side, asked for by name in both converters — and in the inverter when it can answer `Clash`.
- **The perimeter entries are corrected in place**: [Open fold laws and the sum normal form](../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md) for a fold, [Intrinsic fold laws and the free-monoid peel](../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md) for a verdict, and [The bounds oracle and the division family](../soundness/per-term-rules/the-bounds-oracle-and-the-division-family.md) where a row touches it.

## De Morgan and absorption: decided at the comparison, over the atoms

The recorded reason assumes the answer is a normal form. [A stuck comparison is spelled one way](../design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md) already rejected respelling at the fold — a guard's refinement is keyed on its written spelling — and `peel_bool`'s own doc records the `&&`/`||` cliff a whole-tree normalization paid. Both objections are to *rewriting*. Neither touches a decision made where two trees are compared.

- **(a) A capped truth table, probe-side.** Force every leaf through a mixed tree of `&&`, `||`, `xor` and `Bool` equality — `normalize_bool` generalized past one connective — align comparison atoms through `dual_comparison` so `x < y` and `y <= x` are one atom and its negation, and evaluate both sides at every assignment, charged `2ⁿ` times the tree before the first. Agreement everywhere is `Equal`; anything else is `Stuck`. Sound because treating atoms as independent *over*-approximates the values they take together: agreement at every assignment holds at the real ones, whatever correlates `x < y` with `x < y + 1`. For the same reason disagreement proves nothing, so never `Clash`.
- **(b) A reduced ordered BDD** over atoms in the structural-hash order monomials use. Canonical, and as a fold it is the rewriting both records reject; probe-side it is (a) with a different inner loop, under the same cap.
- **(c) Algebraic normal form over GF(2).** Canonical and exponential where the common case is not: an `n`-way `||` is `2ⁿ - 1` monomials.

(a) is the one that fits both records. **To decide:** the cap, which is a theory constant; and whether a tautology should *reduce* to `true`. A comparison-time rule closes `Eq(e, true)` by `refl`, and `Bool/holds_of_eq` turns that into `Bool/Holds(e)` in one written step, but an obligation the elaborator fills by reduction is not filled — that would take the decision into the fold, where the cliff is.

## Adjacent items that are not this one

- **Hypotheses feeding the algebra.** With no hypothesis in scope `compare_nat` is already complete for `<` and `<=` — a side still holding a symbol is unbounded — so beyond the gcd test what an omega-style procedure adds is hypotheses. [A bound is stated in a decided proposition and discharged by reduction](../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md) parks it as "a larger and separate capability" and [A closed term evaluates at an interpreter's speed](faster-conversion-oracle-spec.md) says it wants its own file. The smallest step that way is `nat_dominators` reading a guard in scope, which puts a hypothesis inside conversion and owes the metatheory that move has always owed. The route that fits [Proof automation writes terms into the source and is never trusted](proof-automation-spec.md) is a reflected checker with a certificate.
- **Summands paired up to conversion.** `Eq(f(a + b) + g(c + d), g(d + c) + f(b + a))` is refused while `Eq(f(a + b), f(b + a))` and commutation each hold: `Nat::cancel_common` pairs summands by structural identity, the incompleteness falling on the declining side by design. It is the same family as the map row — a syntactic test where conversion is what is meant — and a larger change, since `cancel_common` takes no reducer and its stability contract is what keeps the peel terminating.
- **Fusion.** `map` after `map` is a refused row of the grid with its reason; `fold` after `map` is the same candidate one fold over.

## What has to be decided

- Whether a Boolean tautology reduces, or only compares; and the cap.
- Whether the missing refused rows are stated as one change ahead of every lift, or each with its own.

## Deliberately not specified

The value of the truth table's cap and the unit it is charged in. `Int/shr`'s law, which depends on whether the shift floors where the division truncates. The order of the lifts: they share no code.

## The seam it comes back through

`curios/src/tests/laws.rs` for every row. `normalize_bool`, `align_comparisons`, `dual_comparison` and `spine.rs`'s `peel_bool` for the Boolean rows. Both converters' intrinsic congruences — `curios-cert/src/kernel/convert/intrinsic.rs` and `curios-elab/src/convert/intrinsic.rs` — and `curios-analysis/src/invert.rs` for anything probe-side. `reduce::intrinsic::laws_tests`, `compare_tests` and `spine`'s tests for the value grids.

## How to retake the measurements

```sh
cargo run --package curios -- wonder diagnostics - <<'CRS' | grep -E '^ +[0-9]+ \| let|≈ Eq/refl'
use /std/{Bool, Eq, Io};

let absorb(b: Bool, c: Bool) -> Eq(b || (b && c), b) = ?;
let excluded(b: Bool) -> Eq(b || Bool/not(b), true) = ?;
Io/pure(())
CRS
```

A report prints its candidates above the source line it belongs to, so a `? ≈ Eq/refl()` line marks the `let` that *follows* it as held, and the bare listing after the last report repeats every line without them. `excluded` is held and `absorb` is not.
