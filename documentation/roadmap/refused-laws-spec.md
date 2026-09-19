# A refused law is lifted as a row moving, at every carrier

## Status

Not refined yet. The refused half of `curios/src/tests/laws.rs` was read row by row against the tree, and what that found is below: one recorded reason describes a run time the emitter no longer has, one row is refused for a reason other than the one written beside it, and the refused half is itself incomplete by the grid's own rule. Every row has a mechanism within reach and at least one decision still open. Nothing is started. The rows are independent of one another, so this file is a set of lifts under one acceptance bar and not a sequence.

## Why it exists

The grid keeps its refused rows "so the refused set is a record rather than a rumor, and so that taking one later is a row moving, not a test appearing". That record is what gets quoted when someone asks where conversion's algebra ends, so a reason that has gone stale is a wrong answer given with the project's own authority — and two of the rows are marked "not one to take", one on a ground the tree no longer supports and one on no ground that makes it inadmissible.

Each lift is an equation beneath both checkers. `reduce::intrinsic` and `spine` are `curios-core`'s, so a wrong `Equal` admits by [the perimeter's shared route](../design/language/the-soundness-perimeter.md) and a wrong `Clash` by the vacuous-elimination route [Intrinsic fold laws and the free-monoid peel](../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md) records. A lift is therefore priced here as an addition to the trusted base, with the evidence it owes, and never as a convenience.

## What was found

Taken **2026-09-19** at `c1fc7d16`, through the line the grid itself reads: each claim stated as a written goal, held when the report carries `? ≈ Eq/refl()` and refused when it does not.

| Row | Today | Recorded reason | Does the reason stand |
| --- | --- | --- | --- |
| `Eq(x * 2 + 1 == y * 2, false)` | refused | "Parity: not a law of any monoid here, and not one to take" | True as far as it goes: it is a law of divisibility, which `compare_nat` does not read. Nothing makes it inadmissible |
| `Eq(Nat/shl(x, 1), x * 2)` | refused | "true on the unbounded ℕ the type level folds, false on the truncating carrier the runtime imposes" | **No.** See below |
| `Eq(Bool/not(b && c), Bool/not(b) \|\| Bool/not(c))` and `Eq(b \|\| (b && c), b)` | refused | "need a normal form past the leaf set" | Yes for a *rewriting* normal form; a comparison-time decision needs none |
| `Eq(List/map(xs, (v) => v + 0), xs)` | refused | "Function extensionality in disguise" | **No.** See below |
| `Eq(List/get(…List/slice(xs, s, l)…, 0), List/get(xs, s))` | refused | a bound on the base no term in hand proves, which a reducer may not invent | Yes for a *reduction*; a comparison builds no term and so needs no bound |

**The shift's reason describes a carrier the run time does not have.** `curios-emit`'s `emit_shift_left` widens to 64 bits, clamps the count through `emit_clamped_shift`, and refuses with `Panic::NatCarrier` when the shifted value leaves the i31 envelope; it has since `9be0e7f6` (2026-08-21), one day before the reason was written into `nat_shift_laws` by `36a299ad`. [Numeric carriers narrow by refusing, never by changing a value](../design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md) decides the same thing in general: "Core is the oracle: an erased stage must produce Core's value, decline to produce one, or refuse — never a third value." Under that decision no law true on ℕ can be false at run time; the two sides of this one refuse exactly when `2ᵏ · x` leaves the carrier. The reason is written in three places — the row's comment, `nat_shift_laws`' doc in `curios-core/src/reduce/intrinsic/laws.rs`, and the last sentence of the `Bool` paragraph of [Open fold laws and the sum normal form](../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md). What *does* stand against an unguarded rule is the one the bounds oracle states: a left shift is the fold whose result size its operands do not bound.

**The map row is a syntactic test, not extensionality.** `Eq(@(Nat) -> Nat, (v: Nat) => v + 0, (v: Nat) => v)` is held, and so is `Eq(List/map(xs, (v) => v), xs)`. Their composition is refused, and so is `Eq(List/map(xs, (v) => v + 0), List/map(xs, (v) => v))`. The cause is `is_identity` in `curios-core/src/reduce/intrinsic.rs`: one binder whose body *is* that binder, tested on the lambda as `reducer.reduce` left it, which is weak-head and so never looks under the binder. A function that is the identity only pointwise — `(v) => match v | 0 => 0 | k + 1 => k + 1 end` — is the extensionality case, and it is not the row the grid states.

**The refused half is incomplete by the grid's own rule.** "State a row at every carrier, and state it first." Refused today and stated nowhere: the window row at `Bytes` and `Bits`; its general position, `get(slice(xs, s, l), i) = get(xs, s + i)`; a window through a window, `slice(slice(xs, s, l), t, m) = slice(xs, s + t, m)`, which the open-fold entry says is "recorded as such in `tests::laws`" and is not; parity at `!=` and at `Int`; the shift at `Int`; the duals of De Morgan and absorption, and distribution of `&&` over `||`; and map fusion, `map(map(xs, f), g) = map(xs, (v) => g(f(v)))`.

**One perimeter sentence contradicts the code in the other direction.** The open-fold entry says "`map(xs, (v) => v) = xs` is declined: it is function extensionality". The fold takes it, the grid holds it, and the compiler closes it by `refl`.

## The acceptance every lift shares

A row is lifted when all of these hold, and the spec's per-row sections add only what is particular to the row.

- **The rows are stated first, on the refused side, at every carrier the rule will reach** — the twins above and a near-miss control beside each, a claim the lift must *leave* refused. The change that follows is then a refactor with an oracle, and `every_row_is_on_the_side_the_compiler_puts_it` names every row the rule moved, wanted or not.
- **The row moves from `refused` to `held`**, where `every_held_law_closes_by_refl` puts it to both checkers as an `Eq/refl()` proof.
- **The value grid holds it.** A fold law is a shape in `reduce::intrinsic::laws_tests`' `every_open_fold_law_preserves_the_value_at_every_closed_instantiation`, pinned to its stated reduct so a law that stops firing fails. A peel verdict is a shape in that carrier's `every_*_peel_verdict_holds_at_every_closed_instantiation` under the verdict's own obligation, with the tally moved, and the near-miss control beside it.
- **One mutation is run and caught**, and the entry names the mutation and the instantiation that caught it, so it can be run again.
- **Whatever the rule builds is charged before it is built**, in the price list's own units, and a count or a cap is the theory's and never the host's.
- **The rule sits where the two checkers cannot disagree about it**: in `curios-core`, and where it is probe-side, asked for by name in both converters — and in the inverter when it can answer `Clash`.
- **The perimeter entries are corrected in place**: [Open fold laws and the sum normal form](../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md) for a fold, [Intrinsic fold laws and the free-monoid peel](../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md) for a verdict, and [The bounds oracle and the division family](../soundness/per-term-rules/the-bounds-oracle-and-the-division-family.md) where a row touches it.

## Parity: a verdict the comparison cannot spell

`compare_nat` normalizes, cancels, and reads floors, bounds and dominators. After cancellation each side is a floor over literal coefficients of monomials. Let `g` be the gcd of every symbolic coefficient on both sides: every monomial is a natural, so the symbolic parts are `0` modulo `g`, and two sides whose floors differ modulo `g` are equal at no instantiation. That is the first step of the omega test and the whole of what this row needs. An opaque summand has coefficient `1`, so the test goes quiet exactly where it has nothing to say.

`Comparison` cannot carry the answer. Read as the set of orderings the operands may take, its variants are `{<}`, `{=}`, `{>}`, `{<, =}`, `{>, =}` and everything; the missing non-empty subset is `{<, >}` — forced unequal, order undecided. The verdict is that seventh subset, `==` reads it as `false` and `!=` as `true`, `<` and `<=` read it as undecided, and `dominated` passes it by. It composes with what is already there by intersection: a forced `<=` that is also forced unequal is `<`.

- **Carriers:** `Nat` and `Int` — `compare_int` reads the same residuals through `int_cancel_common`, and the argument needs integers, not naturals.
- **Controls that must stay refused:** `Eq(x * 2 == y * 2 + 2, false)`, satisfiable at `x = y + 1`; `Eq(x * 2 + 1 == y * 3, false)`, where `g` is `1`.
- **To decide: whether the peel reads it as `Clash`.** Conversion needs only "not `Equal`" and is unchanged either way. A `Clash` from `classify_nat` reaches inversion as *impossible*, so `match h end` would close a hypothesis `Eq(x * 2 + 1, y * 2)` with no arm — useful, and the second admission route. It is a row of its own with [Coverage](../soundness/per-term-rules/coverage.md)'s evidence behind it, not a by-product of this one.

## A shift by a literal count is a coefficient

`Nat/shl(x, k)` with `k` a literal is `2ᵏ · x`, so it enters the sum normal form through `Nat::scaled` and every row the form already holds applies to it. `reduce_nat_shl` has the reducer in hand: the coefficient is charged under `shift_bound` before it is built, and a symbolic count, or one past `u64`, declines as it does today. `nat_bound` needs no arm — a shift that reduced to a product is bounded by the `NatMul` arm, with the allocation already paid where there was a budget to pay it — so the oracle's refusal of `NatShl` stands for the symbolic count it is actually about.

- **Carriers:** `Nat` and `Int`; `2ᵏ · i` holds below zero.
- **What may move is a refusal, never a value.** A term the elaborator solves by unification can reach the emitted program in its reduced spelling, so `2ᵏ · x` may run where `shl(x, k)` was written. Both refuse when the product leaves the carrier. Whether they refuse alike at `x = 0` with `k` past the envelope — where the shift answers `0` and the product must first materialize `2ᵏ` — is not checked, and is the same question every held law that deletes an operand already raises.
- **`shr` by a literal could join the division family**, `⌊x / 2ᵏ⌋`, by handing the dividend to `nat_euclid_split` directly. It must not build a `NatDiv` node: that node carries a `non_zero` proof and a reducer may not invent one. Its own rows, `Eq(Nat/shr(x * 4, 2), x)` first.

## De Morgan and absorption: decided at the comparison, over the atoms

The recorded reason assumes the answer is a normal form. [A stuck comparison is spelled one way](../design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md) already rejected respelling at the fold — a guard's refinement is keyed on its written spelling — and `peel_bool`'s own doc records the `&&`/`||` cliff a whole-tree normalization paid. Both objections are to *rewriting*. Neither touches a decision made where two trees are compared.

- **(a) A capped truth table, probe-side.** Force every leaf through a mixed tree of `&&`, `||`, `xor` and `Bool` equality — `normalize_bool` generalized past one connective — align comparison atoms through `dual_comparison` so `x < y` and `y <= x` are one atom and its negation, and evaluate both sides at every assignment, charged `2ⁿ` times the tree before the first. Agreement everywhere is `Equal`; anything else is `Stuck`. Sound because treating atoms as independent *over*-approximates the values they take together: agreement at every assignment holds at the real ones, whatever correlates `x < y` with `x < y + 1`. For the same reason disagreement proves nothing, so never `Clash`.
- **(b) A reduced ordered BDD** over atoms in the structural-hash order monomials use. Canonical, and as a fold it is the rewriting both records reject; probe-side it is (a) with a different inner loop, under the same cap.
- **(c) Algebraic normal form over GF(2).** Canonical and exponential where the common case is not: an `n`-way `||` is `2ⁿ - 1` monomials.

(a) is the one that fits both records. **To decide:** the cap, which is a theory constant; and whether a tautology should *reduce* to `true`. A comparison-time rule closes `Eq(e, true)` by `refl`, and `Bool/holds_of_eq` turns that into `Bool/Holds(e)` in one written step, but an obligation the elaborator fills by reduction is not filled — that would take the decision into the fold, where the cliff is.

## A map by a function convertible to the identity

The test is the defect, so the lift is the test. Open the binder, weak-head reduce the body, and ask whether what is left is the binder: `(v) => v + 0` and `(v) => ((w) => w)(v)` pass, the pointwise `match` stays stuck and stays refused — and becomes the row that carries the extensionality comment, which belongs to it.

- **Fold-side or probe-side.** Fold-side, `map(xs, (v) => v + 0)` *reduces* to `xs`, so `len`, `get` and every later fold see through it; it would be the first fold in `reduce::intrinsic` to open a binder — none does today — and the `Reducer` trait offers `reduce`, `reduce_forced` and `spend`, no fresh variable and no conversion. Probe-side, in both converters' intrinsic congruence, going under a binder is everyday work and the two sides merely compare equal. **To decide**, on what opening a binder inside a fold costs the trait.
- **Carrier:** `List` alone has `map`.

## A position inside a window

`get(slice(xs, s, l, ok), i, inside)` is `xs[s + i]`, and the proposition a rewritten node would owe — `s + i < len(xs)` — follows from `ok` and `inside` by transitivity and is convertible with neither. Window fusion and the last-operand law hand a proof on because cancellation makes the propositions *one*; here there is a derivation to perform, which is why the row is stuck.

- **(a) A comparison-time rule in the peel.** Read each stuck `get` through its windows to a root base and an absolute position, and each stuck `slice` to a root base, an absolute start and a count; two are `Equal` when the bases are one and `nat_equal` decides the positions one. It builds no term and so owes no proof — the bound is never compared, which is the line `Atom::Window`'s `within` already draws. `Stuck` otherwise, never `Clash`.
- **(b) A reduction that constructs the bound** from `ok` and `inside` through a `/std` lemma reached by the `SyntaxRegistry`. No axiom, and the node normalizes, so later folds see through the window. Its price is that the trusted reducer's output would name a library declaration: the rule cannot fire where the registry slot is unfilled, and the standard library compiling itself is such a place.

(a) is in the peel's existing character — `peel_symmetric` and `peel_bool` also equate what reduction leaves apart. **To decide**, on whether anything needs the window *reduced* rather than equated.

- **Carriers:** `List`, `Bytes`, `Bits`, at the first position, the general one, and a window through a window.

## Adjacent items that are not this one

- **Hypotheses feeding the algebra.** With no hypothesis in scope `compare_nat` is already complete for `<` and `<=` — a side still holding a symbol is unbounded — so beyond the gcd test what an omega-style procedure adds is hypotheses. [A bound is stated in a decided proposition and discharged by reduction](../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md) parks it as "a larger and separate capability" and [A closed term evaluates at an interpreter's speed](faster-conversion-oracle-spec.md) says it wants its own file. The smallest step that way is `nat_dominators` reading a guard in scope, which puts a hypothesis inside conversion and owes the metatheory that move has always owed. The route that fits [Proof automation writes terms into the source and is never trusted](proof-automation-spec.md) is a reflected checker with a certificate.
- **Summands paired up to conversion.** `Eq(f(a + b) + g(c + d), g(d + c) + f(b + a))` is refused while `Eq(f(a + b), f(b + a))` and commutation each hold: `Nat::cancel_common` pairs summands by structural identity, the incompleteness falling on the declining side by design. It is the same family as the map row — a syntactic test where conversion is what is meant — and a larger change, since `cancel_common` takes no reducer and its stability contract is what keeps the peel terminating.
- **Fusion.** `map` after `map`, and `fold` after `map`. Stated here only as refused rows to add.

## What has to be decided

- Whether parity reaches inversion as a `Clash`, or stays a fold verdict.
- Whether a Boolean tautology reduces, or only compares; and the cap.
- Whether a fold may open a binder, which settles the map row's side.
- Whether a window is equated or reduced.
- Whether the missing refused rows are stated as one change ahead of every lift, or each with its own.

## Deliberately not specified

The value of the truth table's cap and the unit it is charged in. `Int/shr`'s law, which depends on whether the shift floors where the division truncates. The order of the lifts: they share no code beyond `Comparison`'s readers, and the shift is the smallest.

## The seam it comes back through

`curios/src/tests/laws.rs` for every row. `curios-core/src/reduce/intrinsic/compare.rs` with the eight readers of `Comparison` in `reduce/intrinsic.rs` — `==`, `!=`, `<` and `<=` at each of `Nat` and `Int` — and `compare_int` in `reduce/intrinsic/int.rs` for parity; `reduce_nat_shl`, `reduce_int_shift`, `nat_shift_laws` and `cost.rs`'s `shift_bound` for the shift; `normalize_bool`, `align_comparisons`, `dual_comparison` and `spine.rs`'s `peel_bool` for the Boolean rows; `is_identity` and the `ListMap` arm for the map row; `spine.rs`'s `Atom::Window`, `peel_bin` and `peel_list` for the window rows. Both converters' intrinsic congruences — `curios-cert/src/kernel/convert/intrinsic.rs` and `curios-elab/src/convert/intrinsic.rs` — and `curios-analysis/src/invert.rs` for anything probe-side. `reduce::intrinsic::laws_tests`, `compare_tests` and `spine`'s tests for the value grids.

## How to retake the measurements

```sh
cargo run --package curios -- wonder diagnostics - <<'CRS' | grep -E '^ +[0-9]+ \| let|≈ Eq/refl'
use /std/{Nat, Int, Bool, List, Eq, Io};

let parity(x: Nat, y: Nat) -> Eq(x * 2 + 1 == y * 2, false) = ?;
let shift(x: Nat) -> Eq(Nat/shl(x, 1), x * 2) = ?;
let absorb(b: Bool, c: Bool) -> Eq(b || (b && c), b) = ?;
let mapped(xs: List(Nat)) -> Eq(List/map(xs, (v) => v + 0), xs) = ?;
let functions: Eq(@(Nat) -> Nat, (v: Nat) => v + 0, (v: Nat) => v) = ?;
let identity(xs: List(Nat)) -> Eq(List/map(xs, (v) => v), xs) = ?;
Io/pure(())
CRS
```

A report prints its candidates above the source line it belongs to, so a `? ≈ Eq/refl()` line marks the `let` that *follows* it as held, and the bare listing after the last report repeats every line without them. `functions` and `identity` are held and the other four are not; the grid's own binders state the window rows.
