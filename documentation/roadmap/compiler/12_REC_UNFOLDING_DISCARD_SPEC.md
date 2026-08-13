# The unfolding discard decides on shape where it means to decide on progress

`force_rec` grants a folded recursive call one definitional unfolding, and then throws the result away if it does not like how it looks:

```rust
other => {
    return Ok(match other {
        Subterm::Match(_) | Subterm::Var(_) | Subterm::Metavar(_) | Subterm::Proj(_) => folded,
        value => value.into(),
    });
}
```

The clause is load-bearing and its intent is right. Without it a recursive function applied to a symbolic argument grows one more copy of its own body at every demand and never reaches a normal form — the unfold-and-restuck cycle. What it actually tests, though, is the *head constructor of the reduct*, and that conflates two different situations: an unfolding that made no progress, and one that finished at an answer which happens to be a variable.

So `go(0, acc)` reduces correctly to `acc` — and `acc` is discarded, because it is a `Var`.

## The evidence

A minimal pair, identical in signature and in `match`, differing only in what the base arm returns:

| Base arm | Head of the reduct | Verdict |
| --- | --- | --- |
| `\| 0 => 5` | `Intrinsic` | compiles |
| `\| 0 => n` | `Var` | refused |

Both state the same goal shape, `Nat/Le(<base>, f(0, n))`, discharged by `Nat/Le/refl`. The second reports `inferred: True / expected: Bool.match n <= rec #0: … #0(0, n)` with the application left folded.

Both halves are fixtured, in `curios/src/tests/numeric.rs`: `a_bound_over_a_recursion_returning_a_literal_discharges` is the control and runs; `a_bound_over_a_recursion_returning_a_parameter_discharges` is the reproducer and is `#[ignore]`d against this specification, having been confirmed to fail with exactly the diagnostic above. It is the acceptance check — it compiling, with the control still compiling, is what M1 has to achieve. Note that recursion is spelled `rec`, not `let`; a `let` that names itself is an unbound variable, which is what "a bound mentioning a program-local `rec`" means literally.

The prelude contains the working side of that pair already, and by the same mechanism rather than by coincidence. `/std/Str/utf8`'s `drop_width` returns `0` in its empty arm and `1 + drop_width(…)` in its recursive ones — an `Intrinsic` head in every case — which is why `drop_width_within` can state `Nat/Le(drop_width(n, s, b), Bytes/len(b))` and have its arms discharged outright. That function was recorded as evidence the defect was not universal; it is better read as evidence for exactly this rule.

## What the clause decides, measured

Instrumenting the arm and building the fixed prelude once — 613,610 decisions in a single `cargo build -p curios-prelude`, counted by an `eprintln!` per arm in `curios-elab`'s `force_rec` and read out of the build script's captured `stderr`:

| Reduct head | Count | Verdict today |
| --- | --- | --- |
| `Intrinsic` | 318,357 | keep |
| catch-all (`Func`, `Tuple`, …) | 254,756 | keep |
| `Var` | 16,919 | **discard** |
| `Variant` | 6,472 | keep |
| `Struct` | 5,725 | keep |
| `Match` | 5,703 | discard |
| `Metavar` | 4,948 | discard |
| `Proj` | 730 | discard |

Two things follow, and both bear on M1 rather than on the diagnosis.

**The `Variant` row is productive corecursion, and it is kept today.** A constructor exposed with the recursive call underneath it — `cons(x, go(k, …))` — has a head, so the neutrality test keeps it. M1's candidate rule discards any reduct containing an occurrence of the member, which discards every one of those 6,472. `curios-elab/src/totality.rs` names `/std/Async` as corecursive, so this is a shape the corpus has rather than one it might grow.

**No change here is surgical.** 16,919 `Var` decisions flip in the fixed prelude alone, and each one changes what reduction *returns* — the reduct rather than the folded spelling — which conversion, refinement keys and diagnostics all then see. "The corpus still compiles" is therefore not evidence for a rule change here; M2's argument is.

## Why conversion does not see it

The same unfolding is reached by two paths, and only one of them discards.

- **Reduction** — `reduce_forced` → `force_rec`, with the clause above. The kernel's `force` carries the identical four arms.
- **Conversion** — `curios-elab/src/convert.rs` calls `unfold_rec_apply` directly for an `Apply` with a `rec` head against anything else; the kernel's `unfold_spelling` is the same rule and states its purpose as letting conversion "see the two spellings as one". Neither discards.

That is why `Eq(go(0, acc), acc)` succeeds while `Nat/Le` over the same terms fails: the decided proposition goes through `NatLte` → `compare_nat` → `reduce_forced`, and never reaches conversion. Both `compare_nat` and the `BinLen` arm beneath it *do* force their operands, so the force is not missing — the force is what discards.

## Why it matters

Bounds are stated as decided propositions, and the documented escape hatch when the evaluator cannot reach a fact is to prove it by induction and pass it as `@`. That hatch is open for a bound over prelude functions shaped like `drop_width` and shut for any bound mentioning a program-local `rec` whose base case returns one of its parameters — which is the ordinary way to write an accumulator. It is why the corpus's slicing test could not be fixed inductively.

The refusing direction is the safe one and nothing here is unsound. What is wrong is that the boundary is drawn where nobody chose to draw it.

**And a refusal is not the worst of it.** A bound whose subject is a *computed* value is discharged by evaluating that value, so `Bytes/slice(built, 0, 10)` puts the whole of `built` in a type. The step budget bounds steps and nothing bounds the memory a reduction allocates on the way, so at corpus scale this does not refuse and does not time out — it exhausts the machine, which it did once, measured the hard way. `curios/src/tests/runtime.rs`'s accumulation measurement carries a `head_of` indirection specifically to keep its subject opaque, and `tests::numeric`'s `a_bound_on_a_computed_subject_evaluates_it` / `a_bound_behind_a_parameter_evaluates_nothing` pin the pair under a stated budget through `typecheck_within`. Any fixture in this area states a budget; none of them may be allowed to run to completion.

## Milestones

- **M1 — a rule that tests progress.** Replace the head-shape test with one that asks whether unfolding achieved anything. The candidate this document first proposed — keep the reduct when it contains no occurrence of the member being unfolded — is **wrong as stated**, and the measurement above is why: it discards the 6,472 productive reducts the neutrality test correctly keeps. The two tests are each right about a different kind of progress, and neither is the rule alone. *Keep when the reduct has a head constructor **or** mentions no member of the **group**; discard only when it is still neutral **and** still mentions the group* — which is exactly "we unfolded and got another folded call back", the restuck case the clause exists for. Scoping occurrence to the group rather than to the member is what closes the mutual-group hole named below. Note that this is the current rule with a disjunct *added*, so it can only accept more, never less — which is also what makes M2 tractable. *Acceptance:* both rows of the fixtured pair compile, the control still compiles, and the `#[ignore]` comes off the reproducer.
  - *An obstacle this document previously understated.* `force_rec` does not have the group in scope where it decides: it holds `folded` and the reduct, while `(group, index)` is extracted inside `unfold_rec_apply` and dropped before it returns. Testing "mentions no member of the group" therefore needs the group threaded back out or re-derived from `folded` at each decision — on a path taken 613,610 times in one prelude build. This is not a one-clause swap.
- **M2 — the termination argument, written before the edit and not after.** The cycle this clause prevents is real, and the induction-hypothesis shape is the case it was most likely written for: an arm's `ih` is a raw stuck fold-match on the same argument. *Acceptance:* an argument that every unfolding either strictly consumes a redex or is discarded, stated in the module that owns the clause — `curios-elab`'s `force_rec` and `curios-cert`'s `force`, separately, for M3's reason — plus a fixture that spins under the old rule and terminates under the new one, or a recorded reason no such fixture exists. A fixture that *spins* is exactly the shape that took a machine down, so build it through `typecheck_within`: state a budget, assert exhaustion under the old rule and success under the new one. That is also the only honest form of the claim, since the difference being pinned is whether the reduction terminates, not how long it takes.
- **M3 — both copies, independently.** `curios-elab`'s `force_rec` and `curios-cert`'s `force` carry the identical clause, and the crate boundary exists so that the two are written separately and disagree when one is wrong. *Acceptance:* both changed, neither by extracting a shared helper; the differential between them still able to catch a mistake in either.

## What must not happen

**Do not delete the clause.** Removing it trades a refusal for a hang, which is strictly worse: a refusal names a term, and a reducer that stops terminating shows up as a prelude build that never finishes.

**Do not fix it in one crate.** The duplication is the design. A shared helper would make a bug in the rule invisible to the only mechanism that could catch it, which is the same trade `whnf.rs`'s module documentation refuses for the reduction strategy as a whole.

**The prelude is the referee, and it answers slowly.** 1,106 items are re-elaborated and re-certified on any change to either copy, so a non-terminating rule presents as a build that hangs rather than as a test that fails. Budget for that when iterating.

## Open, and deliberately not decided here

Whether "contains no occurrence of the member" is the right progress test at all. It is the most promising candidate and it is not the only one — comparing the reduct against the folded spelling, or requiring a strict decrease in some measure, are the obvious alternatives, and each has a different failure mode on mutual groups where unfolding one member exposes another. M2 is where that gets settled, on the strength of the termination argument rather than on which one makes the reproducer pass.
