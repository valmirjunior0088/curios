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

The prelude contains the working side of that pair already, and by the same mechanism rather than by coincidence. `/std/Str/utf8`'s `drop_width` returns `0` in its empty arm and `1 + drop_width(…)` in its recursive ones — an `Intrinsic` head in every case — which is why `drop_width_within` can state `Nat/Le(drop_width(n, s, b), Bytes/len(b))` and have its arms discharged outright. That function was recorded as evidence the defect was not universal; it is better read as evidence for exactly this rule.

## Why conversion does not see it

The same unfolding is reached by two paths, and only one of them discards.

- **Reduction** — `reduce_forced` → `force_rec`, with the clause above. The kernel's `force` carries the identical four arms.
- **Conversion** — `curios-elab/src/convert.rs` calls `unfold_rec_apply` directly for an `Apply` with a `rec` head against anything else; the kernel's `unfold_spelling` is the same rule and states its purpose as letting conversion "see the two spellings as one". Neither discards.

That is why `Eq(go(0, acc), acc)` succeeds while `Nat/Le` over the same terms fails: the decided proposition goes through `NatLte` → `compare_nat` → `reduce_forced`, and never reaches conversion. Both `compare_nat` and the `BinLen` arm beneath it *do* force their operands, so the force is not missing — the force is what discards.

## Why it matters

Bounds are stated as decided propositions, and the documented escape hatch when the evaluator cannot reach a fact is to prove it by induction and pass it as `@`. That hatch is open for a bound over prelude functions shaped like `drop_width` and shut for any bound mentioning a program-local `rec` whose base case returns one of its parameters — which is the ordinary way to write an accumulator. It is why the corpus's slicing test could not be fixed inductively.

The refusing direction is the safe one and nothing here is unsound. What is wrong is that the boundary is drawn where nobody chose to draw it.

## Milestones

- **M1 — a rule that tests progress.** Replace the head-shape test with one that asks whether unfolding achieved anything. The candidate: keep the reduct when it contains **no occurrence of the member being unfolded**, discard otherwise. `acc` mentions no `go` and is finished; a restuck `go(k, …)` mentions `go` and is not. *Acceptance:* the pair above compiles on both rows, and the reproducer in the task record compiles.
- **M2 — the termination argument, written before the edit and not after.** The cycle this clause prevents is real, and the induction-hypothesis shape is the case it was most likely written for: an arm's `ih` is a raw stuck fold-match on the same argument. *Acceptance:* an argument that every unfolding either strictly consumes a redex or is discarded, stated in the module that owns the clause, plus a fixture that spins under the old rule and terminates under the new one — or a recorded reason no such fixture exists.
- **M3 — both copies, independently.** `curios-elab`'s `force_rec` and `curios-cert`'s `force` carry the identical clause, and the crate boundary exists so that the two are written separately and disagree when one is wrong. *Acceptance:* both changed, neither by extracting a shared helper; the differential between them still able to catch a mistake in either.

## What must not happen

**Do not delete the clause.** Removing it trades a refusal for a hang, which is strictly worse: a refusal names a term, and a reducer that stops terminating shows up as a prelude build that never finishes.

**Do not fix it in one crate.** The duplication is the design. A shared helper would make a bug in the rule invisible to the only mechanism that could catch it, which is the same trade `whnf.rs`'s module documentation refuses for the reduction strategy as a whole.

**The prelude is the referee, and it answers slowly.** 1,106 items are re-elaborated and re-certified on any change to either copy, so a non-terminating rule presents as a build that hangs rather than as a test that fails. Budget for that when iterating.

## Open, and deliberately not decided here

Whether "contains no occurrence of the member" is the right progress test at all. It is the most promising candidate and it is not the only one — comparing the reduct against the folded spelling, or requiring a strict decrease in some measure, are the obvious alternatives, and each has a different failure mode on mutual groups where unfolding one member exposes another. M2 is where that gets settled, on the strength of the termination argument rather than on which one makes the reproducer pass.
