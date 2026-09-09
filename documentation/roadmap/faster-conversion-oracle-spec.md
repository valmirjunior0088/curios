# A closed term evaluates at an interpreter's speed

## Status

Not refined yet. The ceiling is measured and the space is unusually well mapped already — [Evaluating a closed term is representation, not judgment](../design/toolchain/evaluating-a-closed-term-is-representation-not-judgment.md) built the closed machine and rejected four alternatives on the way, so what remains open is narrower than it looks. What is *not* done is the measurement that picks between the survivors: two probes taken for this file disagree about where the time goes, and the profiler has not been run. Nothing is started, and no option below is preferred.

## Why it exists

A proposition proved by reduction is priced by the checker's evaluator, so the reach of proof-by-computation is set by an interpreter's constant factor rather than by the claim. [A bound is stated in a decided proposition and discharged by reduction](../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md) already meets that ceiling in production — "a bound over a *computed* value runs that computation at elaboration time, and the step budget is what decides whether it finishes" — and [The soundness perimeter](../design/language/the-soundness-perimeter.md) states the consequence as a boundary: *well-typed* is budget-relative. Every proof this ceiling refuses is one an author must instead write as a lemma, which is often right and is sometimes only cheaper for the checker.

What lifts the ceiling lifts it for a class, not a spelling: the same evaluator serves the bounds oracle, the decided propositions the elaborator fills, conversion at every goal, and the fixed prelude's own certification, which is the bulk of what `cargo clippy --workspace` spends.

## What was measured

Taken **2026-09-08** at `3d043492`, with `target/release/curios` — a debug binary is four to eight times slower and is not evidence about the evaluator.

A right fold over `Nat` proved by `refl`, so that conversion must run the whole fold:

| `N` | wall | work over the 1.22 s floor | per iteration |
| --- | --- | --- | --- |
| 0 | 1.22 s | — | — |
| 20 000 | 4.30 s | 3.08 s | 154 µs |
| 40 000 | 7.51 s | 6.29 s | 157 µs |
| 80 000 | 11.22 s | 10.00 s | 125 µs |

Linear, and no exhaustion at 80 000. An iteration is on the order of ten to twenty machine transitions, which puts a transition near 10 µs against the 50–200 ns a tight interpreter spends. **That gap — about two orders of magnitude — is the whole of what this item is about.** It is a gap against interpreters generally, not against a compiled evaluator, so closing it does not require one.

The second probe tested the obvious culprit and did not confirm it. Binders are opened by substitution — `Scope::open` calls `release` (`curios-core/src/scope.rs:204`), a full `traverse` that rebuilds the body — so an arm body is rebuilt whether or not it is demanded. Padding the recursive arm with a branch that is never evaluated, at `N` = 20 000:

| dead branch mentioning the binder | work |
| --- | --- |
| absent | 5.93 s |
| ~80 nodes | 10.47 s |
| ~240 nodes | 11.51 s |

Substituting into unevaluated code costs real time, and the cost does **not** scale with the branch's size. `Visit::pruning` (`curios-core/src/scope.rs:1107`) shares a subtree with no loose bound variable in O(1) off the cached `reach`, and `Term` is `Rc`-shared with cached hash, `reach` and containment bits, so equality short-circuits on pointer identity (`curios-core/src/term.rs:52`). Substitution is therefore *a* cost and is not established as *the* cost. Anyone picking this up should profile before choosing.

## What is already built

- **The closed machine** (`curios-core/src/machine.rs`) evaluates a closed, metavariable-free term on an explicit frame stack, shared by both checkers, with substituted terms taken to values first and a run-scoped value memo. Its gate is `accelerable` (`curios-core/src/machine.rs:54`) plus a host-side no-refinements-in-scope condition. The perimeter entry is [The closed machine](../soundness/per-term-rules/the-closed-machine.md).
- **Three memo tables and a retention quota.** `curios-cert/src/kernel/memos.rs` holds a name-keyed unfold table that survives the module and two term-keyed tables cleared at each declaration boundary; `curios-core/src/retention.rs` bounds what may outlive the budget that built it. The cheap wins of this shape are taken.
- **A construction-priced budget.** `curios-core/src/cost.rs` charges what a step *builds*, in units no target can move, saturating into refusal. This is what makes exhaustion a fact about the program rather than about the host.
- **Two complete recursive strategies** beside the machine: `curios-cert/src/kernel/whnf.rs` and `curios-elab/src/reduce.rs`, written separately on purpose.

## What the closed-machine decision already rejected

Restated so none of it is re-walked. Grounds are that decision's, not this file's.

- **`native_compute`** — compiling closed terms through the back end. Structurally closed, not merely expensive: `curios-js` depends on `curios-pipeline`, which depends on `curios-cert`, so the kernel runs on `wasm32` in the browser where there is no Wasmtime. It would also make acceptance depend on the whole back end.
- **Per-call-site strategies**, Rocq's `cbv`/`lazy`/`vm_compute`/`native_compute` surface. "The machine is not a strategy anyone chooses; it is what evaluating a closed term is."
- **A shape-recognized accelerator** shadowing authored folds — syntactic where closedness is semantic, and one trusted-base entry per recognized shape.
- **Blessing the types that hurt**, Lean's kernel-level `Str`. Fixes one carrier and leaves every user fold where it was.

## What constrains any answer

- **It must sit below both checkers, or be written twice.** `recheck_module_verdicts` runs the kernel inside `compile_entrypoint`, so an elaborator that got faster alone would still be gated by the kernel at the old speed. `curios-core` is the only crate both reach.
- **No back end, and no host dependence.** Pure Rust that compiles to `wasm32`, per the browser product above.
- **The price list is a contract.** The budget's determinism — "the same program spends the same units on every machine" — is what makes exhaustion a property of the program. An accelerator that builds less must either charge as if it had, or move the price list deliberately and re-set `DEFAULT_STEP_BUDGET` and every cost fixture with it.
- **Only cost may move, and only earlier.** The closed machine's licence is that acceptance may change at an exhaustion point and nowhere else; a value must never differ.
- **It owes a differential.** A defect in a component both checkers share is the one kind neither can catch, so `curios-cert/src/kernel/whnf/closed_machine_tests.rs`'s `the_closed_machine_agrees_with_the_strategy` is the pattern any new tier must extend, at both demands.

## The options that survive

None is preferred here, and the first two are not exclusive — the second contains the first.

- **An environment machine.** Carry `env: Vec<Value>` and close over bodies instead of rebuilding them; `Var` becomes an index resolved when reached, and an unreached node is never touched. This is Agda's call-by-need environment machine and Rocq's `cClosure`, and it is the road the closed machine stopped one step short of. It needs a readback to hand conversion a `Term`, which re-allocates once per query rather than once per step. Its real bill is the price list: opening currently charges what it builds, and under an environment it builds nothing.
- **A bytecode tier.** Compile a closed definition body once into a flat instruction array, cache it beside the unfold table, and run it on the existing frame stack — Rocq's `vm_compute` without the part that was rejected, since what that rejection names is the *back end*. Removes per-step term dispatch and `Rc` traffic, stays in `curios-core`, runs in the browser. Costs a second representation inside the trusted base, its own differential, and a charge for the compile step itself.
- **Constant factors in the machine as it stands.** Arena or interned nodes, memo-insert charges off the hot path, a specialized successor arm. Keeps the price list nearly still and is the only option with no perimeter consequence, but the representation is already carefully shared, so the ceiling is low and should be measured before it is believed.

## Two adjacent items that are not this one

- **A general arithmetic decision procedure.** [A bound is stated in a decided proposition and discharged by reduction](../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md) parks it: "an `omega` equivalent is a larger and separate capability". It takes a class of obligations to zero rather than making them cheaper, which for the obligations the standard library actually generates may be worth more than any speedup here — and it does nothing for reflection over data. It wants its own file.
- **Persisting a closed evaluation across builds.** A closed term's weak-head reduct is a pure function of `(term, definitions)`, which is the memo's own argument, and [Cached verdicts](../soundness/admission-without-judgment/cached-verdicts.md) is the precedent for believing a result instead of re-deciding it — including its statement that the budget is deliberately not part of the key. Recorded as a question rather than an option, because the unit-level cache already delivers most of it: an invocation whose sources are unchanged recompiles nothing, so a costly `refl` is paid once per edit, not once per build.

## What has to be decided

- **Where the time actually goes.** The two probes above disagree, and `cargo x profile` has not been run against one of them. Measuring before designing is the rule this roadmap enforces on itself, and here it decides between an option with a perimeter consequence and one without.
- **Whether the price list moves.** An evaluator that stops building must either keep charging for construction it no longer performs — honest about the budget, dishonest about the machine — or move the list and accept that every cost fixture and the shipped default move with it. This is the single decision the rest depends on.
- **Whether the gate widens.** `accelerable` declines a term with any local free. A fold under a binder is exactly what a proof about *all* `n` looks like, so the acceleration currently reaches closed computations and not general lemmas. Whether an environment machine could serve open terms too, and what the no-refinements condition becomes there, is unasked.
- **What a second representation costs the perimeter.** A bytecode tier adds a translation whose defects are invisible to both strategies. Whether its differential can be as strong as the machine's — the same terms, both demands — or whether it needs a decompiler to be checkable at all.
- **Whether readback is affordable.** An environment machine's value must become a `Term` for conversion to compare. Conversion asks at every goal, so a readback that is cheap per query but frequent may give back what the machine saved.
- **Whether any of this is worth it before a consumer asks.** No standard-library proof is currently blocked on this ceiling; the bounds mechanism routes around it by keeping subjects opaque. A measured case where an author had to write a lemma *because* of the constant factor would turn this from an optimization into a capability, and there is none recorded.

## Deliberately not specified

The instruction set of any bytecode, and whether it is closure-converted. The unit the new price list would be denominated in. Whether the machine's run-scoped memo survives into an environment machine or is subsumed by sharing. Whether the elaborator's strategy and the kernel's stay separate under a shared faster tier — the duplication argument is about judgment and a faster tier is representation, but the population of terms taking each path would change, and that is the [independent kernel](../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) decision's to re-read.

## The seam it comes back through

`curios-core/src/machine.rs` and its `ClosedHost` trait, which already names everything a faster tier would need from a host; `curios-core/src/cost.rs` for whatever the price list becomes; `curios-core/src/scope.rs`'s `release` and `Visit::pruning`, which are what an environment replaces; and the two hosts' entry points, `curios-cert/src/kernel/whnf.rs`'s `machine_admissible` and its counterpart in `curios-elab/src/reduce.rs`. The fixtures that must move with it are `curios-cert/src/kernel/whnf/closed_machine_tests.rs` for agreement and `curios`' `str_literal_cost_measurements` and `kernel_memo_charge_measurements` (`curios/src/tests/reduction.rs`) for cost parity between the checkers.

## How to retake the measurements

```sh
cargo build --release --package curios
N=40000; S=$(( N * (N + 1) / 2 ))
time ./target/release/curios wonder diagnostics - <<CRS
use /std/{Nat, Eq, print};

let _sum_to(n: Nat) -> Nat =
    match n | 0 => 0 | m + 1 => (m + 1) + _sum_to(m) end;

let _pricey: Eq(_sum_to($N), $S) = Eq/refl();

print("ok\n")
CRS
```

Take `N` = 0 first: that is the prelude-restore floor to subtract, 1.22 s on the machine above. For the substitution probe, replace the recursive arm with `match false | true => <term mentioning m> | false => (m + 1) + _sum_to(m) end` and grow the dead branch. Run every probe under a memory cap — an unbounded one has taken a machine down.
