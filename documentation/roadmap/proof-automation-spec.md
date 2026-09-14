# Proof automation writes terms into the source and is never trusted

## Status

Measured and phased, not built. Two audits taken on 2026-09-14 at `7bed2216`, against `target/release/curios`, established what the compiler already offers a proof-writing agent, where the story it rests on breaks, and in what order the breaks cost. Their findings are recorded here rather than left in conversation. Phase 0 is specified to the point of one experiment and is not built; every later phase names the specification or entry that owns its mechanism and adds only what this story needs from it. Nothing here designs a search: a premise index, a bounded-depth fill and an inhabitation search were assessed and are deliberately not specified, and the loop's failure data under [What was measured](#what-was-measured) is what would justify or refute them.

When a phase lands, its durable contract goes to the owner named in that phase — the closed machine's perimeter entry, `curios-wonder`'s rustdoc and [usage.md](../usage.md), the cached-verdicts entry, the standard library's own documentation — and the phase leaves its number behind.

## Why it exists

Curios has no tactic language and will have none. A person writes the proposition; a proof is a term, found by whoever finds it — the person, or an agent driving `curios wonder` and the language server — written into the source, checked by both checkers on every compile, and costing nothing at compile time beyond that check. No tier of automation is trusted, because `curios-cert` re-judges every term from the term alone ([An independent kernel re-checks what the elaborator accepts](../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md)), and none is invisible, because the repository compiles with every tool gone.

The thesis this specification holds to is that reflection over `Holds` plus a machine-readable answer from `wonder` is the whole compiler-side content of that story, and that the rest is a soundness fix, a discipline for writing deciders, and a library. It was attacked, and it holds for decision problems over library-defined data at closed instances. It does not reach the corpus the library actually accumulates — universally quantified lemmas, which no decision procedure discharges — and the phases below are ordered by what the measurements said breaks first, not by what the thesis would prefer.

## What was measured

Release build, one process at a time on an idle machine, 2026-09-14 at `7bed2216`. Units are found by bisecting `--budget` and are exact to about one percent; they are machine-independent. A first run of the wall-clock figures overlapped a build on the same machine and was discarded; the 512-bit row is the only figure not retaken.

**Reflection is production, and its price is known.** A `BigNat` product compared against its literal result by `Eq/refl()`, at top level:

| Operands | Wall | Units |
| --- | --- | --- |
| 64-bit | 0.58 s | 0.50 M |
| 128-bit | 1.8 s | 1.67 M |
| 256-bit | 10.0 s | 6.0 M |
| 512-bit | 66 s | under the 30 M default |

A 20 000-iteration `Nat` fold by `Eq/refl()` costs 1.03 s and 3.36 M units. A fuelled Euclid at closed arguments, a restoring division over `Bits` structural in the dividend, a divisibility proof through a certificate and a soundness lemma, and a `Bool`-valued recursion along an `Accessible` proof each check in about 0.2 s. Between 0.3 µs and 1.7 µs per unit across these programs, so units are the exact refusal criterion and only a proxy for time, which is the wall a proof loop hits first.

**The open-term cliff.** The same 128-bit proof, discharged in four places:

| Where `Eq/refl()` is written | Result |
| --- | --- |
| a top-level `let` with a closed type | 1.8 s |
| under an unused binder `(_n: Nat)` | 2.65 s, still accelerated |
| inside `match n \| 0 => … \| _ + 1 => … end` at the default budget | refused after 8.5 s: "conversion ran out of steps" |
| the same arm at `--budget 300000000` | refused after 70 s |
| as its own top-level `let`, referenced by name in both arms | 3.6 s |

At least 180 times the units and it never finishes; the report names the budget, not the cause. What is decided by reduction must therefore be decided outside any match arm, and nothing tells an author so.

**The loop that exists.** Sixteen propositions restated over `/std` in one standalone file, bodies `?`, driven through `wonder diagnostics <file>` by an agent with the sources at hand: fifteen closed at the second turn, one attempt each after the goal turn, both checkers accepting; the sixteenth, `reverse(reverse(xs)) = xs`, closed at the fifth after two auxiliary lemmas that `/std` does not hold were invented and proved. Turns cost 0.19 s to 0.35 s for that file, 2.0 s once one 128-bit fact sat in it, 0.17 s for a turn on a program whose 128-bit fact lives in a library unit the store already holds, and 1.9 s for every turn on that library once it is edited, until `run` refiles it. The four lemmas the proofs needed — `Le/trans`, `add/comm`, `cmp/to_eq`, `mul/assoc` — were found by reading `curios-prelude-archive/std/`; the `? ≈` lines never offered them, because their pools stop at what the module already references (`curios-elab/src/suggest.rs`). Six turns across the audits' probes went to spelling alone: `()` for `True`, an explicit proof where `Nat/rem` takes an implicit one, a `match` applied without parentheses. A question about a file under `curios-prelude-archive/std/` is refused today as a prefix collision, so for the standard library itself there is no loop but the archive rebuild, which [A unit the store holds is a baseline](incremental-compilation-spec.md) measures at about a hundred seconds.

**The budget is not in the verdict's key.** A library holding `let _heavy: Eq(Nat/shl(1, 1000000000), Nat/shl(2, 999999999)) = Eq/refl();` is refused at the default budget, compiles under `--budget 100000000` and files a verdict and a payload, is then reused at the default budget by `run`, by `wonder diagnostics` and by `run` with the payload deleted, and is refused again once the store is deleted. A slot is addressed by the schema tag, the compiler's content digest, the ordered predecessors, the mounts and the declared prefixes (`curios-package/src/store.rs`, `unit_slot`), and verified by the record of files read (`curios-verdicts/src/verdicts.rs`, `Record`); the budget appears in neither, as [Cached verdicts](../soundness/admission-without-judgment/cached-verdicts.md) records under what it deliberately does not cover.

## What is already built

- **Reflection.** `Holds(b) = match b | true => True | false => False end` in `/sys` (`curios-text/src/sys_module.rs`), filled by reduction wherever the goal reduces to truth ([A bound is stated in a decided proposition and discharged by reduction](../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)), used twenty-five times in `/std`. The closed machine evaluates a closed, metavariable-free term on an explicit stack when no refinement is registered (`curios-core/src/machine.rs`, `accelerable`; `curios-elab/src/reduce.rs` and `curios-cert/src/kernel/whnf.rs`, `machine_admissible`; [The closed machine](../soundness/per-term-rules/the-closed-machine.md)).
- **The goal report as data.** `GoalReport` carries a goal's scope, expected type, solution, the conversions it holds up and its sandboxed candidates as terms (`curios-elab/src/error.rs`), flattened to one string by `Error::reports` and `goal_text` (`curios-elab/src/error/display.rs`). `wonder`'s record keeps severity and span as data and the message as prose (`curios-wonder/src/record.rs`); the server publishes the message string (`curios-wonder/src/server.rs`).
- **The per-declaration ledger, discarded.** `Consumption` — units and peak depth — is computed for every declaration by both checkers and folded into one nameless heaviest at each budget restore (`curios-core/src/consumption.rs`; `curios-elab/src/context.rs`, `restore_budget`; `curios-cert/src/kernel.rs`, `restore_budget`), leaving the pipeline only through `typecheck_measured` and `recheck_measured` (`curios-pipeline/src/compile.rs`), which nothing on the compile path reads.
- **Termination certificates.** `Accessible` and `WellFounded` for `<` on `Nat` (`curios-prelude-archive/std/WellFounded.crs`), and a `Type`-valued course-of-values principle (`curios-prelude-archive/std/Nat/Lt.crs`, `strong`). A `Bool`-valued function may match on an `Accessible` proof, since every payload is a proposition, and passes size-change through the payload rule (`curios-analysis/src/totality.rs`).
- **The kernel's rule for closed scrutinees.** A local-free scrutinee is skipped rather than recorded, and a local-free occurrence is never probed for an equation ([Case equations inside an arm](../soundness/what-the-kernel-consults/case-equations-inside-an-arm.md), `a_local_free_term_is_never_refined`). Phase 0 rests on it.

## Phase 0 — the open-term cliff

**What it is.** Reflection is cheap only because closed terms run on the machine, and the machine's gate has a judgment half: it declines whenever any refinement is registered, because inside an arm a closed scrutinee *is* the arm's assumed value and evaluating it would answer a different question ([Evaluating a closed term is representation, not judgment](../design/toolchain/evaluating-a-closed-term-is-representation-not-judgment.md)). The strategy that runs instead prices every level of guarded recursion at a native frame, which is the 180-fold above. The variable is not the cliff: a decider applied to a variable is stuck and nothing reduces, and a binder that does not occur leaves the term closed and accelerated. The arm is.

**How it could be attacked, and what survives.** Four attacks were weighed.

- *Narrow the gate to "no closed key is registered".* Every term the machine reaches is closed, since it starts closed and substitutes values, and a closed occurrence canonicalizes to a closed term, so only a refinement keyed on a local-free term can fire on it. Sound, and it removes the measured shape — `match n | 0 => … | _ + 1 => … end` keys its one refinement on the local `n` — but it states as a check what should be an invariant, and it is the fallback if the invariant below finds a dependent.
- *Teach the machine to consult the equation store through `ClosedHost`.* Keeps two evaluators coupled to reducer state and re-grades the machine as a component that reads answers out of another; declined.
- *Lower every expression-scrutinee match to the convoy.* Tried and rejected before this specification, because the re-bound hypothesis breaks size-change descent ([An arm is checked in a context specialized by index inversion](../design/language/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md), Rejected).
- *An environment machine.* The right lever for open-term reduction generally and the wrong one here: the cliff is a gate declining a *closed* term, and an evaluator for open terms must still decide what a refined closed scrutinee means inside an arm, which is the question the gate exists to avoid. What it would buy the compiler overall is unmeasured — the span roster carries no span inside `curios-cert` and none on the recursive strategy — and is [A closed term evaluates at an interpreter's speed](faster-conversion-oracle-spec.md)'s question, not this one's.

**The attack that survives is an invariant: a refinement is never keyed on a local-free term, in either checker.** The kernel already lives by it. The elaborator records a stuck-application scrutinee whether or not it is closed — `match classify(c)` at a literal `c` is the example the machine's own documentation gives — and probes closed occurrences by canonical form (`curios-elab/src/context/frames.rs`, `refinement_scrutinees`). Mirroring the kernel's two rules there — never register a local-free key, never probe a local-free occurrence — makes the judgment half of both gates a theorem rather than a check: no equation can fire on a machine-reachable term, so both gates reduce to `accelerable`, in both checkers alike.

**Why it loses nothing.** A closed-key equation in the elaborator can only produce a program the elaborator accepts and the kernel refuses, because the kernel never holds that equation. Such a program does not compile today. The change moves that refusal from the kernel to the elaborator, where it gets a span, deletes a class of two-checker disagreement, and gives both checkers one admission rule for the machine, which is what cost parity between them requires ([A reduction step costs what it builds](../design/toolchain/a-reduction-step-costs-what-it-builds.md)). The design already calls these equations an elaboration convenience rather than a certification contract, with the explicit convoy as the spelling a definition that needs one writes.

**What it makes true for the phases after it.** Evaluation of a closed term is representation unconditionally, in any arm; an item's cost stops depending on ambient equations, which a per-declaration ledger and per-item recompilation need to mean anything; and any future evaluator inherits a store that holds only open keys — exactly the judgment-shaped residue the strategies keep.

**What it touches.** `frames.rs`'s registration of `refinement_scrutinees` and `refinement_projections`; the probe in `curios-elab/src/context.rs`, mirroring the kernel's `refined_reduct`; `any_refinements_registered`, which goes; the two `machine_admissible` gates; an elaborator twin of `a_local_free_term_is_never_refined`; `the_closed_machine_agrees_with_the_strategy` in `curios-cert/src/kernel/whnf/closed_machine_tests.rs`, extended with terms inside arms at both demands; a cost fixture in `curios`'s tests pinning the arm row above as accepted within the default budget; and the entries [The closed machine](../soundness/per-term-rules/the-closed-machine.md), [Case equations inside an arm](../soundness/what-the-kernel-consults/case-equations-inside-an-arm.md) and the machine's module documentation, re-graded from "no refinement in scope" to a registration invariant.

**The experiment.** Apply the skip and the probe refusal, then `cargo x clippy` and `cargo x test`. Zero new refusals is the expected result, by the argument above. A refusal in a perimeter fixture that recorded the accept-refuse quadrant for this shape (`tests::perimeter::the_two_checkers_agree_as_recorded`) is evidence to re-record; a refusal of a `/std` item means the argument is wrong somewhere, and that item is worth more than the change.

**What stays after it.** An open term still takes the strategy, and the exhaustion report should say so: when reduction runs out of steps and the machine was declined, name why and where. Both hosts hold that fact at the gate; it rides on `ReduceExhausted` and `ConvertExhausted` (`curios-elab/src/error.rs`) and is rendered in `display.rs`. Exact, and no perimeter consequence.

## Phase 1 — the per-declaration ledger

[A profile is a fact about the program, not about the machine](profiling-spec.md)'s question 1, taken here because three consumers now ask for it: an author planning a reflective proof against the budget, the incremental specification's own sizing of its first cut, and Phase 2's record. The item's names and `context.consumed()` are both in hand at `elaborate_module_item` before the next restore (`curios-elab/src/elaborate/module.rs`), and the kernel has the name at `check_definition` (`curios-cert/src/kernel/module.rs`); a `wonder spend` query prints both checkers' figures per declaration in `cost`'s tab-separated shape. It is the exact refusal number and a proxy for time, and the two are stated apart wherever it is read.

## Phase 2 — the budget joins the stored record

Record each declaration's consumption in the slot's record, and believe a slot only when every figure is at or below the invoking budget. Spend is deterministic per binary and the binary is in the address, so "recorded at or below the budget" implies the current invocation would reach the same verdict. Per declaration rather than per unit, because [A unit the store holds is a baseline](incremental-compilation-spec.md) reuses items judged by earlier walks, and a unit it files may hold items judged under different budgets. It touches `curios-unit/src/unit.rs`, `compile_unit` and the `Cache` trait in `curios-pipeline/src/compile.rs`, the `Record`s in `curios-verdicts/src/verdicts.rs` and `curios-verdicts/src/payload.rs` with both schema tags bumped, and the read-only cache in `curios-wonder/src/diagnostics.rs`. [Cached verdicts](../soundness/admission-without-judgment/cached-verdicts.md) and [Reused payloads](../soundness/admission-without-judgment/reused-payloads.md) gain the assumption in place of the omission, and a probe: filed under a raised budget, opened under the default, a miss. It is a reproducibility hole rather than an admission route — exhaustion only refuses — so it stays a clause of those entries rather than an entry of its own. A declaration-local budget form in the surface language is the eventual language answer, since it makes the budget the fact about the program the cost model already calls it, and is not taken here; refusing to cache under a non-default budget is not taken either, since it makes every reflection-heavy package recompile whole.

## Phase 3 — the structured channel

First, with no engine change: `wonder diagnostics` gains a flag that emits one JSON object per record, newline-delimited, from the existing `Diagnostic` — severity, the message without its snippet, and the span as source, byte range, line and column — serialized at the command-line edge in `curios-wonder/src/ask.rs`, which is where a transport converts. Then, only if the loop measurably needs the fields, `GoalReport` split where its spelling exists (`Error::reports`): the owning declaration, each scope binder with its type, the goal, the solution, each obligation as its two sides, each candidate with its hole count, and a mismatch as its inferred and expected types, every term as the string the report would have printed. Once the incremental specification's per-item recovery lands, a record also names the item it belongs to and, for a skipped dependent, the refusal that poisoned it; without that a skipped item reads as accepted to a machine. Recovery is what removes the one-failure-hides-every-goal wall the audits measured (`curios-wonder/src/diagnostics.rs`); the kernel's first-refusal-only, unlocated report (`curios-pipeline/src/compile.rs`, `compile_unit`) is unchanged by anything here.

## Phase 4 — the decider discipline and its library

Derived from the constraints, not invented, and to be recorded with the standard library once the first decider written under it lands.

1. Define the data and the operations in the library over `Bits`, never through intrinsic `/` or `%`: intrinsic arithmetic evaluates but cannot be reasoned about symbolically — the bounds oracle decides a stuck comparison only against a literal (`curios-core/src/reduce/intrinsic/compare.rs`, `compare_nat`; `nat_bound`), `/std` states no law about `%`, and [the binary64 conversion specification](big-flt-dyadic/02-binary64-spec.md) says so. `Nat/gcd` and `Nat/sqrt` are general recursion and cannot appear in a type; `BigNat/to_str` recurses on a computed quotient and cannot either.
2. Recurse structurally on a `Bits`, `List` or `Nat` argument, or on a literal fuel, or through an `Accessible` payload with a *provable* decrease. Fuel is no obstacle at closed instances — a fuelled decider passes totality on its fuel and reflects today — and becomes one only in a universally quantified lemma, where sufficiency must be proved. Euclidean division over `BigNat` is structural in the dividend read least-significant bit first; GCD is not structural in either form and needs `WellFounded(BigNat/lt)`, which `/std` lacks, or fuel from `bit_len` with a sufficiency lemma, as [the Euclidean specification](big-flt-general/01-big-nat-euclidean-spec.md) already demands.
3. Return `Bool`, state the claim as `Holds(decider(args))`, discharge with `True/qed()` at closed instances.
4. Put every heavy discharge in its own top-level `let` with a closed type, and consume it by name. Until Phase 0 lands, never discharge inside a match arm.
5. Write the soundness lemma as `Holds(decider(x)) -> Claim(x)` by matching on the decision and reading `h` through the arm's case equation, handing the true arm to the library's bridge lemma. `sound(d, q, n, h: Holds(cmp/eql(mul(d, q), n))) -> Eq(mul(d, q), n)` is three arms over `BigNat/cmp` and `cmp/to_eq`, and divisibility's transitivity follows from `mul/assoc`; both were written in one attempt against the current corpus.
6. For an external solver, make the answer a literal certificate argument to the soundness lemma. It is data with no authority: the checker runs in both checkers, `Holds(false)` is `False`, the checker is total by obligation T, and the lemma is kernel-checked. It reaches the Ersd rung as a retained top-level value, because a top-level item runs whatever its result's sort ([Erased positions are non-strict](../design/language/erased-positions-are-non-strict.md)), and is gone from `ersd-optm` onward by unreachable-item pruning; verify with `wonder stage ersd-optm`.
7. Keep a margin under the budget: a partial walk runs in a different cache state and can move a marginal declaration either way, which the incremental specification says of itself.
8. Plan by time, budget by units: about a microsecond per unit here, and 128 bits is two seconds per turn while the fact is the item under edit.

The library work this names: `WellFounded(BigNat/lt)`, and a `bit_len`-fuelled reformulation of `to_str` if a decimal conversion is ever wanted in a type.

## What constrains any answer

- Every candidate a tool offers is checked by elaboration *and* the kernel, never by the elaborator's oracle alone: the kernel refuses what the elaborator accepts at grounded argument positions ([Eta and untyped child positions](../soundness/per-term-rules/eta-and-untyped-child-positions.md), `a_grounded_argument_forfeits_irrelevance`), so `verifies` in `suggest.rs` is a filter and `check_with_units` is the guarantee.
- Nothing invisible: what a tool finds is written into the source. Witness resolution is invisible and re-run on every compile, which is why lemma-keyed resolution is rejected below.
- A query never writes the store and the engine names no transport's types (`curios-wonder`'s own decisions); JSON belongs to `ask.rs`.
- Obligations T and V: anything a type or a proof reaches must be total ([Totality of the erased program](../design/language/totality-of-the-erased-program.md)), so a decider is structural, fuelled, or certified.
- Acceptance is budget-relative and, under incremental compilation, walk-dependent for a marginal declaration; a refusal under an incremental check may be spurious and is re-checked whole.

## Rejected

- **Lemma-keyed resolution through the witness index.** The table keys type heads only — `satisfy Positive(5)` is refused as unkeyable — and holds one entry per key program-wide — two witnesses of one concept at head `Eq` are a duplicate — with no fuel and the orphan rule on top ([Concepts resolve with global coherence](../design/language/concepts-resolve-with-global-coherence.md), [A witness premise is smaller than its head](../design/language/a-witness-premise-is-smaller-than-its-head.md)). A hint index needs the opposite of each, and it would be automation the source never records.
- **Canonical proof idioms enforced by `curios format` or a lint.** Formatting never changes a program, verified by reparse (`curios-text/src/format.rs`), and a lint is an exact finding read off name resolution ([A lint is an exact finding read off the compilation](../design/toolchain/a-lint-is-an-exact-finding-read-off-the-compilation.md)). The `Eq` surface is five functions and a match on `refl`, and the corpus is small.
- **A `calc` form as this story's deliverable.** An annotated local `let` is already the term-level `have`, and a `calc` is an ordinary closed-forms change — parser, printer, lowering through a registry slot, grammar, editors — that this story does not need first.
- **The three Phase 0 attacks that did not survive**, recorded there.

## What has to be decided

- Whether Phase 0's experiment finds a dependent. It should not; if it does, the gate narrows to "no closed key registered" and the dependent is recorded.
- Whether `wonder spend` reports both checkers or the larger, and from which walk once units recompile incrementally.
- Which budget fix, with Phase 2's recommendation and the declaration-local form as the alternative.
- The wording of the exhaustion diagnostic and of a poisoned skip.
- Whether the JSON is newline-delimited objects or one array, and whether `GoalReport`'s terms cross as strings; strings are recommended, since the spelling exists only where the report is built.
- How `WellFounded(BigNat/lt)` is proved: through `bit_len` and strong induction, or through the order laws.

## Deliberately not specified

- **A premise index, a bounded-depth fill and an inhabitation search.** The loop's data is the demand signal: four lemma lookups the tool never offered argue for retrieval over the prelude, and nothing in sixteen propositions argued for a search — the one that needed invention needed lemmas, not depth. [Goal suggestions are depth-one fits, not proof search](../design/toolchain/goal-suggestions-are-depth-one-fits-not-proof-search.md) records what reopening that costs, and it is reopened by evidence of that shape, not by this file.
- **Incrementality inside a unit and per-item recovery**, which are [A unit the store holds is a baseline](incremental-compilation-spec.md)'s, and on which the loop over `/std` depends entirely.
- **Located kernel refusals and every refusal per compile.**
- **A faster evaluator for open terms**, which is [A closed term evaluates at an interpreter's speed](faster-conversion-oracle-spec.md)'s.

## How to retake the measurements

The cliff, with the operands generated so the literals are exact:

```sh
A=$(python3 -c "print(2**128 - 1)"); B=$(python3 -c "print(2**128 - 3)"); P=$(python3 -c "print((2**128 - 1) * (2**128 - 3))")
time ./target/release/curios wonder diagnostics - <<CRS
use /std/{Nat, Eq, BigNat, print};

let _p(n: Nat) -> Eq(BigNat/cmp/eql(BigNat/mul(BigNat/of_nat($A), BigNat/of_nat($B)), BigNat/of_nat($P)), true) =
    match n | 0 => Eq/refl() | _ + 1 => Eq/refl() end;

print("ok\n")
CRS
```

Replace the body with `Eq/refl()` for the top-level row, and with a reference to a top-level `let _fact: … = Eq/refl();` for the by-name row. Units: bisect `--budget` on the top-level program until the refusal "ran out of steps" appears; a budget below about fifteen thousand refuses inside the prelude's own replay and is not a measurement of the program.

The loop, as its first turn — every proposition a goal — and then one attempt per proposition per turn, timing each `wonder diagnostics` call:

```crs
use /std/{Nat, Bool, Eq, List, Option, BigNat, print};
use /std/Nat/{Lt, Le};

let _p1(x: Nat, y: Nat, p: Eq(x, y)) -> Eq(y, x) = ?;
let _p2(a: Nat, b: Nat, c: Nat, d: Nat, p: Eq(a, b), q: Eq(b, c), r: Eq(c, d)) -> Eq(a, d) = ?;
let _p3(n: Nat) -> Eq(n + 0, n) = ?;
let _p4(n: Nat, m: Nat) -> Eq(n + m, m + n) = ?;
let _p5(xs: List(Nat), ys: List(Nat)) -> Eq(List/len([..xs, ..ys]), List/len(xs) + List/len(ys)) = ?;
let _p6(xs: List(Nat), f: (Nat) -> Nat) -> Eq(List/len(List/map(xs, f)), List/len(xs)) = ?;
let _p7(xs: List(Nat)) -> Eq(List/map(xs, (v) => v), xs) = ?;
let _p8(a: Nat, b: Nat, c: Nat, p: Le(a, b), q: Le(b, c)) -> Le(a, c) = ?;
let _p9(a: BigNat, b: BigNat) -> Eq(BigNat/add(a, b), BigNat/add(b, a)) = ?;
let _p10: Lt(3, 5) = ?;
let _p11(o: Option(Nat), f: (Nat) -> Nat, g: (Nat) -> Nat) -> Eq(Option/map(Option/map(o, f), g), Option/map(o, (x) => g(f(x)))) = ?;
let _p12: Eq(BigNat/mul(BigNat/of_nat(6), BigNat/of_nat(7)), BigNat/of_nat(42)) = ?;
let _p13(b: Bool) -> Eq(Bool/not(Bool/not(b)), b) = ?;
let _p14(d: BigNat, q: BigNat, n: BigNat, h: Bool/Holds(BigNat/cmp/eql(BigNat/mul(d, q), n))) -> Eq(BigNat/mul(d, q), n) = ?;
let _p15(xs: List(Nat)) -> Eq(List/reverse(List/reverse(xs)), xs) = ?;

print("ok\n")
```

The store: `curios new` a package, put the `Nat/shl` proof above in its library, run it at the default budget, at `--budget 100000000`, at the default again, then with `.curios/payloads` removed, then with `.curios` removed.
