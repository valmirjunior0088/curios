# A combinator's closures cost what a loop written in place costs

Working specification for `Handle/retried`, the combinator that states once the retry six readings of `/std` write each for themselves, and that `/std` holds back until its closures cost what the loops written in place cost. A combinator's closures cost that where `curios-cps` can follow a known function into the call ([A known function a call hands is followed into it](../../../curios-cps/README.md#a-known-function-a-call-hands-is-followed-into-it)), and `retried` is a callee it does not follow into, besides weighing past the cap a constant `print`'s description is copied under. Its next steps are a bounded investigation and a decision, the first two open questions, each releasing a stage.

## What this builds on

- **A known function a call hands is followed into it.** `curios-cps` substitutes a function every call or every entry of a loop agrees on, copies a callee into a call handing it a function it applies, and copies a recursive component into the group binding a function its own group may not name, where the substitution reaches the copy ([`curios-cps`'s README](../../../curios-cps/README.md#a-known-function-a-call-hands-is-followed-into-it)). The specializer's passes take a component recursive by a call of a member, `eligible_sccs` (`curios-cps/src/optimize/specialize.rs`), and the inliner and `known_values` decline one recursive through a function a member defines (`CallAnalysis::recursive`).
- **A constant `print`'s description is copied under a cap.** ANF's closed evaluator copies a description a candidate holds only within `DESCRIPTION_COPY_NODE_LIMIT`, 160 nodes (`curios-anf/src/optimize/evaluate/budget.rs`), whose doc states why: a sequencing chain's continuation drags its suffix along as extent, and copying that per link per round compounds. `curios`'s `tests::fmt::a_constant_prints_descriptions_are_copied_within_the_evaluators_cap` holds the library's write loop under it, and `print_constant_args_collapses_at_anf` and `constant_args_leave_no_formatter_in_cps` the folding it buys.

## The gap

**Each reading writes its own retry.** `Handle/wait` states that readiness is advisory, so what waits asks again where the host would still block, and six readings of `/std` write that step as their own `blocked()` arm (`curios-prelude/std/internal/Handle.crs`'s read, write, flush and finish loops, `tcp/Listener.crs`, `tcp/Socket.crs`, `Proc/Child.crs`); a reading that forgot it raised `would_block` as `other(0)`, as `Child/wait` did.

**One combinator stating it costs a constant `print` its fold.** `Handle/retried(h, interest, attempt: Io(R), status: (R) -> Nat) -> Io(R)` runs `attempt` and, where `status` of the reply says the host would block, waits on `h` for `interest` and runs it again. Written into `/std/internal/Handle`'s four readings in a scratch copy of the library, counted on top of `47a4f1dd0` with this tree's optimizer, it costs both. The status projection each reading hands stays a call through a closure (`tests::codegen::structural::closure_tests::a_returned_closure_every_caller_applies_is_absorbed` and `loop_tests::trees_hot_arithmetic_has_no_indirect_calls` fail): `retried` calls itself only through the closure it hands the effect machinery, so it is a component no specializer pass takes and a recursive callee the inliner declines. Counting that recursion in `eligible_sccs` as `CallAnalysis::recursive` counts it puts the projection in place and passes both tests, and leaves the optimizer short of a fixpoint within `ROUND_LIMIT` on `curios-prelude`'s `every_conformance_case_segments_through_the_library_as_the_database_states`, which compiles in seconds without it. And the heaviest description a constant `print` copies weighs 215 nodes against the cap's 160, so the closed evaluator declines the copy and no constant `print` folds: the cap test and the two folding tests fail.

## Open questions

- **What keeps the specializer from converging over a component recursive through what it defines.** Settled by a profile of the conformance program's optimizer rounds with that recursion counted, naming the passes that fire round after round, and a bound on what they undo or repeat; the answer is the first stage.
- **How the description is weighed.** Three designs. The cap raised past what the write loop through `retried` weighs, with the compounding it guards against measured: compile time and module size over `programs/` and `../curios-papers`' harness, before and after. A weight that counts a combinator's body once where several descriptions share it, which keeps the cap and changes what it counts. Or the readings left as written, and this specification retired with its stage unconsumed and filed as a finding. Recommended: the raised cap, measured, since what the cap guards against is a sequencing chain's suffix copied per link, which a combinator's body called at one link is not, and a weight that shares bodies is a second accounting the evaluator would keep beside its reification's.
- **What a reading writes after `retried`.** A reply `retried` answers is never one that would block, and a reading's match over its status still names that case: `read_waiting` would recur there, and `write_waiting`, `flush_waiting` and `finish_waiting` fold it into their `_`. A status type without the case, or a proof beside the reply, removes the arm and costs `retried` a refinement.

## Stages

1. **The specializer takes a component recursive through a function it defines**, with the optimizer converging over `/std` and the corpus. Check: the two indirect-call tests with `retried` in a scratch copy of the library, `every_conformance_case_segments_through_the_library_as_the_database_states` in the time it takes without it, and a unit test of such a component given a copy per context.
2. **A constant `print` through `retried` folds**, by the design the second open question settles. Check: the cap test and the two folding tests with `retried` in a scratch copy of the library; the measurements the design names.
3. **`Handle/retried` replaces the six readings' `blocked()` arms.** Check: the five tests named above, and `curios-prelude`'s tcp, fs, Tty and Tui tests.

## Verification

`cargo xtask workspace lint` and each stage's tests by name, with `curios-prelude`'s and `curios`'s `tests::fmt`. The full gate runs once, after the last stage.

## Completion and retirement

The weight a description is copied under goes to `DESCRIPTION_COPY_NODE_LIMIT`'s doc, or to `curios-anf`'s README where it changes what the evaluator counts, and `Handle/retried`'s contract to its doc in `/std`. Once those are recorded and no reference to this specification remains, this file is deleted.
