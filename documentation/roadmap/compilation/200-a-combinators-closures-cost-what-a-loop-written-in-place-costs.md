# A combinator's closures cost what a loop written in place costs

Working specification for `Handle/retried`, the combinator that states once the retry seven readings of `/std` write each for themselves, and that `/std` holds back until its closures cost what the loops written in place cost. A combinator's closures cost that where `curios-cps` can follow a known function into the call, a component recursive through a function a member defines included ([A known function a call hands is followed into it](../../../curios-cps/README.md#a-known-function-a-call-hands-is-followed-into-it)), and `retried` weighs past the cap a constant `print`'s description is copied under. Its next steps are the two stages below, the cap first, with a question the second settles.

## What this builds on

- **A known function a call hands is followed into it.** `curios-cps` substitutes a function every call or every entry of a loop agrees on, copies a callee into a call handing it a function it applies, and copies a recursive component, one recursive through a function a member defines included, for each function a call hands it ([`curios-cps`'s README](../../../curios-cps/README.md#a-known-function-a-call-hands-is-followed-into-it)).
- **A constant `print`'s description is copied under a cap.** ANF's closed evaluator copies a description a candidate holds only within `DESCRIPTION_COPY_NODE_LIMIT`, 160 nodes (`curios-anf/src/optimize/evaluate/budget.rs`), whose doc states why: a sequencing chain's continuation drags its suffix along as extent, and copying that per link per round compounds. `curios`'s `tests::fmt::a_constant_prints_descriptions_are_copied_within_the_evaluators_cap` holds the library's write loop under it, and `print_constant_args_collapses_at_anf` and `constant_args_leave_no_formatter_in_cps` the folding it buys.

## The gap

**Each reading writes its own retry.** `Handle/wait` states that readiness is advisory, so what waits asks again where the host would still block, and seven readings of `/std` write that step as their own `blocked()` arm (`curios-prelude/std/internal/Handle.crs`'s read, write, flush and finish loops, `tcp/Listener.crs`, `tcp/Socket.crs`, `Proc/Child.crs`); a reading that forgot it raised `would_block` as `other(0)`, as `Child/wait` did.

**One combinator stating it costs a constant `print` its fold.** `Handle/retried(h, interest, attempt: Io(R), status: (R) -> Nat) -> Io(R)` runs `attempt` and, where `status` of the reply says the host would block, waits on `h` for `interest` and runs it again. Written into `/std/internal/Handle`'s four readings in a scratch copy of the library, counted on top of `121f9d5c2` with the specializer taking a component recursive through what it defines, the status projection each reading hands is a direct call, and the heaviest description a constant `print` copies weighs 215 nodes against the cap's 160, so the closed evaluator declines the copy and no constant `print` folds: the cap test and the two folding tests fail.

## Open questions

- **What a reading writes after `retried`.** A reply `retried` answers is never one that would block, and a reading's match over its status still names that case: `read_waiting` would recur there, and `write_waiting`, `flush_waiting` and `finish_waiting` fold it into their `_`. A status type without the case, or a proof beside the reply, removes the arm and costs `retried` a refinement.

## Stages

1. **A constant `print` through `retried` folds**, under a cap raised past what the write loop through `retried` weighs, since what the cap guards against is a sequencing chain's suffix copied per link, which a combinator's body called at one link is not. Check: the cap test and the two folding tests with `retried` in a scratch copy of the library; compile time and module size over `programs/` and `../curios-papers`' harness, at the cap of 160 and the raised one.
2. **`Handle/retried` replaces the seven readings' `blocked()` arms**, by what the open question settles. Check: the cap test, the two folding tests, `closure_tests::a_returned_closure_every_caller_applies_is_absorbed` and `loop_tests::trees_hot_arithmetic_has_no_indirect_calls`, and `curios-prelude`'s tcp, fs, Tty and Tui tests.

## Verification

`cargo xtask workspace lint` and each stage's tests by name, with `curios-prelude`'s and `curios`'s `tests::fmt`. The full gate runs once, after the last stage.

## Completion and retirement

The weight a description is copied under goes to `DESCRIPTION_COPY_NODE_LIMIT`'s doc, and `Handle/retried`'s contract to its doc in `/std`. Once those are recorded and no reference to this specification remains, this file is deleted.
