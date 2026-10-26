# A combinator's closures cost what a loop written in place costs

**Not refined yet.** This specification reserves compiling a combinator that is passed known closures as the loop a reader would have written in its place. It is not an implementation plan: it waits on the choice between the two designs under *Refinement*. Its consumer is `Handle/retried`.

## What is missing

**A closure a combinator is passed stays a closure.** `Handle/wait` states that readiness is advisory, so what waits asks again where the host would still block, and six readings of `/std` write that step as their own `blocked()` arm (`curios-std/src/internal/Handle.crs`'s write, flush and finish loops, `tcp/Listener.crs`, `tcp/Socket.crs`, `Process/Child.crs`); a reading that forgot it raised `would_block` as `other(0)`, as `Child/wait` did. One combinator, `Handle/retried(h, interest, attempt, status)`, states it once and costs two things the print path relies on: the description a constant `print` copies weighs past the closed evaluator's cap (`DESCRIPTION_COPY_NODE_LIMIT`, `curios-anf/src/optimize/evaluate/budget.rs`, 213 nodes against 160, held by `curios`'s `tests::fmt::a_constant_prints_descriptions_are_copied_within_the_evaluators_cap`), so no constant `print` folds; and the projection of each reply's status stays an indirect call (`tests::codegen::structural::closure_tests::a_returned_closure_every_caller_applies_is_absorbed`, `loop_tests::trees_hot_arithmetic_has_no_indirect_calls`). Written as a local loop that captures the attempt and the projection, the copy stays under the cap and a constant `print` still does not fold (`tests::fmt::print_constant_args_collapses_at_anf`, `tests::fmt::constant_args_leave_no_formatter_in_cps`), for a reason the closed evaluator holds. So the six readings stay written until a combinator's closures cost what a loop written in place costs.

## Refinement

- **The design, one of two**: a specialization of a function at a call that passes it known closures, so the combinator's loop runs over direct calls and its description is the loop's own; or a weight for a description that counts a combinator's body once where several descriptions share it.
- **Check**: the five tests above with `Handle/retried` in `/std`'s six readings, and `curios-std`'s tcp, fs, Tty and Tui tests.
