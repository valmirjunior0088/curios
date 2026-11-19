# A call's freedom from effects is read off the code it runs

**Not refined yet.** A call's freedom from effects crosses into the continuation IR as a claim every rewrite must keep true, though the IR states what each of its nodes observes and a dead loop is already judged on its body. This specification reserves reading that freedom off the IR for every call. It is not an implementation plan: its first step is a census deciding whether the reading pays, and its decision reopens a rejection.

## What is missing

A function crosses as `Droppable`, the certifier's termination met with `curios-anf`'s effect summary (`droppable_functions`, `curios-anf/src/summary.rs`), and every pass that copies, substitutes or restates what a call does maps or restates that claim — `Droppable::mapped`, `rewrite_atoms`, `shrink`'s census of what claims name, `uncurry_returns` and `uncurry_nests`, the inliner and the cloner ([An unread call is dropped by what it hands](../../design/compilation/an-unread-call-is-dropped-by-what-it-hands.md)). A pass that forgets loses precision and never soundness, and a pass that keeps a claim over a body that now does more drops an effect.

Termination is the one half the code cannot answer, which the checkers prove; the other is in the IR already, each intrinsic's `IntrinsicEffect` and each cell, channel, host row and closure call a node of its own, which `eliminate_dead_loops` reads a loop's body by, termination alone carried (`Function::terminates`, `Continuation::terminates`). Read so for every call, bottom-up over the call graph and over the values a body applies of what it was handed, the claim would be the code's at each round and nothing would map it.

## Context

- **One reader of the claim stands above the crossing.** `curios-anf`'s rebase (`rebase_one`, `curios-anf/src/optimize/rebase.rs`) leaves a recursion closed by a deciding first operand, `a && f(t)`, as written where the claim says it may be dropped, since placement in the IR then runs it as a tail call only where the first operand does not decide: a decision made in one stage on what the next reads, so a reading of the IR's bodies that refused such a recursion would leave it neither rebased nor placed, a loop lost rather than an answer wrong.
- **`curios-anf`'s summary stays whatever this decides**, `prune`, `invert` and the eager verifier reading it, so the question is the crossing alone.
- **Where the two may part.** A group tied through cells, whose initializer the ANF verifier holds to performing nothing, is read in the IR through a forcing function that polls a cell and takes from a channel (`lower_knot`, `curios-anf/src/into_cps/lowerer.rs`), so a reading of its body refuses what the claim admits, and a recursive witness may be such a group; and a walk applying a field of what it is handed gets no claim, the summary recording an application only of a parameter itself, where a reading over the inlined body admits it once the field is known.

## The decision it waits on

[An unread call is dropped by what it hands](../../design/compilation/an-unread-call-is-dropped-by-what-it-hands.md) rejects "CPS deriving purity again after inlining" as "a second effect lattice over a second representation, which the two stages' summaries would drift apart in". The reading would be a second derivation, and the drift the rejection names would be the rebase's alone, the one place one stage decides on what the other reads. LLVM splits the two halves this way: the frontend states `mustprogress`, and `FunctionAttrs` infers a function's memory effects and `willreturn` from its IR, as Flambda reads its primitives' effects in its own.

## First stage: the census

A temporary reading in `curios-cps`, at the `cps-optm` rung: each function judged bottom-up over the call graph's strongly connected components, observing nothing given the values it applies of what it was handed where every node of its body is an intrinsic of no effect, a call of a function judged so given what the call hands it, or an application of one of those values; any closure call of anything else, host row, cell or channel observes. Over `/std`'s test program and every program of `programs/`, counted: the functions whose claim admits a drop the reading refuses, and those the reading admits that the claim refuses, each with the calls `Module::may_drop_call` answers otherwise; and the fixpoint's rounds and the nodes it visits. The reading is reverted; the figures, named by their commit, decide whether the rejection is restated or this file is dropped.

## Verification and retirement

Once refined, its stages state their checks; the decision's **Rejected** paragraph is restated by whichever way the census decides, and this file is deleted.
