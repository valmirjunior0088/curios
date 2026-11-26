# A value member its callee reads under a lambda is graded where that lambda runs

**Not refined yet.** [Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md) states that a call in a callback is graded where its callee applies it, and a member of no parameters named bare at the top of its own record is graded as a call at the top, where its callee reads it only under a lambda, so a program that descends is refused. It waits on the first stage's decision of where the summary lives.

## What is wrong today

- **A descending program is refused.** Over `struct Inner(A: Type): Type { step: (A) -> True }` and `struct Outer(A: Type): Type { inner: Inner(A), more: Inner(List(A)) }`, with `each(@T: Type, o: Outer(T)) -> Inner(List(T)) = Inner { step(s) = all(o.inner, s) }`, `all` applying `i.step` to each element of `s`, `let w: Outer(Doc) = Outer { inner = Inner { step(d) = True/qed() }, more = each(w) }` over `induct Doc: Type | leaf() | arr(List(Doc)) end` is refused, `a proof in the entrypoint reaches w, which is not known to terminate`, where nothing calls anything; with `more` written inline, `Inner { step(s) = all(w.inner, s) }`, `w` named under a lambda alone, it is accepted.
- **A member named bare is a call where it is handed.** A member of no parameters is answered as a call of its leading lambdas (`handed_member`, `curios-analysis/src/totality/part.rs`), so naming `w` in its own record is a call of `w` from `w` that grades nothing and never descends.
- **No summary says whether a callee reads a position where it is called.** `each`'s summary states what it applies of `o`, and a position applied nowhere may still be read where it is called, as a field holding no function is (`Walk::reads_no_function`, `curios-analysis/src/totality.rs`), so dropping the call on the applications alone would admit a knot such as `w = R { n = f(w) }` with `f` reading `r.n`.
- **The call that runs is recorded by neither checker.** Where `more` holds `each(w)`, the call it makes is `w.inner.step` at an element of `s`, below `w.more.step`'s argument; `Walk::walk_held` (`curios-analysis/src/totality.rs`) and `check_field` (`curios-cert/src/certifier/infer.rs`) record nothing of it.

## Goal

A member handed where its callee does not read it at the call makes no call there, and the call a part of the callee makes through it is graded where that part runs, in both checkers.

## Settled

- The fact is a summary each definition publishes beside its applications, stating for each position whether the definition reads it where it is called, as the eager check's evaluation summary states what an initializer evaluates (`Interface::answers`, `curios-anf/src/verify/eager.rs`).
- Both checkers read it, as they read every summary [Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md) publishes.

## Open

- Where the summary lives: a third state of a position in `PartApplications` (`curios-core/src/call_matrix.rs`), beside applied and used otherwise, or a table of its own.
- Whether the read is the fact the eager check's `Interface` states at erasure, so one summary serves both.

## Stages

1. **The summary.** Each definition publishes whether it reads each position where it is called, read by neither checker yet. Check: a unit test of the shared walk's summary, `each` stating `o` read under `step` alone, and `f` of `w = R { n = f(w) }` stating `r` read where it is called.
2. **Both checkers grade by it.** The elaborator's walk and the certifier grade a member handed where it is not read as no call, and the call its callee's part makes through it where the part runs. Check: the program above with `inner`'s `step` calling `w.more.step(s)`, accepted under one test name for both checkers, with a twin whose `each` reads `o.inner` where it is called refused; each mutation-checked.

## Verification and retirement

Verified by stage 2's pair under `curios/src/tests/rules/` and `/std` built unchanged; retired when both stages land, [Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md) restated with the read summary beside the applications.
