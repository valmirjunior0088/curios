# A call in a callback is graded where its callee applies it

This specification describes a missing capability: a recursion that reaches its own group through a callback a function applies, `has_zero(c)` inside `List/any(children, (c) => has_zero(c))`, is graded against nothing, so its group is `Partial` where the same recursion written over the list is total. Its next step is stage 1, a ready delivery.

## What is missing

**A call inside a function handed to another is graded with that function's binders fresh.** The discovery walk of [`group_totality`](../../../curios-analysis/src/totality.rs) walks a lambda argument as any lambda, its binders minted fresh, so `c` relates to no parameter of the caller and the call `has_zero(c)` contributes a matrix with no decrease; and a member named as a value anywhere but at the head of a spine is a call the walk records as all-unknown. The kernel, which takes a group's calls from the applications it types (`Kernel::record_call`, `curios-cert/src/kernel/calls.rs`), opens a lambda argument's binders the same way. Both grade through `grade` and close through `decide`, so the two agree, and both refuse.

Found at `0a15dcba0`, with a release build of that commit:

- A rose tree, `induct Rose | node(label: Nat, children: List(Rose))`, with `has_zero` recursing through `List/any(children, (c) => has_zero(c))`, is refused in a type position as not known to terminate, and `has_zero_written`, recursing through a member of its own group over the list, stands.
- `/std/Json/encode` recurses through `List/map(xs, encode)`, the member named as a value, and through `List/map(ps, (pair: {Str, Json}) => … encode(pair.1))`: it is not known to terminate, so a type cannot name it.
- `/std/Toml/encode`'s `value_str` recurses through `List/fold(items, start, (item, parts) => [..parts, value_str(item)])` in its `arr` arm, and through `pairs_str`, which reaches its table's values by `Map/to_list(m)`.

A scan of `/std`, `programs/` and `../curios-papers` for a definition calling itself inside a lambda it passes finds no other data-shaped recursion: the rest are `Io` loops, which descend on nothing, and proofs whose lambdas are motives. The idiom is the one user code writes over a tree whose children are a list.

## Context

- **What a verdict is for.** [Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md): a `Partial` group keeps its meaning wherever erasure retains a value, and is refused only where a type or a proof reaches it. A total verdict is also what `curios-anf` reads as `Function::total`, so a definition this specification makes total may also become droppable, where its effect summary allows it.
- **Where a verdict is decided.** [A declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md): each definition's totality is decided once, as the declaration is published, on a budget restored for it, and another declaration is read only through what it published. A callee's body is what it published, so a fact read off the body of `List/any` is read where `List/any` is published.
- **The size order.** An arm's case stands for the binder it scrutinizes and its payloads are that case's children ([`grade`](../../../curios-analysis/src/totality/grade.rs)); a list's element is a child of the cons that holds it, and a constructed tuple's fields are its children. [`SizeContext`](../../../curios-analysis/src/totality/grade.rs) holds what the arms enclosing a position established — a binder's shape, which binders are nonzero, which binders are payloads — and nothing a call established.

## Prior art

- **Rocq's guard condition** reads through `map` when it checks a fixpoint over a nested type: `map`'s own fixpoint is unfolded at the call, and its function parameter must be uniform, handed unchanged to every recursive call, for the recursive call inside the callback to be seen applied to a subterm.
- **Isabelle's function package** gives a recursive call under a higher-order function the premises a congruence rule declares, `x ∈ set xs` for `map` ([`fundef_cong`](https://www.isabelle.in.tum.de/website-Isabelle2011-1/dist/library/HOL/FunDef.html)), a rule the user states for each function.
- **Lean** rewrites `xs.map f` to `xs.attach.map …` before proving termination by well-founded recursion, so the membership of each element is in scope at the call (`wf_preprocess`, [Breitner, 2025](https://www.joachim-breitner.de/blog/816-Extrinsic_termination_proofs_for_well-founded_recursion_in_Lean)); each higher-order function is taught by a lemma of its own.
- **Size-change termination through closures** (Sereni and Jones, *Termination Analysis of Higher-Order Functional Programs*, APLAS 2005) builds the call graph through the closures a control-flow analysis says reach each application.

Curios needs no rule the user states: the callee's body is published, and what Rocq unfolds at each call can be read once, where the callee is published, as a summary of how it applies its function parameters.

## Settled

- **A summary per function parameter.** For each parameter of function type, a definition publishes either nothing or the set of matrices grading the arguments of each application of that parameter in its body against the definition's parameters, closed over the definition's own group: an application reached through a recursive call is graded through that call's matrix. `List/any`'s `pred` is applied to an element below `l`; `List/fold`'s step to an element below its list and to an accumulator unrelated to anything.
- **Only where every use is seen.** A parameter has a summary only where each of its occurrences in the group's bodies is the head of an application or an argument handed unchanged at its own position to a member of the group. Any other occurrence — stored, returned, captured by a lambda, handed to another function — publishes nothing, and a call through it is graded as today.
- **Both drivers consume it.** Where a call hands a summarized parameter a lambda, the walk opens the lambda's binders with what the summary assumes of them, each below or at an argument of the call; where it hands a member of the group itself, that is a call of the member graded through the summary. The elaborator's discovery walk and the kernel's typing walk read the summary through [`Env`](../../../curios-analysis/src/judge.rs), and the kernel computes the summaries it reads from what it certified, never from the elaborator's.
- **Decided where the callee is published and stored with it**, as its totality is: a summary is part of what a definition's record publishes, so a unit reading `/std` reads `List/any`'s and computes none. The certified images of `/sys` and `/std` change with it, and every verdict but the ones a summary makes total stays equal.

## Open

- **Composition across declarations.** `List/all` handing its `pred` to `List/any` inside a lambda publishes nothing under the rule above. Composing a callee's summary at a call inside the definition being summarized would summarize it; whether that is wanted is decided by the count stage 2 takes of what it would admit.
- **A projection of a binder below an argument.** `/std/Json/encode`'s `obj` arm calls `encode(pair.1)`, `pair` an element of `ps`. The grader reads a projection's shape off its subject's expansion, and a binder a summary places below an argument has none, so whether `pair.1` is graded below `ps` is decided with the assumed bound in stage 2.
- **A result's elements below an argument.** `value_str`'s `table` arm reaches its values through `Map/to_list(m)`, which a summary of how `pairs_str` applies a parameter does not relate to `m`. Out of scope: a fact about what a function returns is another analysis.

## Stages

1. **Summaries.** `curios-analysis` computes a definition's summaries over its group, and both drivers publish them beside its totality. Check: `curios-analysis` unit tests for `List/any`, `List/map`, `List/fold` and a parameter that escapes, which has none; the suites under `curios-analysis/tests/driven/`; `cargo xtask workspace lint`, which certifies `/std` unchanged.
2. **Consumption.** Both walks read a summary where a call hands a summarized parameter a lambda or a member. Check: the rose tree stands in a type position; `/std/Json/encode`'s call in its `arr` arm descends, and its group is total once its `obj` arm's is graded too; `value_str`'s call in its `arr` arm descends, and its group stays `Partial` through `pairs_str`; a function that applies its callback to its own result, `twice(f, x) = f(f(x))`, or that stores it, leaves a recursion through it `Partial`; each fixture is put to both checkers (`curios/src/tests/rules/`), with the same verdict. Then a count of what composing summaries across declarations would admit, settling the first open question.
3. **A hunt.** A soundness hunt with a charter on the summaries — a parameter used in a way the rule misses, an application under a binder the summary misread — run on the user's word.

## Verification and retirement

The totality decision states the rule, `curios-analysis`'s README the summary, and the rose tree and `/std/Json/encode` are fixtures of `curios/src/tests/rules/`. Then this specification is deleted.
