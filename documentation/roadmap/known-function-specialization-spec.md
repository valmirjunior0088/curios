# A combinator specialized on a known function argument stops calling through it

## Status

Not refined yet. The cost is measured and the machinery that already exists is inventoried below; what is open is the boundary between them — which calls are left paying indirect dispatch, and why. Nothing is started.

## Why it exists

A higher-order combinator is compiled once, with its function parameter as an ordinary runtime value, so a call site that knows the function still reaches it through a closure. `curios`' `tests::codegen::ladder` counts what an idiomatic UTF-8 walk costs per character in the emitted `/std/Str/fold` and lists the indirect call through `f` as one of six per-character sites, beside the allocations the churn campaign closed. Its own prose names this file: "The indirect call is its own roadmap item."

What specialization buys is not the one dispatch. It is that a substituted callee is visible to every pass downstream — inlining, folding, the representation analysis that decides whether a value rides a register — none of which can see through a closure field.

## What is certain

Read from `curios-cont/src/cps/specialize.rs`, which already specializes on three grounds.

- **A known function argument is already substituted, for a restricted population.** `scc_invariant_knowns` computes the parameters of recursive SCC members "provably a single literal or function reference at every entry", as a monotone `Unknown < Known < Conflict` fixpoint over the known-callee call graph, and substitutes them in place so the parameter dies.
- **The restriction is `eligible_sccs`, and it is three conditions.** The SCC must be recursive, must contain no escaping member, and must not contain the program entry — because an escaping or host-called function receives arguments the analysis cannot observe. A function reference additionally counts only where the member's body may name it, which is `CallAnalysis::lexical_scope`.
- **Disagreeing callers are answered by cloning.** `specialize_scc_calls` copies an SCC for one external call context and repoints that site and its argument-sharing siblings, so the copy has a single agreeing caller and the ordinary propagation folds the arguments in on a later iteration while the original stays polymorphic. It is bounded by `SCC_CLONE_NODE_LIMIT` and a module-wide clone budget.
- **The other two specializers are about constructors, not functions.** `specialize_call_patterns` and `specialize_jump_patterns` are SpecConstr over statically-known tagged tuples, gated on `deconstructs_param` — the callee must actually project a field out of the parameter — and bounded by `BRANCH_SPECIALIZATION_GROWTH_LIMIT`.
- **What can be known is an atom.** `Knowledge::Known` carries a `CpsAtom`, and `CpsAtom::Fun` is a direct function reference. A closure that captured anything is a `CpsAtom::Value`, so two call sites passing what a reader would call the same lambda are two different atoms and join to `Conflict`.
- **Dispatch is a table index, not a reference.** [A closure carries its code as a table index](../../curios-cont/README.md) is landed: construction writes an `i32` code field, dispatch reads it into `call_indirect`, and `curios`' `tests::codegen::structural::closure_tests` asserts `call_ref` is absent from a module entirely. **The ladder's per-character table names `call_ref`, which the emitted module no longer contains** — read the emitted body rather than that row when retaking the measurement.

## What has to be decided

- **Which calls are actually left.** The gates above exclude a combinator that is not in a recursive SCC, one that escapes, one reachable as the entry, callers that disagree past the clone budget, and every function argument arriving as a capturing closure. Which of those `/std/Str/fold` falls into is a measurement, and measuring before designing is the rule this roadmap already enforces on itself.
- **Whether the answer widens a gate or adds a pass.** Relaxing `eligible_sccs` and writing a fourth specializer are different changes with different blast radii, and the first is only available where the gate's stated reason — arguments the analysis cannot observe — does not actually apply.
- **Whether a capturing closure is in scope.** Specializing on a bare function reference is substitution; specializing on a closure means agreeing about its captures too, which is a different analysis and probably a different item.
- **Where the budget comes from.** Specialization is cloning, a module-wide clone budget already exists and is shared by three passes, and a fourth claimant changes what the existing three get.

## Deliberately not specified

The pass's position in the fixpoint. The profitability gate, which for the constructor specializers is `deconstructs_param` and has no obvious analogue here. Whether any of this changes what the ladder measures, which is the instrument's own to report.
