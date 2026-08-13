# A decided bound evaluates its subject, without a memory bound

Not refined yet. This placeholder reserves the specification location for bounding what a decided proposition costs to discharge.

## The issue

A precondition is a *decided* proposition — its body a `match` on the machine comparison — so it is discharged by **reducing its subject**. Where the subject is a computed value, elaboration therefore runs that computation: `Bytes/slice(built, 0, 10)` states `10 <= Bytes/len(built)`, and against `built = go(100000, x[])` the compiler evaluates the whole accumulation at the type level.

The step budget bounds *steps*; nothing bounds the memory a reduction allocates on the way. At corpus scale this does not refuse and does not time out — it exhausts the machine, observed once on 2026-08-13.

## Known constraints

- Nothing here is unsound, and the design it follows from is deliberate: `documentation/DESIGN.md`'s *A bound is stated in a decided proposition and discharged by reduction* records why the decided form exists and what stating preconditions inductively would cost instead.
- This is **not** the unfolding-discard defect, which is fixed. That one made such a bound *unprovable*; this one makes it *expensive*. Only the first is gone, and a fix for it does not touch this.
- The workaround is to keep a bound's subject opaque — a parameter, refined once behind a guard — which is what `curios/src/tests/runtime.rs`'s accumulation measurement does and why. Any refinement should decide whether that stays an idiom callers must know or becomes something the language enforces.
- `curios/src/tests/numeric.rs` pins the pair under a stated budget: `a_bound_on_a_computed_subject_evaluates_it` and `a_bound_behind_a_parameter_evaluates_nothing`, through the `typecheck_within` harness helper. Fixtures in this area state a budget; none may be allowed to run to completion.
