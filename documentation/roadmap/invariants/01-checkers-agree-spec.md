# Standard-library invariants, part 1: the checkers agree on what they accept

Working specification for the places where the elaborator and the kernel read one rule two ways, or where the elaborator refuses a program the theory licenses. Each gap below is pinned by a program, re-taken on the tree this part starts from. None is a soundness hole — the kernel refuses what it would have to trust — but each makes a well-typed program fail, or pass only through an explicit argument the language should not need.

It needs nothing. Its fourth stage changes solving, which [part 2](02-unrecorded-universes-spec.md)'s fix changes too, so that fix waits for it; it runs with [part 3](03-shared-term-costs-spec.md)'s instruments in place, so its cost is measured rather than guessed.

## What this builds on

- **Recording a case equation.** The elaborator's `refine_head` (`curios-elab/src/typing.rs`) records one by the scrutinee's head shape: a variable, and a local definition's body in turn; a projection; and any other scrutinee in the term-keyed store, under its written spelling, the spelling its dispatch resolves to, and the spelling with its local definitions unfolded. The kernel's `Scope::refine` (`curios-cert/src/kernel/scope.rs`) records one list of equations, keyed on the scrutinee as written and on its resolved spelling, each only when it mentions a local; its documentation says why — a local-free term's evaluation memo entry outlives the arm, so an equation about one could leave an entry resting on an equation later retracted.
- **Re-validation.** `Convert::solve` (`curios-elab/src/convert.rs`) re-checks a candidate against the metavariable's birth type under its birth telescope, inside `Context::with_oracle`, which withholds every refinement registered so far; a frame the candidate itself enters keeps its own. `solve_refinement_free` solves a flexible side by the rigid side's reduct with refinements withheld, and by its written spelling only when the reduct is a `stalled_unfolding`; the imitation rule beside it guards its candidate the same way. Both gates read `has_refinements`.
- **Frozen frames.** A parked problem freezes the frame it was born under — assumptions, definitions and every kind of refinement — as a `FrozenFrame` (`curios-elab/src/context/frames.rs`), and `Context::with_retry_frame` runs its retry under exactly that frame, the live one hidden below a retry floor.
- **Birth records.** A metavariable's `MetaEntry` freezes its Γ as a telescope shared by every metavariable born under an unchanged Γ (`Frames::identity_snapshot`), which keeps minting O(1). `Context::metavar_context_contained` decides whether one metavariable's birth context lies inside another's by comparing telescope names.
- **The kernel's evaluation memos.** `curios-cert/src/kernel/memos.rs` keeps local-free reducts for the declaration and local-bearing ones for as long as the equations in force stand. `whnf_within` (`kernel/whnf.rs`) asks the memo before it asks the equations. `whnf::equations_tests` holds the interlock: `a_case_equation_reaches_the_reduct_and_not_the_memos`, `a_remembered_reduct_does_not_outlive_the_equations_it_was_taken_under` and `a_local_free_term_is_never_refined`.
- **The closed machine.** `curios-core/src/machine.rs` evaluates a closed term under a demand with a run-scoped value memo, held to the strategies by `the_closed_machine_agrees_with_the_strategy` ([The closed machine](../../soundness/per-term-rules/the-closed-machine.md)).

## The gap

**The checkers record different equations.** The kernel records an equation only under a spelling that mentions a local; the elaborator records one wherever an arm is entered. Four routes reach the difference, and each is accepted by the elaborator and refused by the kernel with `expected Holds(…), found True`:

```
pub struct U: pub Type { Nat }

satisfy Eql(U) {
    eql(a, b) = Nat/lt(3, 2),
    neq(a, b) = true,
}

pub let dead(x: U, y: U) -> Nat =
    match x == y
    | true =>
        let _p: Bool/Holds(Nat/lt(3, 2)) = Bool/True/qed();
        0
    | false => 1
    end;
```

- a resolved spelling that drops its locals, as above: `x == y` resolves to `Nat/lt(3, 2)`, which the elaborator records and the kernel filters;
- a top-level name as the scrutinee, `match flag` over `let flag: Bool = false`, which the elaborator's variable store refines and the kernel skips as local-free;
- a projection of a top-level value, `match pair.0` over `let pair: {Bool, Nat} = (false, 0)`, the same through the projection store;
- a local definition of a closed term, `let c = Nat/lt(3, 2); match c`, which the elaborator refines under the local name while the kernel, substituting the definition, meets a closed scrutinee.

The kernel is right to keep its gate — its memos rest on it — so the elaborator is the side that moves.

**Re-validation withholds the refinements a metavariable was born under.** Withholding every refinement is right for a metavariable born outside the arm — `solve_refinement_free`'s own example is `?k := 0` because the nil arm refined `n := 0` — and wrong for one born inside it, whose solution may rest on the arm's guard. `Eq/sym`'s `@x` is born inside:

```
pub let probe(b: Bytes, k: Nat, f: Byte, P: (Byte) -> Type, lead: P(f), fallback: Nat, consume: (x: Byte, P(x)) -> Nat) -> Nat =
    match k < Bytes/len(b)
    | true =>
        match Bytes/get(b, k) == f
        | true =>
            let found = Byte/eq_of_eql(Bytes/get(b, k), f, Bool/True/qed());
            consume(Bytes/get(b, k), Eq/subst((c: Byte) => P(c), Eq/sym(found), lead))
        | false => fallback
        end
    | false => fallback
    end;
```

is refused at `found`: inferred `Eq(@Byte, Bytes/get(b, k), f)`, expected `Eq(@Byte, ?, ?)`, because the candidate `Bytes/get(b, k, @qed())` types only under the arm's `k < Bytes/len(b)`. Writing `Eq/sym`'s implicits by hand only moves the failure to `Eq/subst`'s `@y`. The standard library carries this as explicit implicit arguments in `Str.crs`'s `meets` and `occurrence` — `Nat/Le/trans`'s, and `Bytes/get(…, @inside)`.

**A guard on anything but a variable counts as no refinement.** `Frames::has_refinements` reads the variable store alone, so under a guard on a projection or an application — `k < n`, `x == y` — both gates it opens are skipped. A metavariable born outside an inner guard then commits the inner arm's literal:

```
pub induct W: (Bool) -> pub Type
| mk(b: Bool): (b)
end

let pick(@b: Bool, w: W(b)) -> Bool = b;

pub let stuck(k: Nat, n: Nat) -> Bool =
    pick(match k < n | true => W/mk(k < n) | false => W/mk(k < n) end);
```

is refused — inferred `W(true)`, expected `W(false)` — and `pick(match c | true => W/mk(c) | false => W/mk(c) end)` over a variable `c` passes. Counting every store alone brings back a failure of the previous kind — a middle implicit whose candidate is a position unfolded down to the `@here := qed()` it was built with — so the two land together.

**The closed machine's memo ignores demand for applications and matches.** It records a forced value under an application's key or a match's whatever the demand (`Frame::Head` pushes its `Frame::Memo` before it knows the demand's outcome), so a plain request after a forced one in the same run is answered with the unfolded value where the strategy answers the folded recursive call. The perimeter entry records the projection case as found and closed; that fix decided a projection from its shape, and the application and match keys share the defect.

**The memo order rests on an unwritten precondition.** `whnf_within` answers from the memo before it consults the equations. That is sound because the gate keeps every term the memo holds past an arm local-free and every local-free term unrefined — but the argument is stated in `memos.rs`, not where the order is, and no fixture fails if the gate is removed while the order stays.

## Stages

1. **One recording rule.** Landed: `curios_analysis::records_case_equation` is the one statement of which spelling may carry a case equation — one that mentions a local once local definitions are unfolded, the spelling the kernel is handed. `Scope::refine` calls it for both of its spellings, and `refine_head` on the scrutinee as the kernel spells it and on every spelling its term-keyed store would hold. The four programs above are refused by the elaborator where they are written, each report noting that the arm is never taken because its guard is always the other case (`curios/src/tests/matching/recording_tests.rs`, mutation-checked, with a parameter's guard as the control); the predicate's own cases are unit tests beside it.
2. **The machine's memo keyed by demand.** Landed: a value is recorded in the table of the demand it was produced at, a plain request reads the plain table alone and a forced one either, since every weak-head value that is not a folded spelling is its own forced form and folded spellings are never recorded. `the_closed_machine_agrees_with_the_strategy` carries the call and the match beside the bare selection, mutation-checked, and the perimeter entry says what each fix covers.
3. **The memo order stated where it lives.** Landed: `whnf_within` says why the memo may answer first and which rule it rests on, and `a_remembered_closed_term_answers_inside_an_arm_as_an_uncached_kernel_does` reduces a term over a global definition outside an arm and again inside one whose equation would refine it were it recorded, a cached and an uncached kernel agreeing; with the rule of stage 1 removed and the order kept, it fails.
4. **Solving under the refinements a metavariable was born under.** Landed: a metavariable's birth record carries the refinements visible at its birth (`Frames::refinement_snapshot`, shared between births under unchanged refinements), and one oracle judges a solution and a goal's candidate alike under the checked term's birth refinements and no others (`Context::with_oracle`, through `with_refinements`): at once where they are the ones in view, otherwise withholding every refinement and reinstalling the birth's. `solve_at_birth` and the imitation guard spell a candidate in the same view, containment compares refinements as well as names, and a goal's suggestions reinstall its refinements beside its telescope. `Str.crs`'s `meets` and `occurrence` drop their explicit implicit arguments; the two programs above are `implicit_tests`' `an_implicit_born_in_an_arm_is_solved_under_the_arms_guard` and `an_implicit_born_outside_an_arm_is_solved_without_its_guard`, the solver's own cases are `solve_tests`', and a goal in an arm is offered what fits under its guard (`suggestion_tests`). When this landed, [algebra part 2](../algebra/02-bounds-from-facts-spec.md)'s "When it runs" can use guards on retry too.
5. **Stuck-match annotations.** Both checkers compare the annotations of stuck matches up to conversion, and the syntactic comparisons include them; whether an annotation can decide a comparison its re-derived form would not is an open question with no known program. It is recorded as an unprobed entry of [the soundness perimeter](../../design/language/the-soundness-perimeter.md), with what would answer it, and leaves this part.

## Verification

- Each program above is a fixture beside the tests of the rule it exercises, asserting the diagnostic or the acceptance, with its control: the variable guard, and an arm-only solution for a metavariable born outside the arm, refused.
- The four recording programs reach the same verdict in both checkers, and each stage's fixture is mutation-checked: restoring the old rule fails it.
- The two-checker fixtures and `kernel_disagreements` stay clean, `/std` certifies after `meets` and `occurrence` drop their explicit implicits, and `a_type_named_through_a_higher_universe_solves_at_its_own` holds throughout.
- Part 3's benchmark set stays within its budgets across stage 4.

## Retirement

Record the recording rule in `curios-analysis`'s documentation and in [An independent kernel re-checks what the elaborator accepts](../../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), the re-validation contract in `curios-elab`'s, and the machine's demand keying in the closed-machine entry. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
