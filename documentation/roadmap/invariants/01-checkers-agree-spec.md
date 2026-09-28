# Standard-library invariants, part 1: the checkers agree on what they accept

Working specification for the places where the elaborator and the kernel read one rule two ways, or where the elaborator refuses a program the theory licenses. Each was met writing the standard library's proofs and pinned down with a program; each program below still behaves as stated on the tree this part starts from. None is a soundness hole — the kernel refuses what it would have to trust — but each makes a well-typed program fail, or pass only through an explicit argument the language should not need.

Its first two stages are the campaign's first wave and need nothing. Its third runs in the third wave, after [part 3](03-shared-term-costs-spec.md)'s instruments and with [part 2](02-unrecorded-universes-spec.md)'s investigation answered, alone on the elaborator's solver: the last change there cascaded from written-first solving into universes and into walks over shared terms, and two changes to the solver at once would hide each other's regressions.

## What this builds on

- **Re-validation.** A solution is re-checked before it commits, inside `Context::with_oracle` (`curios-elab/src/context.rs`), which suppresses every refinement in scope so a committed solution is refinement-free; within the bracket each memoizable node is elaborated once. `solve_refinement_free` (`curios-elab/src/convert.rs`) solves a flexible side by the rigid side's reduct, and by its written spelling only when the reduct is a `stalled_unfolding`.
- **Frozen frames.** A parked problem freezes the frame it was born under — assumptions, definitions and every kind of refinement — as a `FrozenFrame` (`curios-elab/src/context/frames.rs`), and a retry runs under exactly that frame.
- **The kernel's refinement gate.** `Scope::refine` (`curios-cert/src/kernel/scope.rs`) records an arm's case equation only when its scrutinee mentions a local, and gates the resolved spelling the same way; its documentation says why — a local-free term's memo entry outlives the arm, so an equation about one could leave an entry resting on an equation later retracted.
- **The closed machine.** `curios-core/src/machine.rs` evaluates a closed term under a demand, held to the strategies by `the_closed_machine_agrees_with_the_strategy` ([The closed machine](../../soundness/per-term-rules/the-closed-machine.md)).

## The gap

**Re-validation withholds the refinements a metavariable was born under.** Suppressing every refinement is right for a metavariable born outside the arm — `solve_refinement_free`'s own example is `?k := 0` because the nil arm refined `n := 0` — and wrong for one born inside it, whose solution may rest on the arm's guard. `Eq/sym`'s `@x` is born inside:

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

is refused at `found`: inferred `Eq(@Byte, Bytes/get(b, k), f)`, expected `Eq(@Byte, ?, ?)`, because the candidate `Bytes/get(b, k, @qed())` types only under the arm's `k < Bytes/len(b)`. Writing `Eq/sym`'s implicits by hand only moves the failure to `Eq/subst`'s `@y`. The standard library carries this as explicit implicit arguments in `Str.crs`'s `occurrence` and `meets` — `Nat/Le/trans`'s, and `Bytes/get(…, @inside)`.

**A guard on anything but a variable counts as no refinement.** `has_refinements` (`curios-elab/src/context/frames.rs`) counts refinements of plain variables only, so under a guard on a projection or an application — `k < n`, `x == y` — `solve_refinement_free`'s safeguard and the imitation guard beside it are skipped. A metavariable born outside an inner guard whose written candidate fails re-validation then falls back to the inner arm's literal:

```
pub induct W: (Bool) -> pub Type
| mk(b: Bool): (b)
end

let pick(@b: Bool, w: W(b)) -> Bool = b;

pub let stuck(k: Nat, n: Nat) -> Bool =
    pick(match k < n | true => W/mk(k < n) | false => W/mk(k < n) end);
```

is refused — inferred `W(true)`, expected `W(false)` — and so is the same shape under an outer `k < Bytes/len(bs)` guard with `Bytes/get(bs, k) == f` inside. `pick(match c | true => W/mk(c) | false => W/mk(c) end)` over a variable `c` passes. Counting every kind alone brings back a second failure of the first kind — a middle implicit whose candidate is a position unfolded down to the `@here := qed()` it was built with — so the two land together.

**The checkers read a local-free refinement key two ways.** The elaborator registers a case equation whose key mentions no local (`curios-elab/src/typing.rs`, `context.rs`); the kernel's gate skips it. So

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

is accepted by the elaborator and refused by the kernel: `expected Holds(Nat/lt(3, 2)), found True`. The kernel is right to keep its gate — its memos rest on it — so the elaborator is the side that moves.

**The kernel's closed memo is consulted before the case equations.** `whnf_within` (`curios-cert/src/kernel/whnf.rs`) asks `whnf_hit` before `refinement_of`, and a closed term's entry is not gated on the equations in force, where the machine and the `infer` memo are. It is sound while the gate above keeps every closed term unrefined; it is recorded so that no change to the gate — this part's own included — lands without the memo order in view.

**The closed machine's memo ignores demand for applications and matches.** It records a forced value under an application's key or a match's whatever the demand (`curios-core/src/machine.rs`), so a plain request after a forced one in the same run is answered `0` where the strategy answers the unevaluated recursive call. A differential in a scratch copy of the machine's tests confirmed it, with controls. It is the class of the closed projection the perimeter entry records as closed.

**Stuck-match annotations are compared, not re-derived.** Both checkers compare the annotations of stuck matches up to conversion, and the syntactic comparisons include them. No program reaching a wrong answer through it is known.

## Stages

1. **The closed memo and the refinement gate.** State the soundness argument where the order lives — the gate keeps closed terms unrefined, so a closed entry never meets an equation — and hold it with a fixture that probes both directions: a closed term reduced outside an arm and asked inside, and the reverse. A hit is fixed at once, ahead of every other stage.
2. **The machine's memo keyed by demand.** A forced value is recorded for the forced demand; a plain request reads only what a plain request produced. The differential joins the machine's tests, and the perimeter entry is corrected.
3. **Solving under the refinements a metavariable was born under.** A metavariable freezes the refinements in force at its birth, as a parked problem freezes its frame, and re-validation runs under exactly those — nothing of the arm it is solved in, all of the arm it was born in. Context containment counts refinement depth, so a solution that holds only under an arm cannot escape through a metavariable born outside it. `has_refinements` counts every kind of refinement in both places it is read. The written spelling is not assumed: counting every refinement and re-validating under the birth refinements are tried first on the stalled-unfolding rule as it stands, since written-first solving was reverted after a type-valued metavariable solved by its written spelling sat a universe too high, and its cost argument fell when re-validation stopped walking shared terms per path. Written-first for metavariables that range over values, a type-valued one keeping the stalled-unfolding rule, is the fallback, taken only if the programs above do not pass without it, and then with [part 3](03-shared-term-costs-spec.md)'s benchmark set within its budgets. When this lands, [algebra part 2](../algebra/02-bounds-from-facts-spec.md)'s "When it runs" can use guards on retry too.
4. **One gate for local-free keys.** The elaborator adopts the kernel's: a local-free scrutinee, or a resolved spelling that drops its locals, is not recorded as a case equation. The dead-arm program is refused by both, with a diagnostic that names the dead arm rather than the proof.
5. **Stuck-match annotations.** Investigate whether a stuck match's annotation can decide a comparison its re-derived form would not, with a program or an argument; a program found goes to the perimeter's regression discipline.

## Verification

- Each program above is a fixture beside the tests of the rule it exercises, asserting the diagnostic or the acceptance, with its control: the variable-match program, and an arm-only solution for a metavariable born outside the arm, refused.
- `Str.crs`'s `occurrence` and `meets` drop their explicit implicit arguments once stage 3 lands, and `/std` certifies.
- `a_type_named_through_a_higher_universe_solves_at_its_own` holds throughout.
- The two-checker fixtures and `kernel_disagreements` stay clean, and each fixture is mutation-checked: restoring the old rule fails it.
- Part 3's benchmark set stays within its budgets across stage 3.

## Retirement

Record the re-validation contract in `curios-elab`'s documentation, the gate's shared statement in both checkers' and in [An independent kernel re-checks what the elaborator accepts](../../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), and the machine's demand keying in the closed-machine entry. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
