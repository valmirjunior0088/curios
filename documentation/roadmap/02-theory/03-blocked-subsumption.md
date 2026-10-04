# A subsumption blocked on a metavariable waits as a subsumption

Working specification for the elaborator deciding subsumption wherever a metavariable stands in the way. [Subsumption is a relation](../../design/theory/subsumption-is-a-relation-not-a-traversal-order.md), which the elaborator decides in conversion, a problem carrying the relation it is asked at. Today it decides it only between two rigid function types, and hands every other pair to conversion as an equation, which compares a codomain's sort by equality. Here a conversion problem carries its relation, so a problem that parks, resumes or solves a metavariable does so at that relation.

It is independent of every other spec, and changes nothing in the kernel: `curios-cert`'s `subsumes` meets no metavariable.

## What this builds on

- **The relation, outside conversion.** `subsume` (`curios-elab/src/typing.rs`) reduces both sides, adds `lower ≤ upper` where both are sorts, and walks two function types through `subsume_telescope`: each domain by `convert_outcome`, the terminal codomains by `subsume` again.
- **Three ways out of the relation.** A side that is no sort and no function type goes to `convert_outcome`. A function type stuck on a metavariable is excluded up front (`stuck_on_metavar`). A rung that answers `Outcome::Blocked` abandons the walk, and the whole pair goes to `convert_outcome`. `subsume_telescope`'s documentation gives the two reasons a rung is not parked: `Context::park` freezes the live frame, which holds the binders the walk assumed, and `retry_one` retries a parked goal through `convert_outcome`.
- **Conversion's problems need no frame.** `Convert` (`curios-elab/src/convert.rs`) queues `Problem { type_, this, that }`, opens binders as bare labels (`Convert::opening`), and surrenders what stays blocked as `Outcome::Blocked(Vec<Problem>)`, which `expect` parks as `ParkedWork::Conversion`. `compare_func_type` enqueues every domain and the codomain as problems.
- **Four places conversion equates levels**: the sort arm of `drain` (`add_eq`), the levels-only shortcut ahead of reduction (`identify_universe_levels`), the same-global spine rule (`compare_same_global_apply`), and `level_question`, at a recurrence and in `drain`'s arm for two applied projections of one group.
- **A flexible side is solved on the spot.** `solve_at_birth` commits `?m := t` for a pattern spine, after re-validating `t` at the metavariable's type.
- **Levels already take a join.** `UniverseSolver::finalize` settles a level bounded from below at its principal lower bound.

## The gap

Each program is refused, and is accepted with the implicit written out.

**A blocked rung decides the codomain by equality.** The domain `Bool` against `?F(0)` blocks, the walk is abandoned, and conversion compares `Prop` with `Type`:

```crs
use /std/{Bool, Nat};
let is(b: Bool) -> Prop = Bool/Holds(b);
let app(@F: (Nat) -> Type, f: (F(0)) -> Type, g: (n: Nat) -> F(n)) -> Nat = 0;
let probe: Nat = app(is, (n) => true);
```

`type mismatch`, `inferred: (b: Bool) -> Prop`, `expected: (?(0)) -> Type`; `app(@(n) => Bool, is, (n) => true)` is accepted.

**A parked subsumption resumes as an equation.** `Prop` against `sort_of(?c)` parks, `Eq/refl()` solves `c`, and the retry compares `Prop` with `Type`:

```crs
use /std/{Bool, Nat, Eq};
let is(b: Bool) -> Prop = Bool/Holds(b);
let sort_of(b: Bool) -> Type = match b | true => Type | false => Prop end;
let pick(@c: Bool, f: (Bool) -> sort_of(c), w: Eq()(c, true)) -> Nat = 0;
let probe: Nat = pick(is, Eq/refl());
```

`type mismatch`, `inferred: Prop`, `expected: Type`; `pick(@true, is, Eq/refl())` is accepted.

**A metavariable met in a subsumption is solved by equality, so a call is decided by the order of its arguments.** With `let both(@S: Type, f: (Bool) -> S, g: (Bool) -> S) -> Nat = 0;`:

```crs
let fam(b: Bool) -> Type = Nat;
let fam1(b: Bool) -> Type = Type;
both(fam1, fam)   -- accepted
both(fam, fam1)   -- refused: this Type would need to be strictly below itself, 1 ≤ 0
both(fam, is)     -- accepted
both(is, fam)     -- refused: inferred (b: Bool) -> Type, expected (b: Bool) -> Prop
```

The first argument solves `S` to its own sort, and the second is then held to it. `same(@S: Type, x: S, y: S)` at `same(Bool/Holds(true), Nat)` against `Type` is the same refusal, and `same(Nat, Bool/Holds(true))` is accepted.

## Prior art

- **Rocq keeps the relation in the problem.** A postponed unification problem is `evar_constraint = conv_pb * env * econstr * econstr`, stored by `add_conv_pb` and taken back by `extract_changed_conv_pbs`, so a problem waiting on an evar resumes as the conversion or the cumulativity it was posed as ([`engine/evd.mli`](https://github.com/rocq-prover/rocq/blob/master/engine/evd.mli)).
- **Rocq solves an evar met under cumulativity to a refreshed copy.** `refresh_universes pbty env evd t` replaces each `Type` sort it reaches by a fresh level constrained toward the original in the problem's direction (`set_leq_sort`), descends into a product's codomain and nowhere else, and returns `Prop`, `SProp` and `Set` as they are ([`pretyping/evarsolve.ml`](https://github.com/rocq-prover/rocq/blob/master/pretyping/evarsolve.ml)). A level is refreshed; a `Prop` is not.
- **Agda under `--cumulativity`** gives the assignment's comparison to the solution's sort check, where without the flag it is equality (`assign`, [`TypeChecking/MetaVars.hs`](https://github.com/agda/agda/blob/master/src/full/Agda/TypeChecking/MetaVars.hs)), and instantiates a level metavariable bounded only from below to the join of its bounds, a heuristic its documentation calls experimental ([*Cumulativity*](https://agda.readthedocs.io/en/latest/language/cumulativity.html)).
- **Lean 4** has no cumulativity, and its unifier one relation.

## Decisions

1. **A problem carries its relation.** `Problem` gains it, equal or below; `expect` asks conversion at below; `Outcome::Blocked` surrenders problems that keep theirs, so `ParkedWork::Conversion` parks and `retry_one` resumes at the relation asked. The history key holds it, as it holds every field.
2. **Conversion decides the relation's three rules.** At below: two sorts add `lower ≤ upper`, `Prop` below any `Type`; two function types of one plicity vector enqueue each domain at equal and the codomain at below; every other pair is the problem at equal. `subsume` and `subsume_telescope` reduce to the entry that asks.
3. **The level shortcuts run at equal alone.** `identify_universe_levels` and `compare_same_global_apply` commit equalities, so a problem at below skips both and reaches the structural arms.
4. **A metavariable met at below is solved to a refreshed copy.** The candidate has each `Type` in a codomain position replaced by a fresh level bounded by the original in the problem's direction, as Rocq's `refresh_universes` does; domains are not entered. `finalize` then settles the fresh level at the join of what bounded it.
5. **`Prop` is not refreshed.** A metavariable first met against `Prop` is solved to `Prop`, and `both(is, fam)` stays refused: the one place the order of a call's arguments decides it, held by a fixture.

## Stages

1. **The relation in the problem**, decisions 1 to 3. The first two programs under *The gap* are accepted. `cargo xtask clippy` elaborates and certifies `/std`, and `curios-prelude-archive`'s `universe_parameter_census` is retaken: a declaration whose count moves is named, each one a pair that reached conversion through an abandoned walk and got an equality there.
2. **Refreshed solutions**, decision 4. `both(fam, fam1)` is accepted in both orders. The census is retaken; a declaration whose count moves is named with the level that stopped being identified.
3. **The stated limit**, decision 5: its fixture, refused in one order and accepted in the other.

## Verification

- `curios/src/tests/board/subsumption_tests.rs` gains the three programs with their controls: a blocked rung, a parked subsumption, and a level met in either order. `both(is, fam)` is held as refused, with `both(fam, is)` its control.
- `curios-elab`'s `convert/solve_tests.rs` holds a problem at below that parks and resumes at below, mutation-checked against resuming at equal.
- `kernel_disagreements` reports zero: every program the elaborator newly accepts, the kernel's `subsumes` accepts.
- `cargo xtask clippy` and `cargo xtask fmt`.

## Rejected

- **A `ParkedWork` variant holding the pair, retried through `subsume`.** It closes the second program and not the first without a second fix: the walk still stops at a blocked rung, so the rungs after it and the codomain solve nothing until the wake, where conversion's queue goes on past a blocked problem. It also leaves two deciders of one relation, the walk and conversion's function-type rule.
- **A bound that waits, solved at the drain to the join of what it met**, which would accept `both(is, fam)`. A metavariable left unsolved until the drain gives later arguments nothing to check against, so witness goals keyed on it, projections and matches all wait with it, and the join of two function types needs a rule of its own. It waits for a program that needs it.
- **Contravariant domains**, for the reason the design decision records.
- **Teaching the kernel anything.** It decides the relation on finished terms already.

## Completion and retirement

Done when every subsumption the elaborator is asked is decided at the relation asked, parked or not, as [Subsumption is a relation, not a traversal order](../../design/theory/subsumption-is-a-relation-not-a-traversal-order.md) states it. [Subsumption and level entailment](../../design/soundness/formation/subsumption-and-level-entailment.md) names the new fixtures, and `subsume_telescope`'s account of why a rung is not parked goes with the walk. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
