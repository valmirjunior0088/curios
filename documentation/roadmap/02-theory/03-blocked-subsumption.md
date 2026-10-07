# A subsumption blocked on a metavariable waits as a subsumption

Working specification for the elaborator deciding subsumption wherever a metavariable stands in the way. [Subsumption is a relation](../../design/theory/subsumption-is-a-relation-not-a-traversal-order.md), and what the elaborator admits may not follow where the metavariables happen to stand: a call is admitted with an implicit left out exactly where it is admitted with that implicit written, whichever argument is read first. Today the elaborator decides the relation only between two rigid sides, hands every other pair to conversion as an equation, and solves a metavariable met in one by equality.

It changes nothing in the kernel: `curios-cert`'s `subsumes` meets no metavariable.

## What this builds on

- **The relation, outside conversion.** `subsume` (`curios-elab/src/typing.rs`) reduces both sides, adds `lower ≤ upper` where both are sorts, and walks two function types through `subsume_telescope`: each domain by `convert_outcome`, the terminal codomains by `subsume` again. Every other pair, a function type stuck on a metavariable and a walk a blocked rung abandoned among them, goes to `convert_outcome` as an equation.
- **Conversion's problems need no frame.** `Convert` (`curios-elab/src/convert.rs`) queues `Problem { type_, this, that, binders }`, the binders it opened carried with the problem, and surrenders what stays blocked as `Outcome::Blocked(Vec<Problem>)`, which `expect` parks as `ParkedWork::Conversion` and `retry_one` retries under those binders. `compare_func_type` enqueues every domain and the codomain as problems, so a blocked one does not stop the rest.
- **A flexible side is solved on the spot, to a copy of its own where it is a family.** `solve_at_birth` commits `?m := t` through `solve_rigid`, which takes a family read off the rigid side at an occurrence of its own (`family_at_its_own_occurrence`), minted and offered in one transaction, and re-validates the candidate at the metavariable's type.
- **A metavariable under an application is imitated or left.** `imitate_flex_apply` answers a nominal former or a unary intrinsic one on the other side with the former over fresh binders, and equates the arguments pairwise; any other pair parks.
- **Levels take a join.** A level nothing holds is minimized or settled at its principal lower bound where its declaration closes (`UniverseSolver::finalize`), and a `@Type` written at a call is such a level: `both(@Type, fam1, f)` closes at `@Type.{max(1,u)}`.
- **A term settles what it left open.** A call checks the arguments it postponed once its own expectation has been met, the force tier of `elaborate_apply`, and a form that can be synthesized settles there and at the drain (`SettleTier`).

## The gap

Each program is refused, and is accepted with the implicit written out. `is` is `let is(b: Bool) -> Prop = Bool/Holds(b);`.

**A blocked rung decides the codomain by equality.** With `let dom(c: Bool) -> Type = match c | true => Bool | false => Nat end;`, the domain `Bool` against `dom(?c)` blocks, the walk is abandoned, and conversion compares `Prop` with `Type`:

```crs
let app(@c: Bool, f: (dom(c)) -> Type, w: Eq()(c, true)) -> Nat = 0;
let probe: Nat = app(is, Eq/refl());
```

`type mismatch`, `inferred: (b: Bool) -> Prop`, `expected: (dom(?)) -> Type`; `app(@true, is, Eq/refl())` is accepted, and so is the call with a family into `Type` in the place of `is`.

**A parked subsumption resumes as an equation.** `Prop` against `sort_of(?c)` parks, `Eq/refl()` solves `c`, and the retry compares `Prop` with `Type`:

```crs
let sort_of(b: Bool) -> Type = match b | true => Type | false => Prop end;
let pick(@c: Bool, f: (Bool) -> sort_of(c), w: Eq()(c, true)) -> Nat = 0;
let probe: Nat = pick(is, Eq/refl());
```

`type mismatch`, `inferred: Prop`, `expected: Type`; `pick(@true, is, Eq/refl())` is accepted.

**A metavariable met in a subsumption is solved by equality, so the first sort it meets holds the rest to it.** With `let both(@S: Type, f: (Bool) -> S, g: (Bool) -> S) -> Nat = 0;`, `let fam(b: Bool) -> Type = Nat;` and `let fam1(b: Bool) -> Type = Type;`:

```crs
both(fam1, fam)                      -- accepted
both(fam, fam1)                      -- refused: this Type would need to be strictly below itself, 1 ≤ 0
[Type, Nat]                          -- accepted, at List(Type.{1})
[Nat, Type]                          -- refused the same way
both(fam, is)                        -- accepted
both(is, fam)                        -- refused: inferred (b: Bool) -> Type, expected (b: Bool) -> Prop
[Nat, Bool/Holds(true)]              -- accepted, at List(Type)
[Bool/Holds(true), Nat]              -- refused: inferred Type, expected Prop
Eq()(Nat, Bool/Holds(true))          -- accepted
Eq()(Bool/Holds(true), Nat)          -- refused
```

A match with no motive answers the same way by the order of its constructors: `match b | true => Type | false => Nat end` is refused and `match b | true => Nat | false => Type end` accepted. An argument is read before the expectation, so it decides against that too: `let some_prop: Option(Type) = Option/some(Bool/Holds(true));` is refused, `inferred: Option(Prop)`, `expected: Option(Type)`, and accepted at `Option/some(@Type, Bool/Holds(true))`.

**A declaration keeps the order in its scheme.** `let outer(f: (Bool) -> Type) -> Nat = both(fam1, f);` is accepted with `S` solved to `fam1`'s sort and `f`'s level bounded by it, so with `let fam2(b: Bool) -> Type = {K: Type, same: Eq()(K, Type)};` the call `outer(fam2)` is refused, `2 ≤ 1`, where `both(f, fam1)` and `both(@Type, fam1, f)` in the body accept it.

**A family applied to its binder is not solved, though it has one solution.** With `let fam_of(@G: (Nat) -> Type, g: (n: Nat) -> G(n)) -> Nat = 0;`, each of `fam_of((n) => Nat)`, `fam_of((n) => n)` and `fam_of((n) => Option/some(3))` is refused, `cannot decide a postponed conversion` between `Type`, `Nat` or `Option(Nat)` and `?(n)`, `never solved: the implicit argument 'G' of '/fam_of'`, and each is accepted with `@(n) => Type`, `@(n) => Nat` or `@(n) => Option(Nat)` written. `?G(n)` against a term over the bound `n` is a pattern, and abstracting `n` is its only solution; against `Option(Nat)` imitation answers `(n) => Option(n)` and then holds `n` to `Nat`. A blocked rung over such a family waits on both: with `let app(@F: (Nat) -> Type, f: (F(0)) -> Type, g: (n: Nat) -> F(n)) -> Nat = 0;`, `app(is, (n) => true)` is refused as a `type mismatch` at `is`, and with a family into `Type` in the place of `is` it is refused still, `never solved: the implicit argument 'F'`; `app(@(n) => Bool, is, (n) => true)` is accepted.

**A goal is offered only what converts.** At `let pick(is: (b: Bool) -> Prop, fam: (b: Bool) -> Type) -> (Bool) -> Type = ?;` the report offers `fam` and not `is`, which checks there: the scope fit and `apply_fit` (`curios-elab/src/suggest.rs`) ask conversion.

## Prior art

- **Rocq keeps the relation in the problem.** A postponed unification problem is `evar_constraint = conv_pb * env * econstr * econstr`, stored by `add_conv_pb` and taken back by `extract_changed_conv_pbs` ([`engine/evd.mli`](https://github.com/rocq-prover/rocq/blob/master/engine/evd.mli)), and `evar_define` is handed the relation and the side the evar stands on ([`pretyping/evarsolve.ml`](https://github.com/rocq-prover/rocq/blob/master/pretyping/evarsolve.ml)).
- **Rocq solves an evar met under cumulativity to a refreshed copy.** Where the solution is an arity, `refresh_universes` descends its product codomains and replaces a `Type` whose universe is algebraic or foreign to the evar map by a fresh level bounded by the original in the problem's direction (`set_leq_sort`); a level of the evar map's own it leaves, since later constraints still move it, and `Prop`, `SProp` and `Set` it returns as they are.
- **Rocq delays the choice between `Prop` and `Type` on a sort it does not know.** Since 8.18 "the universe unification algorithm … is now able to delay the definition of a sort" ([#16903](https://github.com/rocq-prover/rocq/pull/16903)); `Prop ≤ q` under cumulativity is recorded on the sort variable and not solved (`set_above_prop`, [`engine/uState.ml`](https://github.com/rocq-prover/rocq/blob/master/engine/uState.ml)), and minimization collapses it at the end of the definition: "The choice of assignment can be delayed during type inference", and "If there is only a constraint `Prop <= q` then `q` will be set to Prop, otherwise it will be set to Type" ([reference manual, *Universe polymorphism*](https://rocq-prover.org/doc/master/refman/addendum/universe-polymorphism.html); the first half since 9.3, [#22170](https://github.com/rocq-prover/rocq/pull/22170)). Its `Prop` is relevant, so nothing reads the choice meanwhile; for the sort that is irrelevant, "In order to keep conversion tractable, cumulativity for `SProp` is forbidden" ([reference manual, *SProp*](https://rocq-prover.org/doc/master/refman/addendum/sprop.html)).
- **Agda under `--cumulativity`** solves a term metavariable to the other side and gives the comparison to the solution's sort check (`assign`, [`TypeChecking/MetaVars.hs`](https://github.com/agda/agda/blob/master/src/full/Agda/TypeChecking/MetaVars.hs)), a level being a term of its own: `a ≤ _l` is solved as `_l := a ⊔ _l'` at a fresh `_l'` (`leqLevel`, [`TypeChecking/Conversion.hs`](https://github.com/agda/agda/blob/master/src/full/Agda/TypeChecking/Conversion.hs)), and a level bounded only from below is instantiated to the join of its bounds at the end of each signature or declaration, a heuristic its documentation calls experimental ([*Cumulativity*](https://agda.readthedocs.io/en/latest/language/cumulativity.html)).
- **Languages with subtyping decide a type argument from every argument.** Java collects an inference variable's bounds across the invocation and resolves it afterwards, "If αi has one or more proper lower bounds, L1, ..., Lk, then Ti = lub(L1, ..., Lk)" ([JLS §18.4](https://docs.oracle.com/javase/specs/jls/se21/html/jls-18.html)).
- **Lean 4** has no cumulativity, and its unifier one relation.

## Decisions

1. **A problem carries its relation.** `Problem` gains it, equal or below; `expect` asks conversion at below; `Outcome::Blocked` surrenders problems that keep theirs, so `ParkedWork::Conversion` parks and `retry_one` resumes at the relation asked. The recurrence key holds it. A run carries the span of the term it was asked for, which a level bound raised inside it reports as `subsume`'s does today.
2. **Below is decided as equal is, in all but four places.** A problem at below has its two sides reduced first. Two sorts add `lower ≤ upper`, `Prop` below any `Type`. Two function types of one plicity vector enqueue each domain at equal and the codomain at below. A metavariable against a rigid side is decisions 3 and 4. Any other pair goes through conversion's steps as the equation it is today, from its written spelling, the level shortcut and the spine rule among them: an equation is sufficient for a subsumption, and it is the whole of one wherever neither side can come to a sort or a function type. Where those steps leave the problem waiting on a metavariable, it waits at below. `subsume` and `subsume_telescope` reduce to the entry that asks.
3. **A metavariable met at below against a `Type` takes levels of its own.** Where the rigid side ends in `Type u` — a sort, or a function type whose codomain, read reduced as the relation reads it, ends in one — the candidate has that `Type` at a fresh level bounded by the original in the problem's direction, minted and offered in the transaction `solve_rigid` gives a family's occurrence. The fresh level stands for the `Type` the program would have written at the implicit: it takes the provenance of the level it copies where that is one level, and an invented classifier's otherwise, so the call elaborates to what it elaborates to with `@Type` written.
4. **A metavariable met at `Prop` from below waits for the term that minted it.** `Prop ≤ ?m`, or a function type ending in `Prop` below `?m`, has two solutions the sorts below tell apart and the first argument cannot. Where `?m` is awaited the problem waits at below: a later `Type` solves `?m` by decision 3 and the wake finds `Prop` below it, and an equation solves it outright. A term awaits what it minted while it reads its parts — a call the implicits it inserts and those its head's spine still leaves open, as `Eq()(x, y)` does; a structure literal the parameters and hidden fields it leaves out; a list literal its element type; a match its elided motive — and once they are read, a call after its expectation is met and before its force tier, each problem still waiting is taken as the equation, which is the answer given today. A metavariable no term awaits any longer is solved on the spot, as today.
5. **A goal's candidates are asked at below.** The scope fit and `apply_fit` ask what `check` would; `probe_match`, which witness resolution shares, stays at equal.
6. **A metavariable applied to distinct bound variables is solved by abstraction.** `?m(x̄)` against `t`, each of `x̄` a distinct local variable the metavariable's own spine does not carry, has the one solution `(x̄) => t`, over the binders of the metavariable's frozen type; it is offered to `solve` ahead of imitation, which stays the rule for arguments that are not such variables. Decisions 3 and 4 read it as the metavariable alone under those binders.

## Stages

1. **A family applied to its binders**, decision 6. The three `fam_of` calls are accepted, and `app` with a family into `Type`. `cargo xtask clippy` elaborates and certifies `/std`, and `curios-prelude-archive`'s `universe_parameter_census` is retaken: a declaration whose line moves is named.
2. **The relation in the problem**, decisions 1, 2 and 5. The blocked rung, the parked subsumption and `app(is, (n) => true)` are accepted, and the goal's report offers `is`. The census is retaken: a declaration whose line moves is named, each one a pair that reached conversion through an abandoned walk and got an equality there. The rustdoc of `curios-cert`'s `subsumes` stops saying the elaborator never forms the function type it subsumes.
3. **Levels of its own**, decision 3. The level rows of the third program are accepted in both orders, the match in either, and `outer(fam2)`; each call's `core-elab` reprint is its written twin's. The census is retaken; a declaration whose line moves is named with the level that stopped being identified.
4. **The waiting metavariable**, decision 4. The `Prop` rows are accepted in both orders, and `some_prop`. `/std` elaborates to what it did, by the census and by `kernel_disagreements`; a declaration that moves is named.

A timed figure a stage may have moved is named and not retaken; the census, `kernel_disagreements` and a profile's samples are counted.

## Verification

- `curios/src/tests/board/subsumption_tests.rs` gains each program under *The gap* with its control: a blocked rung, a parked subsumption, a level met in either order in a call, a list and a match, the declaration whose caller was refused, and a proposition met before a type in a call, a list, a two-group call and against an expectation.
- `curios-elab`'s `convert/solve_tests.rs` holds a problem at below that parks and resumes at below, mutation-checked against resuming at equal; a metavariable met at `Type` from either side, with the bound it takes; and one met at `Prop` that waits where it is awaited and is solved where it is not.
- `curios-elab`'s `convert/imitation_tests.rs` holds the pattern, a duplicated variable and a variable the spine shares, which wait, and `?M(?A)`, which is imitated as before.
- `curios-pipeline`'s `goal_tests.rs` holds the candidate that fits by subsumption, and its `inference_tests.rs` the `fam_of` calls.
- `kernel_disagreements` reports zero: every program the elaborator newly accepts, the kernel's `subsumes` accepts.
- `cargo xtask clippy` and `cargo xtask fmt`.

## Rejected

- **A `ParkedWork` variant holding the pair, retried through `subsume`.** It closes the second program and not the first without a second fix: the walk still stops at a blocked rung. It also leaves two deciders of one relation.
- **Skipping the level shortcut and the spine rule at below**, so a pair at below commits no equality. With `let Handler(c: Bool) -> Type = match c | true => (Nat) -> Nat | false => (Bool) -> Bool end;`, `make(given)` for `given: Handler(true)` against `Handler(c)` solves `c` by the spine rule alone, a stuck type against a function type, and is accepted today.
- **Refreshing only a level that cannot move**, as Rocq does. A written `@Type` is a level of its own whatever it stands beside, and a call that left one out would bound its caller's level by the order of its arguments, `both(f, fam1)` putting `1 ≤ u` on `outer` where `both(fam1, f)` puts nothing.
- **`Prop` kept wherever it is met first.** It is the one rule under which the order of a call's arguments, of a list's elements or of an argument and the expectation decides the call.
- **`Prop` raised to a fresh `Type` where it is met first.** `id(Bool/Holds(b))` would no longer stand where a `Prop` is wanted.
- **A sort variable in the solver, or the choice left to the item's drain**, as Rocq leaves it to minimization. Rocq's `Prop` is relevant; here whether a type is a proposition is read once and kept wherever it is read — conversion's irrelevance, the large-elimination guard, erasure's classing, an implicit minted as a bound — so a choice left open past the term that minted it is one each of those would answer before it is made.
- **A join computed over the bounds a metavariable met.** The level solver already takes it for every `Type`, and the join of a `Prop` with nothing else is the equation.
- **Imitation ahead of abstraction.** For a metavariable applied to variables, abstraction is the solution and imitation a guess: `?G(n)` against `Option(Nat)` is imitated as `(n) => Option(n)`, which holds `n` to `Nat`.
- **Contravariant domains**, for the reason the design decision records.
- **Teaching the kernel anything.** It decides the relation on finished terms already.

## Completion and retirement

Done when every subsumption the elaborator is asked is decided at the relation asked, parked or not, and no program under *The gap* is told from its written twin, as [Subsumption is a relation, not a traversal order](../../design/theory/subsumption-is-a-relation-not-a-traversal-order.md) states it. `subsume_telescope`'s account of why a rung is not parked goes with the walk. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
