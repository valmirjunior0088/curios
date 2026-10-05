# A universe level settled before its evidence is in

Working specification for three places where [a universe level that settles by where it came from](../../design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-came-from.md) settles before what would decide it is known: a declaration whose witness goal resolves after its scheme has closed, two instances whose levels conversion commits equal without unfolding, and a signature's level that settles at zero through a bound a witness's side condition put on it. The first is closed by [one environment](../05-compilation/02-one-environment.md)'s first stage and held here; the second is measured before anything is changed; the third has no rule chosen yet.

[`!` sequences actions whatever level their payloads sit at](01-bang-sequences-at-every-level.md) takes what a nominal type's levels compare as, what the commitments below pair at a nominal type's irrelevant level, and the closing of a witness resolved while its consumer is open; what is held here is independent of it.

## What this builds on

- **A goal with no table entry defers.** `resolve_witness` answering `Resolution::Missing` sends the goal to `Context::defer_witness`; `retry_deferred_witnesses` retries the store after every item (`elaborate_module_item`), and `finish_deferred_witnesses` reports what is left once the unit has elaborated (`curios-elab/src/resolve.rs`).
- **A declaration closes over its pending goals.** `UniverseSolver::finalize` takes the levels a still-deferred goal names as `pending` and settles them, and every level a settlement lands on, until the goal is ground (`curios-elab/src/universe_solver.rs`). The design decision states it: the goal's least levels are the one assignment every witness answers.
- **A late resolution cannot reach the scheme.** The witness's instance is pinned to the levels the goal fixed; a level that slips through is a `UniverseInvariant` attributed to the item that raised the goal, and a constraint the witness brings reaches no scheme.
- **The lowering orders some uses after their witnesses.** `witness_dep_nodes` (`curios-text/src/into_core/order.rs`) adds a soft edge from an item to every witness row of a concept it reaches by infix operator or by naming one of the concept's method wrappers. Postfix `!` adds none, by a stated choice: `!` cannot appear in a type, and its edges would widen the deadlocks the soft edges already meet. A soft edge is dropped where it deadlocks.
- **Four places commit two levels equal.** `identify_universe_levels` (`curios-elab/src/convert/neutral.rs`) is asked by `drain` ahead of reduction for two sides differing only in levels, by `compare_same_global_apply` once two spines of one polymorphic global have converted, and by `level_question`, at a recurrence and in `drain`'s arm for two applied projections of one group. It commits a pair where either level is undecided, and declines, inserting nothing, on two unequal ground levels and on a pair under a universe binder.

## The gap

**A witness declared after its use leaves the using declaration at its least levels.** Through `!`:

```crs
use /std/{Monad, Nat};
pub struct Box(A: Type): pub Type { value: A }
let rewrap(@A: Type, b: Box(A)) -> Box(A) = let a = b!; Box { value = a };
satisfy Monad(Box) { pure(@_, a) = Box { value = a }, bind(@_, @_, m, f) = f(m.value) }
let big: Box(Type) = rewrap(Box { value = Nat });
```

and through a `use` premise of a function another unit declares, `Test/equal(@A: Type, use Eql(A), use Spell(A), …)`:

```crs
let same(@A: Type, b: Box(A)) -> Test = Test/equal(b, b);
satisfy (@A: Type) => Eql(Box(A)) { eql(_, _) = true, neq(_, _) = false, }
satisfy (@A: Type) => Spell(Box(A)) { spell(_) = "box", }
let big: Test = same(Box { value = Nat });
```

Each is refused at `big`, and each is accepted with the witness declared first. `tests::universes::a_goal_deferred_past_its_declaration_settles_at_its_least_levels` pins the first as a parameter count of zero against one. A `use` premise reached through a function of the same unit whose body names a method wrapper is ordered by that function's soft edge, and is accepted.

**The refusal says none of it.** Both read `type mismatch`, `inferred: Type`, `expected: ?`, "a type was given where a value of it was expected". It names neither the level, nor the declaration that sits at it, nor the witness whose position put it there. Where the report is raised has not been traced: it reads as a candidate solution, `?A := Type`, refused on re-validation at a level `rewrap` no longer offers, and stage 1 begins by confirming that.

**A commitment unfolding would not need.** Two instances of one definition whose body does not carry the level convert by unfolding with their levels unrelated; identification relates them. A bare former passed as a family is the program it refuses:

```crs
use /std/{Nat, Io};
let small(n: Nat) -> Io(Nat) = Io/pure(n);
let through(F: (Type) -> Type, x: F(Nat), y: F(Type)) -> Nat = 0;
let probe(n: Nat) -> Nat = through(Io, small(n), Io/pure(Nat));
```

It is refused, `this Type would need to be strictly below itself`, `1 ≤ 0`, and is accepted with `(A) => Io(A)` in `Io`'s place. `List` in `Io`'s place and an alias, `let Act(A: Type) -> Type = Io(A)`, are refused alike, and `curios`'s `tests::universes::a_bare_former_passed_as_a_family_is_held_at_one_instance` holds them. The cause is not traced; it reads as `F(Nat)` being `Io(Nat)` at the instance `F` was passed at, so the sides differ in levels alone and the pair is committed before `Io` unfolds to a former that carries none, where under the lambda `F(Nat)` is a redex, the sides differ in more than levels, and both are reduced. What the commitment costs across `/std` is not known: an occurrence's level settles at its floor, which two occurrences over one argument share, so a pair identification commits may be a pair `finalize` would have equated anyway.

The commitment reaches `!` through a transformer, and there it is traced. With `large(n: Nat) -> Try(Io, Str, Type) = Try/pure(Nat)` and `huge(n: Nat) -> Try(Io, Str, Type)` binding `large(n)!` before answering `Try/pure(Type)`, the action's `Io` sits at one decided level and the region's a level above: checking the action, `drain`'s levels-only shortcut meets the region's bare `Io` against the action's and `identify_universe_levels` commits the region's level to the action's, `?u ≤ 1`, which the region's payload then contradicts, `2 ≤ ?u`. `Try/bind` and `Monad/bind` written out are accepted over the same pair, one side there being a metavariable's solution and both compared reduced, and so is `!` over `(A: Type) => Io(A)`. `curios`'s `tests::concepts::monad_tests::a_bang_over_a_bare_base_monad_holds_its_region_at_a_decided_actions_level` holds it.

**A level a witness's side condition bounds settles at zero.** `Value` carries a type, so its level is its caller's to choose:

```crs
use /std/{Str, List, Result};
pub struct Value: pub Type { A: Type, read: (Str) -> Result(Str, A) }
let all(v: Value, xs: List(Str)) -> Result(Str, List(v.A)) =
    let read_with(w: Value, s: Str) -> Result(Str, w.A) =
        Result/map_failure(w.read(s), (reason) => Str/concat("bad: ", reason));
    List/traverse(xs, (s) => read_with(v, s));
```

`all` elaborates at `Value.{0}`, and so cannot be handed a `Value` whose `A` is `Type`; with the local definition alone, or with `List/traverse` alone, the same function keeps `Value`'s level as a parameter (`tests::universes::a_level_bounded_through_a_witnesses_side_condition_settles_at_zero`). The `Monad` witness for `(A) => Result(E, A)` requires the error type's level to be at most the payload's, since the monad takes `Type a` to `Type a`. A witness's levels close with the declaration that resolves it, so that condition is live when `UniverseSolver::settle` runs over the signature's occurrences: the error type's level is floored at zero and not yet settled, the payload's, visited first, takes it as its principal lower bound, and when the error type's settles at zero the payload's is zero with it. Following a bound down is what settles `Io(List(Str))` at zero, as the universe decision asks, so the order is not the defect. What separates the two is where the bound came from, an occurrence's own argument or a witness's side condition, and the solver keeps no such difference. `/std/Cli`'s `field` and `fill` lose their `Kind`'s level by the census the same way and are refused nothing, their callers sitting at zero; whether their cause is this one exactly is not traced.

## Prior art

- **GHC reads every instance head before it checks any body.** "Typechecking instance declarations is done in two passes. The first pass, made by `tcInstDecls1`, collects information to be used in the second pass", whose bindings "are type-checked in the second pass, when the class-instance envs and GVE contain all the info from all the instance and value decls. Indeed that's the reason we need two passes over the instance declarations" ([`GHC.Tc.TyCl.Instance`](https://hackage.haskell.org/package/ghc-9.6.4/docs/src/GHC.Tc.TyCl.Instance.html)). So what a binding resolves through does not depend on where an instance is written. The design decision rejects taking a witness's scheme from its head, since a Curios witness's scheme depends on its body; what a head does give is its key.
- **Rocq commits a level unification that unfolding might not have needed, and gives up the attempt where it cannot.** "Lub constraints … correspond to unification of two levels which might not be necessary if unfolding is performed. UWeak constraints come from irrelevant universes in cumulative polymorphism" ([`engine/univProblem.mli`](https://github.com/rocq-prover/rocq/blob/master/engine/univProblem.mli)). `ULub (l, r)` is processed as `equalize_variables true l r`: a flexible level is instantiated to the other, and two rigid levels that differ raise `UniversesDiffer`, failing the first-order attempt. Only `UWeak` waits: it is stored while `Cumulativity Weak Constraints` is set, and at minimization becomes an equality or is forgotten ([`engine/uState.ml`](https://github.com/rocq-prover/rocq/blob/master/engine/uState.ml)). Identification is `ULub`'s rule already: commit where a level is undecided, decline where both are decided.
- **A weak constraint is licensed by irrelevance**, which is why Rocq may forget one: the two instances convert whatever the levels are. That license belongs to [irrelevant universe levels](01-bang-sequences-at-every-level.md), where a pair met at a nominal type's irrelevant level is a weak equation, and no definition has one.

## Decisions

1. **Deferral is removed, not scheduled around.** One environment resolves a goal against a key index built from every `satisfy` head before any body elaborates, and choosing a witness waits for its declaration; the deferred store, its sweeps, `finalize`'s `pending` and the settlement loop behind it are deleted there. The design decision rejects elaborating a missing witness on demand because elaboration would have to be re-entrant, which holds of one context per unit; one environment's context per item is what answers that reason. No edge is added to the lowering's sort, which that spec's second stage deletes.
2. **Until then the refusal states the level.** A candidate solution refused for a level alone is reported as the level constraint, with the declaration whose level it is, as a call above a recursive group's level already is.
3. **Identification is measured before it is touched.** It is Rocq's rule, and the one program here it refuses is accepted eta-expanded.

## Stages

1. **The report.** A candidate that fails re-validation on a universe constraint raises that constraint's report, naming the declaration that fixed the level, in place of the mismatch against `?`. Checked by a diagnostic fixture over both programs under *The gap*.
2. **The acceptance, with one environment's first stage.** Both programs are accepted in either order; the fixture's first count turns to one and its name to what then holds; the design decision loses its clause on a deferred goal made ground and the rationale beside it, and of its rejections, elaborating a missing witness on demand and retrying goals before the scheme closes are restated as what is done, while requiring a witness before its use, taking a scheme from a head and fixing a scheme from the signature stay rejected. One environment's list of the decisions it overturns does not name this one. This stage is that change's, listed here so it is not lost.
3. **The census of commitments.** Over `/std`, each call of `identify_universe_levels` that commits is counted by its site, with whether the pair it committed is one class when its declaration finalizes with the commitment withheld. The protocol: withhold the commitment at one site at a time, answer `Distinct` there so the structural path decides, and retake `curios-prelude-archive`'s `universe_parameter_census`; a declaration whose count moves, or that stops elaborating, is named. The reading is `counted`, at the commit it is taken at. If nothing moves, this half retires with a sentence in the design decision; if something does, the pairs that moved say what rule replaces the commitment.

## Verification

- Stage 1: the two refusals name the level constraint and the declaration, and no other diagnostic fixture moves.
- Stage 2: `tests::universes` holds both programs in both orders, and `/std/tcp/Listener`'s `close_raising` and `/std/Io`'s `Read(Async, Input)` witness, the two cases `finalize` names for the settlement loop, elaborate and certify without it.
- Stage 3: the census as its protocol states it.

## Rejected

- **Soft edges for `!` and for `use` premises.** A `use` premise is reached through any function that takes one, so the edge is the call graph's closure; a soft edge dropped at a deadlock leaves the goal deferred and the declaration grounded, so the fix holds only where the sort happens not to deadlock; and the sort itself is deleted by one environment.
- **Weak constraints for identified levels.** A constraint that may be forgotten needs the conversion it justified to hold without it, which for a definition means unfolding it, the cost identification exists to avoid.
- **Generalizing a pending goal's levels.** `finalize`'s documentation names the two `/std` declarations the kernel then refuses.
- **Reading a bound only once the level it names has settled.** It keeps `all`'s level and leaves `field`'s and `fill`'s where they are, and it gives twenty-two of `/std`'s definitions a parameter no caller needs, `/std/Io/read_line`'s and `/sys/Handle/read`'s among them: an `Io` over a payload at zero stops following its payload down, which is what settling an occurrence at its argument is for.

## Completion and retirement

Done when a declaration's levels do not depend on where its witnesses are declared, the interim report is in, the census is taken and acted on, and a signature's level is not settled through a witness's side condition. The design decision carries what remains. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
