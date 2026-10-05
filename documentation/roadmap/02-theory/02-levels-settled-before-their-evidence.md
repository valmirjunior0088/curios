# A universe level settled before its evidence is in

Working specification for two places where [a universe level that settles by where it came from](../../design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-came-from.md) settles before what would decide it is known: two instances whose levels conversion commits equal without unfolding, and a signature's level that settles at zero through a bound a witness's side condition put on it. The first is measured before anything is changed; the second has no rule chosen yet.

What a nominal type's levels compare as, the weak equation the commitments below record at a nominal type's irrelevant level, and the closing of a witness resolved while its consumer is open are that decision's; what is held here is independent of them.

## What this builds on

- **A goal is resolved while its declaration is open**, wherever its witness is written ([A declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md)), and a witness's levels close with the declaration that resolves it, so a witness's side condition is live when `UniverseSolver::settle` runs over a signature's occurrences.
- **Four places commit two levels equal.** `identify_universe_levels` (`curios-elab/src/convert/neutral.rs`) is asked by `drain` ahead of reduction for two sides differing only in levels, by `compare_same_global_apply` once two spines of one polymorphic global have converted, and by `level_question`, at a recurrence and in `drain`'s arm for two applied projections of one group. It commits a pair where either level is undecided, and declines, inserting nothing, on two unequal ground levels and on a pair under a universe binder.

## The gap

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

- **Rocq commits a level unification that unfolding might not have needed, and gives up the attempt where it cannot.** "Lub constraints … correspond to unification of two levels which might not be necessary if unfolding is performed. UWeak constraints come from irrelevant universes in cumulative polymorphism" ([`engine/univProblem.mli`](https://github.com/rocq-prover/rocq/blob/master/engine/univProblem.mli)). `ULub (l, r)` is processed as `equalize_variables true l r`: a flexible level is instantiated to the other, and two rigid levels that differ raise `UniversesDiffer`, failing the first-order attempt. Only `UWeak` waits: it is stored while `Cumulativity Weak Constraints` is set, and at minimization becomes an equality or is forgotten ([`engine/uState.ml`](https://github.com/rocq-prover/rocq/blob/master/engine/uState.ml)). Identification is `ULub`'s rule already: commit where a level is undecided, decline where both are decided.
- **A weak constraint is licensed by irrelevance**, which is why Rocq may forget one: the two instances convert whatever the levels are. That license belongs to a nominal type's irrelevant level, where a pair met is the weak equation the universe decision states, and no definition has one.

## Decisions

1. **Identification is measured before it is touched.** It is Rocq's rule, and the one program here it refuses is accepted eta-expanded.

## Stages

1. **The census of commitments.** Over `/std`, each call of `identify_universe_levels` that commits is counted by its site, with whether the pair it committed is one class when its declaration finalizes with the commitment withheld. The protocol: withhold the commitment at one site at a time, answer `Distinct` there so the structural path decides, and retake `curios-prelude-archive`'s `universe_parameter_census`; a declaration whose count moves, or that stops elaborating, is named. The reading is `counted`, at the commit it is taken at. If nothing moves, this half retires with a sentence in the design decision; if something does, the pairs that moved say what rule replaces the commitment.

## Verification

- The census as its protocol states it.

## Rejected

- **Weak constraints for identified levels.** A constraint that may be forgotten needs the conversion it justified to hold without it, which for a definition means unfolding it, the cost identification exists to avoid.
- **Reading a bound only once the level it names has settled.** It keeps `all`'s level and leaves `field`'s and `fill`'s where they are, and it gives twenty-two of `/std`'s definitions a parameter no caller needs, `/std/Io/read_line`'s and `/sys/Handle/read`'s among them: an `Io` over a payload at zero stops following its payload down, which is what settling an occurrence at its argument is for.

## Completion and retirement

Done when the census is taken and acted on, and a signature's level is not settled through a witness's side condition. The design decision carries what remains. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
