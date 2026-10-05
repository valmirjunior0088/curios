# A term is one where conversion says so

Working specification for the places where a verdict still follows how a term is spelled. [An atom is one where conversion says so](../../design/arithmetic/an-atom-is-one-where-conversion-says-so.md) settles the question where two intrinsics meet in conversion; reduction still answers it by spelling, in a guard's equation and in a fold. So the laws and the guards do not compose: `n + m` and `m + n` are one term, an arm's scrutinee is `true`, and under `match n + m < k` the term `m + n < k` is not `true`.

## What this builds on

- **The chain and its classing.** `curios-analysis`'s `convert_intrinsics` reads a pair through the carriers' algebra, hands the pair's atoms back where its readers decide nothing, and reads again once each checker has classed them by its own conversion (`curios-core`'s `Classes::of`, `atoms_of`, `classed`). It calls no judgment.
- **The case equations.** One rule records an equation for both checkers (`curios-analysis`'s `records_case_equation`), under the scrutinee as written, the spelling a dispatch resolves to, and a reduct settled at most once, with the equation and every equation inside it withheld ([Case equations and their key](../../design/soundness/elimination/case-equations-and-their-key.md)). A stuck comparison is also asked under its dual and its successor spelling (`curios-core`'s `probe_spellings`).
- **The audit.** `tests::laws::audit` holds conversion symmetric, transitive and closed under substitution at every declared law, each stated at the top of an item, outside any arm ([The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md)).
- **The decisions it keeps.** The refinement store is a lookup and not a theory, and a hypothesis reaches a bound through a proof ([A bound is stated in a decided proposition and discharged by reduction](../../design/arithmetic/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)); a fold does not respell ([A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)).

## The gap

Each is a program this tree's compiler refuses, put to it on standard input, whose two spellings it holds equal at the top of an item.

**A question is answered by plain reduction, so an answer that needs a question inside a question is refused.** Where the shared rule's readers decide nothing of a stuck form and an equation's scrutinee, a judgment's reduction puts the question to its checker's conversion, which answers by plain reduction: reduction that asks nothing. So under `match f(a + b) < 10` and, in its `true` arm, `match h(f(a + b) < 10) < 5`, the term `h(f(b + a) < 10) < 5` is not `true`: it is the inner guard's scrutinee only to a conversion that asks the outer guard about the respelled argument. `f(b + a) < 10` is `true` there, one question deep. The same holds of a fold: `h(f(a + b) == f(b + a)) == h(true)` does not reduce, its two calls being one only to a conversion that takes the fold in the first one's argument again.

**An equation is put only to a form over its key's own binders, proofs apart.** Which equations a stuck form is asked against is a cost filter, `curios-analysis`'s `could_reduce_to`: it keeps a key's reduced spelling from being settled for every stuck form in its arm. It counts the binders the form names that the key does not, and passes over one that is itself a proof, so `w(a, p2) < 5` is `true` under `match w(a, p1) < 5`. A binder that stands where reduction would erase it is counted: under `match f(b) < 10`, `f(b + 0 * c) < 10` converts with the guard outside the arm and is not `true` inside it. So is one inside a proof term that is no variable.

**Each checker still has a lookup of its own around the shared rule.** The kernel's is `whnf`'s `refined_reduct` over `Scope`; the elaborator's is `reduce`'s probe before decomposition, `refined_after_fold` and `refined_reduct` over its term-keyed store. Both ask `answers` of each settled spelling, and they differ in what reaches it: the elaborator escalates a key it missed to its arguments reduced, capped by an allowance, which the kernel does not; its key erases universe instances and declines at the read; and it records the kernel's spelling of a guard over local definitions as an alias. Under `let n = m + 0; match Nat/in_range(n, 240, 244)` the elaborator accepts `Nat/le/of_in_range(m, 240, 244, True/qed())` and the kernel refuses it. [The findings](00-findings.md) hold a second direction that does not reproduce. Settling reduced spellings is a measured cost of elaboration, and a settled spelling is kept beside its entry for as long as nothing can change what its key reduces to (`Frames::scrutinee_spellings`, `curios-elab/src/context/frames.rs`).

## The goal

No respelling moves a verdict: where conversion holds two terms equal at the top of an item, either stands for the other in any position, under any arm, and both checkers answer as before. That is conversion a congruence under every fold, stable under the equation an arm assumes, closed under the substitution an arm performs, and decided by one rule in both checkers.

The end state is reduction defined with conversion at two points, and nowhere else: a stuck fold's atoms are classed by the checker's conversion before the fold reads them, and a stuck term is answered by an equation in force whose key the checker's conversion equates with it. Coq Modulo Theory fires its eliminations by matching modulo the theory (Jouannaud and Strub, 2017), and the smart case defines normalisation and convertibility under an arm's equations mutually (Altenkirch, 2011).

## Settled

- **The store stays a lookup.** An equation answers the terms conversion equates with its scrutinee and nothing else: no consequence of two equations, no hypothesis, and an `==` guard does not equate its sides.
- **The shared layer calls no judgment.** `curios-core` and `curios-analysis` hand atoms and candidates to the checker, as the chain does.
- **A fold classed again is kept only where it decides more.** A stuck operation whose classed spelling folds no further keeps the spelling it had.
- **Two reductions, and a question answered by the plain one.** A judgment's reduction asks its checker's conversion at a missed equation and at a stuck fold. Plain reduction asks nothing. It answers every question, which makes a question one conversion the budget bounds, and it is what a shared analysis reads a term by (`Env::force`): conversion's proof irrelevance and its recurrence rule are sound because acceptance waits on totality, so whether a term is total may rest on no verdict of conversion's ([Definitional proof irrelevance](../../design/soundness/conversion/definitional-proof-irrelevance.md)). Each keeps its own reducts, so a reduct is a function of the term, the definitions, the equations in force and which of the two took it.
- **Not a mutual definition bounded by the budget alone, not a fixpoint with cycles detected, not a third reduction.** The first is complete to any depth with no argument for its soundness to stand on, and a question starts a history of its own, so a comparison the recurrence rule closes today would regress until the budget ran out. Under the second a reduct taken while a lookup declined is less reduced than the same term reduced outside it, so every memo would need to know and a verdict would follow the order terms were reduced in. The third answers a question inside a question by the same argument once more, and waits for a program that needs it.
- **Not canonical forms.** A spelling per class would make identity the relation, and there is none: `Bool` has no normal form the decisions accept, irrelevance is typed, and an atom's arguments would need full normal forms.
- **Not a congruence-closure store.** It decides the consequences of two equations, which is hypotheses in conversion.
- **Not completion at the judgment alone.** Conversion completing a fold or a lookup only where it compares leaves a scrutinee inside reduction decided by spelling.
- **A dead arm is refused where it is written, not excused.** Where an arm's case contradicts a guard around it, the guard's equation answers nothing in that arm, in either checker, and a proof resting on it is the elaborator's to refuse, with the note that the arm is never taken. Excusing the arm as impossible would make a contradicted guard a source of coverage.

## Open

- What reduction asking conversion costs `/std`'s elaboration and certification in time, which wants a quiet machine; the count below is what it asks.
- Whether index inversion should read an index by a judgment's reduction. It forces through `Env::force`, so it reads by plain reduction, which is the refusing direction, and no cell of the grid reaches it.
- Whether a reduct the elaborator took by asking may outlive the universe solver's state it was answered under. Its question answers no where the pair would need a solution or a level constraint committed and yes on the constraints that stand (`convert`'s `same_uncommitted`), while a reduct is remembered whatever its levels (`Context::reduce`) and a discarded universe transaction clears no reduct (`Caches::invalidate_for_universe_transaction`). So a stuck reduct can outlive a no the solver later turns into a yes, which refuses, and a decided one a constraint that is withdrawn, which the kernel would then refuse. Either needs two spellings of one atom at different unsolved levels. The kernel has no solver and is not affected.
- Which of the special spellings the last stage retires, and which a test still needs for what a probe may spend.
- Whether the second key direction in [the findings](00-findings.md) is a third program or a difference since closed.

## The respelling grid

A grid states the goal as a test, `curios`'s `tests::respelling`, each cell one program put to both checkers and stated as held, refused, or refused by the kernel alone, so a stage is cells moving.

The binders are `a: Nat, b: Nat, c: Nat, f: (Nat) -> Nat, w: (n: Nat, at: Holds(n < 10)) -> Nat, p1: Holds(a < 10), p2: Holds(a < 10), u: ((Nat) -> Nat) -> Nat, i: Int, j: Int, v: (Int) -> Int, xs: List(Nat), flag: Bool, g: (Bool) -> Nat`, swept over orders drawn as `tests::laws` draws them.

- **Guard rows**, a guard and a respelling of it: `a + b < 10` and `b + a < 10`; `a + b + c < 10` and `c + a + b < 10`; `a + b < c + 10` and `b + a < 10 + c`; `a == b` and `b == a`; `f(a + b) < 10` and `f(b + a) < 10`; `w(a, p1) < 5` and `w(a, p2) < 5`; `u((x) => x + a) < 5` and `u((x) => a + x) < 5`; `Nat/in_range(a + b, 1, 5)` and `Nat/in_range(b + a, 1, 5)`; and `f(b) < 10` and `f(b + 0 * c) < 10`, which is stated refused in every arm.
- **Guard columns.** The control, `Eq()(guard, respelling)` by `Eq/refl()` at the top of an item, which every row holds. In the guard's `true` arm: `Holds(respelling)` by `True/qed()`, the same by `True/proved()`, `Eq()(match respelling | true => 0 | false => 1 end, 0)` by `Eq/refl()`, and `()` at the type `match respelling | true => {} | false => Nat end`. In its `false` arm, for an ordering: `Holds` of the respelling's dual by `True/qed()`.
- **Atom rows**, two terms that convert: `f(a + b)` and `f(b + a)`; `w(a, p1)` and `w(a, p2)`; `u((x) => x + a)` and `u((x) => a + x)`; and at `Int`, `v(i + j)` and `v(j + i)`.
- **Atom columns.** The control, `Eq()(left, right)`. Then `Eq()(left == right, true)`, `Eq()(left <= right, true)`, `Eq()(left - right, 0)`, `Eq()(match left == right | true => 0 | false => 1 end, 0)`, and at `Nat` the `0` arm of `match left` holding `Eq()(right, 0)`.
- **Substitution rows**, a guard and a second match on a variable it names, inside the guard's `true` arm: `a < List/len(xs)` matched on `a` at `0`; `a + b < 10` matched on `b` at `0`; `g(flag) < 5` matched on `flag` at `true`.
- **Substitution columns.** `Holds(guard)` as written by `True/qed()` and by `True/proved()`; `Holds` of the guard with the case value written; and the control, the two matches nested the other way.

A cell is held where `wonder diagnostics -` reports nothing but warnings, the kernel's where its report opens "the kernel refused", and refused otherwise.

**The count**, `counted`, at `361b36e8c`: two samples in each checker's reducer, read from the profiles `cargo xtask clippy` files for `/std` — `curios-prelude-archive/.artifacts/profile.tsv` for elaboration, `curios-prelude/.artifacts/profile.tsv` for certification — folded by `curios profile`. `classable_fold` is the atoms of a stuck operation the readers read through, where `classable` says some two may be one; `missed_lookup` is the equations in force a stuck reduct could be a reduct of, where none answers.

| Sample | Stuck terms | Asked in all | At most |
| --- | --- | --- | --- |
| `reduce::classable_fold`, elaboration | 4,297 | 14,784 atoms | 10 |
| `reduce::missed_lookup`, elaboration | 33,415 | 96,571 equations | 14 |
| `whnf::classable_fold`, certification | 1,280 | 3,812 atoms | 10 |
| `whnf::missed_lookup`, certification | 11,442 | 26,332 equations | 7 |

## Stages

Each lands alone, its tests stated first, each test mutation-checked.

0. **The instrument.** The grid, stated as the compiler answers today, and the count, written here.
1. **An arm's substitution reaches the equations in force.** Landed: where the kernel substitutes an arm's solution, each equation in force that names a solved variable is restated under it, within the arm's bracket and through the recording rule, the recorded one stepping aside until the arm retracts. The substitution rows' as-written cells are held, and the cells with the case value written where the elaborator accepts them.
2. **A dead arm under a second match is refused where it is written.** Landed: the elaborator judges the recording rule on a scrutinee as the kernel spells it — solved metavariables materialized, local definitions substituted, refined variables spelled as their values — where an equation is recorded and again where an arm refines a variable, withholding in that arm an equation the refinement leaves naming no local. A restated key that names no local always computes, the one declaration with no body being a `foreign` one, whose result is an `Io` no guard compares: where it computes to the guard's case the fact holds by reduction, and where it computes to another the arm is dead, a proof resting on the guard is refused with the note that the arm is never taken, and the bound procedure reads no fact from the guard.
3. **One rule for whether an equation answers a term.** Landed: `curios-analysis`'s `answers`, asked by both reducers of each settled spelling at the stuck-reduct probe — the term is the key, or the conversion chain's readers hold the two equal outright, or hold the term equal to the key negated. It reduces nothing: both terms reach it reduced, and what a lookup forces again the kernel's memo replays the identities of on every read. The guard rows whose respelling stays inside the theory are held in every column.
4. **Reduction asks its checker.** Landed. At a missed equation: where the shared rule hands a question back (`Answered`), each reducer puts it to its own conversion — the atoms of two operations of one kind classed and the pair read once more, or a stuck application, projection or `match` compared with the equation's settled spelling under the view it was settled in. At a stuck fold, once every equation has been asked: the atoms its fold pairs are classed and the fold taken again over one spelling to a class (`curios-core`'s `refold`), kept only where the result is no longer that operation. Conversion answers either by plain reduction, which `Env::force` reads by as well. Every cell of the grid is held but the row the filter refuses, `kernel_memo_parity` holds, and the count's two samples are the work done.
5. **What the rule retires.** The special spellings at the stuck-reduct probe, the elaborator's escalation and its aliases, and the bound procedure's writing of a guard's fact at its key's spelling, each removed where the grid and the equation tests hold without it.

## Verification

- The grid, at every order of its binders, through both checkers; the law grid and its audit unmoved.
- `cargo xboard` holding, the entries for case equations, the evaluation memo and conversion recurrence each naming the tests that hold what this changes.
- `/std` and the corpus elaborate and certify with no refusal by exhaustion added; what `/std`'s elaboration and its certification consume of their budgets, as `budget::consumed` sums it in the two profiles the count reads, is the figure a stage may move.

## Retirement

Rewrite [An atom is one where conversion says so](../../design/arithmetic/an-atom-is-one-where-conversion-says-so.md) as the decision this lands, a term's identity in the chain, the fold and the lookup; correct the decisions and board entries that rest on a key's spelling; replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
