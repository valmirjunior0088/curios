# A term is one where conversion says so

Working specification for the places where a verdict still follows how a term is spelled. [An atom is one where conversion says so](../../design/arithmetic/an-atom-is-one-where-conversion-says-so.md) settles the question where two intrinsics meet in conversion; reduction still answers it by spelling, in a guard's equation and in a fold. So the laws and the guards do not compose: `n + m` and `m + n` are one term, an arm's scrutinee is `true`, and under `match n + m < k` the term `m + n < k` is not `true`.

## What this builds on

- **The chain and its classing.** `curios-analysis`'s `convert_intrinsics` reads a pair through the carriers' algebra, hands the pair's atoms back where its readers decide nothing, and reads again once each checker has classed them by its own conversion (`curios-core`'s `Classes::of`, `atoms_of`, `classed`). It calls no judgment.
- **The case equations.** One rule records an equation for both checkers (`curios-analysis`'s `records_case_equation`), under the scrutinee as written, the spelling a dispatch resolves to, and a reduct settled at most once, with the equation and every equation inside it withheld ([Case equations and their key](../../design/soundness/elimination/case-equations-and-their-key.md)). A stuck comparison is also asked under its dual and its successor spelling (`curios-core`'s `probe_spellings`).
- **The audit.** `tests::laws::audit` holds conversion symmetric, transitive and closed under substitution at every declared law, each stated at the top of an item, outside any arm ([The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md)).
- **The decisions it keeps.** The refinement store is a lookup and not a theory, and a hypothesis reaches a bound through a proof ([A bound is stated in a decided proposition and discharged by reduction](../../design/arithmetic/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)); a fold does not respell ([A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)).

## The gap

Each is a program put to the compiler on standard input at `b900e392f`; a compiler built without the classing of atoms answers the same.

**An arm's substitution does not reach the equations in force.** Under `match i < List/len(xs) | true => (match i | 0 => … | _ => … end) | false => … end`, `let _: Holds(i < List/len(xs)) = True/qed()` in the `0` arm is accepted by the elaborator and reported as "the kernel refused", its expected type printed with `0` where the program wrote `i`. So is the same shape over `a + b < 10` matched on `b`, over `g(flag) < 5` matched on a `Bool`, and with `True/proved()` for the proof; the default arm, a second match on a variable the guard does not name, and the two matches nested the other way are accepted. `curios-cert`'s `check_arm`, `check_cases` and `check_free_monoid` substitute a variable scrutinee's solution through the arm's body and its expected type (`assume_case_value`, the kernel holding no refinement store), and the equations in force keep the keys they were recorded under, which still name the variable. The elaborator refines the variable in its store, so its key still matches. With the case value written the kernel refuses as well where the elaborator's escalation reaches the key — `Holds(0 < List/len(xs))`, `Holds(a + 0 < 10)` — and where it does not, `Holds(f(0) < 5)` under `match f(a) < 5` and `match a | 0`, the elaborator refuses, in either nesting.

**An arm's equation answers a spelling, so conversion is weaker inside the arm than outside it.** `Eq()(a + b < 10, b + a < 10)` closes by `Eq/refl()` at the top of an item and is refused in the `true` arm of `match a + b < 10`: the written spelling reduces to `true` by the arm's equation before the two are compared, and nothing answers the other. So in that arm `Holds(b + a < 10)` is not `True/qed()`'s, a `match b + a < 10` stays stuck in a term and in a type, and the `false` arm refuses `Holds(10 <= b + a)`. The same holds for a sum of three permuted, a comparison with both sides commuted, an `==` with its sides swapped, a call whose argument commutes a sum or differs in a proof, a range check over a commuted sum, and the `0` arm of `match f(a + b)` against `f(b + a)`. Each of those pairs converts outside an arm, and each refusal is the elaborator's, so the kernel is not asked. A guard reassociated, a product commuted and the successor spelling are answered.

**The procedure that proves a bound meets the same refusal in its own proof.** In the `true` arm of `match a + b < 10`, `let _: Holds(b + a < 10) = True/proved()` is refused with "a proof was found and did not check, which is the compiler's fault", and so are a sum of three permuted and a comparison with both sides commuted; a goal that scales the guard, `a * 2 + b * 2 < 20`, is proved. The fact is the guard at the spelling its key records, which the arm's equation makes `true`, and the goal's spelling is one nothing answers.

**A fold pairs atoms by their spelling, so conversion is no congruence under one.** `f(a + b)` and `f(b + a)` convert, and `f(a + b) == f(b + a)` does not convert with `f(a + b) == f(a + b)`, which reduces to `true`; nor does their `<=` with `true`, their difference with `0`, or a `match` on their `==` with its `true` arm. The same holds of one call under two proofs of its bound and of two calls on functions whose bodies commute a sum. At `Int` the `==` and the `<=` are refused and the difference is held. `Nat::cancel_common`, `compare_nat`'s shared inner, `Nat::same` and `int_same` pair summands by identity up to universe instances; the chain classes the pair's atoms and reads the classed pair again without folding it (`read_classed`), and a truncated difference is not among the operations whose atoms are handed out.

**Each checker has its own lookup.** The kernel's is `whnf`'s `refined_reduct` over `Scope`; the elaborator's is `reduce`'s probe before decomposition, `refined_after_fold` and `refined_reduct` over its term-keyed store. They differ: the elaborator escalates a key it missed to its arguments reduced, capped by an allowance, which the kernel does not; its key erases universe instances and declines at the read; and it records the kernel's spelling of a guard over local definitions as an alias. Under `let n = m + 0; match Nat/in_range(n, 240, 244)` the elaborator accepts `Nat/le/of_in_range(m, 240, 244, True/qed())` and the kernel refuses it. [The findings](00-findings.md) hold a second direction that does not reproduce. Settling reduced spellings is a measured cost of elaboration, and a settled spelling is kept beside its entry for as long as nothing can change what its key reduces to (`Frames::scrutinee_spellings`, `curios-elab/src/context/frames.rs`).

## The goal

No respelling moves a verdict: where conversion holds two terms equal at the top of an item, either stands for the other in any position, under any arm, and both checkers answer as before. That is conversion a congruence under every fold, stable under the equation an arm assumes, closed under the substitution an arm performs, and decided by one rule in both checkers.

The end state is reduction defined with conversion at two points, and nowhere else: a stuck fold's atoms are classed by the checker's conversion before the fold reads them, and a stuck term is answered by an equation in force whose key the checker's conversion equates with it. Coq Modulo Theory fires its eliminations by matching modulo the theory (Jouannaud and Strub, 2017), and the smart case defines normalisation and convertibility under an arm's equations mutually (Altenkirch, 2011).

## Settled

- **The store stays a lookup.** An equation answers the terms conversion equates with its scrutinee and nothing else: no consequence of two equations, no hypothesis, and an `==` guard does not equate its sides.
- **The shared layer calls no judgment.** `curios-core` and `curios-analysis` hand atoms and candidates to the checker, as the chain does.
- **A fold classed again is kept only where it decides more.** A stuck operation whose classed spelling folds no further keeps the spelling it had.
- **Not canonical forms.** A spelling per class would make identity the relation, and there is none: `Bool` has no normal form the decisions accept, irrelevance is typed, and an atom's arguments would need full normal forms.
- **Not a congruence-closure store.** It decides the consequences of two equations, which is hypotheses in conversion.
- **Not completion at the judgment alone.** Conversion completing a fold or a lookup only where it compares leaves a scrutinee inside reduction decided by spelling.

## Open

- What reduction asking conversion costs over `/std`; the count below is taken before the stage that needs it.
- How the elaborator compares two reduced forms from inside reduction without looking the same term up again, and what its reduction cache may keep of a reduct taken meanwhile.
- Whether the kernel's history key needs more than the number of equations in force once a comparison runs inside a settlement.
- Which of the special spellings the last stage retires, and which a test still needs for what a probe may spend.
- Whether the second key direction in [the findings](00-findings.md) is a third program or a difference since closed.

## The respelling grid

A grid states the goal as a test, `curios`'s `tests::respelling`, each cell one program put to both checkers and stated as held, refused, or refused by the kernel alone, so a stage is cells moving.

The binders are `a: Nat, b: Nat, c: Nat, f: (Nat) -> Nat, w: (n: Nat, at: Holds(n < 10)) -> Nat, p1: Holds(a < 10), p2: Holds(a < 10), u: ((Nat) -> Nat) -> Nat, i: Int, j: Int, v: (Int) -> Int, xs: List(Nat), flag: Bool, g: (Bool) -> Nat`, swept over orders drawn as `tests::laws` draws them.

- **Guard rows**, a guard and a respelling of it: `a + b < 10` and `b + a < 10`; `a + b + c < 10` and `c + a + b < 10`; `a + b < c + 10` and `b + a < 10 + c`; `a == b` and `b == a`; `f(a + b) < 10` and `f(b + a) < 10`; `w(a, p1) < 5` and `w(a, p2) < 5`; `u((x) => x + a) < 5` and `u((x) => a + x) < 5`; `Nat/in_range(a + b, 1, 5)` and `Nat/in_range(b + a, 1, 5)`.
- **Guard columns.** The control, `Eq()(guard, respelling)` by `Eq/refl()` at the top of an item, which every row holds. In the guard's `true` arm: `Holds(respelling)` by `True/qed()`, the same by `True/proved()`, `Eq()(match respelling | true => 0 | false => 1 end, 0)` by `Eq/refl()`, and `()` at the type `match respelling | true => {} | false => Nat end`. In its `false` arm, for an ordering: `Holds` of the respelling's dual by `True/qed()`.
- **Atom rows**, two terms that convert: `f(a + b)` and `f(b + a)`; `w(a, p1)` and `w(a, p2)`; `u((x) => x + a)` and `u((x) => a + x)`; and at `Int`, `v(i + j)` and `v(j + i)`.
- **Atom columns.** The control, `Eq()(left, right)`. Then `Eq()(left == right, true)`, `Eq()(left <= right, true)`, `Eq()(left - right, 0)`, `Eq()(match left == right | true => 0 | false => 1 end, 0)`, and at `Nat` the `0` arm of `match left` holding `Eq()(right, 0)`.
- **Substitution rows**, a guard and a second match on a variable it names, inside the guard's `true` arm: `a < List/len(xs)` matched on `a` at `0`; `a + b < 10` matched on `b` at `0`; `g(flag) < 5` matched on `flag` at `true`.
- **Substitution columns.** `Holds(guard)` as written by `True/qed()` and by `True/proved()`; `Holds` of the guard with the case value written; and the control, the two matches nested the other way.

A cell is held where `wonder diagnostics -` reports nothing but warnings, the kernel's where its report opens "the kernel refused", and refused otherwise.

**The count**, `counted`, at `6b3ae2c3c`: two samples in each checker's reducer, read from the profiles `cargo xtask clippy` files for `/std` — `curios-prelude-archive/.artifacts/profile.tsv` for elaboration, `curios-prelude/.artifacts/profile.tsv` for certification — folded by `curios profile`. `classable_fold` is the atoms of a stuck operation the readers read through, where `classable` says some two may be one; `missed_lookup` is the equations in force a stuck reduct could be a reduct of, where none answers.

| Sample | Stuck terms | Asked in all | At most |
| --- | --- | --- | --- |
| `reduce::classable_fold`, elaboration | 4,334 | 14,866 atoms | 10 |
| `reduce::missed_lookup`, elaboration | 32,978 | 95,421 equations | 14 |
| `whnf::classable_fold`, certification | 1,265 | 3,777 atoms | 10 |
| `whnf::missed_lookup`, certification | 11,368 | 26,243 equations | 7 |

## Stages

Each lands alone, its tests stated first, each test mutation-checked.

0. **The instrument.** The grid, stated as the compiler answers today, and the count, written here.
1. **An arm's substitution reaches the equations in force.** Where the kernel substitutes an arm's solution, each equation in force whose key names a solved variable is restated under it, within the arm's bracket and through the recording rule. Check: the substitution rows' as-written cells are held.
2. **One rule for whether an equation answers a term**, in `curios-analysis`, for both reducers at the stuck-reduct probe: the term is the key, or the chain's readers hold the two equal outright, or hold the term equal to the key negated. Check: the guard rows whose respelling stays inside the theory are held, in every column, and the kernel and the elaborator call the one function.
3. **Reduction asks its checker.** The lookup classes what the readers hand back and compares a stuck application, projection or `match` with a key by the checker's conversion, under a history of its own; and a stuck fold's atoms are classed and the fold taken again. Check: every cell is held; `kernel_memo_parity` holds; the count's two samples are the work done.
4. **What the rule retires.** The special spellings at the stuck-reduct probe, the elaborator's escalation and its aliases, and the bound procedure's writing of a guard's fact at its key's spelling, each removed where the grid and the equation tests hold without it.

## Verification

- The grid, at every order of its binders, through both checkers; the law grid and its audit unmoved.
- `cargo xboard` holding, the entries for case equations, the evaluation memo and conversion recurrence each naming the tests that hold what this changes.
- `/std` and the corpus elaborate and certify with no refusal by exhaustion added; what `/std`'s elaboration and its certification consume of their budgets, as `budget::consumed` sums it in the two profiles the count reads, is the figure a stage may move.

## Retirement

Rewrite [An atom is one where conversion says so](../../design/arithmetic/an-atom-is-one-where-conversion-says-so.md) as the decision this lands, a term's identity in the chain, the fold and the lookup; correct the decisions and board entries that rest on a key's spelling; replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
