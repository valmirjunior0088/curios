# A guard's equation reaches a form beneath a binder of its own

`curios_analysis::could_answer` (`curios-analysis/src/case_equation.rs`), the filter that decides which equations in force a stuck form is put to, sorts a form's parts three ways: what reduction left (a stuck form's head, a fold's operands, a projection's head, a match's scrutinee), read as it stands; an application's argument and a constructed value's field, which conversion reduces before comparing, reduced and read; and every other part, read as it stands with nothing below it reduced. That third way holds two different things. One is a part conversion compares only in step with the other side, beneath a binder or in an arm of a stuck match; the other is a part of a type that conversion reduces before comparing it as it does an argument: a type former's parameters and indices, a structure type's parameters, a function type's domain. Each is refused where conversion holds the form equal to the key, re-shown at `3459b5a2b` with the pair accepted as equal at the top of an item:

- under `match u((x) => x) < 10`, `u((x) => x + 0 * c) < 10`, `c` beneath the lambda's binder;
- under `match u((x) => Nat/div(x, b, @q)) < 5`, `u((x) => Nat/div(x, b, @nonzero(b, c, q))) < 5`, a proof beneath the binder, whose refusal prints the fact as `Holds(u((x) => Nat/div(x, b)) < 5)`, the guard's own spelling, the proof left out;
- under `match ft((y: F(b)) -> Nat) < 5`, `ft((y: F(b + 0 * c)) -> Nat) < 5`, a function type's domain;
- under `match fp(Eq()(b, b)) < 5`, `fp(Eq()(b + 0 * c, b)) < 5`, an inductive type's index;
- under `match ft(Box(b)) < 5`, `ft(Box(b + 0 * c)) < 5`, a structure type's parameter.

The filter's contract is that every form conversion could hold equal to a reduct of the key passes it; these are where it does not. Both checkers' `question_tests` pin the first as `a_binder_beneath_a_lambda_is_counted_where_it_stands`.

The next step is stage 1.

## Decided

Settled by the user on 2026-11-20.

- **A refusal the theory does not make is a defect whether or not a program meets it**, so this waits on a design and not on a consumer.
- **The walk reads a part as conversion reads it, and passes what conversion compares only in step.** Where conversion reduces a part before comparing it, the walk reduces it: an argument, a field, a type former's parameters and indices, a constructor's parameters, a telescope's entries outside its binders, a match's goal and the instance it states. What conversion meets only by opening a binder or entering an arm passes, and the comparison an admitted form is put to compares it in step: a lambda's body, every arm of a stuck match, a telescope's entries under its binders. Reduced one side alone there, the walk has no bound: reducing a stuck match's arms that bind nothing unfolds `/std/Nat/log2`'s recursive call into a match stuck again, without end, and spends the elaborator's budget across `/std`. What eta never reads passes too: a lambda's annotations, and a struct literal's parameters, where conversion compares the literal against a neutral through its type.
- **Elaboration's own forms end the test.** An unsolved metavariable and an elaboration-transient node are no Core term the certifier ever compares, and the elaborator's conversion holds a metavariable equal to nothing but itself inside a question. Reducing a metavariable's spine unfolds a local whose definition holds the metavariable again, while a recursive definition is elaborated, and the walk, which files what it read of a part once that part is read, never meets the part again.
- **A part the walk can reduce is read, not passed.** Passing every part the walk does not reduce keeps the contract too, and lets type formers and metavariables standing over a binder the key does not name through to equations they never answer, `/std`'s check among the programs that pay for it; read reduced, they are refused as before.
- **A proof is passed over wherever the walk reads**: a type former's parameters and indices and a constructor's parameters join the positions each checker's `proof_parts` types, as an application's arguments are typed.
- Lean's discrimination trees key a lambda, a forall and a sort as one opaque key and pass over proofs and implicit arguments, leaving them to the comparison (`isDefEq`) a retrieved candidate is put to.

## Stages

1. **The refusals as tests.** Rows of `curios/src/tests/respelling.rs`'s guard grid for the five programs, stated refused at every arm past the control, and near misses in both checkers' `question_tests` — a lambda's body, a domain and an index each differing past every reduction — stuck.
2. **The walk reads as conversion reads.** `Stands::Beneath` goes: every part reduction did not leave is reduced and read, and passes where conversion meets it only in step or eta never reads it; a metavariable and a transient end the test. The five rows turn held, the near misses stay stuck, and both `question_tests` turn to the answer; `could_answer`'s contract and [Case equations and their key](../../design/soundness/case-equations-and-their-key.md) are restated.
3. **Proofs at a type's parameters.** Both checkers' `proof_parts` type a type former's parameters and indices and a constructor's parameters; a grid row whose respelling stands in a proof at a family's parameter turns held.

## Verification

- Each stage's rows and near misses on the side stated, each mutation-checked.
- `curios`'s `tests::unfolding` compared door (`Consumed::Compared`) at 8, 14 and 20 definitions, an empty program, and `/std`'s check, counted in user-space instructions retired, against the walk as it stands.

## Retirement

When stage 3 lands, this file is deleted.
