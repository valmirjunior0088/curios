# A guard's equation reaches a form beneath a binder of its own

`curios_analysis::could_answer` (`curios-analysis/src/case_equation.rs`), the filter that decides which equations in force a stuck form is put to, reads a part as conversion reads it ([Case equations and their key](../../design/soundness/case-equations-and-their-key.md)), and passes over a part its checker's conversion compares at a proposition, which each checker's `proof_parts` types: an application's arguments, a constructor's payload, a struct literal's fields and an operation's operands. A proof at a type former's parameter or index, or at a constructor's parameter, is read like any term, though conversion compares it at a proposition there too: under `match ft(Within(a, p1)) < 5`, `ft(Within(a, lemma(a, c, p1))) < 5` is refused, and so are its family, index and constructor counterparts, each a row of `curios/src/tests/respelling.rs`'s guard grid stated refused.

The next step is stage 1.

## Decided

Settled by the user on 2026-11-20.

- **A refusal the theory does not make is a defect whether or not a program meets it**, so this waits on a design and not on a consumer.
- **A proof is passed over wherever the walk reads**: a type former's parameters and indices and a constructor's parameters join the positions each checker's `proof_parts` types, at the telescopes its conversion compares them at, as an application's arguments are typed.

## Stages

1. **Proofs at a type's parameters.** Both checkers' `proof_parts` type a type former's parameters and indices and a constructor's parameters; the four rows of a proof at one turn held.

## Verification

- The four rows held, each mutation-checked.
- `curios`'s `tests::unfolding` compared door (`Consumed::Compared`) at 8, 14 and 20 definitions, a trivial program, and `/std`'s check, counted in user-space instructions retired, against the walk as it stood before this specification's first stage.

## Retirement

When stage 1 lands, this file is deleted.
