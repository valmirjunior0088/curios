# A value a loop reads from outside is judged and kept as the loop's own parameter would be

A loop parameter every transfer into it hands itself or one value from outside stays a parameter, passed back to itself each round, where it could be that value: the sharing analysis judges a part a loop reads alike whether its source is bound outside the loop or handed on as its parameter, so substituting the parameter by its value loses no verdict. The next step is stage 1, a ready fix.

## What is wrong today

- **A loop parameter every transfer hands itself or one value from outside stays a parameter, since the invariant fixpoint takes only a function.** [A known function a call hands is followed into it](../../../curios-cps/README.md#a-known-function-a-call-hands-is-followed-into-it) substitutes a function or a literal every entry of a loop hands a parameter its back edges pass on, and `known_values` (`curios-cps/src/analysis.rs`) takes nothing else from the fixpoint (`invariant_fixpoint`), `resolve_atom` reading an argument that is a value from outside the fixpoint's domain as a conflict, "an unobservable runtime value". So a recursion that hands a value on unchanged keeps it as a parameter of the loop contification makes of it: at `5930c771a`, `programs/parse_digits.crs` optimizes to `/loop(k, digits, acc)`, each back edge handing its own `digits`. Counted at `5930c771a` with stage 1 prototyped, a census beside the substitution: 3 to 50 parameters a program of `programs/` taken as their value, 643 in `toml_round_trip`, and the corpus's binaries (`wonder --stage wasm`, `wasm-tools parse`) 0.85% smaller, 5,729,990 bytes to 5,681,396 over the programs but `toml_round_trip`, from 0.26% (`sort_scattered`) to 2.91% (`parse_bindless`). LLVM folds a phi whose operands are itself and one value into that value (`simplifyPHINode`), MLIR's region simplification drops a block argument every predecessor hands the same value (`dropRedundantArguments`), Braun et al. remove such a φ as trivial (*Simple and Efficient Construction of Static Single Assignment Form*, CC 2013), and GHC's static argument transformation takes such an argument out of a recursion at the source.

## Settled

- The substitution, as the user settled it on 2026-11-19: the fixpoint carries a value from outside, `resolve_atom` reading it as known rather than a conflict, so a parameter every transfer hands itself, another invariant parameter or one outside value is found, and `known_values` takes such a value as it takes a function, only where it is in scope where the continuation is bound; that scope is read off the walk `Module::scope_walk` makes (`curios-cps/src/verify.rs`), which holds the values beside the functions at each `LetCont` and keeps only the functions, and hands its maps as a struct with named fields where it hands a tuple.

## Stages

**1. The substitution.** As settled above, and the decision restated to values. Check: tests of a value substituted, of one handed through another loop's invariant parameter, of two values kept and of one out of scope kept, each mutation-checked; `remove`'s rebuild tests and the ladder's passing; the corpus's binaries smaller, counted at the fix's commit by the protocol above; every program's answers unchanged.

## Verification and retirement

Stage 1's check, then `cargo xtask workspace lint`, the suite and `curios-prelude`'s corpus tests. Once it lands, [A known function a call hands is followed into it](../../../curios-cps/README.md#a-known-function-a-call-hands-is-followed-into-it) is restated to values, and this file is deleted.
