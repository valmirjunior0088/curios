# A report's term elaborates back to the term shown

**Not refined yet.** [A diagnostic spells what its reader can write](../../design/tools/a-diagnostic-spells-what-its-reader-can-write.md) decides that a term a report shows is spelled so that, written where the reader stands, it elaborates back to the term shown, and that a `use` argument is left out exactly when witness resolution would put it back. This specification reserves the place where a printed term falls short of that. It is not an implementation plan.

## What is missing

**A report writes out a witness resolution would put back, where a binder in scope is at an argument that computes.** With `pub let Same(A: Type) -> Type = A;` and `pub let Wrap(A: Type, use Show(A)) -> Type = {A};`, a goal on `(@A: Type, use Show(Same(A)), x: Wrap(Nat))` reports `x: Wrap(Nat, use /std/Nat/witness@/std/Show/Show(Nat))` under `a term above is shown as the compiler holds it`: resolution passes over the binder, a `Show` of `A`, and takes the table's witness, which the report should then leave out. The translation tells two applications apart by the heads they are written with (`rigid_head`, `curios-text/src/from_core.rs`), a name read through its definition as a witness's key is (`Keying::head`, `curios-core/src/witness.rs`), and reduces nothing: `Same(A)` is headed by nothing until it is reduced, so the binder is not known to be another's, and the walk stops (`resolved`). `a_binder_at_an_argument_that_computes_keeps_the_table_witness_written` (`curios-pipeline/src/tests/paste_back_tests.rs`) holds the report as it stands. The same reading takes a variable an enclosing arm's pattern solved for a binder of its own, as it did before it compared applications; no program is known that shows it.

## Refinement

- **The witness.** What the translation would reduce with, a report being printed from a spelling that holds lexicons and no environment, and whether a program that was not written to show it meets either reading. Two directions: the reader's printer is handed the reader's own reduction, or the elaborator states beside a term which of its witnesses its resolution would restore. Check: the test above reads back.
