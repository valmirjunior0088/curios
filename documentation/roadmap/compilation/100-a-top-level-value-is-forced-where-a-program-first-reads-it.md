# A top-level value is forced where a program first reads it

A program computes a top-level value the first time it reads it, and not where nothing reads it (`lower_to_cps`, `curios-anf/src/into_cps.rs`), yet one whose computation is not known to end, and which its unit's compile could not finish, is still linked into every program its unit is in scope of, whether or not the program reads it: "a top-level value whose evaluation is observable runs before the entry in every program its unit is in scope of, which is the language's meaning" (`curios-erase/src/link.rs`). Its unit's compile decides that observability for the link (`observable_since`) and records the verdicts it read for it, and a program's prune drops what the link took for its own sake. The lint `startup-value` reports each such value where it is declared ([A lint is an exact finding read off the compilation](../../design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md)) as computed at every start, which no program does. Re-shown at `8a57b09ac`, the lint unchanged since, in files whose entry prints `entry`, each program printing it and exiting 0:

- `let forever: Nat = climb(0)`, where `climb` recurses without descending, is reported, and `curios lint` says at the same name both that `forever` is never used and that it is computed at every start.
- `let server: Io({}) = serve(0)`, a description whose recursion stands under a bind, is reported, where `documentation/usage.md` exempts a description.

The language states no such run: a program "closes with one final term, the description it performs" (`documentation/language.md`), and of a recursive value it states the opposite, "a recursive value computed the first time it is read". The lowering of a recursive group argues the same, "Forcing on first use is what the language means by a recursive value", sound because "an initializer performs no effect" (`Lowerer::lower_knot`, `curios-anf/src/into_cps/lowerer.rs`), and the compile-time evaluator already reads every top-level value so, "a CAF forced on demand and memoized" (`force_toplevel`, `curios-anf/src/optimize/evaluate/interpret.rs`). A top-level value performs no effect either, so what its computation can show a program is not ending. And recursion at the value level is unrestricted: a group the certifier cannot see descend is classified `Partial` and kept ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md)). So the lint asks an author to restructure code the language admits, under a design where no lint can be suppressed.

The next step is stage 1.

## Decided

Settled by the user on 2026-11-20.

- **A top-level value is computed the first time a program reads it, once, and not where nothing reads it**, as a recursive value is, and a program that reads one whose computation does not end diverges at the read. Swift initializes a global at its first access, Kotlin/Native moved its top-level properties from the start to the first access, Java initializes a class at its first active use, and Haskell evaluates a top-level value where it is demanded. OCaml and Go initialize what is linked at the start, which they owe to initializers that perform effects, a registration or an output, and a Curios value performs none. Lean 4 initialized every closed term before `main` until 4.29, a constant computed by a partial definition hanging every executable, and since embeds a constant value in its executable where it can and computes every other on demand ([v4.29.0](https://lean-lang.org/doc/reference/stable/releases/v4.29.0/)). This reopens the rejected "Evaluating a top-level value only where it is read" of [A top-level value is computed where its unit is compiled](../../design/compilation/a-top-level-value-is-computed-where-its-unit-is-compiled.md): the meaning that rejection kept, every program in scope running an observable initialization, is stated in no document of the language, and the only initialization a program can observe is one that does not end.
- **What no program can tell apart is computed ahead.** A value its unit's compile finishes is bound to what it computed. At a program's lowering, a top-level statement that computes — applies a function or an intrinsic, matches, or may trap (`Rhs::computes`) — or reads a value lowered by need is lowered by need, as a recursive group's computed member is: a write-once cell, its initializer and a force, reentrant forcing a cycle. Every other, an alias or a construction from constants and values bound ahead of it, is bound where it stands, which no program can tell from by need. Swift initializes a global whose initializer is a constant statically and every other at its first access, and Lean 4 since 4.29 does the same. Counted at `8a57b09ac`, no program of `programs/` keeps a top-level value that computes past its optimizer (`curios wonder --stage anf-optm`: 1,455 constructions, 763 rows, 54 list builds and 20 aliases over the 31 programs), so none of them changes.
- **A read forces where it stands.** A value lowered by need is forced before the first statement that reads it on each path, and what that path runs after it reads the forced value; a function or an initializer reads one forced where it is bound, and forces one that is not at its own read, as a recursive group's members are forced (`Lowerer::with_cell_reads`). GHC forces a thunk at the case that scrutinizes it, and Swift's accessor for a global checks its once-flag at each access.
- **A program links what it reaches, and nothing for its own sake.** The link's second clause goes, with the observability decided for it where a unit is erased, the verdicts recorded for it, and what an item's forms and a stored unit carry of it.
- **`startup-value` is retired**, the condition it reports gone; the lint decision lists seven lints.

## Stages

1. **The lint retired.** `curios-erase/src/lint.rs`, its row of `documentation/usage.md`, the lint decision's eighth lint, and what `.claude/rules/` says of it.
2. **Nothing linked for its own sake.** `curios-erase/src/link.rs`'s second clause, `ErasedForms::observable` and the stored unit's ranks of it, the observability and the verdicts an evaluation asks for it (`Evaluation::observable`, `Evaluation::judged`, `curios_anf::observable_since`), the verdict schema tag advanced; the decision restated with its rejected alternative as the decision, and the language reference's top-level definitions stating the rule.

## Verification

- Stage 1: `curios lint` reports neither `forever` nor `server`.
- Stage 2: a program links no item of its scope it does not reach, a library declaring `forever` among them.

## Retirement

When stage 2 lands, the rule lives in the decision and the language reference, and this file is deleted.
