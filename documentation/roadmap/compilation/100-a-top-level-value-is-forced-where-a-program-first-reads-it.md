# A top-level value is forced where a program first reads it

A top-level value whose computation is not known to end, and which its unit's compile could not finish, is linked into every program its unit is in scope of and computed before the entry, whether or not the program reads it: "a top-level value whose evaluation is observable runs before the entry in every program its unit is in scope of, which is the language's meaning" (`curios-erase/src/link.rs`). The lint `startup-value` reports each such value where it is declared ([A lint is an exact finding read off the compilation](../../design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md)). Re-shown at `19d53645f`, the rule unchanged since, in files whose entry prints `entry`:

- `let forever: Nat = climb(0)`, where `climb` recurses without descending, hangs its program before it prints, though nothing reads `forever`.
- `let server: Io({}) = serve(0)`, a description that a recursion without descent builds at once, runs and exits 0, yet is reported, where `documentation/usage.md` exempts a description.

The language states no such run: a program "closes with one final term, the description it performs" (`documentation/language.md`). The lowering of a recursive group states the opposite of the link's rule: "Forcing on first use is what the language means by a recursive value", sound because "an initializer performs no effect, so running it later, or not at all, is unobservable, and the only behaviours forcing can move are a trap and divergence, which it only delays" (`Lowerer::lower_knot`, `curios-anf/src/into_cps/lowerer.rs`). And recursion at the value level is unrestricted: a group the certifier cannot see descend is classified `Partial` and kept ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md)). So the lint asks an author to restructure code the language admits, under a design where no lint can be suppressed.

The next step is stage 1.

## Decided

Settled by the user on 2026-11-20.

- **A value its unit's compile could not finish is forced where a program first reads it**, once, as a recursive group's computed member is: a write-once cell, its initializer, a force at the entry of each region that reads it, and reentrant forcing a cycle (`lower_knot`). No program computes what it does not read, and a program that reads a value whose computation does not end diverges where it reads it. Swift initializes a global at its first access, Java a class at its first active use, and Haskell evaluates a top-level value where it is demanded; OCaml and Go initialize what is linked at the start. This reopens the rejected "Evaluating a top-level value only where it is read" of [A top-level value is computed where its unit is compiled](../../design/compilation/a-top-level-value-is-computed-where-its-unit-is-compiled.md): the meaning that rejection kept, every program in scope running an observable initialization, is stated in no document of the language, and is what the knot's lowering says the language does not mean.
- **A program links what it reaches, and nothing for its own sake.** The link's second clause goes, with the observability decided for it where a unit is erased and what an item's forms carry of it.
- **`startup-value` is retired**, the condition it reports gone; the lint decision lists seven lints.

## Stages

1. **By need.** The values a unit's compile could not finish are lowered as a knot's members are, each forced at its readers' entries, and a program's start computes none of them.
2. **Nothing linked for its own sake.** `curios-erase/src/link.rs`'s second clause, `ErasedForms::observable` and the observability asked for them where a unit is erased go, and the decision is restated with its rejected alternative as the decision.
3. **The lint retired.** `curios-erase/src/lint.rs`, its row of `documentation/usage.md`, and the lint decision's eighth lint.

## Verification

- Stage 1: `forever` above leaves its program printing `entry` and exiting 0; a program that reads it diverges at the read; a value read twice is computed once; each mutation-checked.
- Stage 2: a program links no item of its scope it does not reach, a library declaring `forever` among them.
- Stage 3: `curios lint` reports neither `forever` nor `server`.

## Retirement

When stage 3 lands, the decisions live in the two design documents, and this file is deleted.
