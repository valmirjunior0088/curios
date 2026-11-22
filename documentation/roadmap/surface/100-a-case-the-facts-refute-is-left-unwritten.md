# A case the facts refute is left unwritten

**Adopted by the user on 2026-11-22.** A match compiles without an arm for a case the facts in scope rule out, where today it is refused for a value it cannot take: a leaf of its tree that no arm reaches is closed by the contradiction the elaborator proves there, the proof it writes for an omitted bound at `False`, and is refused naming its value and the facts read where none is found. The next step is a ready delivery, under *Stages*.

## What is wrong today

**A case the facts in scope rule out must still be written as an arm.** Coverage reads a column's cases off its type ([Patterns are compiled where their types are known](../../design/surface/patterns-are-compiled-where-their-types-are-known.md)), and a case no arm reaches is a value no arm matches, refused before its split is resolved (`Compiler::uncovered` and `Compiler::guarded`, `curios-elab/src/elaborate/matrix.rs`). Each of these is refused so, probed at `d89632920`, and each compiles with the arm it misses written as `match proved(@False) end`:

- over `r: Vec(Nat, 3)`, `match r.list | [a, b, c] => a + b + c end` misses `[]`, where a bound refused in the same scope lists `r.counted` as `List/len(r.list) <= 3` and `3 <= List/len(r.list)`;
- under `Holds(n < 3)`, `match n | 0 => … | 1 => … | 2 => … end` misses `3`;
- under `Holds(Option/is_some(o))`, `match o | some(x) => x end` and the binder `let some(x) = o;` miss `none()`;
- under `Holds(n <= 10)`, `match n > 20 | false => n end` misses `true`;
- under `Holds(n < 3)`, `match n | k, k < 5 => k end` misses `_` where its guard fails.

`/std` writes 45 arms as `match proved(@Bool/False) end` (counted at `d89632920`), and `documentation/language.md` and `/std/proved`'s documentation teach that arm as the way a case the facts rule out is written.

## Goal

A leaf of the tree that no arm reaches — a case no row names, or the failure of a guarded arm with no arm after it — compiles where the facts in scope refute it, and is refused otherwise, naming its value and the facts read.

## Settled

- **Every split enters each of its cases.** A case no row names is entered as an arm of it is: its payload's binders assumed, the scrutinee refined, the index equations solved, and a dispatch's other values under each key's test failed. So a missing case is found where its leaf stands, under that leaf's facts, rather than before the split.
- **A leaf no row reaches is the zero-arm match on a proof of `/sys/Bool/False`**, asked of the procedure that proves a bound from the facts in scope ([A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)) where the leaf stands, as a bound is at insertion. The arm is written into Core, and both checkers check it as they check one a reader wrote, so nothing new is trusted: the certifier's rule for an absent arm stays inversion's ([An arm is checked in a context specialized by index inversion](../../design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md)).
- **Every unreached leaf, a guard's failure among them.** A lone condition's failure is a fact where it fails, as it is for the arms after it; the failure of a row of several guards or of a bind guard is read with the leaf's facts alone, as the arms after such a row are.
- **A match that misses no arm asks nothing new.** A match accepted today has no leaf no row reaches, and a case inversion rules out is never entered, so the procedure runs only where a match was refused before.
- **The refusal is today's, with the procedure's report beneath it**: "no arm matches `v`", or "… where the guards of `x` fail", then the facts read, those not read, and values at which they all hold, as an undischarged bound reports them. A leaf whose scope holds no fact the procedure reads reports as today.
- **An arm written for such a case stays legal**, typed as written, as one inversion rules out is.
- **An index equation inversion leaves standing is no fact**: the procedure reads hypotheses, their proof fields and guards, and an arm whose indices only arithmetic tells apart stays mandatory where no fact refutes it.

## Stages

**1. The matrix compiler closes each unreached leaf.** Every split enters each case, the leaf no row reaches is closed by the procedure or refused with its report, and the large-elimination guard counts a closed arm as an arm, as the certifier does. Tests in `curios/src/tests/rules/` put each program above to both checkers and run it, refuse the control whose facts do not refute its case with the facts named, and keep inversion's and coverage's refusals. The two decisions and `documentation/language.md`'s *Match shell* and *Guards* are restated. Check: those tests, and the suite's coverage, inversion and index tests.

**2. `/std` leaves the refuted cases unwritten.** Each of the 45 arms is removed where `/std` still compiles without it: an arm followed by a binder row hands its case to that row, which then has to type there. `/std/proved`'s documentation and the reference's `clamp` example are restated. The hand-proved arms, eliminating a contradiction proved by name, stay as written. Check: `/std`'s tests, and the count of arms removed.

## Verification and retirement

Each stage's check, then `cargo xtask workspace lint`, the suite and `cargo xtask docs build`. Once stage 2 lands, the bound decision names a leaf no arm reaches among the procedure's entry points, nothing links this file, and it is deleted.
