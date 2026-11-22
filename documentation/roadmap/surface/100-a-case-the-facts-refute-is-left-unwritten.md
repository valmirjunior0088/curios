# A case the facts refute is left unwritten

**Adopted by the user on 2026-11-22; not refined yet.** A match over a value whose facts in scope rule a case out compiles without an arm for it, where today the program is refused for a value it cannot take: a leaf of the match's tree that no arm reaches is closed with the contradiction the facts prove, as a bound is proved from them. Its design is open, under *Open questions*.

## What is missing

**A case the facts in scope refute must still be written as an arm.** Over `r: Vec(Nat, 3)`, `match r.list | [a, b, c] => a + b + c end` is refused with "no arm matches `[]`", though a bound refused in the same scope lists `r.counted: List/len(r.list) <= 3` and `3 <= List/len(r.list)` among its facts, and the four arms the facts rule out compile written as `match proved(@Bool/False) end`. Coverage reads its cases off types ([Patterns are compiled where their types are known](../../design/surface/patterns-are-compiled-where-their-types-are-known.md)), and an arm is absent only where index inversion proves it impossible ([An arm is checked in a context specialized by index inversion](../../design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md)).

## What is known

- **Nothing new is trusted.** The elaborator proves `False` from the facts in scope where a bound asks it ([A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)), and the certifier checks that proof as a term, so a leaf closed by it is a written arm to the certifier.
- **Peers.** F* checks a match's exhaustiveness against the refinements in scope through its SMT solver, so a case they rule out is left unwritten, and Liquid Haskell proves a missing case unreachable the same way; Agda, Idris and Lean omit only a case unification refutes, and otherwise ask for an absurd or `impossible` clause, as Curios does.
- **A destructuring of a vector's list waits on it too**: a binder pattern that coverage proves total ([Surface: findings](000-findings.md)) is total over `r.list` only where the facts refute the other lengths.

## Open questions

- The cost of asking the facts at each leaf no arm reaches, measured against the matches where no arm is missing.
- How a refusal reports facts that do not refute: the leaf's value, and the facts read.
- How the two decisions above are restated: coverage's "a case no arm reaches is a value no arm matches", and the specialization's arm absent "only where index inversion proves it impossible".

## Verification and retirement

Check, once refined: the refused match above compiles in both checkers; a match whose facts do not refute the missing case is refused naming it and the facts read; the cost over the corpus and `/std` is stated against the matches that miss no arm. Once it lands, the two decisions are restated and this file is deleted.
