# A claim over a domain nobody enumerated

## Status

Not refined yet. Property-based testing shipped once, in the form recorded below, and was removed rather than revised; this file is the reset. Nothing is started, and the shape the replacement should take is deliberately open.

## Why it exists

A `test` declaration's parentheses carry its quantifier, and the language now honours that: a parameterized test is a claim about every instantiation, proved by the kernel or refused at the declaration. That leaves one thing unsaid. A claim that is *true* but not a theorem the kernel settles — `List/sort` returns a sorted list, a decoder's remainder is a suffix of its input — can be checked only at instances the author writes out by hand, as a `let` scheduled through `Test/all` over a table.

That is honest and it is not enough. The author picks the cases, so the cases are the ones the author already thought of.

## What was tried, and what it cost

The first implementation made an unsettled parameterized test a *property*: `Test/property` resolved a witness of `/std/Test/Property` at the function type over the test's explicit parameters and ran it over drawn arguments, exhausting instead when the domain was small.

It was removed for one reason above all others: **the declaration's quantifier and the report's quantifier disagreed.** `test add_commutes(n, m)` says *for all*; `passed` meant *for these hundred*. Worse, the fall from the kernel to the sampler was silent, so the same body could mean either and the report never said which.

What it cost to carry, measured over the whole repository at the time of removal:

- ~1,200 lines: `/std/Test/Property.crs` (567), `/std/Test/Draw.crs` and `/std/Test/Seed.crs` (40), 16 `Draw` witnesses across 13 modules, 9 `Property` witnesses, 441 lines of Rust tests.
- Two call sites. Of 164 `test` declarations, 154 were nullary and 5 parameterized; three of those five were `Test/refl` proofs. One of the two remaining was an editor sample.
- Of 16 `Draw` witnesses only `Bool`, `Ordering` and composites over them ever exhausted: `Byte`'s roster of 256 exceeded the case budget of 100, and ten witnesses declined `all` outright.
- It was the sole consumer of witnesses keyed on a function type, a language feature retired with it.

## What is worth keeping from it

- **`Draw` had two methods that were one idea.** `all() -> Option(List(A))` and `draw(Seed, Nat) -> {A, Seed}` answered the same question at two granularities, and every `draw` in the standard library was already a uniform pick from the values of *bounded size* — `Nat` was `n % (size + 1)`, `List`/`Str`/`Bytes` drew a size-bounded count then that many elements, and `Bool`/`Ordering`/`Byte`/`Char` ignored size and picked from the whole finite roster. Whatever replaces this should probably be one method.
- **The roster-or-nothing cliff was the wrong shape.** Ten of sixteen carriers could say nothing about their small values, so `Nat` and `List` had no exhaustive coverage at any size. A size-indexed enumeration gives every carrier a small-value prefix, makes the reported counterexample minimal by construction — the shrinking that was deferred and never landed — and removes the seed from the interface entirely.
- **Three tuning constants decided everything and none was reachable from a program**: 100 cases, a roster ceiling of 1024, and a fixed seed. Two of them disagreed, which is why `Byte` was documented as exhaustive and never was.
- **A conditional property was never expressible.** `test bounded(n: Nat, p: Nat/Lt(n, 100))` had no witness, so a claim over a constrained domain could not be stated. This is the gap where a dependently typed language should be ahead of QuickCheck, and the published route is compiling an inductive relation into a generator by narrowing.

## The seam it comes back through

Three places, and no more: `close_over_explicit`'s refusal in `curios-elab/src/builders.rs`, the `TestSyntax` slots in `curios-utilities/src/syntax.rs`, and the shape of a generator concept under `/std/Test`.

## Open questions

- Is the replacement a third meaning for a `test` declaration, or a combinator a nullary test schedules — the way a table already is?
- If it is a declaration form, what does its report say, so that its quantifier and the declaration's agree?
- Does the generator interface enumerate, sample, or interpolate; and is it one method or two?
- Are constrained domains in scope, and if so does the generator come from the relation or from the author?
