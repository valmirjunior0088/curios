# An element is read by an index operator that asks its bound

A program reads an element by a call: `List/get(xs, i)` and `Bytes/get(b, i)`, each under a bound `i < len` proved from the facts in scope, `Map/get(m, key)` under `Holds(Map/has(m, key))`, and `List/try_get` and `Bytes/try_get` answering an `Option` (`curios-prelude-build/std/List.crs`, `Bytes.crs`, `Map.crs`), where most languages write `xs[i]`. Counted on 2026-11-20 at `c39c0e8a5`: `/std` holds 51 bounded reads, `List/get` 28 and `Bytes/get` 23, beside 16 optional ones, and the corpus under `programs/` holds 33 optional reads and no bounded one, so a program reaches for the call that states no bound.

An operator is a call of its concept's method (`curios-prelude-build/std/ops.crs`), and `/` takes the implicit proof its concept's `Ok` declares, filled from the facts in scope ([A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)). Calls, projections and postfix `!` chain after an atom (`documentation/language.md`), and a bracket glued to a name is free: the packed literal is `0x[…]` ([A literal is realized by its expected type](../../design/surface/a-literal-is-realized-by-its-expected-type.md)), so `x[foo]` parses as nothing yet.

The next step is the decision *Open* holds.

## Decided

Settled by the user on 2026-11-20.

- **The operator is the bounded face.** `xs[i]` is the element, and asks its bound as an implicit proof filled from the facts in scope, as `/` asks its divisor's: the short spelling is the one that states its bound. No face stops the program at a failed bound, as Lean's `xs[i]!` does, and no total face of the same type stands beside it ([A partial primitive is totalized by a canonical extension or it states its domain](../../design/arithmetic/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md)).
- **A concept backs it.** `xs[i]` is a call of a method of a concept of `/std/ops`, resolved as any witness is: read off the intrinsic sequences alone it would be the one operator no witness answers. The witness chooses the index's type, as Rust's `Index` and Lean's `GetElem` take it as a parameter, so a program's own type takes part.
- **The optional face stays a call.** `List/try_get(xs, i)` answers an `Option`, taken apart by `!` where a region sequences. One operator, one meaning.
- **`/std` reads by the operator.** Its bounded reads are rewritten to it.
- **Rejected.** An index spelled `xs.[i]`, which is not the spelling a reader brings, as F# found, which carried it and added `xs[i]`; an index spelled as the call of the sequence it reads, `xs(i)`, as Ada, Fortran, MATLAB and Scala's `apply` spell one and Clojure's vectors answer one, a sequence being the function its bounded index reads: it asks no form of the grammar, but it is not the spelling a reader brings either, and a read stops looking unlike a call; an operator on the optional face, which would make the short spelling the one that states no bound.

## Open

- **The concept's shape.** Its name; whether the element's type is a parameter or a field its witness chooses, as `Div`'s `Ok` is a field; and its bound, `Ok` over the sequence and the index.
- **Whether `Map` satisfies it.** `Map/get` asks `Holds(Map/has(m, key))` already, the bounded shape, so a witness is one declaration, and a guard on `Map/has` puts its fact in scope; against it, a read by a key that a reader takes for a position's.
- **What a report prints.** The operator where its bound is proved again and the call with its proof where it is not, as [the tools findings](../tools/000-findings.md)' entry on an operator form states of `/`.

## Stages

1. **The concept and its witnesses**, as *Open* settles: in `/std/ops`, `List`'s and `Bytes`'s over their `get`.
2. **The operator.** A bracket glued after an atom or a link of its chain, lowered to the concept's method as an infix operator is to its own; its bound refused as `/`'s is where nothing discharges it; its printing as *Open* settles; `documentation/language.md`'s operators.
3. **`/std` reads by it.** The 51 bounded reads rewritten, and the corpus's where a bound is a fact in scope.

## Verification

- Stage 2: `xs[i]` under a guard on `i < List/len(xs)` checks, and outside one is refused as an undischarged bound naming the concept's `Ok`; `x[foo]` reads `x` at `foo` under a binder named `x`; a program's own type indexed by a type of its choosing; each mutation-checked.
- Stage 3: `/std`'s tests and the lint, which elaborates, erases and certifies all of it.

## Retirement

When stage 3 lands, the operator is `documentation/language.md`'s and its concept `/std/ops`'s, and this file is deleted.
