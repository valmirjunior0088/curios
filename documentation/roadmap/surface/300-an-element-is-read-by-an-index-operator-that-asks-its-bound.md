# An element is read by an index operator that asks its bound

A program reads an element by a call: `List/get(xs, i)` and `Bytes/get(b, i)`, each under a bound `i < len` proved from the facts in scope, `Map/get(m, key)` under `Holds(Map/has(m, key))`, and `List/try_get` and `Bytes/try_get` answering an `Option` (`curios-prelude/std/List.crs`, `Bytes.crs`, `Map.crs`), where most languages write `xs[i]`. Counted on 2026-11-20 at `c39c0e8a5`: `/std` holds 51 bounded reads, `List/get` 28 and `Bytes/get` 23, beside 16 optional ones, and the corpus under `programs/` holds 33 optional reads and no bounded one, so a program reaches for the call that states no bound.

An operator is a call of its concept's method (`curios-prelude/std/ops.crs`), and `/` takes the implicit proof its concept's `Ok` declares, filled from the facts in scope ([A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)). Calls, projections and postfix `!` chain after an atom (`documentation/language.md`), and a bracket glued to a name is taken already: `x[…]` is the packed literal, its entries terms, so `x[foo]` builds a `Bytes` of the one byte `foo` (`parse_literal("x[")`, `curios-surface/src/parse/literals.rs`).

The next step is stage 1.

## Decided

Settled by the user on 2026-11-20.

- **The operator is the bounded face.** `xs[i]` is the element, and asks its bound as an implicit proof filled from the facts in scope, as `/` asks its divisor's: the short spelling is the one that states its bound. No face stops the program at a failed bound, as Lean's `xs[i]!` does, and no total face of the same type stands beside it ([A partial primitive is totalized by a canonical extension or it states its domain](../../design/arithmetic/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md)).
- **A concept backs it.** `xs[i]` is a call of a method of a concept of `/std/ops`, resolved as any witness is: read off the intrinsic sequences alone it would be the one operator no witness answers. The witness chooses the index's type, as Rust's `Index` and Lean's `GetElem` take it as a parameter, so a program's own type takes part.
- **The optional face stays a call.** `List/try_get(xs, i)` answers an `Option`, taken apart by `!` where a region sequences. One operator, one meaning.
- **The packed literal takes a prefix no name can be, `0x[…]`**, its pattern with it, so `x[foo]` indexes `x` whatever is in scope. A hexadecimal numeral needs a digit after `0x`, so `0x[` is no numeral today.
- **`/std` reads by the operator.** Its bounded reads are rewritten to it.
- **Rejected.** `x[` refused where a binder named `x` is in scope, which leaves the literal no spelling under the commonest binder name, 642 binders in `/std` and the corpus, 17 files holding one beside a packed literal; a bracket's meaning read off what is in scope; an index spelled `xs.[i]`, which is not the spelling a reader brings, as F# found, which carried it and added `xs[i]`; an operator on the optional face, which would make the short spelling the one that states no bound.

## Open

- **The concept's shape.** Its name; whether the element's type is a parameter or a field its witness chooses, as `Div`'s `Ok` is a field; and its bound, `Ok` over the sequence and the index.
- **Whether `Map` satisfies it.** `Map/get` asks `Holds(Map/has(m, key))` already, the bounded shape, so a witness is one declaration, and a guard on `Map/has` puts its fact in scope; against it, a read by a key that a reader takes for a position's.
- **What a report prints.** The operator where its bound is proved again and the call with its proof where it is not, as [the tools findings](../tools/000-findings.md)' entry on an operator form states of `/`.

## Stages

1. **The packed literal is `0x[…]`.** The parser's literal and its pattern's run (`curios-surface/src/parse/literals.rs`, `patterns.rs`), the printer and Core text's spelling, the two tree-sitter grammars with their generated sources and the TextMate grammars, `documentation/language.md`, and every literal written: 626 in `/std`, 8 in the corpus, 32 in the documentation, 27 in fixtures, the board and the editors' samples. `x[` glued is refused, naming `0x[`, until stage 3 gives it its meaning.
2. **The concept and its witnesses**, as *Open* settles: in `/std/ops`, `List`'s and `Bytes`'s over their `get`.
3. **The operator.** A bracket glued after an atom or a link of its chain, lowered to the concept's method as an infix operator is to its own; its bound refused as `/`'s is where nothing discharges it; its printing as *Open* settles; `documentation/language.md`'s operators.
4. **`/std` reads by it.** The 51 bounded reads rewritten, and the corpus's where a bound is a fact in scope.

## Verification

- Stage 1: the suite, the corpus and the documentation's examples hold under the new spelling; `x[1]` is refused with `0x[` named; the Zed extension's pinned grammar check and the TextMate snapshots follow.
- Stage 3: `xs[i]` under a guard on `i < List/len(xs)` checks, and outside one is refused as an undischarged bound naming the concept's `Ok`; `x[foo]` reads `x` at `foo` under a binder named `x`; a program's own type indexed by a type of its choosing; each mutation-checked.
- Stage 4: `/std`'s tests and the lint, which elaborates, erases and certifies all of it.

## Retirement

When stage 4 lands, the operator is `documentation/language.md`'s and its concept `/std/ops`'s, the entry in `surface/000-findings.md` is gone, and this file is deleted.
