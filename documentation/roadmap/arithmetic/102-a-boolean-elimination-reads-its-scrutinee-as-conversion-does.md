# A Boolean elimination reads its scrutinee as conversion does

[Conversion is one relation in both checkers](../../design/soundness/conversion-is-one-relation-in-both-checkers.md) holds conversion to congruence, and [A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md) holds that no verdict follows how a term is spelled or the order it holds its operands in. A `Bool` match whose scrutinee conversion holds equal to a literal takes no arm unless the fold reaches the literal itself, so a law conversion decides of two `Bool` terms does not hold under a `match`.

**Not refined yet.** It waits on the design of where a `Bool` match asks the readers about its scrutinee, under *Refinement*, and on [Boolean agreement is transitive and closed under substitution](100-boolean-agreement-is-transitive-and-closed-under-substitution.md), whose rung it would ask.

## What is wrong

Probed at `0fc2fc1bd` with the release compiler:

- **Commutativity under a context.** `Eq()(b && Bool/not(b), Bool/not(b) && b)` closes by `Eq/refl()`, and `Eq()(match b && Bool/not(b) | true => 1 | false => 2 end, match Bool/not(b) && b | true => 1 | false => 2 end)` is refused: the second `match` reduces to `2` and the first stays stuck.
- **A scrutinee conversion decides.** Against `2`, `Eq/refl()` is refused for `match s | true => 1 | false => 2 end` at `s` each of `b && Bool/not(b)`, `b && Bool/xor(b, true)`, `(b && c) && Bool/not(b)`, `(a0 || a1) && Bool/not(a0) && Bool/not(a1)` and `(x < y) && (y <= x)`, each of which converts with `false`.
- **A type that is such a match.** A binder `f: match b && Bool/not(b) | true => Nat | false => (Nat) -> Nat end` is refused at `f(1)`, "applied a non-function".

A written transport closes each: `Eq/subst` along `Eq()(false, b && Bool/not(b))`, itself `Eq/refl()`.

The fold of `&&` and `||` reads its right operand as written under a stuck left (`reduce_bool_binary`, `curios-core/src/reduce/intrinsic/scalar.rs`), so its complement law sees `Bool/not(b) && b` and not `b && Bool/not(b)`; and every law across connectives is decided only where conversion compares two `Bool` terms, never at a scrutinee. `then_laws`' documentation (`curios-core/src/reduce/intrinsic/laws.rs`) says a law missed on an unreduced operand "is a neutral the next demand reduces"; the next demand reduces it to the same neutral.

`tests::laws::contexts` places the structural seeds under a hole of every former, a `Nat` match's scrutinee among them; no seed there is a law of the law table, and no context is a `Bool` match's scrutinee, so the audit asks none of the above.

## What is known

- **Why the fold stops where it does.** [A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) rejects reducing a tautology to `true`, "which takes the table into the fold", and `reduce_bool_binary`'s documentation states why a connective's right operand stays as written: reducing it made weak-head reduction of a web of predicate definitions its full normalization, `2ⁿ` unfoldings under any demand on its top.
- **Why conversion alone does not answer it.** [A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md) rejects completing a fold or a lookup only where conversion compares: "A scrutinee inside reduction would still be decided by spelling." The type above is such a scrutinee: no comparison reaches it before the application is refused.
- **Where a judgment's reduction already asks.** A judgment's reduction asks its checker at two points, an arm's equation through `curios-analysis`'s `answers` and a stuck fold taken again over its classes, and plain reduction asks nothing ("There are two reductions"). A `Bool` match asking the readers whether its scrutinee is `true` or `false` would be a third, and an equation recorded under a scrutinee is already answered through the same readers.
- **Prior art.** Coq Modulo Theory's eliminations match modulo its theory, "matching modulo T for the weak and strong elimination rules" (Jouannaud and Strub, [*Coq without Type Casts*](https://easychair.org/publications/paper/BKQ), LPAR-21, 2017), the metatheory [The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) rests on.
- **What it would cost.** One diagram of the scrutinee a stuck match, constant where it is `true` or `false` (arithmetic 100's counts: linear on the shapes programs write), against every weak-head reduction of a stuck match on a connective, kept as a reduct is.

## Refinement

- **The design.** Which reduction a stuck `Bool` match asks in, in both checkers alike; what it asks, the scrutinee's diagram against the two literals; what it charges; and whether the fold's own laws change or stand.
- **The decisions it moves.** The rejection of reducing a tautology to `true`, restated for what replaces it, and `then_laws`' documentation corrected.
- **Check**: the three witnesses above close in both checkers; `tests::laws::contexts` gains a `Bool` match's scrutinee and places under it each `Bool` law of the law table whose side reduces to a literal; `/std`'s image digest is compared at one path and what moved named.
