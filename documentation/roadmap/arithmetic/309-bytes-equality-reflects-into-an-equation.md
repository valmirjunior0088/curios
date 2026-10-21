# Bytes equality reflects into an equation

**Not refined yet.** This specification reserves the law that `a == b` at `Bytes` gives `Eq(a, b)`. It is not an implementation plan. It waits for a consumer that needs the equation rather than the step.

## What is missing

`a == b` giving `Eq(a, b)` holds as a lemma for `Nat`, `Byte` and `Int` (`Int/eq_of_eql`), and for `Bytes` nothing gives it.

## What is known

- **It is a new intrinsic law.** `Bytes/eql` does not reduce over a list, so its reflection would be declared as [the declared operations](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) are.
- **Stepping confirms a match without one**, which is why no consumer has needed it yet.

It retires when `a == b` at `Bytes` gives `Eq(a, b)` as it does at `Int`.
