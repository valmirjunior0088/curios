# What unbounded `Nat` and `Int` still cost at run time

**Not refined yet.** This specification reserves the run-time costs [Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md) accepted. It is not an implementation plan.

## What is known

- A `Nat` or `Int` field in a row is a reference, so a read tests and unboxes it.
- An arithmetic result is boxed and unboxed between the steps of a chain, since each step's result may outgrow the i31.
- The fast path pays a tag test per reference operand.

## Previously discussed

Compiling a single-use chain of steps as one 64-bit computation with overflow checks, falling back to the `big/` helpers only on overflow; and typing a field at a word where every value it can be handed is one. Each is measured on `programs/` before it lands, and a decision records the carrier it leaves.
