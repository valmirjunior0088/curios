# A chain of `Nat` or `Int` steps runs in a word

**Not refined yet.** This specification reserves the costs [Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md) accepts, taken back where a program never outgrows a word. It is not an implementation plan.

## What is missing

**An unbounded `Nat` or `Int` is a reference between steps.** [Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md) accepts three costs: a `Nat` or `Int` field in a row is a reference, so a read tests and unboxes it; an arithmetic result is boxed and unboxed between the steps of a chain, since each step's result may outgrow the i31; and the fast path pays a tag test per reference operand.

## Refinement

- **`Nat` and `Int` past a reference.** Compiling a single-use chain of steps as one 64-bit computation with overflow checks, falling back to the `big/` helpers only on overflow; and typing a field at a word where every value it can be handed is one. Each is measured on `programs/` before it lands, and a decision records the carrier it leaves. A step that is modular by intent, a generator's or a digest's, is [arithmetic modulo a word](../arithmetic/300-arithmetic-modulo-a-word.md)'s, which weighs this cost against a carrier of its own.
