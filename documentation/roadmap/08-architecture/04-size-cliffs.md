# Size cliffs: a long `let` chain, deep nesting, many bindings

**Not refined yet.** This specification reserves three places where an ordinary program the compiler accepts at one size is refused or does not finish at a larger one. Each is a cost the compiler could remove from code that is already correct. It is not an implementation plan.

## The cliffs

- **Elaboration is not linear in `let` depth**, so a long enough chain of `let`s does not finish.
- **The parser buys its depth with stack**, as the lowerings do ([Depth is bought with stack, not with hand-rolled frames](../../design/architecture/depth-is-bought-with-stack-not-with-hand-rolled-frames.md)), so deeply nested calls, parentheses or `+` overflow the main stack of a debug build.
- **Every binding gets a fresh local**, and the module is validated before Binaryen merges them, so a function holding enough `let`s panics with "too many locals".

## Refinement

Each cliff is retaken first, with a generated program and the command that runs it written here as its protocol, and on the build it names. Then each states the pass that pays superlinearly or overflows, the bound it should meet, and the fixture that holds it at a size past today's.
