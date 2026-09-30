# Size cliffs: a long `let` chain, deep nesting, many bindings

**Not refined yet.** This specification reserves three places where an ordinary program the compiler accepts at one size is refused or does not finish at a larger one, left after the five measured cliffs the roadmap records as closed. Each is a cost the compiler could remove from code that is already correct. It is not an implementation plan.

## The cliffs

- **Elaboration is not linear in `let` depth.** A chain of 6,000 `let`s did not finish in 25 minutes.
- **The parser buys its depth with stack**, as the lowerings do ([Depth is bought with stack, not with hand-rolled frames](../design/toolchain/depth-is-bought-with-stack-not-with-hand-rolled-frames.md)): nested calls overflow the 8 MiB main stack at about 200 levels in a debug build, and nested parentheses or `+` at about 400.
- **Every binding gets a fresh local**, and the module is validated before Binaryen merges them, so 200 `let`s of 30 operations each panic with "too many locals".

The figures were recorded on the roadmap on 2026-09-28, without the programs that produced them.

## Refinement

Each cliff is retaken first, with a generated program and the command that runs it written here as its protocol, and on the build it names. Then each states the pass that pays superlinearly or overflows, the bound it should meet, and the fixture that holds it at a size past today's.
