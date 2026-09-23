# Alternate floating-point exception handling

Working specification for the remaining IEEE 754-2019 §8 exception-handling capabilities in `/std/Flt/Env`. These apply to existing operations and do not wait for the [elementary functions](flt-elementary-spec.md). The two specifications can complete and retire independently.

## Scope and existing behavior

§8 is provided at block scope: `Env/checked(e, m)` is delayed transfer, answering `none` when `m` raised any exception of `e`, and `Env/substitute(e, v, m)` replaces `m`'s answer with `v`. Not provided:

- **`abruptUnderflow`** — flushing a tiny result to zero, or to the least normal, in place of a subnormal;
- **`substituteXor`** — substitution with the sign the operands' signs would give the result;
- **substitution and recording per operation**, rather than per block — §8's attributes attach to an operation, and an `Env` block of one operation is the nearest the surface comes today.

Each is an `Env` combinator over the status the monad already threads; the per-operation form needs the `rounded`/`signals` pair of an operation to be reachable from the combinator, which `Env`'s own operations already hold.

## Contracts to specify with the combinators

Preserve the existing separation between an operation's rounded result, the exceptions it raises, and the environment's accumulated status. State which exceptions each new policy observes, what value it substitutes, what status it records, and how it composes with block-level `checked` and `substitute`.

The public signatures, policy scope and composition order must be specified before implementation. In particular, define how a per-operation policy receives the operation's rounding and signal behavior without evaluating the operation inconsistently, and which operands determine a substituted sign. Recording these requirements does not select an additional environment representation or a second exception mechanism.

## Verification

- Exercise abrupt-underflow behavior at both signs, zero, subnormal values and the least-normal boundary, checking the resulting value and recorded status in every applicable rounding direction.
- Exercise sign substitution over operand-sign combinations, including signed zeros, and verify the selected result's bits.
- Distinguish per-operation substitution and recording from block-level behavior with sequences containing affected and unaffected operations. Check accumulated status, policy scope and composition with existing combinators.
- Hold the affected results and status to independently stated expectations derived from the chosen contracts, and check folded and executed behavior. Existing default behavior remains covered when no new policy is selected.

## Non-goals

Elementary-function implementation, changing the primitive floating-point model or its NaN rule, and redesigning the default rounding interface.

## Completion and retirement

The three requested capabilities are provided with their contracts and evidence, or each is explicitly declined with its reason. An unresolved design question is not a declined capability.

Before deleting this specification, record the public contracts in `/std/Flt/Env` documentation and signatures, the tests beside their owning operations, and the design rationale and rejected alternatives in the appropriate library documentation. Update the roadmap and remaining references, including the elementary-functions specification, and verify that no reference to this filename remains. Completion does not depend on delivering elementary functions.
