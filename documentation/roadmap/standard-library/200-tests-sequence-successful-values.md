# Tests sequence successful values

`/std/Test` sequences successful values through a sealed `Test(A)` monad, and its declaration lowering and consumers use terminal `Test({})` descriptions. Protected interpretation under brackets, scopes and timeouts remains pending.

The goal is one sealed `Test(A)` carrier with success payloads, first-failure sequencing, explicit monad embeddings, and resource protection around complete test interpretation. Test declarations remain explicit registrations and source functions remain the eligibility boundary for `!`.

## Settled contracts

- `pure` and `bind` witness `Monad(Test)`; assertions return `Test({})`; failures skip continuations. Preserve direct pure descriptions and `all`'s sequential, indexed first-failure behavior.
- Declare `Lift(Io, Test)`, identity `Lift(Test, Test)`, and one generic `Lift(Try(M, E), Test)` under `Lift(M, Test)` and `Show(E)`. Embeddings do not chain.
- Declarations lower to `() -> Test({})`. `Test/main` schedules those thunks and answers `Io(Bool)`; only `Test({})` has a program witness.
- Preserve selected-thunk forcing, per-test guest isolation, declaration order and source records, and the distinction between an answered verdict, any process exit, and a trap.
- `using` and `timeout` take function bodies; `scope` takes a function over `Io/Scope`. A private interpreter keeps outcomes inside `Io` while the entire body is protected. Cleanup follows the existing cooperative cancellation contract; no guest finalizer is promised after a trap or process exit.

## Stages and checks

1. The carrier, combinators, lifts, declaration lowering and consumers are implemented. Their checks cover payload flow, three sequential assertions, skipped effects, Try over both bases, expected errors inspected as data, privacy and non-unit terminal refusal.
2. Add protected interpretation under brackets, scopes and timeouts. Check failure after yielding, normal release, cancellation release, a started child's cancellation, and timeout cleanup.
3. Update the syntax reference and existing test design decision, migrate examples, and run default-feature workspace checks.

Once all stages land, remove this specification after redirecting references to the library documentation, syntax reference, design decision and tests. No measurement is retaken as part of this work.
