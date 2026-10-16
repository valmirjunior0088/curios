# Tests sequence successful values

The sealed `Test(A)` monad, declared embeddings, terminal `Test({})` lowering and consumer migration are implemented, as are protected interpretation under `using`, `scope` and `timeout`. Their contracts belong to [`/std/Test`](../../../curios-std/src/Test.crs), [the syntax reference](../../syntax.md#test-declarations) and [the test design decision](../../design/tools/a-test-is-a-declared-description-and-a-proof-is-a-let.md).

## Remaining verification

Run the complete default-feature workspace suite against the monad and resource changes, including the standard-library corpus, test-runner integration and documentation consumers. The focused sequencing, terminal-type, verdict, suspension and cancellation regressions live in `curios/src/tests/harness/` and `curios-std/src/tests.rs`.

## Retirement

After the workspace checks pass, verify that nothing references this specification and remove it. No measurement is retaken.
