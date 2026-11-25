# A sequence of constant prints compiles in its length

The closed evaluator names a description's continuation where its candidate names it (`ReifyScope::in_scope`, `curios-anf/src/optimize/evaluate/reify.rs`), so a chain of constant prints folds in its length (`curios`'s `tests::growth::a_chain_of_constant_prints_folds_in_its_length`), and a replacement holding a description is held to the caps every replacement is. The write's retry still keeps the loop of its own it kept to stay under the cap those replaced. The next step is a ready fix in one stage: let the write's retry take its shape.

## What is wrong today

- **The write's retry keeps a loop of its own.** `/std/internal/Handle/retried` (`curios-prelude-build/std/internal/Handle.crs`) runs an attempt, waits on a watch of its handle where the host would block, and runs it again, beside `/std/internal/until`, the loop every wait on the program's own state goes through, which does the same around any wait. It was kept apart because folding it into `until` raised the heaviest description a constant `print` copies to 356 nodes, past the cap the evaluator no longer holds; [A wait is a suspension, and only the host waits](../../design/prelude/a-wait-is-a-suspension-and-only-the-host-waits.md) states the split, a handle's operation "retried in a loop of its own around a watch of the handle", and counts a handle's wait among the four places `/std` names the suspension pair.

## Decided

- **The write's retry is a wait like any other.** `retried` waits through `/std/internal/until`, so one loop retries every attempt after a wait. The wait decision gives a handle's retry a loop of its own because a watch reports what is ready already, which says why a handle needs no more than the loop gives, not why it needs a loop apart. Measured over a prototype on top of `e3572cdd8` with the continuation named and the cap retired, counted: a constant `print` still reaches the host write as one constant and the chain stays linear, and the module text over `programs/` grows by 15.8 to 20.2 KB in 29 programs and by 34.3 KB in `hello_world` and `dependent_vectors`, the price of one loop for every wait.

## Stage

1. **Fold the write's retry into `/std/internal/until`.** `retried` asks `until` for a reply that settled, waiting on a watch of its handle between attempts, `wait` suspends on the same watch, and `until` says every wait goes through it, as the wait decision then does, naming the suspension pair in three places: the loop, the driver and the bracket. Check: the tests of `/std/Proc/Child`, `/std/tcp` and `/std/Io`, run by `curios test`; `curios-prelude`'s corpus tests; the module text over `programs/` read again.

## Verification and retirement

The growth test and the restated tests of `curios/src/tests/fmt.rs` hold after the stage; `curios-prelude`'s corpus tests pass; the module text over `programs/` is read before and after, counted, and names the commit. Once the stage lands, this file is deleted.
