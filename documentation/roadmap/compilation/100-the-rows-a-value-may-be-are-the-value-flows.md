# The rows a value may be are the value flow's

[An entry exports only the turns it can return](../../design/compilation/an-entry-exports-only-the-turns-it-can-return.md) reads a call through a closure, a foreign row and a recursion alike in the analysis that simplifies row tests and the one that selects the entry's ABI, and rejects a second analysis beside row flow for the same question. `curios-cps` now holds two: `row_flow.rs`, which the row tests, the rebuild in place, the verifier and the entry's ABI read, and `value_flow.rs`, which `bound_frames` reads and which follows every closure call row flow reads as opaque (*A closure call reaches only what flows to it*, `curios-cps`'s README). Its next step is a ready delivery: `row_flow`'s readers moved onto the value flow, and `row_flow.rs` deleted.

## What is wrong today

`row_flow.rs` answers which rows a value may be over the flows `value_flow.rs` follows, but reads as any what the value flow sees through: a closure call's result, a read out of a value, a cell's or a channel's contents, and an escaping function's parameters. So two analyses state one flow, and `answer_row_tests` (`optimize/simplify.rs`), `rebuild_in_place` (`rebuild.rs`), the verifier's rebuild check (`verify.rs`) and `Module::entry_suspension`, which `curios-emit`'s module emitter reads, read a may-set no closure call, slot or container narrows.

## Goal

The rows a value may be are those of the constructions the value flow says it may be, or every row where that is any. `rows_reaching` and `Module::entry_suspension` are the value flow's, answering at the same names, so their readers are unchanged, and `row_flow.rs` goes.

## Settled

- **The rows it answers are never more than `row_flow`'s**, since every flow `row_flow` reads as any the value flow reads as what reaches it or as any, so a row test answered no today is answered no after. What may change is which row tests are answered no, which rebuilds are admitted and whether an entry exports `ADVANCE`, never an answer.
- **A flow the value flow misses is a wrong answer once row tests read it**, where in `bound_frames` it costs a guard, which is why its rules match each node's whole alphabet.
- **Four of `row_flow`'s tests state the rule they replace**, and each is restated with a construction its read reaches beside a control holding none: a reference read out of a value, which may be any row today, is what its construction stores; what the entry is handed, read through a closure call, a field, a cell or a channel, reaches every function of its arity some atom names and every construction of its kind; an escaping function's parameters, unknown today, take what every call hands, a closure call's among them; and a cell's or a channel's payload, unknown today, is what any fill or push stores. The rest keep their sentences.

## Stage

1. **`row_flow`'s readers on the value flow**, with `row_flow.rs` deleted and its tests restated beside the value flow's, split by theme. Check: `cargo xtask workspace test -E 'test(~value_flow) | test(~frames) | test(~rebuild) | test(~codegen::entry) | test(~codegen::census) | test(~codegen::copying) | test(~host::suspension)'`, the corpus, `cargo xtask workspace test` and `cargo xtask bundle test`.

## Verification

- Counted: the bytes of the Wasm module `curios compile --target browser` writes and the `frame/enter` guards `wonder --stage cps-optm` prints, for `programs/hello_world.crs`, `programs/monad_io.crs`, `programs/monad_suspend.crs`, `programs/spines/spines.crs`, `programs/trees/trees.crs` and a program decoding and encoding JSON, against the commit the value flow landed in.
- Timed: the `cps::answer_row_tests` span over the same programs, by `cargo xtask profile release`, against the commit the value flow landed in, five interleaved rounds, median and span, since the value flow runs where `row_flow` ran, on every optimizer round that holds a row test.

## Retirement

Delete this specification once the stage lands. The reading of opaque boundaries goes to [An entry exports only the turns it can return](../../design/compilation/an-entry-exports-only-the-turns-it-can-return.md), which names `row_flow`; [An optimizer query costs what its answer uses](202-an-optimizer-query-costs-what-its-answer-uses.md) and `verify.rs`'s rebuild check name `row_flow` too, and `curios-cps`'s README decision *A closure call reaches only what flows to it* takes the rows its readers read.
