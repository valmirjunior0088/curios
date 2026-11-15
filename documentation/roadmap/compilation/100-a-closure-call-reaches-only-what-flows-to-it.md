# A closure call reaches only what flows to it

Its benefit is beyond doubt: every edge it removes is spurious, so what it saves is size and guards and never an answer, and it is most of what `bound_frames` costs a program whose only recursion is the `Io` driver's (*What it costs*). So is its approach: what a closure call may reach is what a value may be, a 0CFA's answer, which computes the call graph it reads and abstracts a closure by its function and an aggregate by its construction; and the rows a value may be are the same answer's, so `row_flow` is folded into it (*Settled*). Its next step is a ready delivery: the analysis with `bound_frames` reading it, then `row_flow`'s readers moved onto it.

## What is wrong today

`curios-cps/src/frames.rs`'s `Cycles::of` reads the call graph a recursion's depth is bounded over. For a call through a closure or a list map it adds an edge to every function some atom names that takes as many parameters, since nothing in `curios-cps` says which functions a closure value may hold, and `Cycles::within` asks the same set, so a function some atom names that calls a closure of its own arity is a cycle of one. Every `io/bind` the erasure of a bind writes is such a function, a nullary description forcing a nullary one, and so is a by-need knot's forcing, which calls the initializer it takes out of a channel. Each such member is guarded and copied into a heap twin, the continuations its calls resume lifted to functions (`heap/knot/produced`, `heap/force/resume`).

`curios-cps/src/row_flow.rs` asks the same question of the same flows, what a value may be, for rows: it answers a closure call's result, a read out of a value and a cell's or a channel's contents with every row, so `answer_row_tests`, `rebuild_in_place`, the verifier and `entry_suspension` read a may-set no closure call, slot or container narrows.

## What it costs

Counted at `7281dca52`, as the bytes of the Wasm module `curios compile --target browser` writes and the members of the components `bound_frames` guards: today; with a prototype of the first stage; and with `Cycles` answering no function for a closure call or a list map, which is unsound and the floor no analysis passes.

| program | today | the analysis | no closure edge |
| --- | --- | --- | --- |
| `programs/hello_world.crs` | 25 556 · 10 | 21 456 · 1 | 21 456 · 1 |
| `programs/monad_io.crs` | 48 233 · 11 | 34 014 · 1 | 34 014 · 1 |
| `programs/monad_suspend.crs` | 97 404 · 39 | 84 442 · 21 | 53 204 · 3 |
| `programs/spines/spines.crs` | 58 556 · 12 | 44 945 · 4 | 44 945 · 4 |
| `programs/trees/trees.crs` | 48 026 · 11 | 35 196 · 3 | 35 196 · 3 |
| a program decoding and encoding JSON | 185 341 · 35 | 151 192 · 12 | 118 547 · 6 |

What the analysis keeps past the floor is recursion a context-insensitive analysis does not tell from a real one: `Json/decode`'s parser combinators recurse through closures, and `Io/both`'s scheduler advances again the suspensions an advance surfaced, so a surfaced suspension and one inside it meet in `io/resume`. The JSON program reads its text from its first argument, decodes it with `Json/decode`, and prints `Json/encode`'s answer.

Leaving the edges out is not an option: a recursion through a closure would keep its frames native and stop on the engine's stack, as `curios`'s `codegen::frames` tests show by the closure picked from a list.

## Goal

One analysis on the shared solver answers what each value may be, followed forward through every flow: the functions it may be a closure of and the constructions it may be, a row's or a tuple's, or any; a 0CFA ([Shivers](https://www.ccs.neu.edu/home/shivers/papers/diss.pdf)).

- An `Atom::Fun` is its function, a construction is itself, a rebuild is what its `of` may be, a payload read past `IsImmediate` is what its operand may be, and a literal or a filler is nothing.
- A known call hands its arguments to the callee's parameters and the callee's returns to the call's resume. A closure call does both for every function its callee may be, so the analysis computes the call graph it reads. A jump hands its arguments to the continuation's parameters. A list map hands the list's elements to every function its mapper may be, and their returns to the mapped list's elements.
- A construction's operand flows to its slot or field under that construction and under its row's summary, or its arity's for a tuple. A read takes the slot of every construction the value read may be, and the summary where it may be any. A rebuild writes the slots of every construction its `of` may be, and of every construction of its row where that is any.
- A list's elements, a cell's contents and a channel's contents are one key each: what is stored into any is what any hands out.
- Any is what the module does not see: the entry's parameters, and what a heap call answers, which only a module `bound_frames` rewrote holds. A host's reply is its wire values, no closure and no construction, and the advance enters only the descriptions the entry surfaced, which take no parameter.
- It answers what each closure call and list map may reach, and the rows each value may be: those of the constructions it may be, or every row where it may be any.

## Settled

- **A closure is abstracted by its function, an aggregate by its construction.** One abstraction for both kinds of value is what tells the `Io` driver's surfaced suspension from the ones inside it, which share every slot of the suspension row. Keyed by the row's slot, a full 0CFA keeps `io/surface`, `io/rest` and `io/cancel` guarded beside `io/resume`: 4 members in `programs/hello_world.crs`, where the construction's key leaves 1, and three more than it in each program the table measures but `programs/monad_suspend.crs`, whose scheduler mixes the two suspensions anyway. A key per row slot is also coarser than MLton's key per constructor, which closure conversion reads over a monomorphic program ([`closure-convert.fun`](https://github.com/MLton/mlton/blob/master/mlton/closure-convert/closure-convert.fun)): a row is shared by every instantiation of its family, and its slots by every constructor of a carrier.
- **A closure call is followed, never opaque.** Answered with any, a bind continuation's answer reaches every description, and a knot's initializer every nullary function: the knot's forcing stays lifted as today, `programs/hello_world.crs` keeps 4 members and the JSON program 16. Taking every function some atom names as called from outside, its parameters any, would answer any at every call through one of them alike.
- **A list's elements, a cell's contents and a channel's contents take one key each, not one per construction.** Keyed by its construction, a list operation that passes its operand's elements on answering its operand's lists, as MLton unifies an array with the vector it becomes, a container keeps every member one key each keeps in each program the table measures: what reaches the residue through a container reaches it under either key, so the finer one would follow every list, cell and channel operation for nothing.
- **The edges stay arity-filtered where the fact is any**, so nothing becomes less sound than today.
- **`Reach::Closure` and `Reach::Map` keep their meaning, and `Cycles` asks the analysis what a site may reach.** A site `bound_frames` copies at a fresh identity — `copy_alone`'s copy, the guard's native call, a twin's call resumed through a thin continuation — is answered by the site it was copied from.
- **`row_flow` is the analysis's rows.** [An entry exports only the turns it can return](../../design/compilation/an-entry-exports-only-the-turns-it-can-return.md) reads a closure call, a foreign row and a recursion alike in the analysis that simplifies row tests and the one that selects the entry's ABI, and rejects a second analysis beside row flow for the same question; a 0CFA following closure calls beside a row flow treating them as opaque would be two readings of one call. The rows it answers are never more than `row_flow`'s, so a row test answered no today is answered no after.
- **A flow the analysis misses is a wrong answer once row tests read it**, where in `bound_frames` it costs a guard. So every rule matches its node's whole alphabet, with no wildcard over `Node`, `ValueExpr` or an operation that reads a reference.

## Stages

1. **The analysis, `curios-cps/src/value_flow.rs`, read by `bound_frames`**, which is its first reader, since the workspace denies an item no production code reads. Its unit tests: a closure handed through a known call, a jump, a return, a closure call's argument and its answer, a list map, a row slot, a tuple field, a list, a cell and a channel is resolved; two constructions of one row keep their slots apart; a rebuild writes the slots of what its `of` may be; what the entry is handed is any. Check: `cargo xtask workspace test -E 'test(~value_flow) | test(~frames)'`, the corpus, and `cargo xtask bundle test`.
2. **`row_flow`'s readers on it** — `answer_row_tests`, `rebuild_in_place`, the verifier and `entry_suspension` — with `row_flow.rs` deleted and its tests restated on the analysis. What may change is which row tests are answered no, which rebuilds are admitted and whether an entry exports `ADVANCE`, never an answer. Check: `curios-cps`'s tests, the corpus, `cargo xtask workspace test` and `cargo xtask bundle test`.

## Verification

- Counted: the table above, retaken after each stage.
- Timed, after the second stage: the `cps::answer_row_tests` span over the corpus programs above, by `cargo xtask profile release`, against `7281dca52`, five interleaved rounds, median and span, since the analysis runs where `row_flow` ran, on every optimizer round that holds a row test.
- `curios`'s `codegen::frames` tests hold every recursion a million deep, the one through a closure picked from a list among them, and the differential tests hold the answers alike.

## Retirement

Delete this specification once both stages land. The analysis's contract goes to its rustdoc, the graph `bound_frames` reads to `frames.rs`'s module documentation and to [A recursion is as deep as the heap allows](../../design/compilation/a-recursion-is-as-deep-as-the-heap-allows.md), which states the arity edges today, and the reading of opaque boundaries to [An entry exports only the turns it can return](../../design/compilation/an-entry-exports-only-the-turns-it-can-return.md), which names `row_flow`; [An optimizer query costs what its answer uses](202-an-optimizer-query-costs-what-its-answer-uses.md) and `verify.rs`'s rebuild check name `row_flow` too.
