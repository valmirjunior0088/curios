# A closure call reaches only what flows to it

Its benefit is beyond doubt: every edge it removes is spurious, so what it saves is size and guards and never an answer (*What it costs*). So is its approach: what a closure call may reach is what a control-flow analysis over closure values answers, and the precision it is for fixes its key, a row's slot (*Settled*). Its next step is a ready delivery: the analysis, then its use by `bound_frames`, measured by the protocol below.

## What is wrong today

`curios-cps/src/frames.rs`'s `Cycles::of` reads the call graph a recursion's depth is bounded over. For a call through a closure or a list map it adds an edge to every function some atom names that takes as many parameters, since nothing in `curios-cps` says which functions a closure value may hold. A function that never recurses is then a member of a cycle wherever it calls something that calls a closure of its arity: the `Io` driver's resume (`io/resume`, `io/frames`), a by-need knot's forcing (`knot/produced`, `force/resume`), and the lambdas a program hands a combinator. Each such member is guarded and copied into a heap twin.

## What it costs

The cost is size and guards, never an answer. Counted at `f9322f1f9`, as the bytes of the Wasm module `curios compile --target browser` writes, against the same tree with the closure edges left out of the graph:

- `programs/spines/spines.crs`, `programs/trees/trees.crs`, `programs/monad_io.crs` and `programs/hello_world.crs` grow by 2–7%, `programs/monad_suspend.crs` by 8%;
- a program decoding and encoding JSON grows by 28%. Much of that is genuine: `Json/decode`'s parser combinators recurse through closures, and an exact analysis keeps their twins.

Leaving the edges out is not an option: a recursion through a closure would keep its frames native and stop on the engine's stack.

## Goal

A closure call's edges go to the functions whose closures may flow to the callee, and a list map's to those that may flow to its mapper: a 0CFA over closure values, as MLton's closure conversion uses ([Cejtin, Jagannathan and Weeks](https://www.doi.org/10.1007/3-540-46425-5_4)).

- A value's fact is the set of functions whose closures may reach it, or any escaping function of its arity.
- An `Atom::Fun` contributes its function. A known call's argument flows to the parameter, a jump's to the continuation's parameter, a return to the call's resume.
- A construction's operand flows to its row's slot or its tuple's field, and a read of that slot or field takes the slot's fact. A list's elements share one fact.
- What the module does not see — an escaping function's or the entry's parameters, what a closure call, a host, a cell or a channel hands back — is any escaping function of its arity.
- The analysis runs on the shared solver as `row_flow` does, beside it in `curios-cps`.

## Settled

- The edges stay arity-filtered where the fact is any, so nothing becomes less sound than today.
- `Reach::Closure` and `Reach::Map` keep their meaning; `Cycles` asks the analysis what a site may reach.
- The fact is keyed by a row and its slot. The `Io` driver's descriptions are closures stored in rows, and a key per row joins what every slot of a row may hold, which may keep the spurious cycle through the driver that the analysis exists to remove; a key per slot costs a fact a slot rather than a row. `row_flow` reads nothing out of a value, so it has no key of its own to share.

## Stages

1. **The analysis**, with unit tests in `curios-cps`: a closure handed through a known call, a jump, a return and a row slot is resolved; one from a host or a cell is any. Check: `cargo xtask workspace test -E 'test(~closure_flow)'`.
2. **Its use by `bound_frames`.** Check: `curios`'s `codegen::frames` tests and the corpus, `cargo xtask workspace test`, and `cargo xtask bundle test`.

## Verification

- Counted: the page sizes above, retaken, and the count of twinned functions `wonder --stage cps-optm` prints for each program, before and after.
- `curios`'s `codegen::frames::every_native_call_a_recursion_makes_of_itself_asks_the_budget` and the differential tests hold the answers alike.

## Retirement

Delete this specification once both stages land; the analysis's contract goes to its rustdoc and `frames.rs`'s module documentation.
