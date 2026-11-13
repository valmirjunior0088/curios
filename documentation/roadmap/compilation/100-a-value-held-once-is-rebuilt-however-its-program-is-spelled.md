# A value held once is rebuilt however its program is spelled

[A value costs when it is kept, not when it is named](../../design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md) decides that a value's representation is a compiler fact decided by what its consumers do, never by how it is spelled, and that a construction beside a dying value of its own shape rebuilds that value in place where the compiler has proved it unshared and dead there. The proof (`curios-cps/src/sharing.rs`) loses a map nothing else holds to how its program came by a payload: one read back out of a map or a list takes every rebuild of the program with it. Its next step is a decision, stage one's.

## What is wrong

**A payload read back out of a map or a list shares every node built over it.** Counted at `c77e82db7`: each program is `program(TWO_WALKS, …)` of `curios/src/tests/codegen/rebuild.rs` printing the expression below, and its figure is `rebuilds_of(&module, NODE)`, the rebuilds of `/std/Map/internal/Node`'s row in the module it is emitted from.

| The program prints | Rebuilds |
| --- | --- |
| `sum(walk(n, 0, (n + 1) % 65537, Map/empty()))` | 10 |
| `TWO_WALKS_SUM`, two such walks each summed | 10 |
| `sum(Map/union(walk(n, 0, (n + 1) % 65537, Map/empty()), walk(n, 0, (n + 7) % 65537, Map/empty())))` | 0 |
| `sum(Map/filter(walk(n, 0, (n + 1) % 65537, Map/empty()), (k, v) => (k + v) % 3 == 0))` | 0 |
| `sum(Map/of_list([(n, 1), (n + 1, 2), (n + 2, 3)]))` | 0 |

The union and the filter lose the rebuilds of the walks beside them, whose maps nothing else holds, and not only their own.

## What is known

- **The cause, read from the facts.** A scratch copy prints, in `routes`, for every call of `/std/Map/internal/Node/insert`, each argument's fact, whether `aggregates` holds it and whether `sources` does, and each of the function's results' facts in both versions. In the last three programs `insert`'s payload parameter `v` is in `aggregates` and shared, and the function's second result is shared in both versions; in the first two `v` is not in `aggregates` and that result is unshared. What makes the payload an aggregate is the row: `Node` is one row, `Node(tag, opaque, opaque, opaque)`, a leaf is `Node(0, key, value, pad)` and a fork `Node(1, crit, zero, one)`, so a leaf's value and a fork's `zero` child are one slot, and `aggregates` follows a row's slot whatever the tag. A value `Node/fold` reads out of a leaf is therefore taken for a row, as an element read out of a list is taken for anything, and `sources` holds it shared; `Def::Construction`'s rule then shares the leaf `insert` builds over it, each fork over that leaf and what `insert` hands back, for every caller, the loop threading a map of its own among them.
- **The calls are known.** In all three programs the optimizer leaves `insert` called directly, from `Node/fold`'s copy in the union and the filter and from the program's own body in the list's, so no closure stands between the accumulator and its insert: the payload is what shares it.
- **A routing is never what a rebuild rests on.** `rebuild_in_place` makes its proof over the module as the copies leave it (`curios-cps/README.md`, *A construction beside a dying value of its row is rebuilt in place*), so a change to which call reaches which version needs no soundness argument, and a change to a fact of `Facts::solve` needs one.
- **Two reads of one slot must keep asking of every round.** A field read's consumption is looked for only while its value stays bound; the rule that two reads of one slot name one object is not, and cannot be narrowed the same way. A loop that hands its parameter on as it took it on one arm, after reading a slot there and storing what it read, and reads that slot again on another arm, reads on a later round the object the earlier round stored: the first read is shared by the consumption after it, and only the two-reads rule, asked across the loop's header, shares the second. By that argument the narrowing that stays sound asks more: that neither read is followed, while the value stays bound, by a consumption of the value it reads, so that no object outlives the round that read it. No test holds it yet.
- **No program at hand loses a rebuild to the two-reads rule.** A zipper over a tree, descending by a path and climbing back, holds no construction beside a dying value of its row: its descent builds a `Path` node where a `Tree` node dies and its climb a `Tree` node where a `Path` node dies, and the zipper itself travels as its fields.
- **What a mutable slot costs is measured for one row.** A slot some rebuild writes is declared mutable, which Binaryen no longer reads as the constant its construction stored and V8's collector charges a barrier on. For `Node` it costs nothing measurable; a loop started on a constant or a literal now rebuilds rows of a program's own, which no measurement covers.

## Goal

Every program of the table keeps the rebuilds its walks keep alone, and the union's, the filter's and the list's own inserts rebuild where the accumulator is held once. A payload that is a row some other holder still reads shares what a read of it hands out and nothing else.

## Decisions

Settled:

- The proof stays one module's, made at compile time, with no test of uniqueness at run time and none that a value is no constant; the decision's **Rejected** paragraph holds.
- A stage lands with the hazard it could open as a test: a program whose answer shows a rebuild of what something else holds, compared with the module the optimizer leaves unrebuilt, as `a_map_something_else_still_holds_answers_as_if_copied` compares.

Open, and stage one's to settle:

- **Whether a payload is told from a child by its tag, or a slot carries its own sharing.** By tag: `aggregates` follows a row's slots by the constructor tag a construction writes and a read's tag test decides, as `constant_tags` follows constants, so a leaf's value is a row only where some leaf stores one. It admits a payload that is a number and leaves a map of rows read back shared. By slot: a construction storing a shared value in a slot marks that slot shared for its row and stays unique itself, so a leaf over a shared payload is a leaf nothing else holds and only a read of its value is shared. It admits every payload, and needs a rule for which slots are judged so: a child's slot judged by its row would share the children of every node once one program inserts into a tree something else holds, which `a_search_tree_shared_in_one_place_and_threaded_in_another_rebuilds_in_a_copy` keeps rebuilding today.

## Stages

1. **The payload.** Settle the open decision by prototype, counting the table under each, then land the one chosen. Check: the table's last three rows keep at least the first row's rebuilds, as tests beside `map_insert_rebuilds_its_node_in_each_of_two_loops_threading_a_map_of_its_own`; a map of maps unioned and read again answers as it does unrebuilt, in the hazard suite and in `a_program_answers_alike_rebuilding_in_place_or_not`.
2. **What a mutable slot costs a row of a program's own.** A bounded investigation: `a_loop_started_on_a_constant_rebuilds_the_forks_it_takes_apart`'s program and `a_loop_started_on_a_literal_rebuilds_the_links_it_takes_apart`'s at an input that runs a second, timed with the row's rebuilds made and with them suppressed in a scratch copy, seven interleaved rounds, median and span, natively and under V8 as the decision's figures are. It ends with those figures in `curios-cps/README.md`'s decision, or with a finding where a row pays.
3. **Two reads across a loop's rounds.** Waits on a consumer: a program that holds a value once and loses a rebuild to two reads of one slot on arms of a loop that excludes each other. The narrowing above is its design; its hazard is the loop that hands its parameter on as it took it.

## Verification

`cargo xtask workspace lint`; `curios-cps`'s suite, `curios`'s `tests::codegen::rebuild` and `curios-prelude`'s corpus, `cargo xtask workspace test suite -E 'package(curios-cps) | package(curios-prelude) | test(~codegen::rebuild)'`. The phase stays a few percent of a compile: the two timings `curios-cps/README.md`'s rationale states are retaken with and without it after stage one.

## Retirement

Stage one's rule and its figures go to `curios-cps/README.md`'s decision and `sharing.rs`'s module documentation, stage two's figures to the same decision. Stage three is deleted with the rest where no consumer has shown by then: a limitation with no program is no pending work.
