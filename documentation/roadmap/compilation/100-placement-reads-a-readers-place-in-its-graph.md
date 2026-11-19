# Placement reads a reader's place in its graph

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) has a read visit a node once. Placement (`sink_computations`, `curios-cps/src/optimize/sink.rs`) reads where each reader of a computation stands by climbing, a node at a time, the chain before it: its census keeps each node's predecessor along its chain as its `parent`, so `site_in`, which finds where a reader reads within the function being placed, steps through every node between the reader and that function's body, and `between`, which lists the continuations between a computation and a reader, through every node between the reader and the computation, each once a reader. A body's readers then cost the body before each.

The next step is stage 1, a delivery.

## What is wrong

- **The climbs, counted.** At `3b03c7d7c`, by steps sampled per walk over `curios`'s `Growth::Loops` (`tests::growth`), a program of self-tail loops each called once from its entry, compiled through `wonder --stage wasm`: `site_in` steps 1.44, 5.43 and 21.2 million times at 128, 256 and 512 loops, and `between` 0.19, 0.72 and 2.81 million times; `container`, `terminator`, `avoidable`, `repeats` and the readers read grow linearly, and `holds` takes no step. `sink_computations` retires 1.38 G instructions at 256 loops and 5.60 G at 512, and 59.4 G of the whole `/std` test program's 666 G optimizer, counted by [An optimizer query costs what its answer uses](200-an-optimizer-query-costs-what-its-answer-uses.md)'s protocol at `d8f53c21c`.
- **`container` climbs as they do**, the chain before a node to the continuation or function whose body it is; it is read once a computation and once a transfer a path holds, which is why it grows linearly here and need not elsewhere.

## What is known

- **A move stays within its function, and so does what the pass makes.** A computation moves into a continuation of the function being placed, and every node and continuation the pass makes — a branch's join and arm, a landing, a jump entering a resume — is made in that function, so the function owning a node is fixed through the pass.
- **Every reader stands below what it reads.** Scoping puts a reader of a computation after it along its chain, or in a continuation or a function defined after it, so the continuations between the two are those a reader's block is nested in, up to the computation's.
- **Four moves change what holds a chain.** `move_binding` takes one node out of a chain and puts it at the head of a continuation's body; `land` inserts a `LetCont` within a chain and makes a landing whose body is one jump; `move_call` leaves a jump where a call stood, and puts the call at the head of its target's body, the target's old body becoming a landing's whole, or the call's continuation where that body only hands its results on; `branch` rewrites a decided operation into a switch over its outcome, and the rest of its chain past it becomes a join's body — the one move that splits a chain in two.
- **`Shape` records a node's place as its block and its position along it** (`curios-cps/src/sharing.rs`), read once and never moved, which is the rebuild's and not this pass's, since placement moves what it reads.
- **A split is paid for on its smaller side.** Splitting a sequence and naming the smaller piece afresh, found by walking both pieces from the split point in step, costs every split together the logarithm of the sequence a node, as Hopcroft's minimization processes the smaller half of a split block (Hopcroft, *An n log n Algorithm for Minimizing States in a Finite Automaton*, 1971).

## Decided

- **A site is found by the functions a reader is defined in.** The census files each node's owning function — every live node where the census is made, and each node the pass adds under the function being placed — so `site_in` climbs from a reader's owner through the `LetFun` binding each function inside the one being placed, and reads no chain.
- **A container is a chain's.** The census files each node's chain and each chain's container, and each move keeps both: a node moved or added takes the chain it joins, a landing that takes a whole body changes that chain's container, and a split names its smaller side's nodes afresh. `container` is then a lookup, and `between` climbs from a reader's container through the `LetCont` holding each continuation up to the computation's container, a step a continuation it answers.
- **Debug builds hold the index to the chains** at the end of each placement, the chains walked once from each block's head, as debug builds verify every round of the optimizer.

## Stages

1. **`site_in` by the reader's owner.** The owner index, filed where the census is made and where the pass adds a node; `site_in` climbs functions. Check: `sink`'s tests; the corpus's Wasm byte for byte and the whole `/std` test program passing; `site_in`'s steps over `Growth::Loops` linear. One commit.
2. **`container` and `between` by chains.** The chain index and each chain's container, kept by `move_binding`, `land`, `move_call` and `branch`, a split relabelling its smaller side; the debug-build check. Check: as stage 1, with `between`'s steps linear and `sink_computations`' instructions over `Growth::Loops` under a slope of three. One commit.

## Verification

`cargo xtask workspace lint`, `curios-cps`'s and `curios-prelude`'s tests, the corpus's Wasm against the commit before, and the steps and instructions above counted over `Growth::Loops` at 128, 256 and 512 loops.

## Retirement

Once stage 2 has landed and `growth_slopes` holds `Growth::Loops` among the growths every stage of which is linear (`curios/src/tests/instrumented/growth.rs`), the decision's rule holds of placement as stated, [An optimizer query costs what its answer uses](200-an-optimizer-query-costs-what-its-answer-uses.md) names it no longer, and this file is deleted.
