# A read after an append does not flatten the run

Working specification for what a read costs between two appends. A `Bits`, a `Bytes` and a `List` are ropes: an append is one node, and the first read of an appended node copies the whole of it into a flat payload. A loop that appends and reads in turn therefore copies everything it holds at each turn, and a stream's reader is such a loop. This spec has a read answer from the part that holds its index where that part is already flat, and leave the node unflattened.

It is independent of every other spec.

## What is missing

**The cost model names the hazard.** `curios-emit/src/types.rs` states it: "The documented hazard: *alternating* append and whole-value reads re-forces per read (the new node above a cached one is uncached), which is quadratic. Build fully, then read." One element is such a read: "a single `get` or `slice` on an uncached node — one element read is a whole-value read there".

**The read helper flattens a node it meets.** `$<carrier>/read` (`emit_read_func`, `curios-emit/src/rope_emitter.rs`) answers a leaf from its payload and a window through its base, and where the rope is a node with no cache it calls `force`, which fills a fresh payload of the node's whole length. The bit grain's `emit_bits_read_func` is its twin. A read at its use site takes an immediate and a leaf apart inline and calls the helper for the rest (`emit_bin_get`, `curios-emit/src/code_emitter.rs`), so the helper's node arm is the one place a node is read.

**A stream's reader alternates.** `Io/Reader/take` (`curios-std/src/Io/Reader.crs`) appends each arrival to the bytes its reader holds, `x[..then.bytes, ..fresh]`, and asks its piece again, and a piece's search reads a byte of what it is handed. Every stream of `/std` is read through that loop. A scratch program of the shape holds the bytes it has read, appends each arrival of standard input to them and searches what it holds for a newline from where its last search ended, by a recursion over `Bytes/get` under `Nat/lt/try`, counting the lines and their bytes. Compiled with `curios compile` once asking `Io/read(Io/stdin, 65536)` and once asking 1,024, and fed from a file one line of 1,000,000, 4,000,000 and 16,000,000 letters and then `second`, it takes 0.029, 0.102 and 0.533 s in arrivals of 65,536 bytes and 0.053, 0.405 and 9.819 s in arrivals of 1,024: medians of five readings each, 0.029 to 0.031, 0.101 to 0.107 and 0.524 to 0.536 s, and 0.052 to 0.055, 0.402 to 0.415 and 9.729 to 10.302 s. Timed at `5fbe62825` through the release compiler on a quiet machine: sixteen times the line costs a hundred and eighty times the time where an arrival is small.

**The corpus has its program.** `programs/rope_push_peek.crs` alternates one append with one indexed read over a list growing to N, and `programs/README.md` says what it is for: "it exists so that a change to how a rope answers a read has a program to be measured on". At N of 20,000, 40,000 and 80,000 it takes 0.150, 0.537 and 1.959 s, where its control, `programs/rng_manual.crs`, takes 0.006 s at 80,000: medians of five, 0.147 to 0.151, 0.501 to 0.551, 1.922 to 2.020 and 0.005 to 0.006 s, timed at `5fbe62825` on a quiet machine.

## Prior art

- **A rope's fetch is a descent, and its concatenation a root node.** Boehm, Atkinson and Plass fetch the ith character by a look-up in a search tree, examining "the tree containing the proper position, as determined by the length fields", and concatenate without rebalancing: "binary concatenation normally simply adds a root node, and does not rebalance the tree", which "trades speed of the substring and fetch operations for better concatenation performance" ([Ropes: an Alternative to Strings](https://www.cs.tufts.edu/comp/150FP/archive/hans-boehm/ropes.pdf), Software: Practice and Experience 25(12), 1995).
- **V8 flattens in place, as the rope here does.** "Non-flat ConsStrings are physically flattened by allocating a sequential string with the same data as the given string. The input `string` is mutated to a degenerate form, where the first component is the new sequential string and the second component is the empty string", and of a read of one character, "Repeated calls to this method are not efficient unless the string is flat" ([`string.h`](https://github.com/v8/v8/blob/main/src/objects/string.h)).
- **SpiderMonkey keeps the buffer it flattened into.** A rope is flattened "if and when a linear char array is requested". "To avoid copying the leftmost string when flattening, we may produce an "extensible" string, which tracks not only its actual length but also its buffer's overall size", and where one "appears as the leftmost string in a subsequent flatten, and its buffer has enough unused space, we can simply flatten the rest of the ropes into its buffer, leaving its text in place" ([`StringType.h`](https://searchfox.org/mozilla-central/source/js/src/vm/StringType.h)).
- **Erlang appends in place to the latest version alone.** An append to a binary is "specially optimized by the runtime system to avoid copying the `Acc` binary every time", the object allocated at "twice the size of `Bin1` or 256, whichever is larger", and "only the binary returned from the latest append operation will support further cheap append operations" ([Constructing and matching binaries](https://www.erlang.org/doc/system/binaryhandling.html)).

## Decisions

Each settled on 2026-10-13.

- **A read of an unflattened node answers from the child that holds its index, where that child is flat.** The child is the left where the index is below the left's length and the right otherwise. A leaf answers from its payload, a window through its base and a flattened node from its cache, as the helper answers each today where it is the rope read; the node is left as it is. One level, and no further.
- **A child that is itself unflattened flattens the node it is read through, as today.** A walk over a deep chain that was never read flattens it at its first read and reads one array from there on, which is what the cost model is built for and what the corpus does. So an alternation that reads the older part still flattens, at every second append: after a flattening the next append has two flat children, and the one after it has an unflattened left.
- **A window is cut as it was.** `slice` flattens an unflattened node before it cuts, which is what keeps every window's base flat.
- **The three carriers read alike.** Bytes and lists are read by one helper and bits by its twin, and the node arm is written once for both.
- **Room to grow is another spec.** A flattened payload with room past its length, so that an append then a read anywhere copies the append alone, is SpiderMonkey's and Erlang's way and removes what this leaves. A payload crosses the host boundary as a flat array of exactly its bytes, and which version may write the room is a mark every version shares, so it is filed apart, not refined.
- **No search primitive.** After the fix a line of 16,000,000 letters is read in 0.35 s, 22 ns a byte, and a reader bounds a line at 1 MiB; nothing in the tree waits on a search answered by the run itself.
- **`programs/rope_push_peek.crs` holds the fix.** It stays as it is written, against its control: a regression of the read helper shows there as the square coming back.

The first decision, measured before it is taken. The byte and list helper patched as the first stage has it, in a copy of the tree at `5fbe62825`, against the tree's own compiler at that commit, each program compiled by both and the two run in turn, five rounds on a quiet machine, every answer the same under both. The loop above takes 0.026, 0.091 and 0.352 s in arrivals of 65,536 bytes and 0.029, 0.105 and 0.399 s in arrivals of 1,024, medians with 0.026 to 0.028, 0.090 to 0.094 and 0.349 to 0.355 s, and 0.029 to 0.030, 0.104 to 0.108 and 0.393 to 0.405 s. `rope_push_peek` takes 0.007, 0.008 and 0.021 s at the three sizes, 0.007 to 0.007, 0.008 to 0.009 and 0.021 to 0.022 s. `/std`'s own reader counts 1,000,000 lines of seven letters in 0.338 s before and 0.331 s after, 0.333 to 0.347 and 0.329 to 0.342 s. The bench's six workloads at the sizes `xbench/src/contest.rs` times them at, which read ropes that were flattened once, take 0.372, 0.234, 0.092, 0.511, 0.569 and 0.059 s before and 0.368, 0.234, 0.092, 0.512, 0.561 and 0.056 s after, lcg, trees, chain, churn, walk and spines in that order, each pair's spans meeting. A scratch program that reads every byte of runs in each shape a node's children take — two leaves, a window and a leaf either way round, a flattened node, an unread node on either side, three appends — forward and then backward, answers alike under both.

## Stages

1. **A read answers from a flat child.** The node arms of `emit_read_func` and `emit_bits_read_func`, written once; the cost model in `curios-emit/src/types.rs`, the module's and the helpers' docs in `rope_emitter.rs` and `table.rs`'s lines for the read helpers say what holds, the hazard restated as the one that remains; `programs/rope_push_peek.crs`'s header and `programs/README.md`'s paragraph say what the program holds now. Check: `curios-emit`'s `rope_tests` hold that the read helper reads a node's children and still calls `force`; `curios`'s `aggregates` tests hold that every element of a `Bytes`, a `Bits` and a `List` in each child shape, read forward and backward, is the flat run's; and the whole workspace's tests pass, the emitter being reached by `curios-std`'s corpus.
2. **The decision is written** in `curios-emit/README.md`, and the measurement above is retaken once through a release build of the landed tree.

## Verification

`cargo xtask workspace format`, `cargo xtask workspace lint` and `cargo xtask workspace test` before the first stage's commit. What a contestant's build weighs in the bench's next reading may have moved, every module that reads a rope carrying a larger helper, and so may the counts the ignored census tests report; neither is retaken.

## Rejected

- **A descent to any depth**, the fetch of Boehm, Atkinson and Plass. A walk over a deep chain that was never flattened would pay its depth at every read, where one flattening makes each later read one array access.
- **A balanced sequence in place of the rope**, a relaxed radix tree or a rope rebalanced as it grows. Every read is then a descent, and a flattened run reads at one array access.
- **A search answered by the run**, a `/sys` operation that finds a byte from an index without flattening. It answers one piece's search and no other read, and nothing waits on it.

## Completion and retirement

The decision and what it rejected go to `curios-emit/README.md`, the cost model stays in `curios-emit/src/types.rs`, and the tests hold the behaviour. The line in `documentation/roadmap.md` becomes a checked summary, this file is deleted, and the spec for a payload with room to grow is filed at its number.
