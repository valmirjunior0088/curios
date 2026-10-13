# A read after an append does not flatten the run

Not refined yet. A `Bits`, a `Bytes` and a `List` are ropes: an append is one node, and the first read of an appended node copies the whole of it into a flat payload. A loop that appends and reads in turn therefore copies everything it holds at each turn, and a stream's reader is such a loop. This spec has a read that falls in a part already flat answer from that part. What is known is the cost model's own statement and one measurement; it waits on a reading of how the emitted `get`, `slice` and folds reach a node's children, and on what each candidate costs the reads that are fast today.

## What is missing

**The cost model names the hazard.** `curios-emit/src/types.rs` states it: "The documented hazard: *alternating* append and whole-value reads re-forces per read (the new node above a cached one is uncached), which is quadratic. Build fully, then read." One element is such a read: "a single `get` or `slice` on an uncached node — one element read is a whole-value read there".

**A stream's reader alternates.** `Io/Reader/take` (`curios-std/src/Io/Reader.crs`) appends each arrival to the bytes its reader holds, `x[..then.bytes, ..fresh]`, and asks its piece again, and a piece's search reads a byte of what it is handed. Every stream of `/std` is read through that loop. A scratch program of the shape holds the bytes it has read, appends each arrival of standard input to them and searches what it holds for a newline from where its last search ended, by a recursion over `Bytes/get` under `Nat/lt/try`, counting the lines and their bytes. Compiled with `curios compile` once asking `Io/read(Io/stdin, 65536)` and once asking 1,024, and fed from a file one line of 1,000,000, 4,000,000 and 16,000,000 letters and then `second`, it takes 0.031, 0.109 and 0.545 s in arrivals of 65,536 bytes and 0.054, 0.423 and 10.687 s in arrivals of 1,024: medians of five readings each, 0.030 to 0.032, 0.104 to 0.127 and 0.538 to 0.549 s, and 0.053 to 0.056, 0.406 to 0.434 and 10.213 to 11.560 s. Timed at `aaeb42b28` through the release compiler on a machine in use, so what they show is the growth: sixteen times the line costs about two hundred times the time where an arrival is small.

**The corpus has its program.** `programs/rope_push_peek.crs` alternates one append with one indexed read over a list growing to N, and `programs/README.md` says what it is for: "it exists so that a change to how a rope answers a read has a program to be measured on".

## What is known

- **A bound caps the cost where a reader states one.** A piece refuses past its bound, so what a reader copies grows with the square of the bound and no further: a line of 1,000,000 letters in arrivals of 1,024 bytes is the 0.054 s above. A piece that waits for a count reads the length alone, and no node is flattened for it: "`len` and the tag dispatch never force".
- **A window already reads through.** "A `view`'s base is always *flat-available* — a leaf or an already-cached node … — so `get` reads straight through a window without forcing or copying." After an append to a run that was read, both children of the new node are flat-available in that sense.

## Open questions

- **What a read of a node does where its children are flat.** A `get` whose index falls in a flat child could answer from the child, as a window answers from its base, and leave the node unflattened. Not read: what `slice` and the folds do in its place, how long a chain of unread appends may grow before a read flattens it, and what the test of a child costs every `get` of a flattened run, which is the read the corpus makes most.
- **Whether a search wants a primitive.** `Bytes/index_of` is a fold over every byte of the run with a closure a byte, and walks on past its first hit (`find_index`, `curios-std/src/Bytes.crs`); the pieces of `/std/http` search by a recursion that stops at theirs. A search from an index, answered by the run itself, would read a flat part at the machine's speed.

## Stages

None until the questions are read. The check any candidate takes is `programs/rope_push_peek.crs` against its control, `programs/rng_manual.crs`, the measurement above retaken, and the census corpus's timings beside their readings.
