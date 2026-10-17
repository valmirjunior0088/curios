# An append to a flattened run copies the append alone

Working specification for extending a flattened payload in place. A rope flattens a node into a payload the node keeps, and the next flattening of a run built over it copies that payload again, so a loop that appends and reads — the older part by index, or the whole by a window, an equality or a map — copies everything it holds at every step or every second one. A payload with room past its run, and a written extent that every run over the payload compares its own length with, lets the one run that ends where the payload was last written put its appended pieces in the room and copy nothing else.

## What this builds on

- **The rope's cost model** (`curios-emit/src/types.rs`). An append or a concatenation is one node; the first whole-value read forces a node into a fresh payload, memoized in the node's `cache`, its children nulled; a view's base is flat-available from birth and stays so, since "a cache is written once, never cleared" (`emit_slice_func`). There is no compile-time recognition of any of it.
- **A read of a run answers from a part already flat** ([`curios-emit`'s README](../../../curios-emit/README.md#a-read-of-a-run-answers-from-a-part-already-flat)): one level and no further, a descent to any depth and a balanced sequence rejected.
- **A value costs when it is kept, not when it is named** ([the decision](../../design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md)): growth-shaped consumption keeps the lazy rope; the compiler carries no reuse machinery; reference counts and a sticky sharing bit are rejected, a count on an engine-owned heap being "bought only as a uniqueness test, with dup/drop traffic, mutable payload fields and an undercount corrupting a shared value".
- **A payload crosses to a host as a flat array of its run's length.** Operands are forced and results embedded (`types.rs`); `curios-runtime` lifts an operand by the array's own length (`lift.rs`), and `curios-js`'s bridge reads it by `array.len` (`bridge.rs`).
- **A bit-grain payload's padding is zero.** `force` fills a zeroed payload and copies only the logical run, so the pointwise combiners need no mask, normalisation ORs whole bytes in, and a host reads a `Bits` operand as its bytes' bits (`rope_emitter.rs`; `curios-num`'s `Binary`).
- **A leaf's payload is exact today.** Every leaf is built at its array's length, a reinterpretation to bytes asks `Holds(len(b) % 8 == 0)`, and the inline leaf read (`code_emitter.rs`'s `seq_get` arms) leaves its bounds to the engine's trap on the array.
- **A WebAssembly-GC array's length is fixed when it is allocated**; the instruction set has no resize ([GC MVP](https://github.com/WebAssembly/gc/blob/main/proposals/gc/MVP.md)).

## The gap

**Every flattening copies its run whole.** The force walk (`emit_force_func`, `emit_bits_force_func`) allocates a fresh payload of the run's length and copies every piece into it, the first piece included when that piece is a payload an earlier flattening filled. So a run that is appended to and read in turn is copied whole at each flattening, which is the hazard `types.rs` states in both halves:

- **A read of the older part** flattens at every second append: the node above a flattened one reads through, and the node above that meets a child never flattened.
- **A whole-value read** — a window of a node never flattened, an equality on equal lengths, a map, a host call — flattens at every append.

**Measured** at `34f5e7600` on x86_64-unknown-linux-gnu, timed, on a loaded machine, median of three interleaved runs (two for the window) with their span, each program compiled once with `curios compile` and run with N as its argument. The slope is the argument; the absolute figures need a quiet machine to retake.

| Program | N = 16 000 | 32 000 | 64 000 | 128 000 |
| --- | --- | --- | --- | --- |
| append, then read element 0 | 0.06 s (0.06–0.06) | 0.20 s (0.19–0.20) | 0.69 s (0.67–0.70) | 2.62 s (2.62–2.67) |
| append, then a window past element 0 | 0.105 s (0.10–0.11) | 0.36 s (0.36–0.36) | — | — |
| append, then read the element appended (`rope_push_peek.crs`, the control) | ≤ 0.01 s | ≤ 0.01 s | ≤ 0.01 s | 0.01 s |

**Counted, by the walk's arithmetic, not measured:** the first program copies N²/4 elements, the second N²/2.

**`/std` does not meet it.** A piece of `/std/Io/Reader/piece` or `/std/http/read` resumes where its last look stopped, so it reads only what arrived since: a stream's bytes held are appended to and read, but only at their newest part. A program that reads an older part of what it is growing meets it, and so does one that reads the whole of it at every step.

## Prior art

- **SpiderMonkey moves the buffer's ownership to the newest string.** An "extensible" string "tracks not only its actual length but also its buffer's overall size"; where one is the leftmost string of a later flatten "and its buffer has enough unused space, we can simply flatten the rest of the ropes into its buffer, leaving its text in place. We then transfer ownership of its buffer to the flattened rope" and "mutate the donor extensible string into a dependent string referencing its original buffer" ([`StringType.h`](https://searchfox.org/mozilla-central/source/js/src/vm/StringType.h)). The capacity: "Grow by 12.5% if the buffer is very large. Otherwise, round up to the next power of 2", very large being past 1 MiB (`AllocCharsForFlatten`, [`StringType.cpp`](https://searchfox.org/mozilla-central/source/js/src/vm/StringType.cpp)).
- **Erlang clears the donor's writable flag.** `erts_bs_append_checked` appends in place to a writable sub-bitstring over a writable binary, after a step commented "Make sure that no one else can append to the incoming bitstring"; a binary that is not writable gets "a new writable binary" and a copy, of at least 256 octets, grown by doubling or by 20% past 16 MB ([`erl_bits.c`](https://github.com/erlang/otp/blob/master/erts/emulator/beam/erl_bits.c)). The room does not survive a read: "Matching a binary will also cause it to shrink and the next append operation will copy the binary data", and so does sending one to another process or a port, or passing it to a NIF ([Constructing and matching binaries](https://www.erlang.org/doc/system/binaryhandling.html)).
- **D decides by the used length the block stores.** The runtime keeps "the number of used bytes inside the block itself", appends in place where "the slice ends at the same point valid data ends" and the data fits, and otherwise allocates a block the other slices never see changed, since "no single slice can make any assumptions about what else references the array data" ([D Slices](https://dlang.org/articles/d-array-article.html)).
- **Go checks nothing.** "If the capacity of s is not large enough to fit the additional values, append allocates a new, sufficiently large underlying array … Otherwise, append re-uses the underlying array" ([the specification](https://go.dev/ref/spec#Appending_and_copying_slices)), and two slices over one array overwrite each other's appends.
- **A count decides elsewhere.** CPython extends `left` of `left += right` in place when its reference count says nothing else holds it, "to avoid quadratic behavior when one neglects to use str.join()" ([`bytecodes.c`](https://github.com/python/cpython/blob/main/Python/bytecodes.c)); Lean's exact counts "enable optimizations, such as destructive updates" ([Counting Immutable Beans](https://arxiv.org/abs/1908.05647)); Koka's reuse analysis "allows for guaranteed in-place updates at runtime" ([Perceus](https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/)).
- **V8 flattens without room.** A cons string is flattened "by allocating a sequential string with the same data", the input mutated "to a degenerate form, where the first component is the new sequential string and the second component is the empty string" ([`string.h`](https://github.com/v8/v8/blob/main/src/objects/string.h)) — the rope this tree has.
- **Ropes without mutation pay at the read.** For Boehm, Atkinson and Plass "all operations are completely non-destructive"; they merge a short leaf into the short right leaf of a concatenation so that a rope "built by repeatedly concatenating individual characters to its end" keeps leaves of reasonable size, and trade "speed of the substring and fetch operations for better concatenation performance" ([Ropes: an Alternative to Strings](https://www.cs.tufts.edu/comp/150FP/archive/hans-boehm/ropes.pdf), SPE 25(12), 1995).
- **Baker's persistent array writes the newest version in place.** "A usual array for the last version of the persistent array and indirections for previous versions", each older version a `Diff` that an access reroots ([A Persistent Union-Find Data Structure](https://usr.lmf.cnrs.fr/~jcf/publis/puf-wml07.pdf), Conchon and Filliâtre). An append past every older version's length leaves each of them reading an unchanged prefix, so it needs no diff and no reroot.
- **A builder phase is a type.** Clojure's transients end at `persistent!`, after which "all operations will throw exceptions" ([Transients](https://clojure.org/reference/transients)); Rust's `BytesMut` is "a unique view into a potentially shared memory region" whose `reserve` reclaims the buffer once "all other handles referencing part of the same original buffer have been dropped" ([`bytes`](https://docs.rs/bytes/latest/bytes/struct.BytesMut.html)).
- **A stream parser resumes.** picohttpparser's chunked decoder keeps `bytes_left_in_chunk` across calls and its parsers take the length already parsed ([`picohttpparser.h`](https://github.com/h2o/picohttpparser/blob/master/picohttpparser.h)); Netty's `MERGE_CUMULATOR` copies arrivals into one buffer and its `COMPOSITE_CUMULATOR` keeps them as components ([`ByteToMessageDecoder`](https://netty.io/4.1/api/io/netty/handler/codec/ByteToMessageDecoder.html)).

## Decisions

1. **A flattened node's payload may hold room past its run, and only where the run has shown it grows.** A flattening whose first piece is a leaf allocates exactly, as today; one whose first piece is a view or a flattened node it cannot extend allocates with room, since the run it copies was read, grown and read again. A program that builds and then reads pays nothing.
2. **A payload with room has a written extent, which only grows.** Its slots below the extent are written once and never again, and its slots past it are unwritten. The extent lives in a room record that every flattened node over the payload references; a node over a payload without room references none.
3. **A run that ends at its payload's extent extends it.** A flattening whose first piece is a flattened node whose length is its payload's extent, and whose payload holds the whole run, copies its other pieces past the extent, raises the extent to its own length, and memoizes the payload as its own; at the bit grain the extent is also a multiple of eight. Every other flattening copies as today. Whatever else holds the payload reads below the old extent, which nothing writes, so the rule asks nothing about who holds it.
4. **A leaf's payload is exact.** Room is a flattened node's alone. What makes a leaf of a forced payload — a settle, a reinterpretation — takes the payload where it has no room and a copy of the run where it has, so the inline leaf read keeps the engine's trap as its bounds backstop.
5. **A run's length is its own.** Every consumer of a forced payload reads the run's `len`, or at the bit grain the bytes it occupies, and never the array's length, as the bit grain's consumers already do.
6. **A payload crosses to a host exact.** A reference operand whose payload has room crosses as a copy of its run; `curios-abi`, `curios-runtime` and `curios-js` are unchanged.
7. **Room doubles.** A payload given room holds twice its run, the product taken wide and clamped to the largest length a run holds (`Table::length_instrs`'s `u32::MAX`), so an element is copied at most once more over a run's life on average and a run's room is at most as long as the run.

## Open questions

- **A taper past a threshold.** SpiderMonkey grows by 12.5% past 1 MiB and Erlang by 20% past 16 MB, bounding a large run's room for a few more copies an element. Whether Curios wants one waits on a measurement of a large run's footprint against the heap it is held in.
- **The eager flat builds.** `ListFlat` (`curios-cont`'s `flatten_indexed_lists`) and the fused packed append chain (`fuse_append_chains`) copy their first operand whole, where their result is only indexed; whether that operand is ever a grown run, and whether they claim it as a flattening does, is a probe the claim stage makes.
- **What the record costs where it is null.** A node is a sixteen-octet header and five four-octet fields, 36 octets rounded to 48 by the copying collector's 16-octet alignment (`wasmtime-environ` 47, `gc/copying.rs`); a sixth field is 40, rounded to 48. Read off the layout, not measured.

## The components

| Component | Treatment | Stage |
| --- | --- | --- |
| The force walk's cached-node arm, `$list/<grain>/force`, `$bytes/eql`, the pointwise combiners, `$list/map`, `$list/<leaf>/to_<payload>` | The run's length, never the array's | Length |
| Settle, reinterpretation, every reference host operand, each element `$list/<grain>/force` forces | One `$<carrier>/exact` helper: the payload, or a copy of its run | Exact |
| The node types, the room record, every node construction (append, concat, the packed concat's two arms) | A `room` field, null at construction | Claim |
| The force walk | The destination chosen at the first piece; the claim; the bit grain's guard | Claim |
| `programs/`, `curios`'s `tests::codegen` | The two alternations beside `rope_push_peek.crs`, and the measurement over them | Measure |

## Stages

1. **Length.** Decision 5. Nothing moves, since every payload is exact until the claim lands. Check: the workspace's tests. May move: the helpers' instruction counts and `curios`'s codegen census, named and not retaken.
2. **Exact.** Decisions 4 and 6, through one helper per payload kind. Nothing moves, as above. Check: as above.
3. **Measure.** The two alternations in `programs/` and the measurement of *Verification*, read before the claim. Check: the measurement by name.
4. **Claim.** Decisions 1 to 3 and 7, the room record and field, the walk, the guard, the correctness tests, the counted assertion, `types.rs`'s cost model and the README decision, and the probe of the eager flat builds. Check: the workspace's tests, and the measurement retaken.

## Verification

- **Correctness.** Two runs extending one flattened run, both read after both are built; a run appended to itself; a window of an older version read after a newer one extended its payload; a bit run whose extent is not a byte boundary; a settle and a reinterpretation of a run with room; a host operand with room, which a `MockHost` reads as exactly its run.
- **The alternation copies the append alone.** Counted: the collections the engine announces through `curios_runtime::test_support::collected`, as `tests::codegen::churn` counts them, for each subject and its control at one N, before and after; a subject's falls to its control's order. Timed, on a quiet machine, interleaved: each subject's time against N at doubling sizes, its slope two after.
- **Nothing else moves.** Timed beside them: `rope_push_peek.crs`, `rng_manual.crs` and the census corpus. The prelude's images cannot move: `curios-emit` sits beside the prelude build, never under it.

## Design decisions this overturns or corrects

- [`curios-emit`'s README](../../../curios-emit/README.md#a-read-of-a-run-answers-from-a-part-already-flat), *A read of a run answers from a part already flat*: its "an alternation that reads the older part still flattens, at every second append" becomes a flattening that copies the appended pieces. A decision of its own follows it there, *A flattening extends the payload its first piece wrote last*, with its rationale and this specification's rejections. Claim.
- `curios-emit/src/types.rs`'s cost model: the hazard bullet restated as what is left — a host operand at every step, an alternation at the bit grain off a byte boundary. Claim.
- [A value costs when it is kept, not when it is named](../../design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md) stands: the extent is no count and no sharing bit, and the new decision says why.

## Rejected

- **A moving owner mark**, SpiderMonkey's and Erlang's. Its soundness rests on the mark being unique, which every path that builds or copies a node must keep: a flag set and cleared on two objects at every claim, where the extent is one number that only grows and a comparison any holder can make.
- **A reference count or a uniqueness test**, CPython's, Swift's, Lean's and Koka's: [the decision](../../design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md)'s reasons hold, a count on an engine-owned heap freeing nothing.
- **A static proof that the builder is unshared**: the rope's cost model is recognized nowhere at compile time, and one more use of the run withdraws it.
- **A persistent tree in place of the payload**, or a rebalanced rope that never writes: every read becomes a descent, which the README rejects.
- **Merging short leaves at the append**: bounded leaves leave the alternation flattening whole runs, a constant factor less often.
- **Writing the appended pieces into the room at the append**: a concatenation becomes a copy of its right side at the site, every append site carries the claim's branch, and a run never read is copied all the same.
- **A length beside the array on the wire**: every reference row's import gains an operand in `curios-abi`, `curios-runtime` and `curios-js`, to spare a copy only a run with room pays, on a call whose host already copies the payload.
- **Room at every flattening**: a program that builds and then reads pays room it never uses, and a copy at every host crossing.
- **Writing into a bit payload's partial final byte**: it sets the padding an older version holds, which the pointwise combiners, normalisation and the host read as zero.
- **A builder form in the source**, Clojure's transients, Python's `bytearray`: a representation verb, as the decision rejects a `List/settle` form.
- **Leaving the hazard to each reader to avoid**, as `/std`'s pieces do by resuming: it holds for the programs written that way and no other.

## Completion and retirement

Done when a flattening of a run that ends at its payload's extent copies only the run's other pieces. The rule goes to `curios-emit`'s README and `types.rs`, the programs to `programs/README.md`, and the measurement beside `tests::codegen::churn`'s. Verify that nothing references this filename, and delete it.
