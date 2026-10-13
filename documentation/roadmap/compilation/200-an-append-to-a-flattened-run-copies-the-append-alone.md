# An append to a flattened run copies the append alone

Not refined yet. [A read of a run answers from a part already flat](../../../curios-emit/README.md#a-read-of-a-run-answers-from-a-part-already-flat), one level down, so an append then a read of the part just appended copies nothing. A loop that appends and then reads the older part still flattens: the append after a flattening has two flat children and is read through, and the append after that stands over a node never flattened, which a read of its older part flattens whole. This spec gives a flattened payload room past its length, so that flattening such a node copies the append into the room and nothing else. What is known is the two runtimes that do it and where it meets the host boundary; it waits on a measurement of a program that alternates so, on a prototype, and on what a payload with room pays where it crosses to the host.

## What is missing

**Every second append of such a loop copies the whole.** `curios-emit/src/types.rs` states it as the hazard that remains: "alternating it with a `get` of the older part re-forces at every second append — the node above a cached one reads through, and the node above that has an uncached child — which is quadratic either way".

**No program of the tree measures it.** `programs/rope_push_peek.crs` reads the element it has just appended. Read off the code, and not measured: the pieces of `/std/http` that look at the start of what they hold at each arrival, `head_from`'s `line_end(held, start)` and `chunked`'s `line_end(held, 0)` (`curios-std/src/http/read.crs`), the second under a bound of `limits.content`, which a chunked body sent as one chunk reaches.

## Prior art

- **SpiderMonkey keeps the buffer it flattened into.** "To avoid copying the leftmost string when flattening, we may produce an "extensible" string, which tracks not only its actual length but also its buffer's overall size", and where one "appears as the leftmost string in a subsequent flatten, and its buffer has enough unused space, we can simply flatten the rest of the ropes into its buffer, leaving its text in place" ([`StringType.h`](https://searchfox.org/mozilla-central/source/js/src/vm/StringType.h)).
- **Erlang appends in place to the latest version alone.** An append to a binary is "specially optimized by the runtime system to avoid copying the `Acc` binary every time", the object allocated at "twice the size of `Bin1` or 256, whichever is larger", and "only the binary returned from the latest append operation will support further cheap append operations" ([Constructing and matching binaries](https://www.erlang.org/doc/system/binaryhandling.html)).

## Open questions

- **What a payload with room pays at the host boundary.** A payload crosses to the host as a flat array whose length is its bytes': "wire `Bytes` payloads cross the boundary as the flat `$bytes`/`$elems` arrays (params are forced before the call, results are embedded into fresh leaves after it)" (`curios-emit/src/types.rs`). One with room is longer than what it holds, so it is copied to its length at each host call or the wire carries a length beside the array, which is `curios-abi`'s and both hosts' to change.
- **Which version may write the room.** Two runs may share a flattened payload as their prefix, and only the one appended to last may extend it, as Erlang's rule has it. That is a mark every version shares: mutable state beside a value, where a node's cache is written once.
- **What it measures.** A program of `programs/` that alternates an append with a read of its first element, against a control, and a chunked reply of one large chunk read in small arrivals.

## Stages

None until the questions are read.
