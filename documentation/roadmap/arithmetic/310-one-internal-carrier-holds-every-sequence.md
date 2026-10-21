# One internal carrier holds every sequence

**Not refined yet.** This specification reserves one internal sequence carrier and common operations, preserving `List` and `Bytes` for the guest. It is not an implementation plan. It waits for a consumer, and whether a reasoning capability needs it or it can proceed on its own is under *Refinement*.

## What is missing

Sequences are held in today's three representations, each with operations of its own, where one internal carrier and one set of operations would serve them all.

## What is known

- **Previously discussed.** A carrier and kind pair, with representation changes through the erased and continuation IRs, emission and stored archives.
- **The algebra already spans them.** [The carriers' algebra](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) shares one word algebra, which deliberately works over today's three representations.

## Refinement

- The representation, carrier and element typing, packed storage, proof operands, intrinsic signatures, stage boundaries, archive compatibility and performance.
- Whether a reasoning capability needs it or it can proceed on its own.

It retires when one internal carrier holds the sequences `List` and `Bytes` are, with the guest seeing both as it does today.
