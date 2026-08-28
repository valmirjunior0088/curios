# TOML's numbers are wider than the carriers under them

## Status

Researched, not designed. This specification records what is certain about the distance between the landed `std/Toml` and TOML 1.0.0 — read from the released TOML specification, from the module's own documented contract, and from the roadmap's numeric sequence — and names the decisions that have to be answered before anything is written. Nothing is started.

The landed module documents its own contract and its own product limits, and this file deliberately does not restate them: a copy here would drift the first time a bound moved. Read `curios-prelude-archive/std/Toml.crs` for what the codec does today; read this for what conformance would additionally take.

## Why it exists

TOML 1.0.0 states two numeric requirements. On integers: arbitrary 64-bit signed integers should be accepted and handled losslessly, and an integer that cannot be represented losslessly must raise an error. On floats: floats should be implemented as IEEE 754 binary64 values.

The landed codec stores integers in the native `Int` envelope and floats as binary32 `Flt`. Both are narrower than what the specification asks for, and neither narrowing is an accident: they are the widths the language has. That is the gap the roadmap's "not fully conforming" names, and as named it is a question about carriers rather than about grammar: nothing in the accepted or rejected *spelling* of a document is at issue, only where an accepted lexeme's value is put. Whether it is the only gap is the open question below.

## The two halves are not the same kind of work

**The integer half is unblocked.** It needs a carrier wider than the native envelope, and `/std/BigInt` is landed. Nothing in the roadmap sequences ahead of it.

**The float half has no destination yet.** There is no binary64 carrier anywhere in this toolchain: `Flt` is binary32 at the intrinsic level, and every planned narrowing in the `BigFlt` sequence targets binary32 as well. So binary64 will not arrive as a side effect of the numeric campaign already scheduled — it has to be chosen, and each way of choosing it leaves `/std/Toml` entirely.

## What is certain

- **Conversion is codec-owned.** The module states that `Nat/of_str` and `Flt/of_str` are never used and every accepted lexeme is converted by the codec's own ladder against explicit bounds. Widening therefore moves a bound the codec already owns; it does not open a wrapping hole somewhere the codec was trusting a general conversion.
- **The exact value is already in hand at the point of narrowing.** The float path accumulates every significand digit exactly and narrows once. Whatever the destination format turns out to be, the change is to the narrowing step and its bound, not to the accumulation.
- **The integer half is value-preserving.** Out-of-range integers are refused rather than wrapped, so widening the envelope turns refusals into successes and changes no value that decodes today. That is a migration no existing document can notice.
- **The float half is not.** A finite decimal outside binary32's range is not refused: it saturates, to infinity above the range and to zero below it. Widening the float carrier therefore changes what an already-accepted document means. The two halves carry different compatibility obligations and should be decided separately even if they land together.
- **The round-trip guarantee rests on the carrier.** The module's encode/decode round trip holds for NaN because binary32 as modelled here has exactly one NaN, leaving an implementation nothing to choose. A wider or exact carrier does not inherit that argument; it needs a new one, or an explicit statement that the guarantee narrows.

## What is not certain

Whether the numeric limits are the *only* nonconformance. The module's tests pin the behavior its authors specified; no language-independent conformance corpus has been run against it. Settling that is the cheap first act of picking this specification up, and it is worth doing before any carrier decision, because a grammar or table-construction finding would change what this campaign is.

## The float destination

Three routes, recorded so the choice is made once.

**A binary64 intrinsic.** Buys conformance exactly, and buys it for every other consumer of floats rather than for the codec alone. It is also the largest commitment on this page: a carrier in `curios-num` — which CLAUDE.md marks as adding to the trusted base — a row in the intrinsic roster that the kernel's typing, elaboration, both congruences and `/sys`'s declarations all restate, constant folding in each folder that shares the erased carriers, and emission down to the WebAssembly the target already has a type for.

**Exact rational `BigFlt`.** Buys more than conformance: no rounding at any point. [The general decimal specification](big-flt-general/06-decimal-spec.md) already anticipates exactly this, and already parks the decision here — it states that a TOML profile storing exact values, or using exact parsing as an intermediate before an explicit rounding step, is a separately approved change and not part of that work. It is sequenced behind the whole general `BigFlt` chain.

**Neither.** Close the integer half, keep binary32, and state the float limitation as a permanent product decision rather than a pending one. The roadmap item then splits rather than being checked, and the module's documented limits become the specification of record.

The first two are not exclusive. Parsing exactly and rounding once is what the codec's float path already does internally; a destination that is itself exact simply removes the rounding step.

## What has to be decided

- **Whether the public data model changes.** `Toml/int` and `Toml/flt` are public constructors; changing what they carry is a data-model migration for every consumer, not an internal widening. The alternatives — widening in place, a second profile beside the current one, or parameterizing the value type — have not been weighed.
- **How wide the integer half goes.** TOML asks that 64-bit signed integers be accepted losslessly; it does not cap acceptance there. An arbitrary-precision carrier has no representable-integer failure at all, which satisfies the requirement trivially and also removes the error TOML describes. Whether the codec keeps a documented ceiling for interoperability with 64-bit consumers, or accepts what the carrier accepts, is a decision and not a consequence.
- **Which float route**, and therefore whether this specification is a `/std/Toml` change at all or the consumer of one made elsewhere.
- **Whether the two halves land together.** The integer half is unblocked and compatible; the float half is blocked and value-changing. Holding the first for the second buys nothing but a single roadmap checkbox.
- **How conformance is evidenced.** Whether an external corpus is vendored, how it enters the test surface, and what a conformance claim in the module's documentation is allowed to say once it exists.
- **Whether the encode side follows.** Emission is codec-owned for the same reason parsing is, and it renders against the carrier's boundaries; a wider carrier moves those boundaries rather than removing the need for them.

## Deliberately not specified

The bounds, the ladders and the refusal messages the landed codec uses — they are the module's, documented there, and would go stale here. The order of the two halves. Any timing tied to the `BigFlt` sequence beyond the dependency itself. And TOML 1.1, which is unreleased: the target is 1.0.0, and it is frozen.

Calendar types are outside this campaign. They are owned by `/std/Toml` because `/std/time` is monotonic-only, which is a reason that can stop being true — but that would be a question about where calendar types live, not about conformance, and it should not be folded in here.
