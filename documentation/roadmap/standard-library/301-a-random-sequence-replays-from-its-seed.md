# A random sequence replays from its seed

**Not refined yet.** This specification reserves a generator of random values that a seed determines, and its draws. It is not an implementation plan: a consumer of replayable draws releases it. No module of `/std` consumes it yet: its first consumer is the measurement corpus, which rolls its own.

## What is missing

- **`/std/rand` reads the host and nothing else.** `curios-prelude/std/rand.crs` is `bytes(n) -> Io(Bytes)` over the `rand_bytes` row, the operating system's entropy inside `Io`. No value of `/std` computes a sequence from a seed, so a program cannot replay a run, and a function outside `Io` cannot draw at all.
- **The corpus rolls its own.** `programs/rng_state.crs`, `programs/rng_manual.crs`, `programs/lcg/lcg.crs`, `programs/chain/chain.crs`, `programs/spines/spines.crs`, `programs/churn/churn.crs` and the three `programs/rope_push_*.crs` each step `75 * x % 65537`, a Lehmer generator of period at most 65,536, chosen so that every product stays below `2^30`, the range a `Nat` is held unboxed in ([Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)).

## What is known

- **Peers ship a seeded generator beside the entropy source.** Go's [`math/rand/v2`](https://pkg.go.dev/math/rand/v2) (PCG and ChaCha8 sources), Python's [`random`](https://docs.python.org/3/library/random.html) (Mersenne Twister), OCaml 5's `Random` (LXM, splittable), Java 17's [`RandomGenerator`](https://openjdk.org/jeps/356) (LXM and SplitMix), Lean 4's core `StdGen`, and Haskell's [`random`](https://hackage.haskell.org/package/random), whose `StdGen` is SplitMix since 1.2.
- **A splittable generator hands a pure program independent streams** without threading one state through it: SplitMix (Steele, Lea and Flood, OOPSLA 2014) and LXM (Steele and Vigna, OOPSLA 2021).
- **A bounded draw is its own algorithm.** A draw below `n` by `%` is biased toward the low residues; Lemire's nearly divisionless method (ACM TOMACS 2019) and plain rejection are unbiased, and a float in `[0, 1)` is 53 drawn bits scaled by `2^-53`.
- **A type under proofs is drawn through a total constructor.** `Date/of_days`, `Time/of_seconds` and `Weekday/of_days_from_monday` are total over their counts, so a generator of a calendar value draws a count. A type whose fields a decided bound guards draws and decides through `Bool/holds`, and the retries are a cost to bound.

## Refinement

- **The word.** The proposed SplitMix or PCG step uses 32- or 64-bit arithmetic, with intermediates past `2^30`: [arithmetic modulo a word](../arithmetic/301-arithmetic-modulo-a-word.md)'s question, settled there for this and for [bytes are written as text and digested](300-bytes-are-written-as-text-and-digested.md).
- **The state.** A value threaded through `State`, as `programs/rng_state.crs` threads its seed, or a splittable seed; and whether `/std/rand` offers a generator seeded from `rand_bytes`.
- **The draws.** Bounded naturals and integers, floats, booleans, a choice from a list and a shuffle, each with its law as a proof where one is stated, a draw below its bound first among them.
- **What moves to it.** Whether the corpus programs keep their Lehmer step, which is what they measure, or draw from the library.

## The delivery

A replayable generator and the draws its consumer requires. Settle the algorithm, word and state contracts for a named consumer, with explicit seed input and state or split-stream output. Provide only the draws the consumer needs, including an unbiased bounded draw where requested; its caller records and passes its seed explicitly. Check published algorithm vectors, deterministic replay of a seed and draw sequence, bounds and edge cases, and agreement of folded and executed draws. The measurement corpus changes generator only by the separate decision above.

## Completion and retirement

A delivered generator records its algorithm, seed and draw contracts and tests beside `/std/rand`, and leaves any unrequested draws pending here under their own consumer gates. The specification retires when no reserved work remains, a seed determines every offered draw, and dependent references name the implemented contracts.
