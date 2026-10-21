# A random sequence replays from its seed

**Not refined yet.** This specification reserves a generator of random values that a seed determines, its draws, and the property tests built on it. It is not an implementation plan. No module of `/std` consumes it yet: its first consumers are the measurement corpus, which rolls its own, and a claim a test means to try on values its author did not write out.

## What is missing

- **`/std/rand` reads the host and nothing else.** `curios-std/src/rand.crs` is `bytes(n) -> Io(Bytes)` over the `rand_bytes` row, the operating system's entropy inside `Io`. No value of `/std` computes a sequence from a seed, so a program cannot replay a run, a test cannot reproduce a case, and a function outside `Io` cannot draw at all.
- **The corpus rolls its own.** `programs/rng_state.crs`, `programs/rng_manual.crs`, `programs/spines/spines.crs` and `programs/churn/churn.crs` each step `75 * x % 65537`, a Lehmer generator of period at most 65,536, chosen so that every product stays below `2^30`, the range a `Nat` is held unboxed in ([Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)).
- **`Test` tries a claim on the cases written.** `curios-std/src/Test.crs` has `assert`, `equal`, `all` and the effects around them, and no generator: a claim not yet proved is tried only on the values its author spelled out.

## What is known

- **Peers ship a seeded generator beside the entropy source.** Go's [`math/rand/v2`](https://pkg.go.dev/math/rand/v2) (PCG and ChaCha8 sources), Python's [`random`](https://docs.python.org/3/library/random.html) (Mersenne Twister), OCaml 5's `Random` (LXM, splittable), Java 17's [`RandomGenerator`](https://openjdk.org/jeps/356) (LXM and SplitMix), Lean 4's core `StdGen`, and Haskell's [`random`](https://hackage.haskell.org/package/random), whose `StdGen` is SplitMix since 1.2.
- **A splittable generator hands a pure program independent streams** without threading one state through it: SplitMix (Steele, Lea and Flood, OOPSLA 2014) and LXM (Steele and Vigna, OOPSLA 2021).
- **A bounded draw is its own algorithm.** A draw below `n` by `%` is biased toward the low residues; Lemire's nearly divisionless method (ACM TOMACS 2019) and plain rejection are unbiased, and a float in `0, 1)` is 53 drawn bits scaled by `2^-53`.
- **A property test is generators and a way to shrink.** QuickCheck ([Claessen and Hughes, ICFP 2000) shrinks by type; [Hedgehog](https://hedgehog.qa/) builds the shrinking into the generator; Hypothesis reduces the sequence of choices a generator made (MacIver and Donaldson, ECOOP 2020); QuickChick derives generators of the values an inductive relation admits (Lampropoulos, Paraskevopoulou and Pierce, POPL 2018); Lean's [Plausible](https://github.com/leanprover-community/plausible) tries a goal before it is proved.
- **A type under proofs is drawn through a total constructor.** `Date/of_days`, `Time/of_seconds` and `Weekday/of_days_from_monday` are total over their counts, so a generator of a calendar value draws a count. A type whose fields a decided bound guards draws and decides through `Bool/holds`, and the retries are a cost to bound.

## Refinement

- **The word.** Every generator above steps in 32 or 64 bits, past `2^30`, so over `Nat` each step allocates: [arithmetic modulo a word](../arithmetic/300-arithmetic-modulo-a-word.md)'s question, settled there for this and for [bytes are written as text and digested](300-bytes-are-written-as-text-and-digested.md).
- **The state.** A value threaded through `State`, as `programs/rng_state.crs` threads its seed, or a splittable seed; and whether `/std/rand` offers a generator seeded from `rand_bytes`.
- **The draws.** Bounded naturals and integers, floats, booleans, a choice from a list and a shuffle, each with its law as a proof where one is stated, a draw below its bound first among them.
- **Property tests.** Where generators live, in `Test` or a module of their own; how a failing case shrinks; how a run reports its seed and is replayed from one, which a `curios test` option would reach (`documentation/usage.md`, and the keys of `curios-verdicts` where a seed changes a verdict); and how a generator of a type under proofs is written.
- **What moves to it.** Whether the corpus programs keep their Lehmer step, which is what they measure, or draw from the library.

It retires when a seed determines every draw of a generator `/std` offers, a test can try a claim on generated values and replay a failure from its seed, and the word it steps in is decided.
