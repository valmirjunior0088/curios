# Numeric laws: Euclid's layer, the binary scale, the integers through their sign, and the float's bits

Working specification for the certified functions and theorems `/std/Nat`, `/std/Int` and `/std/Flt` lack, proved in the library over what conversion decides. [Exact rationals](04-exact-rationals.md) is the first consumer and [the binary64 conversion proofs](07-binary64-conversion-proofs.md) the second; each stage below names which of their stages it serves, so that neither writes number theory of its own.

It waits on no other spec.

## What this builds on

- **`Nat` and `Int` are unbounded at every layer** ([Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)), so a law stated over them holds of the compiled program.
- **Conversion decides the carriers' algebra** ([A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)): the sum normal form, Euclid's identity and the remainder's bound, a comparison through its difference, `Nat/to_int` as an ordered-semiring embedding, `shl(x, k)` as `x * pow(2, k)`, a sum in an exponent as a product, a power of a base at least one as at least one, and what is declared of `min`, `max`, `abs` and `sign`.
- **The elaborator proves a linear step from the facts in scope** ([A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)), a quotient's bounds, a product of two facts and the cases of `min`, `max`, `abs` and `sign` among them. Its goal is a `<` or a `<=`, so an equation is `le/antisym` over two of them.
- **Totality reads an arithmetic descent from a guard** ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md)): `n / k` is below `n` where an enclosing arm rules `n`'s zero out, and a hypothesis is no such arm.
- **The library** has `/std/Nat/DivMod/of`, which hands back `/`'s quotient and `%`'s remainder with Euclid's identity and the bound, and `DivMod/unique`, by which a quotient and a remainder that satisfy both are those two; `/std/Nat/Divides` with `refl`, `trans`, `zero`, `one`, `add`, `mul`, `sub`, `antisym`, its two bridges to a zero remainder, `of_rem` and `to_rem`, the exact quotient `exact` and the remainder's divisor `rem`; `/std/Nat/Gcd`, whose `of` builds a greatest common divisor with its two divisibilities and the property that makes it greatest, with `unique`, `symmetric`, `euclid` and `scaled`, and `Nat/gcd` as its value, which a type may mention; `/std/Nat/Coprime` with its decision `try`, `common` and `of_common`, `quotients`, Euclid's lemma `cancel` and the reduced fraction's `unique`; `/std/Nat/Scale`, whose `of` writes a positive number as an odd part at a count of factors of two, `n = shl(2 * half + 1, count)`, with `unique`, by which a count and a half that rebuild a number are one pair, and `Nat/trailing_zeros` and `Nat/odd_part` as its count and its odd part; `/std/Nat/pow`'s `of_pow`, `of_mul` and `positive`; `/std/Nat/log2/bounds`, a positive number between the power of two at its `log2` and the next; `/std/Nat/shl/split`, a shift split at a count below its own; `/std/WellFounded/recurse` over `WellFounded/lt`; `/std/Int`'s `Sign` view with `trichotomy` and `eq_of_eql`; and the laws a hypothesis decides `min`, `max`, `abs` and `sign` by.

## The gap

- **`/std/Dyadic/normalized` strips a mantissa's factors of two with no proof**: with `Nat/and`, `Nat/xor`, `Nat/log2` and `Int/shr`, none of which the logic reads, so the odd mantissa it answers is a fact no type states, where `/std/Nat/Scale/of` states it of a natural.
- **`ord` carries no law** at `Nat` or at `Int`: that it answers `eq` exactly at an equation, and `lt` and `gt` exactly at their comparisons, is unstated.
- **`Int` has no multiplicative cancellation**, and `abs` and `sign` of a product are refused rows of the law grid with no lemma beside them.
- **`Flt`'s identity as bits is unstated**: that `to_le_bytes` is injective, and that the natural `ord` falls back on tells every pattern apart.

## Prior art

- **A greatest common divisor is total by well-founded recursion or by a checker that sees the descent.** Lean 4 defines `Nat.gcd` by recursion on its first operand with `decreasing_by` over `mod_lt` ([`Init/Data/Nat/Gcd.lean`](https://github.com/leanprover/lean4/blob/master/src/Init/Data/Nat/Gcd.lean)), and Agda's standard library threads an accessibility proof through `gcd′` ([`Data.Nat.GCD`](https://github.com/agda/agda-stdlib/blob/master/src/Data/Nat/GCD.agda)). Rocq writes it as a structural `Fixpoint`, accepted because "behind modulo there is a subtraction, which now preserves being a subterm" ([`Corelib.Init.Nat`](https://rocq-prover.org/doc/v9.0/corelib/Corelib.Init.Nat.html)). Lean's reference says of the first route that such a definition typically does not reduce definitionally and its unfoldings hold as propositional equations ([Recursive definitions](https://lean-lang.org/doc/reference/latest/Definitions/Recursive-Definitions/)).
- **A canonical rational carries its coprimality as a field**: Lean's `Rat` holds `reduced : num.natAbs.Coprime den` ([`Init.Data.Rat.Basic`](https://lean-lang.org/doc/api/Init/Data/Rat/Basic.html)), and Rocq's `Qc` holds `canon : Qred this = this` ([`QArith.Qcanon`](https://rocq-prover.org/doc/v8.9/stdlib/Coq.QArith.Qcanon.html)).
- **A dyadic's normal form is an odd mantissa found by counting trailing zeros.** Lean's `Dyadic` is `zero` or `ofOdd (n k : Int) (hn : n % 2 = 1)`, built by `i >>> i.trailingZeros`, where `Int.trailingZeros` halves until it meets an odd number ([`Init.Data.Dyadic.Basic`](https://lean-lang.org/doc/api/Init/Data/Dyadic/Basic.html)). It is provable there because Lean's right shift is defined through `/ 2` and its `Int` remainder is Euclidean, the two things [the carriers' algebra leaves undecided](09-left-undecided-by-the-carriers-algebra.md) here.
- **A comparator's laws are stated beside it and taken by what needs them.** Lean's `Std` keeps `OrientedCmp`, `TransCmp` and `LawfulEqCmp` as classes of their own, which its tree maps require ([`Init.Data.Order.Ord`](https://lean-lang.org/doc/api/Init/Data/Order/Ord.html)).
- **A float's total order is the order of its bits with the sign folded in.** Rust's `f64::total_cmp` compares the two patterns as integers after flipping a negative one's magnitude bits ([`core::f64`](https://doc.rust-lang.org/src/core/num/f64.rs.html)).

## Decisions

- **A certified function hands back its result with its proofs, in a structure.** `Gcd(a, b)` and `Scale(n)` follow `DivMod(n, d)`: one recursion builds the value and what is true of it, the proofs erase, and a law is read off a field or follows from the structure's uniqueness, with no second induction over the function's recursion.
- **`gcd` recurses on well-founded `<`**, through `WellFounded/recurse` on its second operand, the step's progress being the remainder's bound, which conversion decides. It is the one `gcd`: the partial definition is replaced.
- **The binary scale is stated over `shl`, `pow` and halving.** A positive `n` is `shl(2 * half + 1, count)` for one `count` and one `half`, so oddness is a shape and no proposition, and nothing here reads a right shift.
- **No second spelling of division.** A quotient under `Divides` is `/`'s, with a law, and no `exact_div` stands beside it.
- **A lemma is listed where it has a proof step or a consumer needs its name.** A fact conversion decides, or the elaborator proves where it is used, is no lemma; [Closed where it is used](#closed-where-it-is-used) records each one the law grid lists as a lemma.
- **An order's laws are stated beside it**, as lemmas over `ord`, and a consumer that needs a lawful comparator takes them as hypotheses. `Ord` stays a concept of one function.
- **`Int` goes through its sign view.** A statement about `Int` is split by `Int/view` of its operands and each case is a `Nat` law read through `Nat/to_int`; a shared step is added beside the view once.

## Stages

Each lands alone and is one commit.

**Stage 4 — `Int` through its sign view.** Serves [exact rationals](04-exact-rationals.md)' stage 4.

- `ord`'s laws at `Nat` and at `Int`: reflexive, `eq` exactly at an equation, `lt` and `gt` exactly at their comparisons, the flip, and transitivity.
- Multiplicative cancellation under `NonZero`, on both sides of the product.
- `abs(a * b)` is `abs(a) * abs(b)`, `sign(a * b)` is `sign(a) * sign(b)`, `sign` is idempotent, and `n` is `sign(n) * Nat/to_int(abs(n))`.
- The signed scale: two integers of odd magnitude at two counts that denote one value are one integer at one count, `Scale/unique` carried through the `abs` and the `sign` of a shift, which are declared.
- `/std/Dyadic/normalized` is rebuilt over `Scale/of` on the mantissa's magnitude, so its odd mantissa is a fact of its construction.

Check: rows of the `numeric` unit at zero, both signs and equal magnitudes of opposite sign; `curios-std`'s `flt` unit, which holds `normalized` at a type.

**Stage 5 — `Flt`'s bits.** No stage of exact rationals needs it; it is where [the binary64 conversion proofs](07-binary64-conversion-proofs.md) and a decision on `Key(Flt)` start.

- `/std/Bytes/to_nat` is injective on runs of one length, and below `pow(256, len)`.
- `Flt/to_le_bytes` is injective.
- `Flt/total_key` is public and injective, so the order `Nat/ord` reads through it is total and agrees with `Eq` by stage 4's laws. `ord`'s documentation names it as the order `ord` computes.

Check: `curios-std`'s `flt` unit holds `ord` to the key's order at the zeros, the infinities and NaNs of both signs and kinds.

## Closed where it is used

Each is a fact a proof might expect a lemma for, most of them rows `curios`'s `tests::laws::written` refuses as lemmas, and none is a lemma here.

- `min(a, b) + max(a, b) = a + b`, `(a - b) + b = max(a, b)` and `min(a + c, b + c) = min(a, b) + c`, at `Nat` and at `Int`: `le/antisym` over two `True/proved()`. `+0 - min(i, j) = max(+0 - i, +0 - j)` the same way.
- `pow(2, y) <= pow(2, y + z)`, `x <= shl(x, y)`, and `shl(x, k) / pow(2, k) = x`: `True/proved()`, the last under `le/antisym`.
- Two magnitudes at two counts, `shl(a, i) <= shl(b, j)` with `j <= i`, compare as `shl(a, i - j) <= b`: `shl/split` rewrites the first at `j`, and `True/proved()` cancels the shift the two then share.
- At `Int`: a bound's reversal under negation and its additive cancellation are conversion's; monotonicity under a factor at most zero, the triangle inequality of `abs` and `abs`'s two bounds are `True/proved()`; and `abs(n) == 0` gives `n = +0` by `le/antisym`.

## Verification

- Each law elaborates over symbolic operands and is certified, which `cargo xtask clippy` holds for all of `/std`.
- Each stage's rows are stated in `curios-std`'s corpus units, at the boundaries its statement names.
- The executable `gcd`, `trailing_zeros` and `odd_part` agree with `curios-num` over the differential grid, folded and executed.
- No `Int` proof opens a case analysis the sign view performs.

## Rejected

- **A totality rule that grades a remainder below its divisor**, which would accept `gcd` as it is written and is how Rocq takes it. It is a rule of the trusted grading for one function, and [`curios-std`'s README](../../../curios-std/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position) already declines a totality rule where the recursion principle states the progress beside its proof.
- **A certified `gcd` beside the partial one**, two spellings of one function; **an `exact_div`**, a second spelling of `/`.
- **The scale over `shr` and the bit operations**, as `/std/Dyadic/normalized` computes it: [the carriers' algebra](09-left-undecided-by-the-carriers-algebra.md) ties a right shift to no quotient, so nothing about it is provable.
- **`Flt/ord` redefined as the key's order alone**, which every law above would then reach: each comparison would build a `Bytes` and a boxed `Nat` where the comparison it leads with reads two floats.
- **A disjunction for an order's totality**: `/std` has none, and `Int/Trichotomy` carries the three cases with their evidence.
- **A lemma for each row the law grid refuses**, where the elaborator proves the row at the use site: a name that stands for one `True/proved()`.

## Left to other specifications

- **A right shift undone, a right shift as a floor, and `Int`'s shift agreeing with `Nat`'s** wait on the shift's law, and **parity through `Int`'s remainder** on the remainder's bound: both are rows of [what the carriers' algebra leaves undecided](09-left-undecided-by-the-carriers-algebra.md).
- **`Flt/ord` being the key's order**, and so a total order agreeing with `Eq`, needs a comparison tied to the bits: [the binary64 conversion proofs](07-binary64-conversion-proofs.md).
- **An order's totality holding by conversion** is [the relational layer](08-relational-layer.md)'s.

## Completion and retirement

- Every stage's declarations exist under `/std/Nat`, `/std/Int`, `/std/Bytes` and `/std/Flt` with operation-first names, and [exact rationals](04-exact-rationals.md) needs no private division, `gcd`, bit-stripping or sign reasoning.
- Before this specification is deleted, each contract is its declaration's documentation and signature, the decision that a certified function hands back its result with its proofs and what it rejects are in [`curios-std`'s README](../../../curios-std/README.md), the roadmap entry is a checked summary, and no reference to this filename remains.
