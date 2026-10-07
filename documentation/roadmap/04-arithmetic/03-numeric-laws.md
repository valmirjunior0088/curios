# Numeric laws: Euclid's layer, the binary scale, the integers through their sign, and the float's bits

Working specification for the certified functions and theorems `/std/Nat`, `/std/Int` and `/std/Flt` lack, proved in the library over what conversion decides. [Exact rationals](04-exact-rationals.md) is the first consumer and [the binary64 conversion proofs](07-binary64-conversion-proofs.md) the second; each stage below names which of their stages it serves, so that neither writes number theory of its own.

It waits on no other spec.

## What this builds on

- **`Nat` and `Int` are unbounded at every layer** ([Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)), so a law stated over them holds of the compiled program.
- **Conversion decides the carriers' algebra** ([A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)): the sum normal form, Euclid's identity and the remainder's bound, a comparison through its difference, `Nat/to_int` as an ordered-semiring embedding, `shl(x, k)` as `x * pow(2, k)`, a sum in an exponent as a product, a power of a base at least one as at least one, and what is declared of `min`, `max`, `abs` and `sign`.
- **The elaborator proves a linear step from the facts in scope** ([A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)), a quotient's bounds, a product of two facts and the cases of `min`, `max`, `abs` and `sign` among them. Its goal is a `<` or a `<=`, so an equation is `le/antisym` over two of them.
- **Totality reads an arithmetic descent from a guard** ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md)): `n / k` is below `n` where an enclosing arm rules `n`'s zero out, and a hypothesis is no such arm.
- **The library** has `/std/Nat/DivMod/of`, which hands back `/`'s quotient and `%`'s remainder with Euclid's identity and the bound; `/std/Nat/Divides` with `refl`, `trans`, `zero`, `one`, `add`, `mul` and `of_rem`; `/std/WellFounded/recurse` over `WellFounded/lt`; `/std/Int`'s `Sign` view with `trichotomy` and `eq_of_eql`; and the laws a hypothesis decides `min`, `max`, `abs` and `sign` by.

## The gap

- **`Divides(d, n)` does not give a zero remainder.** `(k * d) % d` at a symbolic `d` is neither conversion's nor the elaborator's, so `of_rem` has no converse and a quotient under `Divides` has no law.
- **No type may mention a greatest common divisor.** `/std/Nat/gcd` recurses on a remainder, which totality does not grade, so a type that reaches it is refused as not known to terminate (`curios`'s `tests::numeric::arithmetic_tests`). No proposition says two numbers are coprime or a fraction reduced.
- **Nothing strips a number's factors of two with a proof.** `/std/Dyadic/normalized` does it with `Nat/and`, `Nat/xor`, `Nat/log2` and `Int/shr`, none of which the logic reads, so the odd mantissa it answers is a fact no type states. `Nat/log2` carries no bound, and a power of a power or of a product has no lemma.
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

**Stage 1 — Euclid on `Nat`.** Serves [exact rationals](04-exact-rationals.md)' stages 1 and 4.

- `DivMod/unique`: a `q` and an `r` with `n = q * d + r` and `r < d` are `n / d` and `n % d`. It is what every Euclidean argument ends in.
- `Divides/to_rem`, the converse of `of_rem`, and `Divides/exact`: under `Divides(d, n)` and a positive `d`, `n` is `(n / d) * d`.
- `Divides/sub`, a divisor of a sum and of one summand divides the other; `Divides/rem`, a common divisor of a dividend and its divisor divides the remainder; and `Divides/antisym`.

Check: each law at its boundaries as type-level rows of `curios-std`'s `numeric` unit, a zero dividend, a divisor of one, a dividend below its divisor and an exact multiple with its neighbours.

**Stage 2 — `Gcd` and `Coprime`.** Serves [exact rationals](04-exact-rationals.md)' stages 1 and 4.

```crs
pub struct Gcd(a: Nat, b: Nat): pub Type {
    value: Nat,
    left: Divides(value, a),
    right: Divides(value, b),
    greatest: (d: Nat, p: Divides(d, a), q: Divides(d, b)) -> Divides(d, value),
}
```

- `Gcd/of(a, b)` builds it, and `gcd(a, b)` is its `value`, which a type may mention and which computes there on closed operands. `lcm` stands over it.
- `Gcd/unique`: any value with the three properties is `gcd(a, b)`. Symmetry and the values at a zero and at a one follow from it with no unfolding.
- `gcd(a * c, b * c)` is `gcd(a, b) * c`, by the one recursion that follows `of`'s, through an unfolding lemma `/std/WellFounded` gains for `recurse`.
- `Coprime(a, b)` is `Eq()(gcd(a, b), 1)`, with its decision and its reading as every common divisor being one. The quotients of two numbers by their `gcd` are coprime; `Coprime(a, b)` and `Divides(a, b * c)` give `Divides(a, c)`, Euclid's lemma; and two reduced fractions with equal cross products have equal numerators and equal denominators.

Check: the laws at a zero, a one and equal operands as rows of the `numeric` unit; `gcd` against `curios_num::Natural::gcd` in `curios`'s `tests::numeric::grid_tests`; and `tests::numeric::arithmetic_tests` holding `gcd` accepted in a type, with `Nat/sqrt` as its partial specimen.

**Stage 3 — the binary scale on `Nat`.** Serves [exact rationals](04-exact-rationals.md)' stages 1, 2 and 4, and the rounding [the binary64 conversion proofs](07-binary64-conversion-proofs.md) reason about.

```crs
pub struct Scale(n: Nat): pub Type {
    count: Nat,
    half: Nat,
    joined: Eq()(n, Nat/shl(2 * half + 1, count)),
}
```

- `Scale/of(n, @ok: Holds(0 < n))` builds it by halving, and `Scale/unique` says a `count` and a `half` that rebuild `n` are its own. `trailing_zeros` and `odd_part` are its `count` and `2 * half + 1`. Zero is excluded by the premise and given no conventional count.
- `pow(pow(x, a), b)` is `pow(x, a * b)`, `pow(x * y, a)` is `pow(x, a) * pow(y, a)`, and `0 < x` gives `0 < pow(x, k)`: each an induction whose step is the declared law at a successor.
- `log2`'s bounds: for a positive `n`, `pow(2, log2(n)) <= n` and `n < 2 * pow(2, log2(n))`.
- Two magnitudes at two counts compare as the one shifted by the difference against the other. The lemma is named only where the elaborator does not prove the comparison where it is used.

Check: rows of the `numeric` unit at counts across the i31 and past 64 bits; `trailing_zeros` and `odd_part` against `curios_num` in `tests::numeric::grid_tests`.

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

- `min(a, b) + max(a, b) = a + b` and `(a - b) + b = max(a, b)`, at `Nat` and at `Int`: `le/antisym` over two `True/proved()`. `+0 - min(i, j) = max(+0 - i, +0 - j)` the same way.
- `min(a + c, b + c) = min(a, b) + c`: a match on `a <= b` naming `min/of_le` and `min/of_ge` in its arms, since one direction meets [a finding](00-findings.md) of the procedure that proves a bound.
- `pow(2, y) <= pow(2, y + z)`, `x <= shl(x, y)`, and `shl(x, k) / pow(2, k) = x`: `True/proved()`, the last under `le/antisym`.
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
