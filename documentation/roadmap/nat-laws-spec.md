# `Nat` laws: the Euclidean remainder, the binary scale, and `min` and `max`

Working specification for the `Nat` theorems and operations the standard library still lacks: the Euclidean layer past certified division, the unsigned binary-scale layer, and `min` and `max` as declared operations. Each is an independent capability of `/std/Nat` that stands without any consumer; [Rat part 1](rat-pt1-spec.md) is the consumer that asks for the first two, and names which layer each stage needs.

The broader algebra dependencies named here belong to [part 2](algebra-pt2-spec.md), whose design is not refined yet. [Part 1](algebra-pt1-spec.md) consolidates existing behavior and does not fulfill these requests for additional reasoning or declarations.

## What this builds on

`Nat` is unbounded at every layer, the running program included ([Nat and Int are an i31 until they outgrow it](../design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md)), so a law stated over `Nat` holds of the compiled program, and nothing below needs a second, library-defined natural. Its arithmetic is reasoned about by the kernel rather than beside it:

- what conversion decides about `Nat` today — the commutative-semiring laws through the sum normal form, Euclid's identity and the remainder's bound, and a shift read through its power and its floor — is recorded in [Open fold laws and the sum normal form](../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md), and what it will decide is [algebra part 2](algebra-pt2-spec.md)'s;
- `/std/Nat/div_mod` hands back the quotient and remainder `/` and `%` compute, with Euclid's identity (`joined`) and the bound (`bounded`) as its proofs;
- `/std/Nat/Divides(d, n)` is a multiple witness, with `refl`, `trans`, `zero`, `one`, `add`, `mul`, and `of_rem`, which turns a zero remainder into divisibility with the quotient as the witness;
- `/std/Nat/Lt` and `/std/Nat/Le` carry the order laws, and `/std/WellFounded/recurse` over `WellFounded/lt` is strong induction along `<` — the measure a Euclidean recursion recurses on.

**Declared or proved.** Every law below is either a declaration — an operation's definition or morphism in the table the algebra specification builds, which conversion then decides — or a lemma proved over the facts above; none is an engine rule of its own. A fact inside the fragment the algebra specification decides that conversion misses is a gap in that specification, and is recorded there. A fact outside it is a lemma, and a refused row in `curios/src/tests/laws.rs` records why conversion does not take it. A declaration is decided in both checkers by the algebra specification's reference implementation, so a promoted operation's definition is written once there and its generated grid holds it.

## The Euclidean remainder

**The converse bridge.** `Divides(d, n)` implies `n % d == 0` for a positive `d`. `of_rem` is the other direction. The converse needs `(k · d) % d = 0` for a symbolic `k`, which it takes from `div_mod`'s uniqueness below. With a literal divisor the remainder is one of the algebra specification's defined operations, and the equation holds by conversion once its ring lands.

**Uniqueness of division.** Two pairs `(q, r)` and `(q′, r′)` with `n = q · d + r`, `r < d` and the same for the primed pair are equal. This is the fact every Euclidean argument ends in, and it is what lets `div_mod(k · d, d)` be read as `(k, 0)`.

**Exact division.** `exact_div(n, d, @ok: Lt(0, d), p: Divides(d, n)) -> Nat` returns the quotient, with `Eq(n, exact_div(n, d, p) · d)` beside it. It is `n / d` in its computation; the law is what the divisibility evidence buys.

**A certified greatest common divisor.** `/std/Nat/gcd` exists as general recursion and a type may not mention it. The certified one recurses on `Lt(b % a, a)` through `WellFounded/recurse` over `WellFounded/lt`, so it is total and may appear in a type; whether it replaces the existing `gcd` or stands beside it is decided when it lands, preferring one `gcd`. Its laws:

- it divides both operands;
- every common divisor divides it;
- symmetry, `gcd(a, 0) = a`, `gcd(a, 1) = 1`.

**Coprimality.** `Coprime(a, b)` is `Eq(gcd(a, b), 1)`, with `is_coprime` as its decision and the bridge to common-divisor reasoning: two numbers are coprime exactly when every common divisor is one. Then:

- the quotients of two numbers by their gcd are coprime;
- coprime cancellation — `Coprime(a, b)` and `Divides(a, b · c)` give `Divides(a, c)` — which is Euclid's lemma;
- the reduced-fraction prerequisites: two fractions in lowest terms with equal cross products have equal numerators and denominators.

## The unsigned binary scale

The dyadic `Rat` normalizes by stripping powers of two and compares values held at different binary exponents; both are unsigned facts about a magnitude and belong here.

- **Power-of-two scaling.** `shl(x, k) = x · pow(2, k)`, `shl(shl(x, a), b) = shl(x, a + b)` and the exponent laws of `pow` are the algebra specification's: its morphism stage promotes `pow` to `/sys` and reads the left shift through it, so none is written here. What is left is `shr(shl(x, k), k) = x`, a lemma.
- **Trailing zeros.** `trailing_zeros(n)` counts the factors of two in a positive `n`, and `odd_part(n)` is what is left, with the reconstruction `n = shl(odd_part(n), trailing_zeros(n))`, the oddness of `odd_part(n)`, and uniqueness: an odd `m` and a count `k` with `n = shl(m, k)` are `odd_part(n)` and `trailing_zeros(n)`. Zero is excluded by a `Lt(0, n)` premise rather than given a conventional count.
- **Comparing differently shifted magnitudes.** `shl(a, i)` against `shl(b, j)` is decided by aligning the smaller count, `shl(a, i - j)` against `b` when `j ≤ i`, with the order and equality it answers proved to be the unshifted comparison's.

## `min` and `max`

`/std/Nat/min` and `max` are library functions, so conversion sees only their unfolded matches. Each is promoted to a `/sys` operation and declared by its Presburger definition — `min(a, b)` is `a` where `a ≤ b` and `b` otherwise, and `max` the other — which the algebra specification's relational layer reads in a comparison and its ring in an equation. The order facts then hold by conversion once the relational layer lands, and the equations once the ring does:

- `min(a, b) ≤ a` and `min(a, b) ≤ b`, `a ≤ max(a, b)` and `b ≤ max(a, b)`;
- commutativity, associativity and idempotence of each;
- `min(a, b) + max(a, b) = a + b`, and `(a - b) + b = max(a, b)`, truncated subtraction read through the same definitions.

The promotion is new arithmetic on a numeric carrier and takes everything `CLAUDE.md`'s change routing names for one: the `/sys` rows, the literal fold in every constant folder, and the emitter's lowering with its i31 fast path.

## Verification

- Each law elaborates over symbolic operands and is exercised at the boundaries it states: a zero dividend, divisor one, a dividend below its divisor, exact multiples and their neighbours, `gcd` with a zero or a one, shifts across the i31 and past 64 bits, and `min` and `max` at equal operands and in either order.
- The executable `exact_div`, `gcd`, `trailing_zeros` and `odd_part` agree with `curios-num` over a generated grid, since the running program computes them over the same unbounded `Nat`, and `min` and `max` fold as they execute.
- A declared law is a row of the grid the algebra specification generates from its declarations. No lemma leans on a fact conversion does not decide without a refused row recording why.

## Completion criteria

- `Divides` has both bridges to the remainder, and `exact_div`, the certified `gcd` and `Coprime` exist with the laws above.
- The binary-scale helpers exist with reconstruction, uniqueness and aligned comparison, beside `shr(shl(x, k), k) = x`.
- `min` and `max` are `/sys` operations whose definitions are declared, and the laws above hold by conversion.
- The `Rat` specification needs no private division, gcd or bit-stripping of its own.
- Before this specification is deleted, the contracts are recorded in `/std/Nat`'s documentation and its modules' signatures, the roadmap entry is a checked summary, and no reference to this filename remains.
