# `Int` laws: the order bridges, multiplicative cancellation, `abs`, `sign`, `min` and `max`, and the signed scale

Working specification for the `Int` theorems and operations the standard library still lacks past its order laws. They are an independent capability of `/std/Int`, and [the `Rat` specification](rat-spec.md) is the consumer that asks for them.

## What this builds on

`Int` is unbounded at every layer, the running program included ([Nat and Int are an i31 until they outgrow it](../design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md)). The kernel reasons about its arithmetic: what conversion decides about `Int` today — the commutative-ring laws through the sum normal form, a comparison through its difference split by sign, and `Nat/to_int` as an ordered-semiring embedding — is recorded in [Open fold laws and the sum normal form](../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md), and what it will decide is [the algebra specification](algebra-spec.md)'s. Two laws a proof might expect to state are already there and are not lemmas: order reversal under negation, `a <= b` being `+0 - b <= +0 - a`, and additive cancellation in its order form, `a + n <= b + n` being `a <= b`.

The library carries what reduction does not state:

- `/std/Int`'s indexed `Sign` view — every `Int` is `nonneg(n)`, the embedding of `n`, or `neg(n)`, which is `-1 - Nat/to_int(n)` — with `view`, `trichotomy` and `eq_of_eql`;
- `/std/Int/Lt`: `try`, `le_of_lt`, `trans`, `lt_of_lt_le`, `lt_of_le_lt` and `le_of_not_lt`;
- `/std/Int/Le`: `refl`, `try`, `trans`, `add_mono_l`, `mul_mono_r` under a non-negative factor, and `antisym`.

Each is proved by carrying the `Nat` law along the embedding through the sign view, and the lemmas below follow the same route: a statement about `Int` is split by the view of its operands, and each case is a `Nat` law read through `Nat/to_int`.

**Declared or proved.** Every law below is either a declaration — an operation's definition or morphism in the table the algebra specification builds, which conversion then decides — or a lemma; none is an engine rule of its own. A fact inside the fragment the algebra specification decides that conversion misses is a gap in that specification, and is recorded there. A fact outside it is a lemma, and a refused row in `curios/src/tests/laws.rs` records why conversion does not take it. A declaration is decided in both checkers by the algebra specification's reference implementation, so a promoted operation's definition is written once there and its generated grid holds it.

## Order

- **Totality**, as a disjunction: `Le(a, b)` or `Le(b, a)`, from `trichotomy`. Its `Bool` form, `a <= b || b <= a`, is refused by conversion today and holds by it once the algebra specification's relational layer lands; the disjunction stays a lemma, since a `Prop` is not a `Bool`.
- **The executable relations agree with the propositions**: `ord`, `==`, `<`, `<=`, `>` and `>=` each decide the proposition their spelling names, and `ord(a, b)` is `eq` exactly when `Eq(a, b)`.
- **Flip symmetry**: `ord(b, a)` is `ord(a, b)` reversed.
- **Multiplication monotonicity under a non-positive factor**: `Le(a, b)` and `Le(k, +0)` give `Le(b · k, a · k)`, the twin of `Le/mul_mono_r`.

## Cancellation

- **Multiplicative cancellation under a nonzero premise**: `Eq(a · c, b · c)` and `NonZero(c)` give `Eq(a, b)`, on both sides of the product. This is the integral-domain fact both `Rat` normalizations use in place of an inverse. Conversion never takes it, since without the premise `a · c = b · c` does not give `a = b`.

## `abs`, `sign`, `min` and `max`

`/std/Int`'s `abs`, `sign`, `min` and `max` are library functions, so conversion sees only their unfolded matches and none of their laws holds by it: `abs(+0 - a)` against `abs(a)`, and `abs(a * b)` against `abs(a) * abs(b)`, are both refused today. Each is promoted to a `/sys` operation, `abs` answering a `Nat` and `sign` an `Int` in `{-1, 0, +1}`, and declared:

- **by its Presburger definition** — `abs(n)` is `n` read as a `Nat` where `+0 ≤ n` and `-n` otherwise, `sign(n)` is `-1`, `+0` or `+1` by the sign of `n`, and `min` and `max` are as `Nat`'s — which the algebra specification's relational layer reads in a comparison and its ring in an equation;
- **`abs` and `sign` as homomorphisms** of `(ℤ, ·)`, into `(ℕ, ·)` and `(ℤ, ·)`, which its morphism stage pushes through a product.

Then these hold by conversion, the order facts once the relational layer lands and the rest once the ring and the morphism stage do:

- **the order facts**: `abs(n)` is zero exactly when `n` is, `Le(+0, n)` exactly when `sign(n)` is not `-1`, and `Le(-Nat/to_int(abs(n)), n)` and `Le(n, Nat/to_int(abs(n)))`;
- `abs(Nat/to_int(m))` is `m`;
- **the absolute difference**: `abs(a - b)` is symmetric and satisfies the triangle inequality `Le(abs(a - c), abs(a - b) + abs(b - c))` in `Nat` — the form a rounding error is compared in;
- **reflection**: `abs(-n)` is `abs(n)`, `sign(-n)` is `-sign(n)`, `sign(a · b)` is `sign(a) · sign(b)`, and `abs(a · b)` is `abs(a) · abs(b)`;
- `min` and `max`'s laws, as [the `Nat` laws](nat-laws-spec.md) list them.

Two stay lemmas:

- **decomposition**, `Eq(n, sign(n) · Nat/to_int(abs(n)))`: a product of two defined operations, which linear arithmetic reads as one opaque atom;
- **`abs(a - b)` is zero exactly when `Eq(a, b)`**: its `Bool` half holds by conversion, and its `Prop` half is a lemma through `eq_of_eql`.

The promotion is new arithmetic on a numeric carrier and takes everything `CLAUDE.md`'s change routing names for one: the `/sys` rows, the literal fold in every constant folder, and the emitter's lowering with its i31 fast path.

## The signed scale

The dyadic `Rat` holds a signed mantissa at a binary exponent, so the unsigned binary-scale layer of [`nat-laws-spec.md`](nat-laws-spec.md) has to reach through a sign.

- **Shifts keep the sign**: `shl(n, k)` is `n` times the widened `pow(2, k)` in the algebra specification, so with the homomorphisms above `sign(shl(n, k))` is `sign(n)` and `abs(shl(n, k))` is `shl(abs(n), k)` by conversion, once its morphism stage lands.
- **A right shift is a floor**: `shr(n, k)` is `⌊n / 2ᵏ⌋`, so for a negative `n` it is `-shr(abs(n) - 1, k) - 1`, and it agrees with the `Nat` shift on a non-negative `n`. A lemma.
- **Parity through the sign**: `n` is odd exactly when `abs(n)` is, so the odd-mantissa invariant is stated once on the magnitude. It holds by conversion in its `Bool` form, the remainder by a literal being a defined operation.
- **Odd-mantissa uniqueness**: two odd mantissas at two exponents denoting one value are equal, and so are their exponents — the unsigned uniqueness of `odd_part` and `trailing_zeros`, carried through the sign. A lemma.

## Verification

- Each law elaborates over symbolic operands and is exercised at zero, at both signs, at equal magnitudes of opposite sign, at a cancellation premise's boundary, and past the i31 and 64 bits.
- No proof opens a case analysis the sign view already performs; a missing shared step is added to the view's neighbourhood once rather than re-derived per law.
- The promoted operations fold as they execute, at both signs and past the i31. A declared law is a row of the grid the algebra specification generates from its declarations, and no lemma leans on a fact conversion does not decide without a refused row recording why.

## Completion criteria

- The order bridges, multiplicative cancellation and the signed-scale lemmas above exist under `/std/Int` with operation-first names.
- `abs`, `sign`, `min` and `max` are `/sys` operations whose declarations hold the laws above by conversion.
- The `Rat` specifications need no private sign reasoning of their own.
- Before this specification is deleted, the contracts are recorded in `/std/Int`'s documentation and its modules' signatures, the roadmap entry is a checked summary, and no reference to this filename remains.
