# `Flt` laws: decided identities and theorems over the bits

Working specification for the laws a proof about floats needs that `/std/Flt` still lacks past its operations: identities conversion decides for every bit pattern, and theorems proved over the encoding. The elementary functions of §9.2 and the rest of §8 are [a specification of their own](flt-elementary-spec.md); they consume these laws only in their proofs, never in their execution.

## What this builds on

- **The model.** `curios_num::Floating` is binary64 with every one of the 2⁶⁴ bit patterns a distinct value, one symmetric NaN rule — an invalid operation with no NaN operand answers `+0x7ff8_0000_0000_0000`, and otherwise the greatest quieted NaN operand, read unsigned, which keeps `add`, `mul`, `min`, `max` and `fma`'s product commutative bit for bit — and five rounding directions. [The binary64 model and its NaN rule](../soundness/per-term-rules/the-binary64-model-and-its-nan-rule.md) holds the running program to it.
- **The primitives.** Exactly IEEE's compute-exactly-round-once operations fold in one step through the model, each carrying its direction: `add`, `sub`, `mul`, `div`, `sqrt`, `fma`, the roundings to an integral value, and the conversions from `Nat` and `Int`; beside them the comparisons, `min`/`max`, the sign operations, the byte conversions, and `to_int`, `to_nat`, `mantissa` and `exponent`, the last four under `Flt/Finite` or `Flt/NonNeg`.
- **The law grid.** `curios/src/tests/laws.rs` states laws as held and refused rows put to both checkers. A law conversion takes is a declaration in the table [the algebra specification](algebra-spec.md) builds, whose rows it generates; what is not a law stays a control written by hand. A declaration is decided in both checkers by that specification's reference implementation.

## Decided laws

These hold for every bit pattern under the model, NaNs included, and conversion decides none of them today. Each is a declaration once the algebra specification's morphism stage provides its kind:

- **Commutativity** of `add`, `mul`, `min` and `max` in every direction, and of `fma`'s two factors — `eql` and `neq` are held already. The NaN rule is what makes these hold of the carrier rather than of numbers alone. Declared as a commutative magma, whose canonical form holds the operands in one order.
- **The sign operations**: `neg(neg(x)) = x`, `abs(abs(x)) = abs(x)`, `abs(neg(x)) = abs(x)`, `copysign(copysign(x, y), z) = copysign(x, z)` and `neg(copysign(x, y)) = copysign(x, neg(y))` — each a bit operation, so each holds of every pattern. Declared as operations derived through an isomorphism between a float and its sign and magnitude — `neg` negates the sign, `abs` clears it, `copysign` takes another float's — so these and every composition of them follow.
- **Subtraction** is addition of the negation, `sub(r, a, b) = add(r, a, neg(b))`, in every direction — the model defines it so. Declared as a derived operation.
- **The roundings to an integral value are idempotent**, `round_integral(r, round_integral(s, x)) = round_integral(s, x)`. Declared as projections onto the integral values, which every direction fixes.
- **The byte round trip, both ways.** `of_le_bytes(to_le_bytes(f)) = f` is held today; `to_le_bytes(of_le_bytes(b, @e)) = b` is refused in the grid's "Flt against Bytes" row and is now true, since every pattern is a value and no NaN is canonicalized. Declared as an isomorphism between `Flt` and eight-byte `Bytes`.

## Theorems over the bits

Provable in `/std` without further trust:

- `ord` is reflexive, transitive and total, antisymmetric to `Eq`, and `ord(a, b) = eq` exactly when `Eq(a, b)` — so `Ord(Flt)` is IEEE's `totalOrder` in fact as well as in intent.
- A `Key(Flt)` over `to_le_bytes` is a congruence, since the byte round trip is injective. Whether to give one is a separate decision: `/std/Map` withholds it because `/std/ops/Eql`'s witness is IEEE `==`, which calls `+0.0` and `-0.0` equal and a NaN equal to nothing, and a map keyed on propositional equality would disagree with it at both ends. The theorem lands whichever way that goes.

## What is not a law

Kept as controls, refused at every direction they apply to, each with its counterexample:

- associativity of `add` and `mul`, and distributivity — rounding;
- `f + 0.0 = f` — false at `f = -0.0`, whose sum with `+0.0` is `+0.0`;
- `f * 1.0 = f` — a signaling NaN is quieted;
- `f - f = 0.0` — an infinity or a NaN;
- `f * 0.0 = 0.0` — an infinity or a NaN, and the sign of a negative `f`;
- `f == f` — a NaN;
- `lt(a, b) = not(ge(a, b))` — a NaN on either side.

## Verification

Each row is stated in `curios/src/tests/laws.rs` first, at every direction it applies to, with the non-laws above as its controls, and moves from refused to held when its declaration lands. The grid the algebra specification generates from its declarations holds each one to the model over a pattern grid that includes both zeros, both infinities, subnormals, and quiet and signaling NaNs with payloads of both signs, and one mutation per kind of declaration is run and caught, as that specification's verification sets. Until a declaration lands, the grid holds its row refused and its comment points here.

## Completion criteria

- Every decided law above is held in the grid by its declaration, with its controls refused.
- The bits theorems are proved in `/std`.
- Before this specification is deleted, what it states is recorded in `/std/Flt`'s documentation, signatures and tests and in the perimeter, the roadmap entry is a checked summary, and no reference to this filename remains.
