# General `BigFlt` exact decimal interop

Post-program-analysis implementation specification for exact decimal parsing and explicit decimal presentation over canonical rational `BigFlt`. This is the final planned general-number layer and follows the general core, laws, field laws, and binary32 boundaries.

## Objective

Parse every finite decimal scientific-notation value exactly into `BigFlt`, without rounding, and provide presentation APIs whose behavior for terminating and nonterminating decimal expansions is explicit.

## Exact parsing

Use the established conversion name:

```text
of_str : Str -> Option(BigFlt)
```

The accepted grammar has an optional sign, decimal digits, an optional fractional part with digits on both sides of the point according to the chosen grammar, and an optional signed base-10 exponent. Separator support, if any, is specified explicitly rather than inherited accidentally from TOML.

Parse the coefficient as `BigInt` and combine the fractional width with the written exponent. For decimal exponent `k`:

- `k >= 0`: multiply the coefficient by `5^k` and add `k` to the binary exponent;
- `k < 0`: use odd denominator `5^(-k)` and subtract `-k` from the binary exponent;
- call `of_scaled_ratio` once to reduce shared factors and canonicalize zero.

No native `Flt`, native `Int`, or intermediate approximate decimal participates in parsing.

## Decimal presentation

Distinguish exact terminating decimal output from general rational presentation:

```text
to_decimal : BigFlt -> Option(Str)
to_ratio_str : BigFlt -> Str
```

`to_decimal` succeeds exactly when the reduced odd denominator contains no prime factor other than five. It emits a deterministic finite decimal spelling with no loss of value. It returns `none` for values such as `1/3`; it never silently rounds.

`to_ratio_str` is total and emits a deterministic exact rational spelling for diagnostics and `Show`. Its syntax is presentation, not automatically accepted by `of_str` unless a later decision deliberately extends that grammar.

A future rounded formatter must take an explicit precision and rounding mode and belongs to a separate specification.

## TOML relationship

The landed `/std/Toml` module deliberately stores native `Int` and `Flt` and does not depend on this work. After general decimal interop lands, a separately approved TOML profile may store exact `BigInt`/`BigFlt` values or use `of_str` as an exact intermediate before explicit binary rounding.

Do not silently change `Toml/int` or `Toml/flt` as part of this specification; that would be a public data-model migration requiring its own decision.

## Proof obligations

- Parsing denotes the exact written integer multiplied by the exact power of ten.
- `of_str(to_decimal(x)) = some(x)` whenever `to_decimal` succeeds.
- Decimal termination is characterized by the reduced odd denominator's power-of-five form.
- `to_ratio_str` reconstructs the exact numerator, binary exponent, and denominator value without exposing the private runtime layout as a stable API.
- Zero has one canonical output and negative zero does not arise.

## Verification

- Compare parsing with an arbitrary-precision rational reference over generated coefficients, scales, signs, and exponents.
- Pin `0.1`, terminating dyadics, shared powers of two and five, very large exponents, zero spellings, and malformed input.
- Generate terminating rationals and test exact decimal round-trip.
- Generate nonterminating rationals and confirm `to_decimal` returns `none`.
- Confirm no native-width overflow or native floating operation appears.
- Run the repository done bar.

## Non-goals

- Infinities, NaNs, signed zero, hexadecimal floats, or locale-sensitive formatting.
- Implicit rounding of nonterminating decimals.
- A shortest rounded decimal algorithm for binary32 or binary64.
- Automatic migration of JSON, TOML, format strings, or native `Flt` APIs.

## Completion criteria

- Every accepted decimal parses to its exact rational value.
- Exact finite decimal formatting succeeds precisely on terminating values.
- General values have a deterministic exact rational presentation.
- Parsing and formatting are independent of native numeric range and floating precision.
- Before this specification is deleted, the accepted grammar, exact parsing contract, termination criterion, and decimal and rational presentation policies are recorded in the owning `/std/BigFlt` documentation and tests; remaining plans refer to landed conversion functions rather than this file; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
