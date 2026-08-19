# `BigNat` Euclidean arithmetic

Post-program-analysis implementation specification for the reusable Euclidean foundation required by general rational `BigFlt`. This is an independent `/std/BigNat` capability rather than float-owned helper code.

## Objective

Add certified division with remainder, exact division, divisibility, greatest common divisors, and coprimality over canonical packed `BigNat` values. The executable algorithms must scale with packed bit length, expose the facts required by normalization, and remain compatible with future termination checking.

## Ownership and naming

Operations and laws over unsigned magnitudes belong beside `BigNat`. Positive-divisor conveniences may consume `NonZero`, but they do not move ownership into `/std/NonZero`.

Use repository conventions:

```text
divmod
exact_div
gcd
Divides
Coprime
is_coprime
```

`divmod` follows the established spelling already used privately by `/std/Flt`. PascalCase names denote propositions or certified result types; executable Boolean predicates use `is_*`; low-level bit algorithms use `_raw`.

## Certified division result

Define a dependent result carrying executable outputs and erased evidence:

```crs
pub struct DivMod(dividend : BigNat, divisor : NonZero) : pub Type {
    quotient : BigNat,
    remainder : BigNat,
    reconstruct : Eq(dividend, add(mul(quotient, divisor.value), remainder)),
    bounded : Lt(remainder, divisor.value),
}

pub let divmod(dividend : BigNat, divisor : NonZero) -> DivMod(dividend, divisor)
```

The final spelling of the bound may use the existing reflected order proposition rather than introducing a duplicate `Lt`. The semantic obligations are reconstruction and a strict remainder bound.

## Executable algorithms

Implement binary long division over the packed most-significant traversal or another structurally bounded bit algorithm. The implementation must not repeatedly subtract the divisor from the full dividend and must not convert arbitrary magnitudes through native `Nat` or `Int`.

The specification requires an explicit termination argument before the main implementation. Acceptable designs include structural recursion over a reversed or indexed bit view, or recursion over explicit fuel derived from packed bit length with a proof that the bound is sufficient. Unchecked semantic decrease is not acceptable merely because the current language accepts general recursion.

`exact_div(value, divisor, proof)` returns the quotient when `divisor` is known to divide `value`; it reuses `divmod` and eliminates an impossible nonzero remainder through the divisibility evidence.

`gcd` may use Euclid's algorithm over certified remainders once the recursion measure is explicit. Binary GCD is also acceptable if its measure and interaction with packed normalization are easier to prove.

## Divisibility and coprimality

Define `Divides(divisor, value)` through an existential multiplication witness or an equivalent proof-irrelevant structure. Define `Coprime(a, b)` by `Eq(gcd(a, b), one)` and prove the bridge to common-divisor reasoning.

Export:

- reflexivity and transitivity of divisibility;
- zero, one, addition, and multiplication interaction;
- GCD divides both operands;
- every common divisor divides the GCD;
- GCD symmetry and zero/one identities;
- quotients by the GCD are coprime;
- coprime multiplication cancellation and Euclid's lemma;
- reduced-fraction uniqueness prerequisites.

Do not expose implementation-specific quotient-digit state in public theorem statements.

## Structural proof obligations

- Every division step preserves the processed-prefix reconstruction equation.
- Every intermediate remainder remains below the divisor.
- Packed normalization does not change represented values.
- `exact_div` reconstructs its dividend.
- GCD is a common divisor and greatest under `Divides`.
- Dividing both operands by their GCD produces coprime values.
- All proof fields erase from runtime results.

## Verification

- Compare `divmod` and `gcd` against an arbitrary-precision reference over generated packed values.
- Pin zero dividends, divisor one, dividend smaller than divisor, equality, exact multiples, one-less and one-more boundaries, and long sparse bit strings.
- Exercise every public theorem over symbolic inputs.
- Benchmark highly unequal operand sizes and adversarial quotient patterns.
- Confirm no default test-thread stack regression and no native-width conversion.
- Run the repository done bar.

## Non-goals

- Signed Euclidean division conventions for `BigInt`.
- Extended GCD or Bézout coefficients unless a proof route demonstrates they are necessary.
- Integer factorization, primality, modular exponentiation, or cryptographic constant-time behavior.
- Rational storage or any `BigFlt` representation change.
- Native `Nat`, `Int`, or `Flt` arithmetic laws.

## Completion criteria

- `/std/BigNat/divmod`, `exact_div`, and `gcd` satisfy their certified contracts.
- `Divides` and `Coprime` support reduced-fraction normalization and uniqueness without an unstated arithmetic axiom.
- The algorithms have explicit structural or bounded termination arguments.
- Proofs erase and executable outputs agree with the reference corpus.
- General rational normalization requires no additional private GCD or division implementation.
- Before this specification is deleted, the Euclidean API contracts, divisibility and coprimality propositions, termination measures, and exported theorem surface are recorded in the owning `/std/BigNat` documentation, signatures, and tests; remaining plans refer to landed operations and laws rather than this file; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
