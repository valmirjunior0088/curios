# Proofs of rational–binary64 conversion

**Not refined yet.** This specification preserves the formal boundary theorems deferred from [exact rationals](04-exact-rationals.md). Their targets are recorded below, but the connection between primitive floating-point results and their exact interpretation still lacks a specified foundation. This is not an implementation plan until that foundation and the proof obligations are refined.

## Scope and dependencies

A `/sys/Flt` fold trusts the Rust model in `curios_num::Floating`; the Curios twin in `/std/Flt/exact` is held to it by the rounding tests but does not fold. Folding by the twin, so that a proof about a fold reaches the Curios code rather than the model, waits on these proofs and on exact rationals.


Exact rationals owns the rational representation, normalization, arithmetic, executable conversions, decimal behavior and library laws. Those contracts are used here, not restated. The proofs here additionally depend on the exact interpretation of `/std/Flt` primitives and the correctness of the Curios rounding implementation.

The boundary between executable evidence and formal theorems remains explicit. Exact rationals can retire with its own proofs and reference tests complete while these theorems remain pending; this split does not weaken its canonicity, ring, order, field or decimal obligations.

## Theorem targets

Stated over `Rat` and `/std/Flt`'s exact layer. The rounding the conversions go through, `Flt/rounded/of_dyadic`, is Curios code, so its correctness is proved here in every direction; the `Flt` primitives beside it are the trusted model. Each statement below reads a primitive's result through its exact value — `Dyadic/of` goes through `/sys/Flt/mantissa` and `/sys/Flt/exponent` — and so needs a stated tie between a primitive and its exact result. The reflected model that was to state it is no part of the [declared](../../design/arithmetic/flt-is-specified-by-a-model-and-the-runtime-conforms.md) float identities, which hold identities of the model rather than tie it to exact values, so this work waits until that foundation has a specified owner and contract.

- **Round trip.** `to_flt(r, of_flt(f, @ok))` is `f` for every finite `f` but `-0.0`, in every direction; `-0.0` widens to canonical zero and canonical zero narrows to `+0.0`.
- **The directions.** For `x` narrowing to a finite `f` toward negative, `f ≤ x` and no finite binary64 lies in `(f, x]`; toward positive and toward zero by symmetry.
- **Nearest value.** For `x` narrowing to a finite `f` to nearest, no finite binary64 `y` is nearer — `abs(f - x) ≤ abs(y - x)` — under the overflow premise `abs(x) < 2¹⁰²⁴ - 2⁹⁷⁰`; ties to even choose the even significand and ties to away the larger magnitude; the half-ulp corollary in the normal range, its absolute-grid form for subnormals, carry into the next exponent and the signed zero follow.
- **Quotients.** `ratio_to_flt` is the correctly rounded quotient for a nonzero denominator, stated with cleared denominators so no interior quotient is built, and the `div_mod` step's quotient and remainder imply the rounding decision.
- **The total order.** `Flt/ord(x, y)` is `Nat/ord(Flt/total_key(x), Flt/total_key(y))` at every pattern, so `Ord(Flt)` is IEEE's `totalOrder` in fact: `/std/Flt/total_key_injective` proves the key injective, so its order is total and agrees with `Eq` by `/std/Nat/ord`'s laws, and `ord` leads with `<`, which decides every ordered pair before the key is read. Nothing declared says what `<` answers of two patterns, so not even `ord(x, x)` being `eq` is provable; the tie this needs is the comparison's, to the sign and magnitude it reads, where the theorems above need an operation's, to its exact result.

The proof library isolates the field interpretation, the grid spacing and adjacent representable values, the guard and sticky characterization of below-, at- and above-half remainders, carry, the overflow threshold, and the cleared-denominator comparison. A proof that needs to open the representation beyond the executable-correctness lemmas strengthens those lemmas instead.

## Decisions required before implementation

- Specify the connection between the primitive floating-point model and the exact values the proofs read, identify its owner, and state which facts are trusted and which are proved. A model reflected into Curios is one proposal, not an interface [the declared operations](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) supply.
- Give the rounding a right shift the logic reads. `/std/Flt/exact`'s `round_in` keeps `Nat/shr(m, s)` and drops `m - Nat/shl(kept, s)`, and conversion ties a right shift to no quotient ([what the carriers' algebra leaves undecided](09-left-undecided-by-the-carriers-algebra.md)), so no statement about what it keeps is provable: either the shift's law lands first, or the rounding is written over a quotient by a power of two, which `/std/Nat/DivMod` already certifies.
- Give each theorem its precise premises and equality notion, including negative zero, finite results, overflow and all rounding directions. The target descriptions above do not replace elaborated theorem statements.
- Identify the reusable lemmas for grid spacing, adjacent values, quotient bounds and guard/sticky reasoning. Keep integer facts in `/std/Nat` and `/std/Int`, beside the numeric laws there, and implementation contracts with the eventual permanent owner of exact rationals.
- Establish proof sequencing, resource bounds and focused validation before implementation. Stronger conversion than [the declared operations](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) give — [the relational layer](08-relational-layer.md), if its consumer appears — is available only when that capability lands.

## Verification to refine

Every boundary theorem must elaborate and certify over symbolic inputs under its stated premises. Keep the executable conversion fixtures from exact rationals, and connect the proof surface to the same conversion definitions rather than proving a parallel algorithm. Exercise theorem instantiation at normal and subnormal boundaries, exact halves, significand carry, signed zero and overflow premises. Tests supplement the proofs; they do not establish the missing model connection.

## Non-goals

A new rational representation or conversion algorithm; adding infinities, NaNs or signed zero to `Rat`; elementary floating-point functions; and general laws for native floating-point arithmetic beyond the facts this boundary requires.

## Completion and retirement

Refinement must turn the targets into explicit theorem signatures and an implementation checklist. Completion requires the round-trip, directed-rounding, nearest-value and quotient theorems in every applicable direction, with the model connection, assumptions and evidence documented.

When exact rationals retires, replace its link with the implemented library contracts. When this work completes, place the theorem surface in `/std/Rat` and `/std/Flt` documentation and signatures, the model assumptions and evidence with their soundness owners, and cross-cutting decisions and rejected alternatives in design documentation. Update the roadmap and all dependent references, verify no reference to this filename remains, then delete this specification.
