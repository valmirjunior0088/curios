# curios-num

The values of the Curios intrinsic carriers: the unbounded `Natural` and `Integer`, the bitwise-identity `Floating`, and the packed `Binary` a `Bytes` value is, with the operations every stage's constant folder shares. It is the workspace's only `num-bigint` and `num-traits` dependency ([One crate is the authority for one external concern](../documentation/design/one-crate-is-the-authority-for-one-external-concern.md)). What a numeric carrier means to the language belongs to [syntax.md](../documentation/syntax.md), and how the carriers stay unbounded at run time is [Nat and Int are an i31 until they outgrow it](../documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md); local architecture belongs to the crate rustdoc.

## Design

### The magnitudes are sealed, not re-exported

**Decision.** `Natural` and `Integer` are newtypes whose magnitudes are private, and this crate re-exports nothing of `num-bigint` or `num-traits`: no crate above it can name a `BigUint` or import a `num-traits` trait to call a method on one.

**Rationale.** A use of `num-traits` is a trait import — `Zero`, `One`, `ToPrimitive`, `FromPrimitive` — existing only to make a method callable on a bignum; sealing turns those into inherent methods, so "only this crate does arithmetic" is enforced by privacy rather than by inventory. Adding an operation here adds to the trusted base, since the kernel decides with these types.

**Rejected.** Re-exporting the bignum types behind an alias, which leaves every caller able to reach the API the seal exists to close.

### One carrier at every layer, and the language's operations are its methods

**Decision.** `Natural` and `Integer` are the values of `Nat` and `Int` at every layer — type-level and erased alike, unbounded, ℕ and ℤ — and `Floating` is `Flt`'s. The operations with semantic freedom are methods of the carrier: the monus (`Natural::monus`), the trap conditions (`Natural::div`, `Integer::rem` and their siblings, answering a `ScalarTrap`), the growing operations that take their caller's allowance and decline past it (`Natural::shl_within`, `Integer::mul_within`), and the conversions whose domain excludes an operand (`Floating::to_natural`). The operators `num-bigint` lends keep its meaning — `Natural`'s `-` panics on underflow — for call sites that have established the result is a natural. Every stage's constant folder calls the same methods, so its arithmetic cannot drift from Core's, and the running program computes the same values, held to them by the differential grid in `curios`'s numeric tests.

**Rationale.** A carrier bounded by a machine word would make a term's meaning depend on the host at the type level, and at the erased level would fold a value to a number the running program computes differently; unbounded at every layer, a folder agrees with Core by construction. One method per operation is one API per operation, so a folder cannot reach for the unchecked spelling of an operation that can fail.

**Rejected.** A separate layer of free functions for the erased carriers beside the carriers' methods, two APIs for one operation; giving an operator the language's meaning, when `Natural`'s `-` would have to saturate for a folder and panic for a caller that knows the difference is a natural.

### `Floating` is binary64 over every pattern, and the language's definition of it

**Decision.** `Floating` is IEEE 754-2019 binary64 computed over unbounded integers: every one of the 2⁶⁴ bit patterns a distinct value, each operation computed exactly and rounded once in the `Rounding` it is given — ties to even, ties to away, toward zero, toward positive, toward negative — and one NaN rule for every operation, the default NaN where no operand is one and otherwise the greatest quieted operand pattern, read unsigned. `to_dyadic` hands out a finite value's exact mantissa and exponent, and `of_dyadic` rounds an exact value in any direction ([`Flt` is specified by a model](../documentation/design/arithmetic/flt-is-specified-by-a-model-and-the-runtime-conforms.md)).

**Rationale.** The kernel folds `Flt` through this type, so it must be a definition rather than an observation of the host: no floating-point unit is on the trusted path, and the host's `f64` is the oracle `floating::tests` checks the model against. `/std/Flt/exact` is the model's rounding in Curios, line for line, and the rounding tests hold the two together, so a change to `round` here is a change there.

**Rejected.** Deriving `Floating` from the host's `f64` with a gate around NaNs, which ties a term's meaning to the compiler's machine exactly where hardware disagrees.

### `Bytes` is the one packed carrier, and `Binary` counts bytes

**Decision.** `Binary` is the value of `Bytes`, the one packed carrier, at every layer: an immutable window over a shared byte buffer, its offset and its length counted in bytes. No bit-granular carrier exists in the language, `/sys`, `/std`, the host ABI or the run-time representation. A program that needs a bit string gets one as a `/std` type built over `Bytes` with its length beside it, and does bit-level work through `Nat`'s `and`, `or`, `xor`, `shl` and `shr` and `Byte`'s conversions to and from `Nat`. Equality, the hash and the order read the bytes: the order is bytewise, the shorter prefix first, which is `/std/Bytes/cmp`'s.

**Rationale.** A carrier read at two grains costs every reader of a packed value a grain to dispatch on, the host boundary a wire type and the run time a second immediate layout, and a window kept in bits makes every byte reader assert an alignment only a bit window can break. With one grain a window's offset and length are the counts its readers take, and every charge on a packed value is already stated in bytes.

**Rejected.** A bit-grained `Bits` carrier kept for a consumer that may come, which no program has written; a grain kept as a one-variant extension point, a type with one inhabitant that states nothing its readers can use; a bit accessor on `Nat` or `Byte`, which nothing asks for.
