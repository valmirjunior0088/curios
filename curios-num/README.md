# curios-num

The values of the Curios intrinsic carriers: the unbounded type-level `Natural` and `Integer`, the bitwise-identity `Floating`, the packed `Binary` a `Bits` or `Bytes` is read from at its `Grain`, and the `scalar` semantics of the erased carriers — `Natural`, `Integer` and binary64 again — every stage's constant folder shares. It is also the workspace's only `num-bigint` and `num-traits` dependency. What a numeric carrier means to the *language* belongs to [syntax.md](../documentation/syntax.md); how the carriers stay unbounded at run time, and where a narrowing refuses, is [Nat and Int are an i31 until they outgrow it](../documentation/design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md); local architecture belongs to the crate rustdoc.

## Design

### One crate is the authority for one external concern

**Decision.** `num-bigint` and `num-traits` are named in this crate's manifest and nowhere else in the workspace, the arrangement [One crate is the authority for one external concern](../documentation/design/toolchain/one-crate-is-the-authority-for-one-external-concern.md) states in general and that `curios-archive` and `curios-profile` follow for rkyv and `tracing`.

**Rationale.** The design entry's: a pin in one manifest concentrates authority, where a `[workspace.dependencies]` row shares only configuration. What that buys here is that the number of places arithmetic can enter the workspace is one.

### The magnitudes are sealed, not re-exported

**Decision.** `Natural` and `Integer` are newtypes whose magnitudes are private, and this crate re-exports nothing it owns. No crate above it can name a `BigUint` or import a `num-traits` trait to call a method on one.

**Rationale.** This is where the crate departs from `curios-archive` and `curios-profile`, which do re-export, and the departure is what makes the boundary real rather than clerical. Every use of `num-traits` in the workspace was a trait import — `Zero`, `One`, `ToPrimitive`, `FromPrimitive` — existing only to make a method callable on a bignum. Sealing turns those into inherent methods and removes the trait from the workspace's code entirely, so "only this crate does arithmetic" is enforced by privacy rather than by inventory.

The consequence to accept is that adding an operation to `Natural` or `Integer` is adding to the trusted base: the kernel decides with these types, so every operation is a rule it can reach.

**Rejected.** Re-exporting the bignum types behind an alias. It leaves every caller able to reach the underlying API, which is the thing the seal exists to prevent, and it would have kept `num-traits` in the workspace's code for the sake of one import line per call site.

### Two layers, and neither is expressible in the other

**Decision.** `Natural`/`Integer` are *type-level* values — unbounded, pretending ℕ and ℤ. The `scalar` functions are a separate layer giving the exact semantics of the *erased* carriers, where `Nat` is again a `Natural` and `Int` an `Integer`, and the operations that can fail — a growing fold past its caller's allowance, a division by zero, a conversion off its domain — decline or refuse rather than answer a changed number. Every stage's constant folder shares the second layer so its arithmetic cannot drift from Core's, and the running program computes the same values — an i31 while small and a boxed magnitude past it, checked against this layer by the differential grid in `curios`'s numeric tests.

**Rationale.** A type-level natural bounded by a machine word would make a term's *meaning* depend on the host, which is not a tradeoff a dependent type theory can take. Conversely an erased carrier that reasoned in ℕ would be describing something the emitted Wasm does not do.

**Rejected.** Conflating the layers behind one type with one set of operations. The two disagree in observable ways at the same spelling — `Natural`'s `-` panics on underflow while `nat_sub` saturates — so a single operation would have to pick one and be wrong for the other caller. They are different operations about different things, and the separation is what says so.

### `Floating` is binary64 over every pattern, and the language's definition of it

**Decision.** `Floating` is IEEE 754-2019 binary64 computed over unbounded integers: every one of the 2⁶⁴ bit patterns a distinct value, each operation computed exactly and rounded once in the `Rounding` it is given — ties to even, ties to away, toward zero, toward positive, toward negative — and one NaN rule for every operation, the default NaN where no operand is one and otherwise the greatest quieted operand pattern, read unsigned. `to_dyadic` hands out a finite value's exact mantissa and exponent, and `of_dyadic` rounds an exact value in any direction. What this means to the language, and what holds the running program to it, is [`Flt` is specified by a model](../documentation/design/language/flt-is-specified-by-a-model-and-the-runtime-conforms.md) and [the binary64 model and its NaN rule](../documentation/soundness/per-term-rules/the-binary64-model-and-its-nan-rule.md).

**Rationale.** The kernel folds `Flt` through this type, so it has to be a definition rather than an observation of the host: no floating-point unit is on the trusted path, and the host's `f64` is the oracle `floating::tests` checks the model against, never its source. `/std/Flt/exact` is the model's rounding written in Curios, line for line, and the rounding tests hold the two together, so a change to `round` here is a change there too.

**Rejected.** Deriving `Floating` from the host's `f64` with a gate around NaNs: it ties a term's meaning to the compiler's machine, and a NaN's bits are exactly what hardware disagrees on.

