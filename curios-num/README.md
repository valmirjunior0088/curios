# curios-num

The Curios numeric tower: the unbounded type-level `Natural` and `Integer`, the bitwise-identity `Floating`, and the `scalar` semantics of the erased `u32`/`i32`/binary32 carriers every stage's constant folder shares. It is also the workspace's only `num-bigint` and `num-traits` dependency. What a numeric carrier means to the *language* belongs to [syntax.md](../documentation/syntax.md); how narrowing refuses rather than wraps is [Numeric carriers narrow by refusing, never by changing a value](../documentation/design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md); local architecture belongs to the crate rustdoc.

## Design

### One crate is the authority for one external concern

**Decision.** `num-bigint` and `num-traits` are named in this crate's manifest and nowhere else in the workspace, the arrangement [One crate is the authority for one external concern](../documentation/design/toolchain/one-crate-is-the-authority-for-one-external-concern.md) states in general and that `curios-archive` and `curios-profile` follow for rkyv and `tracing`.

**Rationale.** A dependency present in exactly one manifest cannot be added elsewhere without someone writing the version down a second time, which is a question a reviewer will ask. A `[workspace.dependencies]` row shares *configuration* and concentrates no authority at all: every crate may still take the row, so the count of places arithmetic can enter stays unbounded.

### The magnitudes are sealed, not re-exported

**Decision.** `Natural` and `Integer` are newtypes whose magnitudes are private, and this crate re-exports nothing it owns. No crate above it can name a `BigUint` or import a `num-traits` trait to call a method on one.

**Rationale.** This is where the crate departs from `curios-archive` and `curios-profile`, which do re-export, and the departure is what makes the boundary real rather than clerical. Every use of `num-traits` in the workspace was a trait import — `Zero`, `One`, `ToPrimitive`, `FromPrimitive` — existing only to make a method callable on a bignum. Sealing turns those into inherent methods and removes the trait from the workspace's code entirely, so "only this crate does arithmetic" is enforced by privacy rather than by inventory.

The consequence to accept is that adding an operation to `Natural` or `Integer` is adding to the trusted base: the kernel decides with these types, so every operation is a rule it can reach.

**Rejected.** Re-exporting the bignum types behind an alias. It leaves every caller able to reach the underlying API, which is the thing the seal exists to prevent, and it would have kept `num-traits` in the workspace's code for the sake of one import line per call site.

### Two layers, and neither is expressible in the other

**Decision.** `Natural`/`Integer` are *type-level* values — unbounded, pretending ℕ and ℤ. The `scalar` functions are a separate layer giving the exact semantics of the *erased* carriers, where `Nat` is a `u32` that wraps and `Int` an `i32` that traps. Every stage's constant folder shares the second layer so its arithmetic cannot drift from the backend's; the runtime's 31-bit range is enforced only where a literal must materialize, in erasure's narrowing and in the runtime's own overflow traps.

**Rationale.** A type-level natural bounded by a machine word would make a term's *meaning* depend on the host, which is not a tradeoff a dependent type theory can take. Conversely an erased carrier that reasoned in ℕ would be describing something the emitted Wasm does not do.

**Rejected.** Conflating the layers behind one type with one set of operations. The two disagree in observable ways at the same spelling — `Natural`'s `-` panics on underflow while `nat_sub` saturates — so a single operation would have to pick one and be wrong for the other caller. They are different operations about different things, and the separation is what says so.
