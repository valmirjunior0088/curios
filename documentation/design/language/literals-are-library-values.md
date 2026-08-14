# Literals are library values

**Decision.** Character and string literals lower to transparent `/syn/Char` and `/syn/Str` values — proof-certified library structures — while the erased runtime carriers remain `Nat` and packed `Bytes`.

**Rationale.** The kernel stays free of ad-hoc literal types, literals arrive already carrying the structure and certificates library code wants to consume, and erasure guarantees those certificates cost nothing at runtime.

**Rejected.** Kernel-intrinsic character and string types.
