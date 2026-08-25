# curios-ersd

The Curios erased IR: the flat, explicit, first-order stage between `curios-elab`'s type-directed erasure and the continuation IR of `curios-cont`. Types, proofs, and erasable binders are gone by construction; the representation and its derived analyses belong to the crate rustdoc.

## Design

### The Ersd optimizer is thin

**Decision.** The Ersd optimizer runs exactly the transformations whose leverage is semantic — pruning, compile-time partial evaluation, and the monoid worker/wrapper rebase — and nothing else. Every structural and local optimization — folding, dead code, inlining, contification, specialization — belongs to `curios-cont`, which runs after the lowering.

**Rationale.** Ersd's leverage is what it still knows: don't hand Cont work it can delete (pruning), run what compile time has already decided (partial evaluation), and re-base what would exhaust the runtime stack (worker/wrapper). A second local-rewrite engine here would restate Cont's reductions over a second representation, and the two would drift.

**Rejected.** Local reductions in Ersd.

### Shapes stay distinct

**Decision.** The erased alphabet keeps erased Core's semantic identities intact — distinct scalar shapes, schema-carrying products and variants, dedicated Bool and Nat switches, first-class folds. One shape's operations are never reused for another, and conversions between shapes are explicit operations.

**Rationale.** Every encoding decision — carriers, tag layouts, dispatch, loop synthesis — belongs exclusively to the lowering into Cont. Collapsing shapes early discards information the backend needs and cannot recover, and an operation reused across shapes acquires a per-context meaning the semantic oracle could no longer classify node-locally.

### Numeric carriers are exact

**Decision.** Core arithmetic is unbounded; the erased carriers are exact machine scalars — `Nat` as `u32`, `Int` as `i32`, `Flt` as binary32 — with their semantics owned by `curios-num`'s `scalar`, the one constant-folding table every stage shares. The runtime's i31 envelope appears nowhere in the IR: a value the backend cannot box traps at the Wasm boundary instead of changing.

**Rationale.** One shared semantics table means the stages' constant folders cannot drift from each other or from emitted code, and keeping the envelope out of the IR keeps a representation limit from becoming a silent semantic one.

### Identity naming is cross-cutting

This crate's arena identities (`id.rs`) follow the naming scheme shared with `curios-cont` and `curios-wasm` — see [One naming scheme for compiler identities](../documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md), which states it once for all three.
