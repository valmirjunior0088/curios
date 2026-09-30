# curios-ersd

The Curios erased IR: the flat, explicit, first-order stage between `curios-elab`'s type-directed erasure and the continuation IR of `curios-cont`, with the one-way door `lower_to_cont` between them. Types, proofs and erasable binders are gone by construction. How the module prints and how its identities are spelled is [A printer states each fact once, where it is bound](../documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md); the representation and its derived analyses belong to the crate rustdoc.

## Design

### The Ersd optimizer is thin

**Decision.** The Ersd optimizer runs exactly the transformations whose leverage is semantic — pruning, compile-time partial evaluation of closed terms and literal spines, and the monoid worker/wrapper rebase — and nothing else. Folding, dead code, inlining, contification and specialization belong to `curios-cont`, after the lowering.

**Rationale.** Ersd's leverage is what it still knows: hand Cont no work it can delete, run what compile time has decided, and re-base what would exhaust the runtime stack. A second local-rewrite engine would restate Cont's reductions over a second representation, and the two would drift.

### Shapes stay distinct

**Decision.** The erased alphabet keeps erased Core's semantic identities — distinct scalar shapes, schema-carrying products and variants, dedicated `Bool` and `Nat` switches, first-class folds. One shape's operations are never reused for another, and conversions between shapes are explicit operations.

**Rationale.** Every encoding decision — carriers, tag layouts, dispatch, loop synthesis — belongs to the lowering into Cont. Collapsing shapes early discards information the backend cannot recover, and an operation reused across shapes acquires a per-context meaning no node-local classification can read.

### Numeric carriers are exact

**Decision.** The erased carriers are Core's, unbounded — `Nat` and `Int` as `curios-num`'s `Natural` and `Integer`, `Flt` as binary64 — with the carrier methods every stage's constant folder shares. The runtime's split between an i31 and a boxed magnitude appears nowhere in the IR; it is `curios-cont`'s and `curios-emit`'s ([Nat and Int are an i31 until they outgrow it](../documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)).

**Rationale.** One shared semantics keeps the folders from drifting from each other or from emitted code, and keeping the runtime's representation out of the IR keeps a representation choice from becoming a semantic one.
