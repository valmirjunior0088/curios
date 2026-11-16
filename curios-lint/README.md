# curios-lint

The lint owner: every lint of a unit decided from the facts the stages recorded of it — the zeros of name resolution the lowering records ([`curios-module`](../curios-module/README.md)), less every written binder a proof the elaborator wrote read — and worded, placed and ordered for its reader, with a package's `unused-dependency` decided from the mounts its units' references were written under. What a lint is, and which ones there are, is [A lint is an exact finding read off the compilation](../documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md); what the API offers belongs to the crate rustdoc.

## Design

### A stage records, the owner decides

**Decision.** No stage decides or words a lint. The lowering records what resolution established — a selector nothing resolved through, a written binder nothing referenced, a declaration nothing reaches — and elaboration records what it read that no written name reaches; this crate reads both and decides which are reported, as what, where and in what order. It sits over `curios-module`, whose records it reads, and under `curios-unit`, which asks it for a unit's lints where the unit is made and stores them with it.

**Rationale.** A lint joins facts two stages know halves of, which neither can join alone, and one owner keeps each later lint one rule in one crate rather than a branch in every stage that knows a fact, the shape in which GHC's warnings spread across its renamer, typechecker and desugarer. Deciding once, where the unit is made, makes a lint the same however its unit is reached — compiled now, restored from a store, compiled over a baseline or read from the prelude's archive — and asking again decides nothing.

**Rejected.** Deciding where the lints are read, which reads a restored unit's baseline half to report it; and a lint walking elaborated terms to re-derive what resolution decided.
