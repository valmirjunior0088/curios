# curios-lint

The binder lints of a unit, decided from the facts two stages recorded of it — the zeros of a written binder the lowering records ([`curios-module`](../curios-module/README.md)), less every written binder a proof the elaborator wrote read, with what elaboration says each written site binds ([`curios-elab`](../curios-elab/README.md)) — and worded for their reader beside the lints the lowering decided itself, `unused-import` and `unused-declaration`, every one in reading order, with a package's `unused-dependency` decided from the mounts its units' references were written under. The vocabulary is `curios-core`'s (`Lint`). What a lint is, and which ones there are, is [A lint is an exact finding read off the compilation](../documentation/design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md); what the API offers belongs to the crate rustdoc.

## Design

### Each lint is decided where its last fact is known

**Decision.** The lowering words the lints resolution alone decides, `unused-import` and `unused-declaration`, and records each zero of a written binder; elaboration records each written binder a proof it wrote read, which no written name reaches, and what each written site binds, `{}`, an action or neither. This crate decides the binder lints from both and gathers every lint of a unit in reading order. It sits over `curios-module` and `curios-elab`, whose records it reads, and under `curios-unit`, which asks it for a unit's lints where the unit is made and stores them with it.

**Rationale.** A binder lint joins facts two stages know halves of: what a written name reads, which only resolution knows, and what a proof the elaborator writes reads, which only elaboration knows. Deciding once, where the unit is made, makes a lint the same however its unit is reached — compiled now, restored from a store, compiled over a baseline or read from the prelude's archive — and asking again decides nothing.

**Rejected.** Deciding where the lints are read, which reads a restored unit's baseline half to report it; and a lint walking elaborated terms to re-derive what resolution decided.
