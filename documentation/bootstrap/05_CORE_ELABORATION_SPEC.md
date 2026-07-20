# Bootstrap Phase 4 — Core, elaboration, and erasure in shadow mode

Working implementation specification for the fourth bootstrap implementation phase. It follows the [shadow-mode surface frontend](04_SURFACE_FRONTEND_SPEC.md) and consumes the contracts in [01_CONTRACTS_SPEC.md](01_CONTRACTS_SPEC.md), in particular the shared-term substrate requirements and the prelude strategy.

Port the Core term representation, contexts, registries, reduction, conversion, unification, inference, checking, inductive inversion, coverage, refinements, witnesses, privacy, zonking, erasure, and all supporting diagnostics.

Implementation should follow semantic dependency rather than Rust file order:

1. Core data, binders, substitution, scopes, and iterative traversals.
2. Context definitions, registries, term caches, and reduction.
3. Conversion and metavariable solving.
4. Typing and elaboration of primitive and structural forms.
5. Inductives, pattern refinement, coverage, visibility, concepts, and witnesses.
6. Whole-module elaboration, zonking, prelude restoration, and erasure.

Every completed family runs in shadow mode against focused Rust unit fixtures and the cross-stage program corpus. Successful comparisons use normalized semantic structures or downstream behavior rather than allocation identities. Negative comparisons require the same acceptance boundary, diagnostic category, and principal source span; exact prose may converge later.

The initial self-hosted frontend may rebuild the prelude from source. Correctness and the ownership cutover come before an on-disk prelude cache, but measurements from this phase determine how soon the Curios-native cache becomes necessary for usable iteration times.
