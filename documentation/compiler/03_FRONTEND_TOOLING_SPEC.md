# Frontend tooling

Not refined yet. This umbrella placeholder reserves the specification location for the roadmap items below until each is refined into a working implementation specification.

## Covered roadmap items

- Code formatter
- Linter
- Language server (hover, go-to-definition, highlighting)
- Documentation generator

## Known constraints

- All four tools consume the Curios-owned parser and source index after the bootstrap frontend cutover rather than a second parsing stack ([bootstrap contracts](../bootstrap/01_CONTRACTS_SPEC.md)).
- Semantic queries flow through the `wonder` analysis model and indexes ([program analysis sequence](../program_analysis/02_WONDER_SPEC.md)) rather than a parallel compiler-facing API.
