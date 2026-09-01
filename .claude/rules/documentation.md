---
paths:
  - "documentation/**"
  - "**/README.md"
---

# Documentation ownership

Document each fact at the narrowest authoritative level and link to it elsewhere; do not maintain parallel explanations that can drift. Do not hardwrap Markdown prose: one source line per paragraph or list item.

| Location | Owns |
| --- | --- |
| `README.md` | Public introduction: what Curios is, the happy path to running one, and where to go next |
| `documentation/usage.md` | Complete command-line and package reference — every subcommand, exit codes, dependencies, umbrellas, and the global flags |
| `CLAUDE.md` | Contributor behavior, ownership boundaries, durable invariants, and validation |
| `.claude/rules/` | Conventions scoped to one kind of file, loaded when such a file is read |
| `documentation/syntax.md` | Complete Curios surface-language reference |
| `documentation/roadmap.md` | Implemented capabilities and pending specifications |
| `documentation/design.md` and `documentation/design/**` | The objectives, and one cross-cutting design decision per file, cited by path |
| `documentation/soundness.md` and `documentation/soundness/**` | The perimeter, how to read a grade, and one perimeter rule per file with its evidence |
| Crate `README.md` files | The crate's mission and its crate-scoped design decisions |
| Crate and module rustdoc | Local architecture, algorithms, invariants, and public APIs |
| `Cargo.toml` descriptions | One-line crate purposes for Cargo tooling |
| `programs/README.md` | The measurement corpus: the layout rule, the instrument families, and the cross-language workloads |
| `benchmarks/README.md` | Benchmark harness mechanics, results, and the caveats that belong beside a number |
