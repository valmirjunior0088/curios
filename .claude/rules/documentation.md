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
| `documentation/design/**` | One cross-cutting design decision per file; a decision scoped to one crate is its crate `README.md`'s |
| `documentation/soundness/**` | One perimeter rule per file with its evidence; the claim, the grade vocabulary and the perimeter's boundaries are `documentation/design/language/the-soundness-perimeter.md`'s |
| Crate `README.md` files | The crate's mission and its crate-scoped design decisions |
| Crate and module rustdoc | Local architecture, algorithms, invariants, and public APIs |
| `Cargo.toml` descriptions | One-line crate purposes for Cargo tooling |
| `programs/README.md` | The measurement corpus: the layout rule, the instrument families, and the cross-language workloads |
| `benchmarks/README.md` | Benchmark harness mechanics, results, and the caveats that belong beside a number |

## Decisions and perimeter entries

A design decision states what was **decided**, the **rationale**, and what was **rejected** — the alternatives that were considered and lost, so a later reader can tell a settled question from an unasked one. A perimeter entry states what it **assumes** and its **status**, and names the fixtures that are its evidence. Neither directory has an index: listing it is how an entry is found, because an index maintained by hand goes stale silently and a directory cannot, and each filename spells its heading out rather than abbreviating it. Cite an entry by its path, so a moved or renamed one fails loudly instead of leaving a quoted title that no longer exists. A sentence that turns out to be wrong is corrected in place, never annotated as amended; git holds what it used to say.
