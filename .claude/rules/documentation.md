---
paths:
  - "documentation/**"
  - "**/README.md"
---

# Writing documentation

Each fact lives at the narrowest authoritative place and is linked from everywhere else; a second explanation drifts. One source line per paragraph or list item, never hardwrapped.

| Location | Owns |
| --- | --- |
| `README.md` | What Curios is, the happy path to running one, and where to go next |
| `documentation/usage.md` | The command line and packages: every subcommand, flag, manifest key, exit code and file |
| `documentation/syntax.md` | The surface language, complete |
| `documentation/roadmap.md` | What exists and what is pending, one line each |
| `documentation/roadmap/**` | The specification of a pending item |
| `documentation/design/**` | One cross-cutting decision per file, in a directory per subject, or at the root for the workspace as a whole; a decision scoped to one crate is its `README.md`'s |
| `xboard/src/board/` | The soundness board: one part of the judgment per file, with a ticket for every proof of `False` found in it |
| `documentation/design/soundness/*/**` | The argument for a rule that can admit a term and the tests that hold it; the claim and the boundaries are `documentation/design/soundness/the-soundness-board.md`'s |
| Crate `README.md` | The crate's mission and its crate-scoped decisions |
| Rustdoc | Local architecture, algorithms, invariants and API contracts |
| `Cargo.toml` `description` | The crate's purpose in one line |
| `CLAUDE.md`, `.claude/` | How an agent works here |
| `programs/README.md`, `xbench/README.md` | The measurement corpus; the benchmark bench and its results |

## Decisions and board entries

- A decision states what was **decided**, the **rationale**, and what was **rejected**, so a settled question is told from an unasked one. It states the intended rule; where the code falls short, the gap is a roadmap item, not a caveat.
- A board entry states what its rule **assumes** and why it holds, and names the tests that are its evidence. It carries no grade: what has been found is the tickets on the board.
- Neither has an index: a directory listing cannot go stale. A filename spells its heading out, and an entry is cited by its path, so a moved one fails loudly.
- A wrong sentence is corrected in place, never annotated as amended. Everything is written in the present tense.
- A measured figure is the latest result of a measurement a reader can retake — a named measurement test or a stated protocol — and stays only where it is the argument. A past reading, a replaced baseline and a rejected alternative's cost are dropped, here and in the comments beside a measurement alike; only `xbench/src/readings/` keeps its readings.
- A measured figure names the commit it was taken at, and a measurement states whether its figures are `counted`, `timed` or `mixed`. A figure that names neither cannot be told from one the code has moved past, and a reader cannot tell which figures need a quiet machine to retake — a `mixed` measurement needs one, and only its counted half transports to another machine. A timed figure is a median over repeated readings with their span, not one reading; and a measurement comparing variants to each other interleaves them, since no amount of repeating a sequence removes the position effect inside it.
- An incident is evidence, not history: state the failure mode in the present tense and keep the concrete case as one clause, without the figures it once measured.

## Roadmap

- When work lands, rewrite the item it extends; don't stack new ones beside it. Open an unchecked item only for a limitation named from the code — what is refused, and where.
- A measurement a spec depends on is written into the spec as a protocol precise enough to rebuild its scripts; the scripts are not checked in.

An absolute Curios path leads with its slash — `/std/Tui/Layout` — while a file path such as `curios-text/std/Tui.crs` keeps its form.
