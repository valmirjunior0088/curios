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
| `documentation/language.md` | Curios, the language, complete as far as its syntax conveys it: each form's spelling and what the grammar refuses; typing, inference, coverage, totality, positivity and resolution are the design decisions', linked from it |
| `documentation/roadmap/**` | Pending work: an area's findings and specifications, each owning its status |
| `documentation/design/**` | One cross-cutting decision per file, in a directory per subject, or at the root for the workspace as a whole; a decision scoped to one crate is its `README.md`'s |
| `xboard/tickets/<part>/` | The soundness board: one part of the judgment per directory, its question in its `metadata.toml`, with a directory for every proof of `False` found in it, and `xboard/archive/<part>/` for the tickets whose construct left the language; the claim and its boundaries are `documentation/design/soundness/the-soundness-board.md`'s |
| Crate `README.md` | The crate's mission and its crate-scoped decisions |
| Rustdoc | Local architecture, algorithms, invariants and API contracts |
| `Cargo.toml` `description` | The crate's purpose in one line |
| `CLAUDE.md`, `.claude/` | How an agent works here |
| `programs/README.md`, `xbench/README.md` | The measurement corpus; the benchmark bench and its results |

## Decisions

- A decision states what was **decided**, the **rationale**, and what was **rejected**, so a settled question is told from an unasked one. It states the intended rule; where the code falls short, the gap is a roadmap item, not a caveat.
- A decision records no evidence: a flaw found is a ticket on the soundness board, an attack that held is a test where [The soundness board](../../documentation/design/soundness/the-soundness-board.md) places one, and neither is inventoried or graded in a document.
- Decisions have no index: a directory listing cannot go stale. A filename spells its heading out, and a decision is cited by its path, so a moved one fails loudly.
- A wrong sentence is corrected in place, never annotated as amended. Everything is written in the present tense.
- A measured figure is a reading of the bench's measurement that took it, cited to that measurement's record ([A measured figure is counted where it can be, and gated only where it repeats](../../documentation/design/a-measured-figure-is-counted-where-it-can-be-and-gated-only-where-it-repeats.md)), and stays only where it is the argument. A figure is one compiler's: a past reading, a replaced baseline, a comparison with the compiler a change replaced or a build that leaves it out, and a rejected alternative's cost are dropped, here and in the comments beside a measurement alike; only xbench's records, under `xbench/records/`, keep their readings, and a profile is read, never cited.
- A cited figure names the record it is read from, which names the commit its sitting was taken at and what it was taken under; a figure that names no record cannot be told from one the code has moved past. A measurement states its spread, whether a figure moves from run to run — a time and an instruction count move, a count the compiler or a pinned build decides repeats — so a reader can tell which records need a quiet machine to retake, and only what repeats transports to another machine. A figure that moves is read with its span over a record's groups, not one reading; and a measurement comparing variants to each other interleaves them, since no amount of repeating a sequence removes the position effect inside it.
- An incident is evidence, not history: state the failure mode in the present tense and keep the concrete case as one clause, without the figures it once measured.

## Roadmap

The filing, refinement and retirement rules are [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md)'s.

An absolute Curios path leads with its slash — `/std/Tui/View` — while a file path such as `curios-prelude-build/std/Tui.crs` keeps its form.

A command a document or a slash command tells the reader to run is spelled as its `cargo xtask` recipe wherever one exists, `cargo xtask compiler test --filterset 'test(~<name>)'` and never `cargo test -p …`; bare cargo stays only where no recipe covers the step.

## Retired words

A word the project renamed is not written again for what it named, in a document, a comment, a message or a command:

| Retired | Written | Kept for |
| --- | --- | --- |
| the surface language, for the language a `.crs` file is written in | Curios | `curios-surface`, the crate that holds its tree |
| the core language, for the representation both checkers judge | Curios/Core, written in prose and never in backticks, and Core where Curios/Core is already the subject; Core text for its text | `curios-core`, and `--stage core` |
| the erased IR, the erased first-order IR | Curios/ANF | `curios-anf`, `--stage anf`, and A-normal form as a form |
| the continuation IR, the continuation-passing IR, high-CPS | Curios/CPS | `curios-cps`, `--stage cps`, and continuation-passing style as a style |
| kernel, for `curios-cert`'s checker | the certifier | the operating system, and another system's checker — Lean's kernel, Rocq's kernel |
| soundness perimeter, the perimeter | the soundness board | nothing |
| cell, for what one stage makes of one declaration | facet | a write-once cell below erasure, `curios_anf::CellOperation` |
| full gate, hand-off gate, `cargo xtask gate` | the handoff, `cargo xtask handoff` | nothing |
| the store, for `.curios/` or what it keeps for reuse | the cabinet, or `.curios/` | the verb, and a unit's `store` node; the elaborator's, the universe solver's and the certifier's own stores; Wasmtime's `Store`, the ABI's `ForeignStore` and a store into memory |
| object, for a content-addressed piece of a stored unit | blob (`curios_common::Blob`) | a unit, or an executable's payloads, as the cabinet keeps it; a heap object, a trait object, a JavaScript or JSON object, and git's |
| slot, for where the cabinet files a unit or a payload | object (`objects/<compiler>/<address>/`), its address, or the file a record frames | a row's field position in the back end, a registry's slot, and a field's or an argument's place in a term |
| verdict, for the cabinet's family or a unit's file in it, a test's result, or the word a report's line closes on | `objects/`, and an object's `unit` file; a test's outcome (`Outcome`), and how it ended (`Ending`); a line's ending (`Tree::ending`) | a checker's judgment, and `curios-verdict` and its `Verdicts`, which keep one as it was filed |
