---
paths:
  - "curios-surface/**"
  - "curios-module/**"
  - "curios-parse/**"
  - "curios-print/**"
---

# The surface: parsing, printing, lowering

- A change to the grammar, the syntax tree or printing also reaches `curios-module/src/into_core/`, `curios-surface/src/from_core.rs`, which builds the tree a report prints through, the parser tests, `documentation/syntax.md`, and `editors/grammar/base/grammar.js` with its committed `src/` — a Core text form also `editors/grammar/core/grammar.js` with its own, the Zed and TextMate grammars of `.crsc`, and their samples.
- The tree keeps sugar verbatim so a term prints back as written; sugar is undone only while lowering.
- A parser alternative commits once it has read the prefix that discriminates it, and asks for commitment explicitly rather than inferring it from consumption.
- The surface `Intrinsic` is core's enum over the surface term, and what the parser writes down is a `Literal`.
