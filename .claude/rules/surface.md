---
paths:
  - "curios-text/**"
  - "curios-parse/**"
  - "curios-print/**"
---

# The surface: parsing, printing, lowering

- A change to the grammar, the syntax tree or printing also reaches `curios-text/src/into_core/`, the parser tests, `documentation/syntax.md`, and `editors/grammar/grammar.js` with its committed `src/`.
- The tree keeps sugar verbatim so a term prints back as written; sugar is undone only while lowering.
- A parser alternative commits once it has read the prefix that discriminates it, and asks for commitment explicitly rather than inferring it from consumption.
- `/sys`'s declarations restate `Intrinsic::signature`'s types; the prelude build checks them against the table.
