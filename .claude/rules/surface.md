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
- A `/sys` roster entry states its name, its parameters' names and its node; its types are read off `Intrinsic::signature`, so an operation is typed in `curios-core` and nowhere in `curios-text`. The surface `Intrinsic` is core's enum over the surface term, and what the parser writes down is a `Literal`.
