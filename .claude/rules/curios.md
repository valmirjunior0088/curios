---
paths:
  - "**/*.crs"
---

# Writing Curios

- Read `documentation/syntax.md` in full before writing or editing a `.crs` file. `curios-prelude-archive/std/` is the reference for idiom and for a standard-library signature.
- Syntax forms are closed: a type opts into an operator or `!` with a `satisfy` witness against the form's `/std` concept, never with syntax of its own. A syntax proposal leads with what is impossible today, not with a shorter spelling of what already works.
- A `/std` module joins the library only through a `mod` line in its parent's header — `lib.crs` for a top-level module, the parent module's own file for a submodule. `curios-prelude-archive/src/syntax.rs` changes only when Rust emits the new name.
- A module housing a type it re-exports is uppercase (`State`, `Flt/Env`); a namespace of functions is lowercase (`Json/encode`, `ops`). No cryptic abbreviations.
- A monad-shaped `/std` type is nominal — a struct over `State`, its own `Monad` witness, and a `Lift` edge from `State` — never an alias.
- An operation a standard names that an existing function already computes keeps the existing name, with the correspondence in its doc; no second spelling.
- An unused binder a signature needs is a bare `_` or `@_`, never `_name`.
- `{}` is the unit type and `()` the unit value; a name's visibility is independent of its representation's.
