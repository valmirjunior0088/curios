---
paths:
  - "**/*.crs"
---

# Writing Curios

- Read [syntax.md](../../documentation/syntax.md) in full before editing any `.crs` file; `curios-text/src/parse.rs` implements the contract.
- The surface grammar's syntax forms are closed: a new type never gets its own operator or keyword. It opts into an existing form by writing a `satisfy` witness against the form's `/syn` concept. See `documentation/design/language/syntax-forms-are-closed-semantics-extend-by-witness.md`.
- Use `curios-prelude-archive/std/` as the reference for idiomatic code, and as where a standard-library signature is read.
- Register a new `curios-prelude-archive/std/Foo.crs` in `curios-prelude-archive/std.crs`, and likewise under `syn/`; update `curios-prelude-archive/src/syntax.rs` only when Rust directly emits the new `/syn` name.
- Names use `/` qualification, `{}` is the unit type, `()` the unit value, and visibility of a nominal name is independent from visibility of its representation. `syntax.md` has the full rules.
- Probe a program on standard input and read the compiler's answer through `wonder` before asserting what it does.
