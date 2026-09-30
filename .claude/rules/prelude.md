---
paths:
  - "curios-prelude-archive/**"
  - "curios-prelude/**"
  - "curios-text/src/sys_module/**"
---

# The prelude: `/sys` and `/std`

- `/sys` and `/std` are two units in that order, each compiled by `curios-prelude-archive`'s build into an `Uncertified` image under its `OUT_DIR`; `curios-prelude`'s build certifies them and files the certifier's record. Nothing enumerates `/std`'s modules but the `mod` lines of their parents' headers, and `the_std_record_names_every_authored_source_and_no_other` refuses a `.crs` file no header declares.
- There is no source fallback: an archive that fails to build or restore is a compiler invariant and fails loudly.
- A package named `std` is the standard library: `curios-pipeline`'s `standard` module withholds the archived `/std` for it when it is the fold's first unit and hands the archive over as its baseline. Any other unit claiming a prelude prefix collides and is refused.
- `cargo tree -p curios-prelude-archive --edges build` contains neither `curios-emit` nor `curios-wasm`: a build script reaching the emitter would re-elaborate the library on every emitter edit.
- A name the compiler emits is filled into the `SyntaxRegistry` from `curios-prelude-archive/src/syntax.rs`; fill it there only when Rust emits it.
