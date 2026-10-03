---
paths:
  - "**/*.rs"
---

# Writing Rust in Curios

- Before changing a stage, read its crate's `README.md` and the `//!` of the modules you touch, and check the next representation or consumer: a parser change reaches printing and lowering, a Core change reaches erasure, an IR change reaches the next lowering and its tests.
- Every name the compiler emits is declared in `/sys` or `/std` and reached through a filled `SyntaxRegistry` slot; no stage spells a prelude name.
- A walk over data-shaped depth recurses inside `curios_utilities::recurse` and works on the default test-thread stack. Never set `RUST_MIN_STACK` to hide a regression.
- Layout: no `mod.rs`. `foo.rs` declares its `foo/` submodules and re-exports them with `mod x; pub use x::*;`, and every crate is one flat namespace — a crate name disambiguates, never a module path. The kept namespaces mark scaffolding: `pub mod test_support` in `curios-runtime`, `curios-ersd`, `curios-core`, `curios-utilities` and `curios-analysis`, each behind a `test-support` feature because another crate's tests read it. A module-local `foo/test_support.rs` is `pub(super)`.
- Import names, except at the lowering seams — `curios-text`→`curios-core`, `curios-elab`→`curios-ersd`, `curios-ersd`→`curios-cont` — where the downstream crate's names stay crate-qualified, and in `curios-emit`, which qualifies every `curios_cont` and `curios_wasm` name. `curios_utilities`, `curios_abi` and `curios_num` are never qualified. A name arriving from two crates stays qualified rather than aliased; a trait is imported by name.
- Tests live beside their implementation, never inline: `foo.rs` declares `#[cfg(test)] mod tests;` for `foo/tests.rs`. Past 25 tests a module splits by subject into `foo/<theme>_tests.rs`, each opening with a `//!` naming it, with shared fixtures in `foo/test_support.rs`. Programs that cross stages go in `curios/src/tests/`, codegen tests in `curios/src/tests/codegen/`.
- A test's name is a sentence stating what is true and repeats nothing its path says. A measurement names its instrument instead (`map_wall_spines_slope`). A proposition put to both checkers keeps one name in both.
- Production code carries nothing only a test reads — no field, return value or parameter that exists so a test can observe. A test drives the production pieces directly or uses `test_support`.
- Before a lint allow, a variant serving one consumer or a parameter past Clippy's limit, find the shape that removes the need. Don't raise, lower or configure a lint without a design decision.
- A value that only makes sense relative to another is computed from the source constant, imported by name, never restated.
- State that lives across requests says what bounds it and is keyed by the identity of what it caches, so a change replaces the entry rather than stranding it.
- Investigate cost with `curios-profile` — `profile!` spans carrying their inputs as fields, `sample!`, `note!`, and the fold — never `eprintln!` or a hand-rolled timer; when it falls short, extend it.
- Per-carrier helpers, fields and emitted functions are named type-first, operation-last: `bin_force`, `list_slice`.
- `//!` states a module's purpose and invariants, `///` an API's contract. One line per paragraph, never hardwrapped. A comment states a non-obvious why — an invariant, a rejected alternative, a trade-off — in the present tense, never what the code already says. An absolute Curios path leads with its slash: `/std/Str`.
