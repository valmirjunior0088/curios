---
paths:
  - "curios-ersd/**"
  - "curios-cont/**"
  - "curios-emit/**"
  - "curios-wasm/**"
---

# The back end

- A lowering belongs to the crate holding its source representation, or the stage crate built over it, and depends on the crate holding its destination.
- Nothing below Ersd is reached by `cargo xtask clippy`'s prelude walk: a change here is checked by the corpus tests that reach it, `curios-std`'s.
- The optimizer stays in `curios-cont`, since its passes rewrite the representation's private arenas.
- `curios-ersd`'s closed-term evaluator runs on a linked program and, an item at a time, where a unit is erased (`evaluate_since`): a change to it moves every stored unit's erased forms and the prelude's images, and reaches `curios-elab`'s erasure tests and `documentation/design/compilation/a-top-level-value-is-computed-where-its-unit-is-compiled.md`.
- `curios-emit` sits beside the prelude build, never under it.
- A numeric fast path agrees with every constant folder sharing `scalar` and with `curios-num`; `documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md` states the carrier.
