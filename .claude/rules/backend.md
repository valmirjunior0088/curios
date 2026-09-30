---
paths:
  - "curios-ersd/**"
  - "curios-cont/**"
  - "curios-emit/**"
  - "curios-wasm/**"
---

# The back end

- A lowering belongs to the crate holding its source representation, or the stage crate built over it, and depends on the crate holding its destination.
- Nothing below Ersd is reached by `cargo x clippy`'s prelude walk: a change here is checked by the `curios` corpus tests that reach it.
- The optimizer stays in `curios-cont`, since its passes rewrite the representation's private arenas.
- `curios-emit` sits beside the prelude build, never under it.
- A numeric fast path agrees with every constant folder sharing `scalar` and with `curios-num`; `documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md` states the carrier.
