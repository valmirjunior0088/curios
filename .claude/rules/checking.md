---
paths:
  - "curios-core/**"
  - "curios-num/**"
  - "curios-algebra/**"
  - "curios-analysis/**"
  - "curios-cert/**"
  - "curios-elab/**"
---

# Checking: the trusted base and the elaborator

- The trusted base is `curios-cert` and the layer both checkers share — `curios-core`, `curios-num`, `curios-algebra`, `curios-analysis`. A rule that can admit a term is designed in its decision — under `documentation/design/soundness/` or `documentation/design/theory/`, or in the README of the crate that implements it — which a change to it must still satisfy; a closed term it admits at `/std/Bool/False` is a ticket in that part of the soundness board, `xboard/src/board/`, filed open before its fix (`xboard/README.md`), and a misjudgment no `False` was built from is a finding.
- Elaboration, typing or conversion also reaches text lowering, erasure, diagnostics and the integration tests. A kernel judgment also reaches `curios-core`'s representation and `recheck.rs`.
- A shared analysis serves two drivers, `curios-cert`'s `Kernel` and `curios-elab`'s `Context`; change both, and `curios-analysis/tests/driven.rs`.
- The carriers' algebra — a law family, an operation's declaration, `curios-algebra` — also reaches `Intrinsic::algebra` and the `atoms` module, `curios-analysis`'s conversion chain, the generated law grid and its audit under `curios/src/tests/laws/`, and `documentation/design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md`. `curios-algebra` depends on `curios-num` alone and names no term; a search — a bound's proof, a certificate — stays on the elaborator's side and reads the views Core publishes.
- `Intrinsic::signature` is what an intrinsic demands and produces; both checkers walk it, and a new operation is typed by adding a row.
- A numeric carrier or its arithmetic also reaches every constant folder sharing `scalar` (`curios-core`, `curios-ersd`, `curios-cont`), `curios-emit`'s fast paths and its `big_emitter` and `flt_emitter`, and for `Flt` the Curios twin of `curios_num::Floating`'s rounding in `/std/Flt/exact`.
- Concepts and witness resolution also reach the surface declarations, the standard library's witnesses and `documentation/syntax.md`. A derivation (`curios-elab/src/derive.rs`) also reaches the `DerivationSyntax` roster in `curios-utilities` with its three fills, the concept's `/std` vocabulary, `curios/src/tests/derive.rs` and `curios-text/src/into_core/ordering_tests.rs`.
- `curios-elab` takes `curios-cert` as a dev-dependency only, so no build script that elaborates reaches the kernel.
