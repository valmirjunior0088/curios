# curios-analysis

The rules both Curios checkers run, and the seam they run behind: index inversion and the singleton determination walk, strict positivity, size-change totality, universe satisfiability, and the erasure-shape queries the obligations are stated over. `curios-core` owns what a term *is* and `curios-cert` what one *means*; this crate owns the judgments neither checker gets to write for itself. The kernel that sits above it is `curios-cert`, the elaborator beside it `curios-elab`, and both reach these rules through `Env`/`Judge` rather than through each other. The two-checker decision and its rationale are cross-cutting and stay in [An independent kernel re-checks what the elaborator accepts](../documentation/design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md); what each rule assumes and how far that has been checked belongs to [the soundness perimeter](../documentation/soundness); local architecture belongs to the crate rustdoc.

## Design

### These rules are shared rather than duplicated, and that costs the second opinion

**Decision.** Each analysis here is a total function of post-zonk terms and declarations. Both checkers call the same function, and neither writes its own copy.

**Rationale.** A second implementation of a pure function of the same input is a second *run*, not a second opinion. It would agree whenever the first was right and agree whenever it was wrong, so the disagreement the two-checker design is built to surface cannot arise — which makes duplication pure cost with no evidential return.

This is the one place the two-checker design buys nothing, and saying so is the point: a wrong rule *here* is a rule neither checker can catch the other getting wrong. [the soundness perimeter](../documentation/soundness) grades these entries on that understanding rather than on the strength the rest of the perimeter draws from being decided twice, and `tests/driven.rs` exists because the compensating evidence has to come from adversarial fixtures instead.

**Rejected.** Transcribing each rule into the kernel to obtain a second opinion. The transcription would inherit whatever the original gets wrong and agree for exactly that reason. Universe satisfiability is the deliberate exception and is written twice, from the constraint semantics rather than from the other copy — `satisfy.rs` records why it earns the duplication the rest do not.

### What a checker supplies for itself is `Env`

**Decision.** Reduction, unfolding, fresh binders, and the registry fallback for declarations outside the analyzed set arrive through the `Env` trait. `curios-elab` implements it over its elaboration `Context`, `curios-cert` over its `Kernel`.

**Rationale.** These are the four operations the two checkers genuinely do differently — one reduces with metavariables and a refinement layer in reach, the other with neither and against a budget it must not exceed. Everything above them is the same decision procedure, so the seam is drawn exactly where the implementations stop agreeing, and no rule in this crate has to know which checker is asking.

### This is a crate because a build script's rebuild granularity is its dependency set

**Decision.** The analyses were part of `curios-cert` and were split out. The trusted base is unchanged and is now two crates rather than one: these rules admit terms, so they are inside it, and `cargo tree -p curios-cert -e normal` still enumerates them one level further out.

**Rationale.** The split is about *rebuilds*, not about trust. Elaboration needs these analyses, so `curios-elab` depended on `curios-cert`, so every crate whose build script reaches elaboration reached the kernel too — and Cargo re-runs a build script whenever any of its dependencies change. The fixed prelude's image was therefore re-elaborated for every certifier edit: 469 s of a ~570 s build, spent re-deriving something no kernel rule can affect. After the split a kernel edit no longer invalidates elaboration.

`curios-elab` taking `curios-cert` as a **dev**-dependency is the other half of the same property, and `curios-unit` exists for the third: a dev-dependency does not propagate, and `curios-prelude-archive`'s build script constructs a `Unit` without reaching the kernel at all.

**Rejected.** Leaving the analyses in `curios-cert` and narrowing what the prelude's build script depends on instead. The dependency is real — the archive genuinely needs elaboration — so the only thing left to narrow was the kernel edge, which is what the split removes structurally rather than by arrangement.
