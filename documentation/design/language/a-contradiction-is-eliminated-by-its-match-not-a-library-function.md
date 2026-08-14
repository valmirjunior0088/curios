# A contradiction is eliminated by its match, not a library function

**Decision.** The zero-arm match is the language's one way to discharge a proof of an empty type: `match contradiction end` in checking position, `match contradiction : (_) => A end` where the result must be spelled. The library exports no `absurd`; `/std`'s ~50 former call sites are spelled as zero-arm matches.

**Rationale.** The match is the primitive `absurd` merely wrapped, and it is strictly more capable: it eliminates any type that *reduces* to an empty inductive under the arm's refinements — the `peel_byte` shape, where the proof's type becomes `False` only inside the arm — which the function cannot accept without an explicitly supplied, refinement-dependent implicit that resolution deliberately refuses to synthesize. The deleted `/syn/False/absurd` also violated the `/syn` law: the compiler never emitted the name, and `/syn` membership means exactly that Rust lowering does.

**Rejected.** Keeping `absurd` as a convenience alias (a second spelling of a primitive, contradicting the no-helpers-restating-primitives discipline); validating metavariable solutions under their birth refinements so `absurd`'s implicit could infer at refinement-dependent sites (sound by the travel-hazard argument, but it unseals part of the conversion oracle's suppression package for one call site).
