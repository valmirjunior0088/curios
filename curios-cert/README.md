# curios-cert

The Curios certifier: every rule that can admit a program, as one crate — the kernel deciding, from a finished term alone, whether the elaborator's output is well-typed, and the analyses both checkers share (index inversion and the singleton determination walk, strict positivity, size-change totality, the level entailment oracle). `curios-core` owns what a term *is*; this crate owns what one *means*. The two-checker decision and its rationale are cross-cutting and stay in [DESIGN.md](../documentation/DESIGN.md) ("An independent kernel re-checks what the elaborator accepts"); the migration's live status belongs to [the working specification](../documentation/compiler/00_TRUSTED_BASE_SPEC.md) and [ROADMAP.md](../documentation/ROADMAP.md); local architecture belongs to the crate rustdoc.

## Design

### The trusted base is a crate boundary

**Decision.** The trusted base is this crate's dependency closure, checkable with `cargo tree -p curios-cert`, rather than the call-closure of the checking entry points inside a larger crate. `curios-elab` depends on `curios-cert` and on `curios-core`, and neither dependency ever reverses, so the kernel cannot consult a metavariable store, a refinement layer, or a cached elaboration; sharing `curios-core` is sharing the representation, never a judgment.

**Rationale.** A call-closure is enumerated by tracing and drifts silently as code moves; a crate boundary is enforced by the compiler and read off the manifest. It also makes "not trusted" structural: builders, printers, and elaboration conveniences physically cannot sit inside the base, where before they had to be kept out by inventory. This supersedes the earlier rejection of a third crate, amended in DESIGN.md where it was recorded: the rejection predated the kernel outgrowing its host — the shared analyses, the `Env`/`Judge` seam, and the evaluation memos made the base a substantial, nameable thing whose boundary deserved enforcement.

**Rejected.** Sharing the representation through `curios-base`, which would put the term language in a crate whose purpose is stage-independent utilities — `curios-core` remains the representation's owner and this crate builds on it.

### The judgments flatten onto the root

**Decision.** The crate is a flat module space: `curios_cert::Kernel`, `curios_cert::convert`, `curios_cert::check_definition`. In `curios-core` the kernel kept a `kernel::` namespace because its judgments name the same things the elaborator names its own; here the crate name is the disambiguator, and `curios_cert::convert` against the elaborator's bare `convert` reads exactly as the second opinion it is.

### The kernel memoizes its own evaluation, transparently

**Decision.** The kernel carries evaluation memos — a per-definition unfold memo and weak-head memos for local-free terms — following the precedent of Lean's trusted `type_checker`. Every entry records what its computation consumed (budget steps and minted binder identities), and a hit charges exactly both, so the whole observable trajectory is bit-identical with the memos on or off; `kernel_memo_parity` holds that to account, and `Kernel::uncached` exists so it can.

**Rationale.** A metavariable heap or refinement store *injects* answers a term alone could not produce; a memo replays the kernel's own pure function of `(term, definitions)`, computed once. The measured cost of refusing even that was a 10× whole-prelude re-check.
