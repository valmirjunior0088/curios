# curios-cert

The Curios certifier: the kernel deciding, from a finished term alone, whether the elaborator's output is well-typed — reduction, sort, conversion, the typing judgment, nominal elimination, subsumption, the whole-module walk that applies all of it, the erasure obligations, and the level entailment oracle, which is this kernel's alone rather than shared. The rules *both* checkers run — index inversion and the singleton determination walk, strict positivity, size-change totality — live one crate down in `curios-analysis`, behind the `Env`/`Judge` seam; `curios-core` owns what a term *is*; this crate owns what one *means*. The two-checker decision and its rationale are cross-cutting and stay in [An independent kernel re-checks what the elaborator accepts](../documentation/design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md); what the kernel covers at any moment belongs to [roadmap.md](../documentation/roadmap.md); local architecture belongs to the crate rustdoc.

## Design

### The trusted base is a crate boundary

**Decision.** The trusted base is this crate's dependency closure, checkable with `cargo tree -p curios-cert -e normal`, rather than the call-closure of the checking entry points inside a larger crate. It reaches `curios-analysis`, `curios-utilities`, `curios-num`, `curios-core`, `curios-abi`, and nothing of the elaborator; the dependency never reverses, so the kernel cannot consult a metavariable store, a refinement layer, or a cached elaboration, and sharing `curios-core` is sharing the representation, never a judgment. In the other direction `curios-elab` takes this crate as a **dev**-dependency only, which does not propagate — the property `curios-prelude-archive`'s build script rests on, since a build script reaching the kernel re-elaborates the whole standard library on every certifier edit.

**Rationale.** A call-closure is enumerated by tracing and drifts silently as code moves; a crate boundary is enforced by the compiler and read off the manifest. It also makes "not trusted" structural: builders, printers, and elaboration conveniences physically cannot sit inside the base, where before they had to be kept out by inventory. This supersedes the earlier rejection of a third crate, amended in design.md where it was recorded: the rejection predated the kernel outgrowing its host — the shared analyses, the `Env`/`Judge` seam, and the evaluation memos made the base a substantial, nameable thing whose boundary deserved enforcement.

**Rejected.** Sharing the representation through `curios-utilities`, which would put the term language in a crate whose purpose is stage-independent utilities — `curios-core` remains the representation's owner and this crate builds on it.

### The judgments flatten onto the root

**Decision.** The crate is a flat module space: `curios_cert::Kernel`, `curios_cert::convert`, `curios_cert::check_definition`. In `curios-core` the kernel kept a `kernel::` namespace because its judgments name the same things the elaborator names its own; here the crate name is the disambiguator, and `curios_cert::convert` against the elaborator's bare `convert` reads exactly as the second opinion it is.

### Incompleteness is the safe direction

**Decision.** A rule that refuses too much produces a disagreement between the two checkers, which is a signal; a rule that accepts too much is silent. Every judgment in this crate is written to that asymmetry: where a check cannot yet decide something, it refuses rather than guesses.

**Rationale.** The two-checker split only catches a systematic mistake if a wrong rule shows up as a disagreement. An overly permissive rule hides in the corpus passing, exactly like the single-checker baseline this crate exists to improve on; an overly strict one is visible the moment a real program hits it.

### The kernel memoizes its own evaluation, transparently

**Decision.** The kernel carries evaluation memos — a per-definition unfold memo and weak-head memos for local-free terms — following the precedent of Lean's trusted `type_checker`. Every entry records what its computation consumed (budget steps and minted binder identities), and a hit charges exactly both, so the whole observable trajectory is bit-identical with the memos on or off; `kernel_memo_parity` holds that to account, and `Kernel::uncached` exists so it can.

**Rationale.** A metavariable heap or refinement store *injects* answers a term alone could not produce; a memo replays the kernel's own pure function of `(term, definitions)`, computed once. The measured cost of refusing even that was a 10× whole-prelude re-check.

### A type is accepted by typing it, and reduction is total so that it can be

**Decision.** Three mechanisms, in this order, each doing a job the others structurally cannot.

**Reduction is total on arbitrary terms.** `whnf` never asserts on a shape a caller could hand it — an application that does not saturate its lambda, an elimination arm that does not match its payload, a projection out of range all go *stuck* rather than aborting. This is not defence in depth: `recheck_module_verdicts` takes a `Module` from anywhere and is documented as walking to the end with each verdict independent of the others, so an abort takes every other verdict with it, and reduction that declines to fire can never admit anything.

**A type is accepted by `infer_type`, not by `Sort::of`.** The kernel used to have two ways to accept a type, and only one of them was a judgment. `Sort::of` classifies a term structurally without typing it, so a declared type reached reduction, conversion and erasure having been *read* rather than *checked* — the root of the motive clause, the β step, the elimination arm and its recursive twin. `infer_type` reduces, types, and destructs the result as a sort, which is Coq's `infer_type`/`type_of_case`; Lean enters through `inferType`; Agda carries the sort on the type so a type in hand is one that was checked. `Sort::of` survives as the fast path with a precondition, the role Coq gives `Retyping.get_sort_of`, and is reached only where typing has already run.

The reduction inside `infer_type` is what makes the two mechanisms interlock: typing a declared type must reduce it first, so reduction meets untyped terms *by construction*, which is why totality is its precondition rather than its complement. It is also load-bearing on its own — `List.{v,w}(Waker)` types as its former's promised `Type v` unreduced, and as the minimal `Type 0` the constructor size condition needs once reduced.

**Counts are checked at the boundary, because typing never sees them.** An occurrence's parameters and indices, a value's parameters, a constructor tag's uniqueness, a plicity vector's parallelism: no typing rule reads a length, so no ordering discipline will ever catch these. They are checked where the declaration is consulted, and removing them leaves a malformed occurrence *certified* rather than merely aborting — the permissive failure, not the loud one.

**Rationale.** Six defects arrived through the gap between "the kernel reads a field" and "something established the field". Two produced level capture and a bypassed large-elimination guard; four aborted the walk. Fixing them one guard at a time closed instances, never the class. The split above is what makes each class impossible rather than caught: shapes by typing, counts by the boundary, and neither able to abort.
