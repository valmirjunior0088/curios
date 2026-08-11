# A dataflow substrate for `curios-cont`, and unboxed scalars as its first payoff

This document specifies extracting the dataflow machinery `curios-cont` already contains — one hand-rolled lattice welded to the specializer — into a substrate the whole optimizer shares, then proving it with clients that use it.

Three of its four milestones have landed. What remains is M4; everything above it is recorded here only as much as M4 needs, and the durable contracts of what shipped belong to `curios-cont`'s module documentation rather than to this file.

## Problem

Every value in an emitted module is a reference, including small integers. That is what makes the interfaces compose — one closure type per arity, one field shape per constructor — but it is paid for at every value, including inside a loop where nothing is interoperating with anything.

The optimizer could not reason about any of it, and that was the deeper problem. Its passes are syntactic rewrites over facts each recomputes per round; with no shared notion of what is known at a program point, every new analysis re-implements a lattice, a worklist, and recursion handling from scratch.

## Design

**The substrate.** A `Lattice` trait — bottom, join, and the laws the solver depends on — plus a solver that iterates a client's constraint system to its least fixpoint, in `curios-cont/src/cps/dataflow.rs`.

Facts are keyed by value identity alone. An earlier draft listed value, function parameter, and continuation parameter as three key spaces; they are one, because a function's and a continuation's parameters *are* values. In CPS the keying is nearly free for the same reason the substrate stays small: a continuation's parameters are the join points and the term nesting is the dominator tree, so the structure a dataflow framework normally computes is already syntactic. Path-sensitive refinement — knowing a scrutinee's tag inside a `Switch` arm — is deliberately out of scope; it is a per-program-point extension the keying can grow into, and no client below needs it.

Two properties the substrate must preserve, both discovered by implementing against the existing analysis and both silent if broken:

- **Absence is not bottom.** An unseeded value is an unobservable runtime value and must force the top of the lattice, which is distinct from a seeded value still sitting at bottom. The solver therefore hands out its fact map rather than a total lookup that would answer bottom for both.
- **A client's observation step is not the lattice join.** Merging an unobservable caller must force top, while the join treats bottom as the identity. They disagree on exactly that pair, and only the join is the lattice operation.

**What the substrate does not do.** Hoisting the call-graph analysis out of the per-round rebuild was specified here and is unsound: every pass takes the module mutably, and inlining rewrites the call graph outright, so an analysis built at the top of a round is stale before the next pass reads it. Sharing those rebuilds needs an invalidation scheme — a pass declaring whether it disturbed the call graph — which is real work and is named in the successor rather than smuggled in here. With that hoist gone, **M1 bought no measured performance at all**; its return is that every analysis after it stops carrying a private lattice, and the byte-identical criterion is what proved it cost nothing.

**Profiling that stage redirected the successor.** The call-graph analysis is a minority of the optimizer's time, while the free-value and available-value walks cost a comparable amount across far more calls — and those are recomputed *within* a single pass, where nothing mutates between calls, not across the rounds an invalidation scheme would guard. They needed no scheme at all, only the observation that available values are the union of owned and used, which collapses three walks of a function body to one. Set identity, so no caller can observe a difference, and no staleness risk whatsoever. What remains for the successor is the call-graph analysis alone — the one of the three that genuinely does need a pass to declare what it disturbed, and now the smallest.

**Representation analysis, and the rule that a use can only take what the definition can give.** Demands alone decide nothing safely: they would raise a function parameter the moment its body did arithmetic on it, and a function parameter arrives through a uniformly-referenced signature with no store site the analysis controls. An `Offer` states the other half, and three of its four withdrawals were found the hard way rather than designed:

- **Function parameters, and the parameters of any continuation a call, host import or cell operation returns to.** Each receives a value the emitter hands over as a reference.
- **Values free in some function's body.** This is the one that shipped broken. Lambda-lifting adds a free value to a directly-called function as an *extra* reference parameter the CPS never spells, so deciding such a value from the scope that binds it sent a reference into integer arithmetic and miscompiled. The withdrawal reads the same free-value set the lifter reads, so it is exact rather than merely conservative. **This is also where the locals-only scope is enforced rather than merely intended:** a lifted free variable is precisely not a local.
- **Recursive shells**, which are allocated empty and filled afterwards.

Offers are collected first and withdrawals applied over them at the end, so the answer does not depend on traversal order — a value can both bind a scalar an intrinsic produced *and* escape into another function, and must settle on the withdrawal whichever the walk reached first.

**The carrier is mostly not in question; the storage always is.** A value's carrier is fixed by its producer — except for a continuation parameter, which has no producer, and whose carrier is knowable only from the demands its uses impose. So the lattice must *name* the carrier rather than carry one bit, and two uses of a continuation parameter can genuinely disagree. Storage is three points, boxed below raw-at-a-carrier below conflict, and the top exists because answering a disagreement with *boxed* would move a fact back *down* the order, which a solver terminating on nothing having changed would not converge under.

**Coercion at the edge, not a join to the bottom.** The draft specified that a value may be raw when every use accepts its carrier, and implementing against a real loop kernel showed that rule misses the headline: a loop-carried value used by arithmetic *and* jumped out on a return edge stays boxed under a plain join, which is exactly the value worth unboxing. So a value is held **raw whenever any use demands the raw carrier**, and every disagreeing use gets an explicit coercion.

That rule is justified by instruction asymmetry rather than by a loop heuristic, which is what makes it safe to apply without loop structure: coercing raw to boxed is one instruction, while boxed to raw is a cast plus an unbox — two, one of them a runtime type check. Preferring raw therefore trades a cheaper coercion for a dearer one at every site where they differ. It also **retires the forward agreement pass**: with edges coercing, a block parameter picks its representation from its own uses and each incoming jump coerces to it, so no two edges need to agree in the first place.

Scope is **locals only**. Nothing crosses a function boundary, so the closure type families, struct fields, and the host ABI are untouched. That restriction is what keeps this client small; lifting it is the successor's subject.

## Milestones

- **M1 — the substrate, with the fixpoint re-hosted. _Landed._** The `Lattice` trait, the solver, and the specializer's fixpoint moved onto them. The specializer's remaining steps stay client code and were never candidates: only one of them is a fixpoint, the others being a syntactic scan, a non-iterated per-call-site join, a record step, and a chain collapse. *Acceptance, met: the erased and continuation stage dumps byte-identical over the benchmark corpus.* Landed as its own commit with no user-visible payoff, deliberately.
- **M2 — the demand table. _Landed._** The per-operand representation demand lifted from the emitter onto the intrinsic roster as `Repr`, read by the emitter rather than restated at each site. Both accessors are exhaustive matches over every variant, so a new intrinsic cannot be added without declaring its representation — the hazard `curios-elab`'s non-exhaustive sort table already demonstrates, where a new former silently types at the bottom level and only the prelude recheck notices. The old wrapping vocabulary is deleted rather than extended, which turns "this result needs no boxing" from a missing case into a stated one. *Acceptance, met: emitted Wasm byte-identical, whole corpus green.*
  - **What that acceptance does not cover.** Byte-identity validates the entries the corpus reaches; the rest are validated only by exhaustiveness, which establishes that every variant was answered, not that any answer is right. The two entries to re-examine first are the float/bytes reinterpretation pair, which read and produce the carrier every sibling does not — the exceptions a prefix-shaped implementation would break silently.
- **M3 — representation analysis and unboxed locals. _Landed._** The analysis, coercion insertion at disagreeing uses, and computed local types, in `curios-cont/src/cps/represent.rs`. On the integer benchmark kernel the null-check instruction disappears entirely, because an edge argument is now loaded at the carrier its target parameter is held in, so a register-to-register back edge is a bare local read — the loop-carried decision the fixpoint exists to produce.

  **The invariant is checked on the emitted bytes, not in the CPS verifier — _changed from the plan._** A verifier rule can only check a CPS-level proxy for what the emitter will do; wasmtime validates what the emitter actually did. `curios::compile::validate` runs it before Binaryen is handed the module, which also fixes a diagnostic problem this milestone paid for in full: Binaryen answers a malformed module with a C++ assertion, aborting the process, so a test run reports *no* failing test name and loses every other result with it. It is the same validator that compiles the module a few lines later, so it costs one linear pass and names the function and offset.

  **Measured at runtime, which no acceptance criterion here asked for and should have.** Instruction counts are a proxy. The benchmark programs were run on one machine and engine with M3's decision suppressed in the control, so the two arms differ in nothing else: the integer kernel improved by a wide margin with no sample overlap between the arms, and the allocation benchmark did not move at all. That second half is what makes the first a representation result rather than a coincidence — allocation-bound work is untouched, which is what the locals-only scope predicts and what the constructor-payload fixture pins structurally.

  *Acceptance, met: whole corpus green. Not met, and withdrawn as unmeetable: "no 64-bit widening" — the widening is not there because the value is boxed, but because a natural product leaving the envelope must trap and 32-bit multiplication wraps rather than trapping, so detecting it needs more than 32 bits of product however the operands are held. What removes it is a bound on an operand, which is a constant-propagation consequence rather than a representation one.*
- **M4 — demand analysis.** A backward analysis of which parameters and results are actually consumed, subsuming `eliminate_dead_parameters`, and the prerequisite for the successor's constructed-product work.

## Non-goals

- Unboxing across function boundaries, in struct fields, in cells, in list elements, or in globals. Each requires two parties to agree on a representation, which is where layout enters a *signature* and becomes a type rather than a decision.
- Constructed-product results, Wasm multivalue returns, and multi-return continuations. They share this substrate and the worker/wrapper mechanism, and are named in the successor below.
- Path-sensitive facts, as above.
- Any change to what Core, Ersd, or the kernel decide. This is a backend representation question throughout.

## Rejected

- **Layout types in the CPS IR, or in `curios-ersd`.** Representation is a property of the backend boundary, not of the semantic IR; `curios-ersd` already states that all local and structural optimization belongs to Cont. Ersd additionally runs before CPS has decided what is a block parameter versus a call argument, which is exactly the distinction that separates this specification from its successor.
- **A peephole collapsing box/unbox pairs during emission.** It treats the symptom at the last stage, informs no earlier decision, and leaves the demand restated at every emitter site — where the next operand position added recreates the problem.
- **Widening the emission IR's operand positions to a name-or-literal sum, alone.** It is the correct shape and it is subsumed: once bindings carry a representation, a literal operand is materialised as an immediate for the same reason every other raw value is. Doing it first would be a second, partial mechanism for one case of the general one.
- **Moving to a direct-style IR with explicit join points**, as GHC did in *Compiling without Continuations* (PLDI 2017). CPS already supplies join points and dominance syntactically, which is what that work had to add; *Compiling with continuations, or without? whatever* (ICFP 2019) then showed the two inter-translatable. There is nothing to buy.
- **Waiting on Binaryen measurements.** The browser path runs no Binaryen at all, so its benefit is unconditional, and [09_WASM_OPTM_STAGE_SPEC.md](09_WASM_OPTM_STAGE_SPEC.md) is not a cheap prerequisite.

## Tests

- M4: the existing optimizer tests unchanged where its rewrites coincide with `eliminate_dead_parameters`, and the emitted module byte-identical for the subsuming step — the same criterion M1 and M2 were held to, and the reason a substrate change is attributable to one commit.
- Throughout: the emitted module validated before Binaryen sees it, so a malformed rewrite fails loudly, by name, rather than aborting the test process or miscompiling. The gap this leaves is `curios-web`, which links neither Binaryen nor wasmtime and carries a malformed module to `WebAssembly.compile`; closing it means a validator in `curios-pipeline`, whose dependency would flow into the wasm32 bundle, and that is left open rather than decided as a side effect here.
- The lesson M3 paid for, which any new client should inherit: a value free in another function's body stays boxed. That miscompilation reached the corpus while every `curios-cont` test passed, because no fixture lowered a function over a free scalar. A unit test that fails against the previous rule is what makes such a fix a fix rather than a patch.

## Successor

Unboxing across boundaries, and the return-protocol family it shares a mechanism with. The design work is done and the open questions are named, so the successor specification begins from these rather than from a survey.

- Function-signature unboxing needs the closure type families re-keyed. The closure supertype is shared by every closure of an arity, so a specialized return or parameter shape means keying by arity *and* shape — the "return-protocol split", and the reason multi-return is naturally restricted to known callees, since an indirect call learns the shape only from the type.
- Constructed-product results with Wasm multivalue, and multi-return continuations after Shivers & Fisher's *Multi-return Function Call* (JFP 2006), which encodes a match-returning call without allocating its sum.
- Four questions must be answered before that document claims a verified constraint: how the machine IR's return terminator lowers and how call sites consume results; whether `curios-wasm` models multi-value *function* types at all, independently of Binaryen accepting the feature; whether re-keying closure types disturbs the shapes `curios-runtime` and the `curios-web` bridge mirror structurally; and how the optimizer classifies effects, which bounds any reordering the substrate enables.

## Retirement criteria

- Before this specification is deleted: the substrate's contract — the lattice laws, the solver's direction and recursion handling, and the per-value keying with its stated limit — is recorded in `curios-cont`'s module documentation; the representation demand table is documented on the intrinsic roster it lives on; the locals-only scope and what lifts it are recorded in `curios-cont/README.md`; the roadmap subitems are checked unlinked summaries; the successor specification exists with its four questions answered or restated; and no reference to this filename remains.
