# Algebra, part 2: broader reasoning and representations

**Not refined yet.** This document preserves the remaining algebra work and the approaches already discussed. It is not an implementation plan: the topics below have no agreed delivery order, complete algorithm contracts, or acceptance criteria. Refinement must establish those before implementation.

## Foundation and boundaries

[Part 1](algebra-pt1-spec.md) consolidates the compiler's existing behavior into one mathematical owner, with term adapters in Core and shared judgment orchestration in Analysis. Its contracts, migration stages, and verification belong there. Completing it does not deliver canonical refinement keys, new laws, arithmetic search, certificates, or a new sequence representation.

This work builds on that foundation. Algebraic result strength remains explicit; inversion cannot deduce sufficient conditions, and search cannot commit elaborator assignments through the shared mathematical interface. The elaborator chooses solutions and the certifier checks what reaches it. Shared implementation is not independent correctness evidence.

The broader direction keeps carrier algebra in conversion and reduction. [Certifier part 2](certifier-pt2-spec.md) records a rejection of replacing that algebra with casts, but the rationale it attributed to the former algebra specification is missing from the current record. Refinement must reconcile that recorded rejection and supply its rationale rather than reconstructing one as an established decision.

Nonlinear decision beyond a separately justified fragment, a carrier-indexed guest `Seq`, and new intrinsic carriers remain outside this scope. The topics here do not implicitly authorize them.

## Canonical forms and refinement keys

**Capability wanted.** Algebraically equivalent expressions should have suitable canonical forms in the chosen fragments, and refinement lookup should use keys that do not depend on incidental term spelling.

**Previously discussed.** A total atom order, complete normalization within each supported fragment, canonical key construction and caching, and retirement of the spelling probes the keys replace. Identity must remain separate from presentation order; hashes alone cannot establish equality.

**Still to refine.** The exact fragments, atom relations, placement of normalization, cache lifetime and invalidation, and the consequences for substitution, universes, proof irrelevance, sharing and cost. Canonical comparison views do not by themselves justify changing reduction's spelling. The existing spelling decisions remain in force until a measured replacement lands.

## Boolean and bitwise reasoning

**Capability wanted.** Reason beyond today's local identities and bounded truth-table agreement, including relationships among comparison atoms and additional natural bitwise laws.

**Previously discussed.** Reduced ordered decision diagrams for Boolean reasoning, algebraic normal form for natural bitwise operations, and replacing the truth table's fixed cap with a priced budget.

**Still to refine.** Which representations serve which operations, how arithmetic relationships enter the Boolean procedure, and which complexity limits preserve predictable work. These are candidate mechanisms, not selected implementations. Agreement, impossibility, and failure to decide need separate contracts; disagreement over independent opaque atoms is not automatically a realizable counterexample.

## Arithmetic reasoning and evidence

**Capability wanted.** Linear integer arithmetic over normalized literals and defined-operation constraints, broader polynomial and sign reasoning, and additional solving proposals beyond today's behavior.

**Previously discussed.** Arithmetic search outside the certifier's dependency closure, a certificate checker in Algebra, and evidence transported with the module and keyed by a canonical formula. A broader ring procedure would share the appropriate mathematics of `Nat` and `Int`, with a solver on the elaborator's side. The certifier specification proposes Farkas certificates for linear integer arithmetic.

**Still to refine.** The exact arithmetic fragment, the treatment of natural coefficients and integer signs, opaque atoms, disequality, divisibility, and defined operations; the certificate language and its adequacy for integer reasoning; evidence lookup, storage and validation; and the division between direct decisions and certificate-backed judgments. A proposed certificate family is not yet a complete integer decision procedure. New unification choices require their own proposal, rollback, scope and acceptance contracts.

Part 1 preserves today's demand-sensitive comparison schedule, including retries. That does not satisfy the certifier specification's broader goal of excluding search and first-answer chains from trusted code. Refinement must identify which existing procedures meet that requirement, which need replacement, and how the transition coordinates with the certifier's evidence and dependency stages.

## Additional operations and morphisms

**Capability wanted.** Extend declarations beyond the laws already implemented, so additional operations can reuse supported reasoning rather than each acquiring a separate special case.

**Previously discussed.** Promotion of `pow`, symbolic exponent laws and literal-base normalization, shifts expressed through the declared power operation, and additional morphism kinds. The [Nat](nat-laws-spec.md), [Int](int-laws-spec.md), and [Flt](flt-laws-spec.md) specifications own the requested carrier operations and laws; the [rational specification](rat-pt1-spec.md) identifies later consumers of the polynomial and power reasoning.

**Still to refine.** The declaration kinds needed for those laws, their domains and exceptions, and which facts belong to conversion versus ordinary lemmas. A row can reuse an implemented algorithm; merely declaring a larger structure does not supply an algorithm for every law of that structure. Floating-point declarations must retain the repository model's distinctions rather than inherit unrestricted ring laws.

## Internal sequence representation

**Capability wanted.** One internal sequence carrier and common operations, preserving `List`, `Bits`, and `Bytes` for the guest.

**Previously discussed.** `SeqCarrier` and `SeqKind`, with representation changes through erased and continuation IRs, emission, and stored archives. Part 1's shared word algebra deliberately works over today's representations.

**Still to refine.** The representation, carrier and element typing, packed storage, proof operands, intrinsic signatures, stage boundaries, archive compatibility, and performance. The proposed names reserve no API. Determine whether this representation work is needed by particular reasoning capabilities or can proceed independently; their presence in this document does not establish an ordering dependency.

## Bounds from hypotheses

**Capability wanted.** Discharge bounds using available hypotheses when direct reduction does not settle them.

**Previously discussed.** Elaboration-side entailment that constructs ordinary proof terms. Hypotheses and bound proofs do not become algebraic assumptions in conversion.

**Still to refine.** Which hypotheses and propositions are admitted, how entailment produces proofs using the available vocabulary, how it interacts with postponed goals and metavariables, and what budget and failure behavior it has. Separate this proof-producing path from arithmetic certificates used to justify conversion judgments.

## Evidence and theory audit

**Capability wanted.** A declaration-generated law grid, independent controls and evidence for the theory's admissibility, with claims restricted to the fragments actually implemented.

**Previously discussed.** Generating laws from declarations, testing each morphism direction, catching mutations to declaration kinds, and auditing the theory against Coq Modulo Theory's conditions. Stronger conversion needs evidence for symmetry, transitivity, substitution, and the freeness properties inversion uses.

**Still to refine.** The relevant metatheoretic conditions, exceptions for each carrier and operation, the independence of generated tests from the implementation, and what supports a completeness claim. A finite grid and agreement between consumers of one engine do not establish a general metatheorem. The carrier specifications retain their concrete laws and controls rather than duplicating them here.

## Dependencies and refinement

The numeric specifications depend on particular future capabilities: relational reasoning, polynomial reasoning, power and morphism declarations, or generated evidence. [Certifier part 1](certifier-pt1-spec.md) owns profiling, independent verdict records and call-site discovery. [Certifier part 2](certifier-pt2-spec.md) owns evidence transport, trusted-code requirements and their perimeter changes. Those dependencies are fulfilled only by the capability they name, not by the existence of the Algebra crate. Unrelated certifier work need not wait for this whole document.

Before turning a topic into implementation work, define its supported fragment and exclusions, resolve its algorithm and representation choices, identify its consumers and dependencies, specify logical and operational contracts, and set focused acceptance, refusal, cost and soundness evidence. Sequence deliveries only after those relationships are known. Newly discovered defects in today's behavior remain separately scoped findings.

This document can be refined incrementally or split further if independent deliveries emerge. It must continue to distinguish agreed boundaries from proposed mechanisms; preserving a brainstorm is not approving all its alternatives.

## Documentation and retirement

Part 1 retires independently. When it lands, replace this document's link to it with links to the foundation's permanent documentation and reassess the proposals against the delivered interfaces.

As refined work lands, put implemented contracts and local decisions in the owning crate's README and rustdoc, cross-cutting rationale and rejected alternatives in design decisions, and assumptions and evidence in the soundness perimeter. Keep pending questions here rather than using permanent documentation to describe capabilities that do not exist.

When all work retained in this specification is complete or explicitly rescoped, update dependent specifications and the roadmap, verify that no references to this filename remain, and delete it. Do not retain completed implementation history here.
