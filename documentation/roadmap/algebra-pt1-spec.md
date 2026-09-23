# Algebra, part 1: one owner for existing behavior

Working specification for consolidating today's intrinsic algebra into `curios-algebra`, integrating it with reduction and the two checkers, and removing the implementations it supersedes. This delivery establishes the ownership and interfaces that the later algebra can build on. Its mathematical scope is the behavior the compiler already implements.

The broader destination is recorded separately in [part 2](algebra-pt2-spec.md), which is deliberately not refined yet. This specification is independently implementable and retirable. A stage named by the [Nat](nat-laws-spec.md), [Int](int-laws-spec.md), [Flt](flt-laws-spec.md), or [certifier part 2](certifier-pt2-spec.md) specifications is fulfilled only when its stated capability exists; creating the crate does not fulfill those dependencies by itself. In particular, preserving today's comparison schedule does not establish the certifier specification's stronger restrictions on trusted code.

## What this builds on

- **The implementation.** The numeric algebra lives in `curios-core/src/nat.rs` and `int.rs`; spine comparison in `spine.rs`; sequence decomposition and normalization in `free_monoid.rs`; and symbolic folds, comparison facts, Boolean laws, and the truth table under `reduce::intrinsic`. These files combine mathematics with term reading, reduction demands, and reconstruction.
- **The consumers.** `curios-elab/src/convert/intrinsic.rs` and `curios-cert/src/kernel/convert/intrinsic.rs` repeat the comparison strategy. Their outer converters also ask the Boolean decision procedure about a connective opposite another kind of term. `curios-analysis/src/invert.rs` consumes a restricted set of peel results.
- **The evidence.** `curios/src/tests/laws.rs` records held laws and refusal controls through both checkers. Core's numeric, spine, truth-table, and intrinsic-law tests check individual operations and verdicts. The current soundness accounts are [Open fold laws and the sum normal form](../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md) and [Intrinsic fold laws and the free-monoid peel](../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md).
- **The ownership already established.** [Core](../../curios-core/README.md) owns terms and their representation; [Analysis](../../curios-analysis/README.md) owns shared judgments; the elaborator owns solving; the kernel rechecks its output. Shared algebra becomes part of the trusted implementation used by both checkers. Their agreement is therefore integration evidence, and independent algebra tests remain necessary.
- **The operational decisions.** [A sum is merged when it is forced, not when it is built](../design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md), [A comparison is spelled one way when it is stuck](../design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md), and [A law is decided where it neither respells nor invents](../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md) explain constraints the extraction must preserve.

## The gap

**Mathematics has no independent owner.** A coefficient, a word segment, or a Boolean atom is usually read directly from a `Term`. Arithmetic collection and cancellation are interleaved with decisions about term spelling. Word algorithms carry element types and proof operands. Moving these functions unchanged into another directory would leave their responsibilities joined.

**The comparison strategy is repeated.** Both converters ask for stuck-product normalization, Boolean agreement, connective normalization, comparison alignment, a chain of peels, and a retry with numeric atom arguments forced before reaching congruence. The elaborator adds solved-metavariable substitution and a packed-literal solving view. A change to the shared strategy currently requires corresponding edits in both drivers.

**A residual has more than one meaning.** `Peel::Continue` can express an equivalent residual equation or a sufficient condition for equality. The distinction is enforced by registration: product-factor peeling is included in conversion and excluded from inversion. The API does not express the restriction that prevents inversion from deducing `f = g` from `x * f = x * g` when `x` may be zero.

**Term spelling is operationally significant.** Sums preserve first appearance, products use structural-hash order, and a stuck Boolean connective can keep its right operand as written. Refinements use written spellings and existing probes. Numeric cancellation deliberately preserves original subterms when no summand cancels, because reconstruction can otherwise keep changing association and order. A new representation must account for these constraints explicitly.

## Scope

This delivery consolidates every existing symbolic rule in the following families, including rules reached only through a fold, inversion, or an elaboration-specific path. Stage 1 records the exact inventory and its callers against the implementation baseline.

| Family | Existing behavior to give one mathematical owner |
| --- | --- |
| `Nat` and `Int` combinations | Literal factors, coefficient collection, monomial matching, addition and multiplication, existing on-demand distribution, common-addend cancellation, negation and sign splitting, and sufficient product-factor obligations. |
| Numeric comparisons and defined operations | Existing order and nonnegativity facts, literal bounds, domination, coefficient-gcd disequality, quotient/remainder recombination, literal-divisor splitting, and comparisons through numeric preimages. |
| `Bool` | Local identities and complements, connective flattening and comparison, comparison duals and alignment, and agreement by the truth table with its existing eight-atom cap. |
| Natural bitwise operations and shifts | Existing zero, identity, idempotence, symmetry, and cancellation rules where implemented; the current literal and symbolic shift decompositions. |
| Words and positions | Existing prefix cancellation, literal-run and concatenation laws, empty-word facts, window fusion and seam handling, and equality of accesses through a common root and absolute position. |
| Morphisms and carrier conversions | Existing length and map laws, the algebraic part of fold equations, the natural-to-integer embedding and its preimage rules, bounded byte conversions, packed regrouping, and currently implemented floating-point conversion identities. |

Concrete scalar evaluation remains in `curios-num` and its callers. Core continues to execute eliminators and apply functions. The map-identity check that inspects a binder and the execution of fold equations retain explicit accounts as term-level operations; extracting their surrounding word algebra does not turn them into first-order algebra over atoms.

The baseline includes acceptance, refusal, inversion conclusions, elaboration choices, reduction demand, and reconstruction behavior. A newly accepted equation, a stronger inversion conclusion, or an additional solving choice is a scope change even when mathematically sound. An unexpected difference is investigated and recorded before the migration proceeds. An existing defect discovered by the work is a separate finding, with its correction scoped explicitly.

## Permanent ownership

| Owner | Responsibility |
| --- | --- |
| `curios-num` | Concrete numeric and packed carriers and their scalar semantics. |
| `curios-algebra` | Algebraic data and algorithms over abstract atoms: the current arithmetic, Boolean, word, comparison, and defined-operation reasoning. It owns the meaning and logical strength of its results. |
| `curios-core` | The mapping from intrinsics to algebra declarations; term views and atom preparation; reduction requests; term and proof provenance; reconstruction; and the folds that use algebra results. |
| `curios-analysis` | The shared algebraic conversion strategy and the restricted interface used by inversion. It produces judgment outcomes and typed residual obligations. |
| `curios-elab` | Solved-metavariable materialization, solving-specific proposals, scheduling, validation, rollback, and commitment. |
| `curios-cert` | Checking residual obligations within the kernel's context, budget, and active conversion history. |

`curios-algebra` depends directly on `curios-num` alone. Its source names no `Term`, `Intrinsic`, elaborator context, or kernel type. Core depends on Algebra; Analysis uses Core's term-facing interface; both checkers use the shared Analysis procedure. Algebra does not acquire dependencies on Core's costing, profiling, traversal, or syntax facilities. This is a direct-dependency rule; `curios-num` retains its own dependencies.

The elaborator remains outside the certifier's normal dependency closure. Its existing dev-dependency on the certifier does not change. The separation of the kernel from prelude construction also remains in force.

The mathematical rule is implemented once in Algebra. Core recognizes the operation, supplies observations, and constructs the chosen result. A callback that makes the mathematical decision on Algebra's behalf would leave ownership in the caller and does not satisfy this boundary.

## Algebra and term adapters

### Algebraic data and atom identity

Algebra uses representations appropriate to the operations it performs: combinations and monomials, Boolean formulas, words, and descriptors of the defined operations whose structure current rules inspect. It does not reproduce the whole Core term language. A quotient, remainder, embedding, or window has an algebraic view where an existing rule needs one; unrelated subterms remain opaque atoms.

Core retains a table associating abstract handles with source terms and reconstruction provenance. The handles and algebraic views have a lifetime bounded by the operation or prepared query. They preserve sharing, so a shared term graph is not expanded into a copied expression tree. Persistent canonical caches and archived algebraic forms are outside the consolidation.

Each view states the equality relation its atoms use. Numeric matching currently permits projection of universe instances in places where the numeric carrier licenses it. That projection does not become equality for arbitrary terms or refinement keys. Proof-insensitive matching of a defined operation likewise has a stated domain. The adapter establishes these identities; Algebra operates on the identities it is given without recursively calling term conversion.

Atom identity and presentation order are separate. Structural hashes may retain their current role in reconstructing a product, but a hash collision never establishes equality. The consolidation does not require a new total structural order or claim that current partially normalized forms are canonical. Its interfaces leave the later canonical forms possible without exposing today's spelling conventions as mathematical identity.

### Progress and reconstruction

An operation reports whether it made the progress that permits reconstruction, along with the algebraic result and any origins needed to rebuild it. Core can retain an unchanged subtree or construct a changed one without rediscovering the mathematical rule.

In particular, natural cancellation that removes no summand retains the original inner terms while removing the shared literal floor. Integer cancellation retains its corresponding stability rule. Probe-only normalization stays local to the probe; an unsuccessful retry returns to the spelling the existing continuation expects. Refinement recording and lookup keep their existing key construction and dual/successor probes.

Reconstruction preserves the existing term shapes and sharing where the algorithm leaves them unchanged. A residual returned to a caller must satisfy its progress contract; repeatedly rebuilding an equivalent but differently associated term is not progress.

### Logical strength of results

The result vocabulary distinguishes the following meanings. Concrete Rust names are chosen with the first implementation; these contracts are mandatory.

| Result | Meaning | Permitted use |
| --- | --- | --- |
| Established equality | The supplied expressions denote equal values under the stated atom contract. | Conversion may succeed. |
| Established impossibility | The equation has no solution for any admissible values of its atoms. | Conversion may refuse and inversion may report a clash. |
| Equivalent obligations | The residual equations hold exactly when the original equation holds. | Conversion may check them; inversion may deduce them. |
| Sufficient obligations | Establishing the residual equations establishes the original equation. | Conversion may check them; elaboration may use them to propose solutions. |
| No conclusion | The operation is inapplicable or made no usable progress. | The caller follows its existing fallback. |

Inversion's entry point cannot return sufficient obligations as deductions. The restriction is expressed by its types and available operations, rather than by a caller remembering which result variants to ignore. Conversion may use sufficient obligations in either checker: the certifier checks those obligations, and the elaborator can enqueue them. Failure to establish a sufficient condition does not become an algebraic impossibility result. The surrounding conversion procedure retains its existing refusal and fallback behavior.

The Boolean truth table is an agreement procedure. Agreement on every assignment proves equality; disagreement supplies no impossibility result because the assignments over opaque atoms over-approximate the combinations real terms can take. Distinct symbolic polynomial representations likewise do not by themselves prove that an equation has no solution. These restrictions remain explicit as representations change.

Algebraic residuals name abstract expressions. Core reconstructs terms from them, and Analysis forms comparison obligations with the types required by the judgment. Congruence continues to read operand demands from `Intrinsic::signature`, including proposition types for proof operands, so ordinary proof irrelevance keeps its existing role.

### Demand and resource accounting

The adapter requests observations as the existing procedure needs them. Preparing a query must not eagerly reduce every reachable term or distribute every symbolic product. Core owns term traversal and reduction requests; Algebra can supply incremental builders or narrowly scoped requests for the next algebraic observation. A general host interface exposing conversion, solving, term construction, and all of reduction is excluded.

The initial demand contract preserves at least these boundaries:

- A symbolic product is distributed only at the current normalization gates; literal-versus-symbolic cases keep their cheaper path.
- Reading a Boolean formula stops at the current atom cap, without forcing the rest of the input first.
- Local Boolean folds preserve the existing treatment of a stuck right operand.
- Numeric atom-argument forcing remains the existing retry, with its current limits on what is inspected.
- A sequence window measures segments incrementally and stops where the existing seam procedure stops.

Algebra exposes the work bounds needed to reserve resources before expensive arithmetic, expansion, or allocation. A narrow allowance or charging interface may be supplied by Core; it carries resource accounting alone. Core maps that work to its budget and separately charges term traversal and reconstruction. Exhaustion follows the existing refusing/error path and cannot manufacture a verdict. Existing charging boundaries and fixed limits form the baseline; any accounting change is identified and measured explicitly.

Traversals must work on the default stack. The extraction preserves DAG sharing and per-operation memoization and avoids introducing repeated hashing, repeated purification, or large temporary copies. Packed runs retain a representation suited to their payload; abstracting words must not require a fresh allocation for every packed bit. Instrumentation stays with the caller that owns it.

### Proof and type provenance

Algebra does not inspect a proof term or construct one. Core keeps element types, bound proofs, and other term-only data behind origin handles. An algebraic transformation states which existing origins its result uses; Core reconstructs the result with those terms.

Window fusion and slicing retain the current restrictions on which proof can be handed on. If a transformation would need a new bound, the procedure declines where it does today. Existing proof-irrelevant comparison of operands does not authorize inventing a proof for a new node. The same discipline applies to quotient/remainder and regrouping operands.

Packed runs and symbolic list elements remain distinct in the algebraic view. Different concrete packed heads can establish a clash. Different list-head spellings do not establish one, because those terms may convert. The consolidation preserves today's prefix behavior and does not add suffix cancellation or stronger element comparison.

## Operation declarations

Core introduces `Intrinsic::algebra` as the source of truth for the algebraic role an intrinsic exposes. The description names an implemented structure, operation, or morphism and the operands needed by its view. Undeclared operations are explicitly opaque. Adding an intrinsic requires classifying its algebraic role, including the opaque case.

Algebra owns the meanings of the supported declaration kinds. Core supplies the mapping from concrete variants and their operands. The table describes only laws exercised by today's implementation: declaring membership must not silently enable every identity of a larger theory. The finite truth-table procedure, current bitwise identities, and existing morphism directions remain their actual capabilities.

The declaration and the operational demand policy have distinct jobs. The declaration supplies semantics; folds and the shared comparison strategy determine when to request the relevant operation under the current gates. Both routes use the same mathematical implementation. `Intrinsic::signature` remains the authority for typing and operand demands.

A new operation of an already implemented kind should reuse its algebraic algorithm through a declaration and a term view. A new kind of reasoning may require an engine change. The broader destination's promise that all of its proposed operations are expressible as rows is deferred until those kinds exist and have evidence.

## The two checkers and inversion

The shared path has three responsibilities with distinct owners:

1. **Prepare.** The elaborator materializes solved metavariables at the existing points. The kernel supplies the terms it rechecks. Core exposes the requested algebraic views through each driver's reduction service.
2. **Derive obligations.** Analysis runs the common comparison strategy through Core and Algebra. It covers the intrinsic pair path, the outer Boolean probe, the existing normalization and retry schedule, and signature-driven congruence. It returns outcomes or typed residual obligations, preserving the logical strength of each transformation.
3. **Discharge.** The elaborator enqueues obligations in its worklist. The kernel checks them using its active recursive judgment. Each driver retains its own error and refusal handling.

The shared procedure must preserve the points at which a driver continuation runs. A staged plan or a narrow continuation interface may be needed for demand-sensitive fallback; eagerly preparing all later alternatives would change the procedure. Neither driver restates the algebraic chain to recover its scheduling behavior.

Kernel recursion keeps the same active `History`. Its keys include the local context and the current goal, and entries live for the current recursion path. Routing a residual through a fresh top-level `Judge::convert_at` call would lose that history. The integration therefore returns obligations to the active kernel conversion or explicitly threads its continuation; it does not assume that every judgment entry point is interchangeable.

The elaborator retains its packed-literal solving view and all choice-making about metavariables. Any reusable word calculation within that view uses Algebra, while decomposition proposals and goal creation remain elaboration's. Assignment still passes through the current materialization, occurs and scope checks, validation, rollback, and commitment path. Algebra cannot mutate the metavariable store, and Analysis cannot commit a solution.

Inversion uses the restricted shared interface. Its equivalent residuals, clashes, and refusals preserve the current meaning of index deductions. Product-factor sufficiency remains unavailable as an inversion deduction.

This delivery introduces no arithmetic search, certificate payload, or module-evidence format. The certifier checks today's residual obligations using the shared implementation. When arithmetic search is added later, its certificate checker belongs in Algebra and the search remains outside the certifier's dependency closure.

## Stages

Each stage has a bounded production migration and removes the mathematical implementation it replaces. Temporary differential machinery is confined to verification and removed when its migration closes. There is no permanent old-engine fallback.

1. **The baseline and contracts.** Inventory each existing law, its callers, declaration, atom relation, demand gate, logical strength, reconstruction restriction, budget behavior, and evidence. Record the current held and refused cases through both checkers, the inversion cases, and elaboration-specific proposals. Establish spelling, progress, substitution, and judgment-history regression cases. Map every inventory entry to a stage and eventual owner. A gap in the existing evidence is recorded, and a behavioral defect is scoped separately.
2. **The crate and a complete arithmetic path.** Add the workspace crate with its dependency boundary, algebraic data, result strengths, resource interface, and Core adapters. Introduce the declarations needed for numeric collection and cancellation. Route actual fold and comparison callers through that implementation, including at least one path used by each checker. Establish the restricted inversion result interface as its first consumers migrate. Delete the replaced numeric bodies. This stage must demonstrate term reading, mathematical work, reconstruction, and judgment use; a crate with no production callers is incomplete.
3. **Existing numeric reasoning.** Complete the migration of natural and integer collection, multiplication and on-demand distribution, Euclidean recombination, comparison facts, bounds and domination, preimages and embeddings, bitwise identities, and shift laws. Keep each carrier's coefficient constraints and the existing comparison-only sign normalization. Reuse common mathematics while retaining the different natural and integer reconstruction contracts. Remove the superseded mathematical implementations and migrate their callers.
4. **Existing Boolean reasoning.** Move local Boolean laws, connective reasoning, comparison alignment, and bounded truth-table agreement behind declarations and adapters. Preserve the eight-atom cap, incremental reading, and agreement-only conclusion. Both the intrinsic converters and their outer Boolean probes use the new implementation. Remove the old mathematical bodies.
5. **Existing word and morphism reasoning.** Move prefix, concatenation, root/position, window, and homomorphism algorithms. Preserve literal fusion limits, packed/list distinctions, lazy measurements, element types, and proof origins. Complete the migration of the remaining carrier-conversion identities. Keep Core's existing sequence representation and its structural eliminators. Route any shared calculation in the elaborator's packed-literal view through the new owner. Remove the replaced word algorithms.
6. **One judgment strategy.** Complete the shared Analysis procedure for algebraic conversion, retries, and typed congruence. Replace both checker copies and the duplicated outer Boolean orchestration. Keep solved-meta preparation, solving-specific proposals, worklist handling, kernel history, and assignment commitment with their drivers. Complete inversion's migration to the restricted result interface. Preserve the existing ordering and short-circuit behavior of the procedure.
7. **Deletion and the durable record.** Audit every baseline inventory entry and caller. Remove obsolete exports, adapters whose only purpose was migration, duplicate algorithms, and temporary differential machinery. Record the implemented contracts in the appropriate crate documentation, design decisions, and soundness entries; add the dependency and ownership invariants to `CLAUDE.md`; update the roadmap to distinguish the completed consolidation from the remaining capabilities. Close the verification and cost record, then retire this specification as required by its final section.

The first complete arithmetic path determines whether the proposed interfaces actually separate responsibilities. Later families may refine those interfaces while preserving their contracts. Shared orchestration can be migrated incrementally alongside the structures, but stage 6 is complete only when both copies are gone.

## Deletion boundaries

Deletion follows the responsibilities that moved. Files containing representation or evaluation code may survive with a narrower purpose.

| Current area | What is retired | What remains with its existing owner |
| --- | --- | --- |
| `nat.rs` and `int.rs` | Independent implementations of coefficient collection, monomial reasoning, cancellation, distribution, recombination, and other migrated mathematics. | Numeric term representation and term-specific decomposition, demand, and reconstruction adapters. A surviving wrapper delegates its mathematical decision. |
| `spine.rs` | The ambiguous `Peel` API and the migrated numeric, Boolean, symmetric, prefix, and position algorithms. | Any necessary term views and provenance adapters, placed with their representation owner. |
| `reduce::intrinsic::{truth,laws,compare,nat,int}` | The old truth-table engine, symbolic laws, comparison algorithms, and defined-operation reasoning. | Literal evaluation, intrinsic dispatch, reduction scheduling, charging, and term construction. |
| `free_monoid.rs` and `reduce::intrinsic::free_monoid` | Independent word normalization, window mathematics, and homomorphism algorithms. | Value decomposition, `FreeMonoid::uncons`, structural-elimination support, and term/run adapters used by other consumers. |
| Both `convert::intrinsic` implementations and their outer Boolean paths | Repeated algebraic orchestration and repeated congruence preparation now owned by Analysis. | Driver preparation and obligation execution, including the elaborator's solving-only view and the kernel's active history. |
| `curios-analysis::invert` | Dependence on a general peel result whose sufficiency restrictions are implicit. | The inversion judgment, consuming only its admissible algebraic conclusions. |

An old function name may remain as a small term-facing wrapper where it remains the right public interface. Its old mathematical body must be gone. Conversely, deleting an entire representation file is not a completion criterion. The inventory and caller audit establish that each law has one production owner.

## Verification

Verification is part of the implementation stages. The baseline is executable evidence gathered against the tree being migrated; this document's source inventory does not substitute for it.

- **Independent Algebra evidence.** Test algebraic operations with simple atoms and concrete valuations through `curios-num`, without importing Core or either checker. Check equivalent residuals in both directions, sufficient residuals in the implication direction, and impossibility against all sampled admissible valuations. Exercise coefficient multiplicities, signs, zero factors, Euclidean recombination, and packed/list distinctions. Handwritten controls remain necessary beside any declaration-driven cases.
- **Adapter evidence.** Check atom identity at universe and proof boundaries; retention of original terms when no progress occurs; reconstruction order and sharing; proof and element-type provenance; normalization retry behavior; and round trips between views and the term shapes they represent. A hash collision must not establish equality.
- **Differential migration evidence.** Compare old and new results, logical strength, reconstructed residuals, refusals, and solving outcomes on the same inputs. Stateful procedures run with isolated equivalent state, so the first run cannot solve a metavariable or warm a cache for the second. Any newly accepted or newly refused case is a finding to resolve against the behavior-preservation contract. Remove the temporary oracle when its replacement is established.
- **Both drivers and inversion.** Retain every held law and refusal control in the existing grids. Exercise solved-meta substitution, the packed-literal solving view, rollback and commitment, proof-typed congruence, recursive kernel history, and inversion's exclusion of sufficient product-factor conclusions. Include the outer Boolean path as well as intrinsic pairs.
- **Operational behavior.** Check fixed caps, refusal under exhausted budgets, charging before expensive work, lazy right operands, incremental window measurements, product-distribution gates, default-stack traversal, and sharing on repeated subexpressions. A symbolic list-head mismatch must never take the packed-literal clash path.
- **Judgment properties.** Carry the existing soundness evidence forward and add targeted symmetry, transitivity, and substitution cases, including substitutions that change atom structure and solved metavariables. Record the scope of these tests; a finite grid does not establish a general metatheorem or completeness claim.
- **Cost and integration.** Measure prelude elaboration and certification before and after each Core or shared-engine stage, naming the measured stage and resource. Investigate allocation, repeated preparation, and build-time regressions before closing a stage. Use the repository's implementation validation gates and preserve the build dependency boundaries.

## Documentation and design record

The implemented mission and crate-local decisions belong in `curios-algebra`'s README; its algorithms and API contracts belong in rustdoc. Core documents the declaration, atom, demand, and reconstruction adapters. Analysis documents the shared procedure and its admissible results. Driver documentation explains the state each checker continues to own.

The cross-crate ownership decision and its alternatives belong in `documentation/design/`. The soundness entries are revised as each implementation moves, retaining their assumptions, evidence, and grades. Moving a rule into Algebra or running it through both checkers does not raise its evidence grade by itself. The binder-sensitive map check and fold equations retain explicit accounts of their special treatment.

The existing decisions about spelling, on-demand sums, refinement lookup, and proof preservation remain applicable during the consolidation. Update statements about where an algorithm lives when its migration lands; revise a behavioral decision only when its replacement is implemented and justified. Canonical keys and a general relational procedure cannot be cited as reasons to remove today's restrictions during extraction.

`CLAUDE.md` gains the direct-dependency and no-`Term` invariants for Algebra, the ownership of declarations in Core and shared judgments in Analysis, and routing for changes to an algebraic structure or operation. The roadmap records the consolidation separately from the broader algebra capabilities. References in dependent specifications continue to identify the capabilities they actually require.

## Rejected for this delivery

- **Deferring the crate until after a Core-only consolidation.** The consolidation must establish and exercise the dependency boundary. Leaving mathematics tied to `Term` would defer the central extraction risk.
- **Moving the current files wholesale into Algebra.** Their term representation, reduction, and proof responsibilities belong in Core and the drivers. The crate must be usable and testable over independent atoms.
- **A general host trait that exposes the whole compiler.** It would allow the mathematical implementation to depend on conversion, solving, and reconstruction through callbacks. Narrow observations and resource accounting are sufficient for the intended boundary.
- **Treating every result as equality, clash, or an unqualified residual.** The distinction between equivalent and sufficient obligations is necessary for inversion and must be enforced by the interface.
- **Eager canonicalization as part of extraction.** It changes demand, spelling, and cost, and would require the canonical-key work excluded from the consolidation.
- **A permanent compatibility engine.** Two production implementations would preserve the ownership problem. Migration adapters are temporary; representation adapters remain because they have an ongoing responsibility.
- **Building unused future solvers or representation variants.** The initial interfaces are exercised by current consumers. Later capabilities extend those interfaces from measured requirements.

## Completion criteria

- `curios-algebra` exists, depends directly on `curios-num` alone, names no Core or checker types, and is used by production reduction and both checkers.
- Every baseline inventory entry has a single mathematical implementation in Algebra or a documented term-level responsibility with its appropriate owner. All replaced mathematical bodies and permanent old-engine fallbacks are gone.
- Core owns term views, carrier-appropriate atom identities, demand, provenance, and reconstruction. Declarations expose only the implemented algebraic capabilities.
- Analysis owns the common algebraic comparison strategy and typed congruence preparation. Both drivers use it, including the outer Boolean path; neither repeats the algebraic chain.
- Equivalent and sufficient obligations are distinguished in the interfaces. Inversion can consume only admissible deductions, and a failed sufficient condition cannot become an algebraic clash.
- Elaboration owns proposal selection and assignment commitment. Kernel residual checking retains its active history. The certifier's dependency closure contains no elaboration search.
- Held laws, refusal controls, solving behavior, proof preservation, spelling stability, demand gates, and fixed limits retain their baseline behavior. Differential findings and cost changes have an explicit resolution.
- Independent algebra evidence, adapter evidence, both-driver coverage, and the implementation validation record are in place. Durable contracts and design decisions have moved to their authoritative documentation.
- Part 2 remains distinguishable from the completed consolidation, and the retirement requirements below are satisfied.

## Retirement

Retire this specification when its implementation, verification, deletion audit, and durable documentation are complete. Part 2 does not block retirement, and this file is not rewritten to hold its remaining work.

Move the implemented contracts, decisions, rejected alternatives, and evidence to their authoritative homes: crate READMEs and rustdoc, cross-crate design decisions, the soundness perimeter, and the contributor invariants. Replace the roadmap's pending part 1 entry with a checked summary of the implemented capability.

Update part 2 and any other dependent specifications to link to the implemented foundation's permanent documentation rather than this working document. Reassess their dependencies against what actually landed without treating extraction as delivery of a broader theory. Verify that nothing still references this specification's filename, then delete it.
