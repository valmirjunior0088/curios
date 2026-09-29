# Algebra, part 1: one owner for the carriers' theory, declared and evidenced

Working specification for consolidating today's intrinsic algebra into `curios-algebra`, integrating it with reduction and the two checkers, declaring what each operation is, generating the law grid from those declarations, and publishing the one view of linear arithmetic the rest of the campaign builds on. It removes the implementations it supersedes. Its mathematical scope is the behavior the compiler already implements, made uniform across the carriers a law is declared at; it adds no search, no certificate and no operation.

The campaign's other parts build on this one. [Part 2](02-bounds-from-facts-spec.md) proves bounds from the facts in scope against the contract published here and may begin once stage 3 lands; [part 3](03-declared-operations-spec.md) adds operations through the declarations and the generated grid defined here; [part 4](04-relational-layer-spec.md), not refined yet, reserves the relational layer until a consumer needs it. This specification is independently implementable and retirable. A stage named by another specification — the campaign's own, [the numeric laws](../numeric-laws-spec.md), or [the certifier's checked evidence](../verdicts/07-checked-evidence-spec.md) — is fulfilled only when its stated capability exists; creating the crate does not fulfill those dependencies by itself. In particular, preserving today's comparison schedule does not establish the certifier specification's stronger restrictions on trusted code.

## What this builds on

- **The implementation.** The numeric algebra lives in `curios-core/src/nat.rs` and `int.rs`; spine comparison in `spine.rs`; sequence decomposition and normalization in `free_monoid.rs`; and symbolic folds, comparison facts, Boolean laws, and the truth table under `reduce::intrinsic`. These files combine mathematics with term reading, reduction demands, and reconstruction.
- **The consumers.** `curios-elab/src/convert/intrinsic.rs` and `curios-cert/src/kernel/convert/intrinsic.rs` repeat the comparison strategy. Their outer converters also ask the Boolean decision procedure about a connective opposite another kind of term. `curios-analysis/src/invert.rs` consumes a restricted set of peel results.
- **The evidence.** `curios/src/tests/laws.rs` records held laws and refusal controls through both checkers, one carrier at a time, each row stated by hand under the grid's own rule to state a row at every carrier and state it first. Core's numeric, spine, truth-table, and intrinsic-law tests check individual operations and verdicts. The current soundness accounts are [Open fold laws and the sum normal form](../../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md) and [Intrinsic fold laws and the free-monoid peel](../../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md).
- **The seam, read alike.** `align_comparisons` reads a `<=` meeting a `<` through `successor_comparison`, the identity the refinement probes key by, which cancels the floor that reading creates, so `x + 1 <= y` meets `x < y` at `Nat` as `Int`'s difference split lets it meet at `Int` (their rustdoc, and [A comparison is spelled one way when it is stuck](../../design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md)); the grid holds the row at both carriers. The `/std` call sites it makes redundant are counted in the baseline inventory below, which is taken on that tree.
- **The standard library as it stands.** `/std` states its invariants rather than answering made-up values, so its proofs — `Str/Valid`, `Str/At`, `Char`, `Bytes` — are the heaviest users of what conversion decides, and the corpus this consolidation is measured against.
- **The ownership already established.** [Core](../../../curios-core/README.md) owns terms and their representation; [Analysis](../../../curios-analysis/README.md) owns shared judgments; the elaborator owns solving; the kernel rechecks its output. Shared algebra becomes part of the trusted implementation used by both checkers. Their agreement is therefore integration evidence, and independent algebra tests remain necessary.
- **The operational decisions.** [A sum is merged when it is forced, not when it is built](../../design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md), [A comparison is spelled one way when it is stuck](../../design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md), and [A law is decided where it neither respells nor invents](../../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md) explain constraints the extraction must preserve.

## The gap

**Mathematics has no independent owner.** A coefficient, a word segment, or a Boolean atom is usually read directly from a `Term`. Arithmetic collection and cancellation are interleaved with decisions about term spelling. Word algorithms carry element types and proof operands. Moving these functions unchanged into another directory would leave their responsibilities joined.

**The comparison strategy is repeated.** Both converters ask for stuck-product normalization, Boolean agreement, connective normalization, comparison alignment, a chain of peels, and a retry with numeric atom arguments forced before reaching congruence. The elaborator adds solved-metavariable substitution and a packed-literal solving view. A change to the shared strategy currently requires corresponding edits in both drivers.

**A residual has more than one meaning.** `Peel::Continue` can express an equivalent residual equation or a sufficient condition for equality. The distinction is enforced by registration: product-factor peeling is included in conversion and excluded from inversion. The API does not express the restriction that prevents inversion from deducing `f = g` from `x * f = x * g` when `x` may be zero.

**Term spelling is operationally significant.** Sums preserve first appearance, products use structural-hash order, and a stuck Boolean connective can keep its right operand as written. Refinements use written spellings and existing probes. Numeric cancellation deliberately preserves original subterms when no summand cancels, because reconstruction can otherwise keep changing association and order. A new representation must account for these constraints explicitly.

**A law is stated at each carrier by hand.** The grid's rule to state a row at every carrier is a convention, and the `<`/`<=` seam showed what a convention misses: `Int` held `i + 1 <= j` against `i < j` through its difference split while `Nat`'s successor reading left its floor uncancelled, and no row at `Nat` said so until one was written. The two carriers' alignments were two procedures deciding one relation. A law declared once and instantiated at every carrier its declaration covers, decided by one procedure, turns the convention into a construction.

**Atom identity is decided at each call site.** `Nat::linear` keys summands up to universe instances, `cancel_common` and `compare_nat` match the same way, and the refinement probes key by written spelling for reasons of their own. No one statement says what "the same atom" is for a carrier, so a consumer outside the converters has to reverse-engineer it from the folds.

**Nothing states what conversion decides for a consumer outside it.** A procedure that writes proofs for conversion to check — [part 2](02-bounds-from-facts-spec.md)'s — needs to know which linear facts conversion closes, and today that is readable only from `compare_nat` and the peels.

**The decision to keep the algebra in conversion has no recorded rationale.** [The certifier's checked evidence](../verdicts/07-checked-evidence-spec.md) retains a rejection of replacing the carriers' algebra with casts whose rationale is missing, and asks for it to be supplied rather than reconstructed as an established decision.

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

Beside the families, the delivery establishes what the later parts stand on:

| Foundation | What it provides |
| --- | --- |
| Declarations and evidence | `Intrinsic::algebra` as the source of truth for every operation's algebraic role; the laws of each declaration kind stated once; the grid generated from them at every carrier a declaration covers; the existing theory's audit. |
| The linear view | One atom identity per carrier, stated once; a canonical linear view of a `Nat` or `Int` comparison over it, with a total atom order; comparison alignment as one procedure over the view; and the contract of what conversion decides, published for elaboration-side consumers. |
| The recorded decision | Why the carriers' algebra stays in conversion rather than being supplied as casts. |

Concrete scalar evaluation remains in `curios-num` and its callers. Core continues to execute eliminators and apply functions. The map-identity check that inspects a binder and the execution of fold equations retain explicit accounts as term-level operations; extracting their surrounding word algebra does not turn them into first-order algebra over atoms.

The baseline includes acceptance, refusal, inversion conclusions, elaboration choices, reduction demand, and reconstruction behavior. A newly accepted equation, a stronger inversion conclusion, or an additional solving choice is a scope change even when mathematically sound. An unexpected difference is investigated and recorded before the migration proceeds. An existing defect discovered by the work is a separate finding, with its correction scoped explicitly.

The one behavioral change in scope is stage 8's. Comparison alignment becomes one procedure over the linear view, so two comparisons of one relation that agree in their view meet at every carrier the relation is declared at, where today each carrier's alignment decides which spellings meet. Each row that becomes held is listed and justified against the total order's laws; any other newly accepted or refused case remains a finding against the behavior-preservation contract.

Outside the scope: arithmetic search and certificates, which are [part 2](02-bounds-from-facts-spec.md)'s and [part 4](04-relational-layer-spec.md)'s; new operations and new declaration kinds, which are [part 3](03-declared-operations-spec.md)'s; and canonical refinement keys, polynomial unification proposals, Boolean normal forms in place of the truth table and one internal sequence carrier, each deferred on the roadmap until a consumer asks for it.

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

The elaborator remains outside the certifier's normal dependency closure. Its existing dev-dependency on the certifier does not change. The separation of the kernel from prelude construction also remains in force. A procedure that searches — [part 2](02-bounds-from-facts-spec.md)'s — lives on the elaborator's side of that boundary and reads the published views through Core; Algebra holds the mathematics both checkers share and no search.

The mathematical rule is implemented once in Algebra. Core recognizes the operation, supplies observations, and constructs the chosen result. A callback that makes the mathematical decision on Algebra's behalf would leave ownership in the caller and does not satisfy this boundary.

## Algebra and term adapters

### Algebraic data and atom identity

Algebra uses representations appropriate to the operations it performs: combinations and monomials, Boolean formulas, words, and descriptors of the defined operations whose structure current rules inspect. It does not reproduce the whole Core term language. A quotient, remainder, embedding, or window has an algebraic view where an existing rule needs one; unrelated subterms remain opaque atoms.

Core retains a table associating abstract handles with source terms and reconstruction provenance. The handles and algebraic views have a lifetime bounded by the operation or prepared query. They preserve sharing, so a shared term graph is not expanded into a copied expression tree. Persistent canonical caches and archived algebraic forms are outside the consolidation.

Each carrier states its atom identity once, in Core's adapter documentation, and every view of that carrier uses it. Numeric matching permits projection of universe instances, because Core offers no elimination from a type or a level into a number, so two occurrences of one polymorphic name differing only in their instances denote one number; the justification is the carrier's, and the projection does not become equality for arbitrary terms or refinement keys. Proof-insensitive matching of a defined operation likewise has a stated domain. The adapter establishes these identities; Algebra operates on the identities it is given without recursively calling term conversion.

Atom identity and presentation order are separate. Identity comes with a total order over atoms, taken from structural hashes with structural equality deciding every tie, so a hash collision can order two atoms but never merge them. Structural hashes may retain their current role in reconstructing a product. The order serves views and keys; it does not become a term spelling.

### The canonical linear view

A comparison or equation over `Nat` or `Int` whose sides are sums of literal multiples of atoms has one canonical view: the difference of its sides as a combination over the carrier's atoms in the atom order, with its constant separated, read at `Int` through the embedding and with every `Nat` atom non-negative. The view is computed where a judgment asks for it. It reads sums without distributing a product the normalization gates would not distribute, and it respells nothing: no term is rebuilt from it except where an existing fold already rebuilds.

Its consumers are the shared strategy's comparison alignment, which stage 8 moves onto it; the contract below, which states in its terms what conversion decides; and the evidence keys [part 4](04-relational-layer-spec.md) reserves. The rejection of eager canonicalization below stands: it is about rebuilding terms and changing refinement keys, whose demand, spelling and cost it names, and a view computed where a comparison is asked rebuilds nothing and changes no key.

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

Traversals must work on the default stack. The extraction preserves DAG sharing and per-operation memoization and avoids introducing repeated hashing, repeated purification, or large temporary copies. `Nat::summands` flattens a sum afresh on every call — the cost behind `Nat::ordered_sums` that the standard-library invariants work measured and left here, since this part moves the code — so the extraction keeps a sum's flattened form rather than recomputing it. Packed runs retain a representation suited to their payload; abstracting words must not require a fresh allocation for every packed bit. Instrumentation stays with the caller that owns it.

### Proof and type provenance

Algebra does not inspect a proof term or construct one. Core keeps element types, bound proofs, and other term-only data behind origin handles. An algebraic transformation states which existing origins its result uses; Core reconstructs the result with those terms.

Window fusion and slicing retain the current restrictions on which proof can be handed on. If a transformation would need a new bound, the procedure declines where it does today. Existing proof-irrelevant comparison of operands does not authorize inventing a proof for a new node. The same discipline applies to quotient/remainder and regrouping operands.

Packed runs and symbolic list elements remain distinct in the algebraic view. Different concrete packed heads can establish a clash. Different list-head spellings do not establish one, because those terms may convert. The consolidation preserves today's prefix behavior and does not add suffix cancellation or stronger element comparison.

## Operation declarations

Core introduces `Intrinsic::algebra` as the source of truth for the algebraic role an intrinsic exposes. The description names an implemented structure, operation, or morphism and the operands needed by its view. Undeclared operations are explicitly opaque. Adding an intrinsic requires classifying its algebraic role, including the opaque case.

Algebra owns the meanings of the supported declaration kinds. Core supplies the mapping from concrete variants and their operands. The table describes only laws exercised by today's implementation: declaring membership must not silently enable every identity of a larger theory. The finite truth-table procedure, current bitwise identities, and existing morphism directions remain their actual capabilities.

The declaration and the operational demand policy have distinct jobs. The declaration supplies semantics; folds and the shared comparison strategy determine when to request the relevant operation under the current gates. Both routes use the same mathematical implementation. `Intrinsic::signature` remains the authority for typing and operand demands.

A new operation of an already implemented kind reuses its algebraic algorithm through a declaration and a term view. A new kind of reasoning may require an engine change. [Part 3](03-declared-operations-spec.md) adds operations of both sorts, each held by the grid below from the stage that declares it.

### The generated grid

Every declaration kind states its laws once, as statements over abstract operands. The grid instantiates each law at every carrier whose declaration has that kind and puts the instance to both checkers, as `curios/src/tests/laws.rs`'s held rows are put today. Each carrier supplies its semantics, against which every instance is also held at closed values: `curios-num`'s arithmetic, and for `Flt` the binary64 model over a pattern grid that includes both zeros, both infinities, subnormals, and quiet and signaling NaNs with payloads of both signs. A law declared at two carriers is therefore stated at both by construction, and the procedure deciding it is the one the declaration names.

Hand-written rows remain for what no declaration states: the controls, each a claim that is not a law with the counterexample that makes it one; the refused candidates, each with the reason conversion does not take it; and the laws whose rule is term-level, the binder-sensitive map check among them. One mutation per declaration kind is run and caught, and the perimeter entry names the mutation and the instance that caught it.

### The existing theory's audit

The laws the compiler already decides are audited against the conditions a theory in conversion must meet — Coq Modulo Theory's, whose metatheory with strong elimination is Jouannaud and Strub's (2017): conversion stays symmetric and transitive and closed under substitution with the theory in it, and constructors stay free modulo the theory, which is what inversion reads when it concludes a clash. Each condition gets targeted cases through both checkers, including substitutions that change atom structure and solved metavariables; freeness gets them at every intrinsic case inversion distinguishes. The audit records its scope — a finite grid is evidence about the implemented fragment, not a metatheorem — and each later part extends it for the laws it adds.

## The two checkers and inversion

The shared path has three responsibilities with distinct owners:

1. **Prepare.** The elaborator materializes solved metavariables at the existing points. The kernel supplies the terms it rechecks. Core exposes the requested algebraic views through each driver's reduction service.
2. **Derive obligations.** Analysis runs the common comparison strategy through Core and Algebra. It covers the intrinsic pair path, the outer Boolean probe, the existing normalization and retry schedule, and signature-driven congruence. It returns outcomes or typed residual obligations, preserving the logical strength of each transformation.
3. **Discharge.** The elaborator enqueues obligations in its worklist. The kernel checks them using its active recursive judgment. Each driver retains its own error and refusal handling.

The shared procedure must preserve the points at which a driver continuation runs. A staged plan or a narrow continuation interface may be needed for demand-sensitive fallback; eagerly preparing all later alternatives would change the procedure. Neither driver restates the algebraic chain to recover its scheduling behavior.

Kernel recursion keeps the same active `History`. Its keys include the local context and the current goal, and entries live for the current recursion path. Routing a residual through a fresh top-level `Judge::convert_at` call would lose that history. The integration therefore returns obligations to the active kernel conversion or explicitly threads its continuation; it does not assume that every judgment entry point is interchangeable.

The elaborator retains its packed-literal solving view and all choice-making about metavariables. Any reusable word calculation within that view uses Algebra, while decomposition proposals and goal creation remain elaboration's. Assignment still passes through the current materialization, occurs and scope checks, validation, rollback, and commitment path. Algebra cannot mutate the metavariable store, and Analysis cannot commit a solution.

Inversion uses the restricted shared interface. Its equivalent residuals, clashes, and refusals preserve the current meaning of index deductions. Product-factor sufficiency remains unavailable as an inversion deduction.

This delivery introduces no arithmetic search, certificate payload, or module-evidence format. The certifier checks today's residual obligations using the shared implementation. [Part 2](02-bounds-from-facts-spec.md)'s search stays outside the certifier's dependency closure and hands the checkers ordinary terms; if [part 4](04-relational-layer-spec.md) is opened, its certificate checker belongs in Algebra and its search stays outside the closure too.

## What conversion decides, published

Conversion decides a `Nat` or `Int` comparison whose canonical view reduces to a constant — every atom cancelled, the constant's sign the answer — at every carrier the comparison is declared at, in both checkers, with the context's hypotheses out of it. Stage 3 found the fold decides three more shapes of view, and the published contract states all four. A view whose monomials all have one sign over non-negative atoms is decided when its constant does not oppose them: a sum of naturals is at least its constant, and `compare_nat`'s floor rule is this. The comparisons the bounds oracle and domination decide are decided, as [The bounds oracle and the division family](../../soundness/per-term-rules/the-bounds-oracle-and-the-division-family.md) states. So is an equality whose constant the coefficients' gcd does not divide. It decides nothing about a comparison whose view keeps an atom those rules do not bound.

That statement, each carrier's atom identity and the view's interface are published in Core's adapter documentation for procedures outside the converters, so a consumer relies on the stated contract rather than on reading the folds. [Part 2](02-bounds-from-facts-spec.md) is the first: it writes proofs whose last step is exactly such a comparison. The interface is read-only — it prepares views and reports their constants, and it neither searches nor commits anything. Every clause of the contract is a held row of the grid, and its complement, a view that keeps an unbounded atom, has controls.

## The carriers' algebra stays in conversion

This delivery records the decision [the certifier's checked evidence](../verdicts/07-checked-evidence-spec.md) left without a rationale: the carriers' algebra stays in conversion, rather than conversion being kept syntactic and the laws supplied as casts.

- A decided bound is discharged by reduction because the comparison it reflects reduces ([A bound is stated in a decided proposition and discharged by reduction](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)). With the laws as casts, a bound over `x + 0` or `len(a ++ b)` would need one written wherever it is used.
- A cast along a hypothetical proof stays stuck until the proof is matched on ([`Prop` is strict, proof-irrelevant, and definitionally K](../../design/language/prop-is-strict-proof-irrelevant-and-definitionally-k.md)), so casts would leave stuck terms in every type a law reaches.
- [Part 2](02-bounds-from-facts-spec.md)'s proofs are checked by conversion's cancellation, which is what keeps each one a sum of facts rather than a chain of lemmas restating the laws.

This is Coq Modulo Theory's premise — a decidable theory in conversion, with the context's hypotheses kept out of it (Strub, 2010) — and stage 9 records it as a design decision with the cast alternative as its rejection.

## Stages

Each stage has a bounded production migration and removes the mathematical implementation it replaces. Temporary differential machinery is confined to verification and removed when its migration closes. There is no permanent old-engine fallback.

1. **The baseline and contracts.** Inventory each existing law, its callers, declaration, atom relation, demand gate, logical strength, reconstruction restriction, budget behavior, and evidence. Record the current held and refused cases through both checkers, the inversion cases, and elaboration-specific proposals. Establish spelling, progress, substitution, and judgment-history regression cases. Record each carrier's atom identity as its call sites use it today, and every place two carriers decide one relation by different procedures. Map every inventory entry to a stage and eventual owner. A gap in the existing evidence is recorded, and a behavioral defect is scoped separately.
2. **The crate and a complete arithmetic path.** Add the workspace crate with its dependency boundary, algebraic data, result strengths, resource interface, and Core adapters. Introduce the declarations needed for numeric collection and cancellation. Route actual fold and comparison callers through that implementation, including at least one path used by each checker. Establish the restricted inversion result interface as its first consumers migrate. Delete the replaced numeric bodies. This stage must demonstrate term reading, mathematical work, reconstruction, and judgment use; a crate with no production callers is incomplete.
3. **Existing numeric reasoning, and the published view.** Complete the migration of natural and integer collection, multiplication and on-demand distribution, Euclidean recombination, comparison facts, bounds and domination, preimages and embeddings, bitwise identities, and shift laws. Keep each carrier's coefficient constraints and the existing comparison-only sign normalization. Reuse common mathematics while retaining the different natural and integer reconstruction contracts. Remove the superseded mathematical implementations and migrate their callers. State each numeric carrier's atom identity and its total order, build the canonical linear view, and publish the contract of what conversion decides with its rows. [Part 2](02-bounds-from-facts-spec.md) may begin once this stage lands.
4. **Existing Boolean reasoning.** Move local Boolean laws, connective reasoning, comparison alignment, and bounded truth-table agreement behind declarations and adapters. Preserve the eight-atom cap, incremental reading, and agreement-only conclusion. Both the intrinsic converters and their outer Boolean probes use the new implementation. Remove the old mathematical bodies.
5. **Existing word and morphism reasoning.** Move prefix, concatenation, root/position, window, and homomorphism algorithms. Preserve literal fusion limits, packed/list distinctions, lazy measurements, element types, and proof origins. Complete the migration of the remaining carrier-conversion identities. Keep Core's existing sequence representation and its structural eliminators. Route any shared calculation in the elaborator's packed-literal view through the new owner. Remove the replaced word algorithms.
6. **One judgment strategy.** Complete the shared Analysis procedure for algebraic conversion, retries, and typed congruence. Replace both checker copies and the duplicated outer Boolean orchestration. Keep solved-meta preparation, solving-specific proposals, worklist handling, kernel history, and assignment commitment with their drivers. Complete inversion's migration to the restricted result interface. Preserve the existing ordering and short-circuit behavior of the procedure.
7. **The generated grid and the existing theory's audit.** State each declaration kind's laws once and generate their rows at every carrier each declaration covers, held through both checkers and against each carrier's semantics, the binary64 pattern grid included. Retire the hand-stated rows the generation now states, keeping controls, refused candidates and term-level laws by hand. Run one mutation per declaration kind. Carry out the audit and record its scope.
8. **Uniform comparison alignment.** Replace the per-carrier alignment — the successor and dual readings, `Int`'s difference split, and the comparison through numeric preimages — with one alignment over the canonical view, used by both converters through the shared strategy. This is the only stage that may change acceptance: every row that becomes held is listed with the total-order law that justifies it, and every other difference is resolved against the behavior-preservation contract before the stage closes.
9. **Deletion and the durable record.** Audit every baseline inventory entry and caller. Remove obsolete exports, adapters whose only purpose was migration, duplicate algorithms, and temporary differential machinery. Record the implemented contracts in the appropriate crate documentation, design decisions, and soundness entries, the decision that the carriers' algebra stays in conversion among them; add the dependency and ownership invariants to `CLAUDE.md`; update the roadmap to distinguish the completed foundation from the campaign's remaining parts. Close the verification and cost record, then retire this specification as required by its final section.

The first complete arithmetic path determines whether the proposed interfaces actually separate responsibilities. Later families may refine those interfaces while preserving their contracts. Shared orchestration can be migrated incrementally alongside the structures, but stage 6 is complete only when both copies are gone. Stages 7 and 8 follow stage 6, because a law generated at every carrier and an alignment shared by every carrier each assume one strategy in both checkers.

## Decisions settled for stages 5 to 8

Settled with the user before stage 5 began, so each stage opens on its design rather than a choice.

- **Word atoms are compared as written** (stage 5). List elements and opaque word chunks are one atom only where they are one term, universe levels included, as `Bool` atoms are. A number is identified up to universe instances because a level is never part of which number a term is; a list's elements may be types, where levels matter, so projecting them would be unsound as well as a behavior change. Window offsets and lengths are `Nat`s and keep the numeric identity.
- **A word's numbers take the numeric identity** (stage 5, a scoped behavior change). Today window lengths, window offsets and positions are compared by `nat_equal`, which falls back to syntactic equality when neither side is a sum or a floor — an accident of reusing the peel's shape gate, since its callers always know both sides are `Nat`s. So `get(xs, len@u(ys))` and `get(xs, len@v(ys))` stay apart while the same two inside sums cancel. From stage 5 every word number is a `Nat` combination under the numeric identity, which is the carrier's stated licence; only the accepting direction moves, and a Core test pins the newly accepted case. The same asymmetry in conversion — two bare, non-sum numbers go to congruence and need equal levels — is fixed in stage 6, where the shared strategy knows each operand's type, and listed there as that stage's scoped change.
- **An operation is declared when a rule reads it** (stage 5b, settled while it opened). 5b declares `BinEql` as an equality at the packed carrier, which retires `peel_symmetric`'s special case, and the conversions the round-trip rule reads. The word operations stage 7's law table reads — concatenation, length and append — are named there with their families; `map`, `fold`, `get`, `slice` and `replicate`, whose laws no family states, stay undeclared, so no declaration exists without a reader.
- **A round trip is one rule over declared pairs** (stage 5b). Each conversion declares `Operation::Conversion` from its source carrier to its target, the algebra states once which pairs cancel and in which order, and one fold rule — a conversion of its declared inverse's result is that inverse's operand — replaces the four hand-written cancellations. `Byte`↔`Nat`, `Nat`↔`Int` and the grain regrouping cancel both ways; `Flt/of_le_bytes` after `Flt/to_le_bytes` cancels one way, as today. Stage 7 generates the pairs' rows from the same table.
- **The shared strategy returns an outcome** (stage 6). `curios-analysis`'s procedure calls no judgment. It returns equality, inequality, a pair that left intrinsic form, or ordered obligations each at its declared type, behind a comparison of levels. The elaborator enqueues them. The kernel discharges them in order with its active `History`, stopping at the first failure, and the history never leaves the kernel. A narrow `Driver` beside the reducer supplies only what differs by driver: solved-metavariable substitution, and the elaborator's packed-literal view.
- **A declaration kind is a law family** (stage 7). Unit, absorber, idempotence, commutativity, associativity, complement, self-cancellation, distribution, dual, successor seam, cancellation, free-monoid prefix, homomorphism and inverse pair each state their laws once. Each carrier's operation declares exactly the families it implements, with its constants — `And` at `Bool` has unit `true` and absorber `false`, `And` at `Nat` absorber `0` and no unit — so declaring membership enables only implemented laws.
- **Alignment decides by the view, else respells** (stage 8). Two comparisons with equal views meet outright. Otherwise both are respelled from their views in one canonical form before congruence: `<=` with the positive part on the left, `<` becoming `<=` with the constant shifted by one, and an equality oriented by atom order. Each is rebuilt at its own carrier, or at `Nat` where every atom is a widened natural. So congruence still meets aligned operands, and `?n < y + 1` against `x <= y` still solves `?n := x`; that one rebuild replaces the successor reading, the sign split and the pull-back to `Nat`.

## Implementation plan for stages 5 to 9

The working plan the settled decisions above shape. Each stage keeps the pattern stages 2 to 4 used: the old bodies run beside the new ones as a debug-build oracle through `cargo x clippy`'s prelude build and the affected suites, a mutation per oracle proves it live, the old bodies are deleted, cost is retaken, and the stage lands as its own commit.

### Stage 5a: word peels

- **The algebra's words.** `curios-algebra` gains a `word` module, generic over its caller's `Alphabet`.
  - A run is a `Run`: `Packed` bits or bytes as `curios-num` holds them, never an element per bit, or a vector of elements.
  - A `Segment` is a run, a single element, an opaque chunk, or a window of a base with an offset, a length and a proof.
  - A `Word` normalizes as it is built: empty runs and empty windows dropped, adjacent runs merged, and abutting windows of one base fused where the second's offset is the first's offset plus its length.
- **The measures are the caller's.** An alphabet supplies a count, zero, the sum, the identity of two numbers, a chunk's length, the rooting of a position through the windows it is read through, and where an operand begins inside a root. Core's sum is `Nat::sum` and its identity `Nat::same`, the cancellation under the numeric identity. Closed combinations were rejected: a window sum is recombined by Euclid, which reads terms, so a sum folded here would stop windows fusing or matching that meet today, and every window number would be read up front where today it is read only when compared.
- **Fusion hands on the second window's proof** and sums the lengths as it fuses, which is the rule today and needs no derivation.
- **What moved.** The normal form, the prefix strip and its verdict (`Stripped`), the verdict against the identity, where an operand begins, and when two positions are one (`same_position`). Core keeps reading terms into words, rebuilding residuals, regrouping, and the rooting.
  - A packed head that differs is `Impossible`, and an element head that differs is only a residual, since two element terms may convert.
- **Identity.** Elements, singles, chunks and bases are symbols compared as written; numbers take the numeric identity, bare or inside a sum — the scoped change the settled decisions record, pinned by `spine::position_tests::a_word_number_is_one_number_across_a_universe_instance`. `peel_symmetric`'s `Bin` case moves with the word declarations in 5b.
- **Oracle.** `peel_bin`, `peel_list` and `peel_position` are pure, so each is compared with its old body directly, the old `nat_equal` given the numeric identity so the oracle checks the port and the test checks the change.

### Stage 5b: the fold's measures, and round trips

- **Measures.** `curios-algebra` gains a `measure` module.
  - `total` and `locate` place a window over lengths known as numbers — `FreeMonoid::measure` and `measured_window` read the segments and materialize the pieces.
  - `seam_window` places a window on a concatenation's seams over symbolic lengths, consuming a distance by the alphabet's new `difference` (`Nat::cancel_common` in Core). The caller measures each operand as the walk reaches it, reducing its `len`, so the walk still spends one measure per operand and nothing past where it stops.
  - `join` is a concatenation's normal form — the identity dropped, all-literal survivors fused, a lone survivor collapsed — over what each operand is; building the fused run, `FUSION_CAP` and every charge stay Core's.
  - `split` cuts a packed literal against a concatenation for the elaborator's packed-literal view. It hands back the ranges placed before it stopped, because the view relates each operand to its range as the split reaches it, so a split that clashes or abstains has already related a prefix; the proposals stay the elaborator's.
- **Declarations**, as the settled decision reads: `BinEql` is an equality at `Carrier::Packed(grain)`, and `Flt`'s `==` and `!=` equalities at `Carrier::Float`, which `peel_symmetric` reads, so its special cases are gone. Every conversion is `Operation::Conversion { from }`, which replaces `FromByte` and `Widening`; `round_trip` states which pairs cancel, `Operation::undoes` reads it, and `Intrinsic::undone` is the one fold rule the six conversion arms call.
- **What stays Core's.** Reading a value's segments, the cons decode (`FreeMonoid::uncons`, `peel_first_*`), `is_identity`'s binder check, `bin_measure`'s crediting of `Flt/to_le_bytes`, the homomorphism fold's execution (`reduce_homomorphism`, whose declarations are stage 7's), and every reduction and charge.
- **Oracle.** The window, the seam walk — replayed over the measures the new walk took, since it reduces — the concatenation's normal form, the packed-literal split, `peel_symmetric` and each round-trip arm are compared with their old bodies.

### Stage 6: one judgment strategy

- **The procedure.** `curios-analysis`'s `conversion` module: `convert_intrinsics` runs the chain both converters ran, in their order — the driver's preparation, a stuck product distributed, the truth table, connective trees flattened, comparisons aligned, the peels, the atom-argument retry, the packed view, two numbers of one operation, congruence. It calls no judgment.
- **Its result** is an `Outcome`: `Equal`, `Unequal`, a `Residual` pair to compare at `Type` (a peel's residual, or a pair a demand took out of intrinsic form), or a `Congruence` of the two result-level vectors and the operand `Obligation`s, each at the type `Intrinsic::signature` declares. The levels come back even where the two sides are not one operation — the operands are then `None` — because the checkers compared levels before shapes and the elaborator's comparison may constrain them.
- **The `Driver`** is a `Reducer` that also supplies `prepare` (the elaborator substitutes solved metavariables, the kernel reads a term as it is), `packed_view` (the elaborator's alone, whose goals join its queue) and the syntax registry. The elaborator's is `Elaborating`, holding its queue and context together; the kernel is its own.
- **Each checker's discharge.** The elaborator enqueues a residual and every operand, and compares levels by its own rule. The kernel compares a residual at `Type`, levels by `levels_eq`, and each operand in order under its active `History`, stopping at the first failure.
- **The outer probe** is `connectives_agree`; the elaborator asks it before its dispatch and the kernel in its fallback ahead of its unfolding retry.
- **The scoped change.** Two applications of one `Nat`- or `Int`-valued operation that are one number up to universe instances meet before their congruence (`Nat::same`, `int_same`), so a bare pair meets as it already met inside a sum. Pinned in both checkers by `one_operation_at_two_universe_instances_is_one_number`.
- **Evidence, settled while the stage opened.** No per-call oracle: a transcription of the old chains that returns an outcome is the new procedure line for line, and running the acting chains beside it would run every conversion twice at every nesting level and double every charge. Both checkers' suites and the prelude's elaboration and certification run through the procedure; a mutation per discharge path shows they catch a plumbing error; the widening gets the prelude differential stage 5a's numbers got.
- **Records revised.** `an-independent-kernel-re-checks-what-the-elaborator-accepts.md` and `curios-analysis/src/judge.rs` now say the carriers' algebra is shared and why: it is algebra, not strategy, its rules were shared already, and its trust rests on the algebra's own tests and the law grid both checkers hold. The converters' "strategies apart on purpose" comments went with their chains, and `the-refinement-key.md` names the two new readers of the numeric identity.
- **The history test** is `kernel::convert::recursion_tests::a_peel_residual_is_compared_under_the_active_history`.

### Stage 7: the generated grid and the audit

- **Law families.** `curios-algebra`'s `law` module states each `Family` once as `Law`s between `Expr`s over variables and a carrier's `Constant`s: unit and absorber at either or both positions, idempotence, self-cancellation, commutativity, associativity, complement, nested cancellation, distribution, dual, successor seam, cancellation, the length homomorphism and the inverse pair.
- **The family table.** `TABLE` names, for each operation at each carrier, the families conversion decides for it, with its constants — `And` at `Bool` has the unit `true` and the absorber `false`, `And` at ℕ the absorber `0` and no unit. It covers `Nat` and `Int` arithmetic, the bitwise operations and shifts, the comparisons, `Bool`'s connectives, the equalities at every carrier that has one, the words' concatenation and length, and the conversions as inverse pairs. The word operations it reads — `Concat`, `Length`, `Append` — and `Carrier::List` join the declaration vocabulary; nothing in Core declares them, since only the table reads them.
- **What stays by hand, and undeclared.** `map`, `fold`, `get`, `slice` and `replicate` have no family: their laws are term-level (a function operand, the identity map's binder check) or carry bounds a family's abstract operands do not, so they stay written rows and their operations stay undeclared — no declaration without a reader.
- **The generator** is `curios`'s `tests::laws`, split by theme: `generated` spells each law as Curios source through a spelling table (an operation's surface form at its carrier, the carrier's constants, a proof binder stating each narrowing's precondition) and puts every carrier's rows to both checkers as an `Eq/refl()` compile and as `? ≈ Eq/refl()` goal lines ending on the sentinel; `semantics` holds every law at every assignment of its variables over a small grid of values per carrier, computed through `curios-num` — at `Flt` the binary64 pattern grid of both zeros, both infinities, subnormals, and quiet and signaling NaNs with payloads of both signs; `written` keeps the rows no family states. 142 rows are generated; the 118 written rows they restate were retired, and the controls, the refused candidates, the term-level laws and the inverse pairs spelled with an implicitly discharged bound stay written.
- **Generator tests.** Withdrawing a family from the table withdraws exactly its rows; a family declared where nothing decides it — associativity of `and` at ℕ, which the closed values confirm — fails through the checkers.
- **The audit** is `tests::laws::audit`, generated from the same rows, beside the generator that makes them rather than in `tests::algebra`. Every law holds reversed, every two laws sharing a side chain, every numeric and Boolean law holds at a compound term that changes its atoms (`x := w + 1`, `i := q + 1`, `b := x < y`), and a case split on an equation between distinct constructors needs no arm at every case inversion distinguishes — two literals of each scalar carrier, a successor against zero, a cons against the empty word at each word carrier, two packed heads — with an equation that may hold as the control. Through a solved metavariable, the audit found an incompleteness and records it rather than fixing it: a law decided by operand identity or by the truth table does not solve a metavariable standing for one of its atoms — commutativity decided by the peel that compares swapped operands as written, `&&` and `||` by their leaf sets, `Bool` distribution by the truth table. Each decides an equation between known atoms and proposes nothing for an unknown one, the refusing direction; the audit holds every other law to solving and every recorded one to not, so a change moving either is seen. A finite grid is evidence about the implemented fragment, not a metatheorem.
- **Mutations.** One per family, fourteen in all, each caught and recorded in the perimeter entry that owns it with the instance that caught it — `intrinsic-fold-laws-and-the-free-monoid-peel.md` for the peel's and the alignment's families, `open-fold-laws-and-the-sum-normal-form.md` for the fold's. Five failed generated rows; seven failed the prelude's elaboration first, which depends on the family; the complement and nested cancellation at `Bool`, which conversion also decides by the truth table, left every generated row holding and were caught by `curios-core`'s open-fold-law grid, which reduces rather than compares. No production path changed, so the stage's costs are stage 6's.

### Stage 8: uniform alignment

- **One alignment.** In the shared strategy, two `Nat` or `Int` comparisons are read by one `LinearViews` reader. `<` becomes `<=` with the constant shifted by one, and an equality is oriented by atom order.
  - Two equal views meet: `Equal`.
  - Otherwise both are respelled from their views before the peels and congruence: `<=` with the positive part on the left, rebuilt at the comparison's own carrier — or at `Nat` where every atom is a widened natural — through Core's sums and products.
- **Deleted.** The successor arms, `int_split_comparison`, `int_split_by_sign`'s use in alignment and `nat_comparison_of_int`. The negation-reading of `Bool/not` stays in front of it. The refinement probes keep `dual_comparison` and `successor_comparison`.
- **The differential.** A temporary instrument writes every pair where the old and the new alignment lead to different outcomes to a scratch log, over the prelude build, the grid and the suites. Every entry must be a newly held equation between two comparisons of one relation; each is listed in this specification with the total-order law that justifies it, and its row moves to the grid's held side. Anything else blocks the stage.

### Stage 9: the record, and the gate

- **Audit and deletion.** Every inventory row is checked; migration-only adapters and aliases are deleted.
- **New decisions.** "The carriers' algebra stays in conversion", with casts as its rejected alternative, replacing the reconciliation item in verdicts part 7. "One owner for the carriers' algebra", the cross-crate ownership, with this specification's rejected alternatives.
- **Revised records.** The design and soundness entries listed under documentation above; the `curios-core` and `curios-analysis` READMEs; `CLAUDE.md`'s invariants — `curios-algebra`'s direct dependency and no-`Term` rule, search outside the certifier's closure, declarations in Core, shared judgments in Analysis — and a routing row for a declaration, a family or an operation, naming the generated grid.
- **The roadmap and links.** A checked summary; parts 2 to 4, the numeric laws, verdicts part 7 and invariants part 3 relinked to the permanent documentation.
- **Retirement.** This file deleted once nothing references it, then the full gate, once, with every step launched in parallel and each npm package's steps chained.

## Baseline inventory

Stage 1's record, taken on the tree after part 0 landed (`1219f9f0`), and checked off row by row as the stages move them: a row whose stage column reads **moved** has its mathematics in `curios-algebra`, with what stays in Core named beside it. Line numbers are that tree's.

**Callers.** F — the fold, `reduce_intrinsic` (`curios-core/src/reduce/intrinsic.rs`), which both reducers run. C — both intrinsic converters (`curios-elab/src/convert/intrinsic.rs`, `curios-cert/src/kernel/convert/intrinsic.rs`). O — both outer Boolean probes (`curios-elab/src/convert.rs:1667`, `curios-cert/src/kernel/convert.rs:501`). I — inversion, through `peel_intrinsic` (`curios-analysis/src/invert.rs:245`). P — both refinement probes (`curios-elab/src/reduce.rs`, `curios-cert/src/kernel/whnf.rs`). V — the elaborator's packed-literal view. W — `spine`'s own window and position reasoning.

**Strength.** EQ — an equation: a rewrite, or a verdict of equality. DEC — a comparison folded to a `Bool` either way, each an established fact. IMP — impossibility: a clash conversion refuses on and inversion excuses an arm on. EQV — an equivalent residual. SUF — a sufficient residual. AGR — agreement only: equality or nothing, never a clash.

**Atoms.** Proj — keyed up to universe instances, through `project_erased_universes`. Syn — syntactic identity, universe levels included. Proof-insensitive — a quotient or remainder matched on its dividend and divisor alone.

### `Nat` and `Int` combinations

| Rule | Where | Callers | Atoms | Gate and charge | Strength and reconstruction | Evidence | Stage |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Successor-floor form | `Nat::decompose`, `rebuild` (`nat.rs:114`, `:124`); the `Nat(Succ)` arm | F, W | — | none | EQ; the representation, staying Core's | every `Nat` test | Core |
| Sum normal form: like terms merged, first-appearance order | `Nat::summands` `:149`, `literal_factor` `:169`, `linear` `:286`, `from_linear` `:447`, `sum_over_floor` `:311`, `sum` `:599`; the `NatAdd` arm | F, W | Proj on the monomial spine | every sum; uncharged | EQ; read-then-rebuild is the identity | grid "Nat under +" and "under \*"; `nat::tests::a_sum_in_normal_form_reads_back_as_itself` | **moved** in 2: `Combination::collect`; Core keeps `summands`, `from_linear` and the floor |
| Common-addend cancellation, as a multiset | `Nat::cancel_common` `:619`; the `NatSub` arm; `classify_nat` (`spine.rs:217`) | F, C, I, P | Proj | uncharged | EQ, EQV, IMP (a surviving floor against zero); cancelling nothing returns the inners untouched, less the shared floor | `nat::tests::cancellation_*`, `spine::nat_tests::peel_nat_*`, `laws_tests::every_nat_peel_verdict_*` | **moved** in 2: `Combination::cancel_common` and its `Deduction`; Core keeps the atoms and the rebuild |
| Signed sum normal form | `int_terms`, `int_monomial`, `int_linear`, `int_from_linear`, `int_sum`, `int_merged`, `int_negate` (`int.rs:37`–`:318`); the `IntAdd`, `IntSub` arms | F | Proj on the sorted factor vector | operand width, two literals only | EQ; a subtraction is a negative coefficient | `int::tests`, grid "Int" | **moved** in 2: collection as `Nat`'s; Core keeps the reading and `int_from_linear` |
| Signed cancellation | `int_cancel_common` `:437`; `peel_int_pair` (`spine.rs:44`) | F, C, I, P | Proj | uncharged | EQ, EQV, IMP (two unequal constants); untouched when nothing is shared | `int::tests::cancellation_*`, `laws_tests::every_int_peel_verdict_*` | **moved** in 2: `Combination::cancel_common` and `split_by_sign` over `Integer`, and their `Deduction` |
| Multiplication and the distribution gate | `Nat::multiply` `:210`, `scaled`, `spine`; `int_multiply` `:338`, `int_product` `:360`; the `NatMul`, `IntMul` arms | F | factors sorted by structural hash, interned | stuck when both sides hold two symbolic summands; operand width, and at `Int` a collection | EQ | grid "Nat under \*", `int::tests::a_product_*` | **moved** in 3: `distribute`, over factors ranked by their structural hash; Core keeps the reading, the spines and the gate |
| Distribution on demand | `Nat::normalize` `:530`, `has_stuck_product` `:578`; `int_normalize` `:391`, `int_has_stuck_product` `:369` | C (first step), F (the comparisons) | as above | neither side a literal and a stuck product present; a collection and two nodes per product; memo by node identity | EQ | grid `(x + 1) * (y + 2)` rows | **moved** in 3: `distribution_size` is the charge; the demand stays Core's |
| The atom-argument retry | `Nat::normalize_atoms` `:489`, `force_arguments` `:506`, `ordered_sums` `:465` | C, on `peel_nat_pair`'s `Stuck` | sums reordered by structural hash | once, probe-side, falling back to the original spelling | EQ | grid `Eq(f(x + y) + g(y + z), …)` | 6; the forcing stays Core's |
| Product-factor peel | `peel_monomial` (`spine.rs:93`) | C only | Proj, as a multiset | none | EQ on one multiset; SUF on one factor left each side; not in I | `spine::nat_tests::peel_monomial_*`, `a_shared_factor_leaves_conversion_a_residual_and_inversion_nothing` | the type **moved** in 2 (`Conclusion::Sufficient`, which `peel_intrinsic`'s `Deduction` cannot carry); the pairing **moved** in 3: `pair_factors`, over positions so Core rebuilds from the factors as written |
| Euclid recombination | `Nat::recombine` `:324`, `euclid_pair` `:354`, `same_monomial` `:403`, `same_factor` `:424`; `int_recombine` `:149`, `int_euclid_pair` `:179`, `int_same_factors` `:239` | F, in every merged sum | proof-insensitive quotient; at `Int` a copy counts only where every sign agrees | uncharged | EQ, shrinking only | grid "Nat under / and %", `Int` Euclid rows | **moved** in 3: `Recombination::natural`, `::integer` and `Combination::recombined`; Core keeps recognizing a remainder and matching its multiple proof-insensitively |

### Comparisons and defined operations

| Rule | Where | Callers | Atoms | Gate and charge | Strength and reconstruction | Evidence | Stage |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Nat` comparison: floors, a shared inner, non-strict bounds | `compare_nat` (`reduce/intrinsic/compare.rs:63`), `reduce_nat_compare` `:165` | F | Proj on the inners | distributes a stuck product first | DEC; an undecided comparison is rebuilt from the cancelled operands | `compare_tests`, grid "Nat comparisons" | **moved** in 3: `Comparison`, `of_floors`, `below`; Core keeps the order of observations |
| The bounds oracle | `nat_bound` (`reduce/intrinsic/nat.rs:60`) | F (comparison, Euclid split) | — | none; `NatShl` absent because it cannot charge | DEC; an under-report is a false equation | `nat_tests::bound_upper_bounds_every_closed_instantiation` | **moved** in 3: `Operation::upper_bound` and `bound_reads`, reached through `Intrinsic::algebra` |
| Domination | `nat_dominators` `:114`, `dominated` (`compare.rs:132`) | F | — | one strict subterm down | DEC | `nat_tests::dominators_upper_bound_every_closed_instantiation` | **moved** in 3: `Operation::dominators`, `Comparison::through_dominator`; the lint allow is gone |
| Divisibility | `apart_modulo` (`compare.rs:39`) | F (both comparisons) | coefficients | last, where equality is open | DEC, unequal only | `compare_tests::*_floors_apart_*`, grid controls | **moved** in 3: `apart_modulo` |
| `Int` comparison and preimages | `compare_int` (`reduce/intrinsic/int.rs:15`), `compare_preimages` `:55`; `int_preimage` (`int.rs:299`), `int_split_by_sign` `:469` | F | Proj | distributes a stuck product first | DEC | grid "Int", "Int against Nat" | **moved** in 3: the verdicts, and `Combination::natural` for the preimage |
| Literal divisors: zero, unit, self, floor law, Euclid split | `reduce_nat_division` `:177`, `nat_euclid_split` `:141` | F | — | a literal divisor | EQ; the proof is carried unreduced | `nat_tests::euclid_split_*`, `a_bounded_digit_*` | **moved** in 3: `Half`, `floor_law`, `cofactor`, `euclid_split` |
| Comparison alignment | `align_comparisons` (`reduce/intrinsic.rs:106`) through `successor_comparison` `:255`, `int_split_comparison` `:173`, `nat_comparison_of_int` `:160` | C | Proj | probe-side | EQ | grid seam, dual and split rows; `compare_tests::a_floored_le_meets_the_lt_it_spells` | 8 |

### `Bool`

| Rule | Where | Callers | Atoms | Gate and charge | Strength and reconstruction | Evidence | Stage |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Local laws: unit, absorber, idempotence, complement, `xor` cancellation, equality against a literal | `then_laws`, `bool_lattice_laws`, `complementary`, `bool_xor_laws`, `bool_eql_laws` (`reduce/intrinsic/laws.rs:11`–`:94`) | F | Syn, a comparison against its dual | a stuck `&&`/`||` keeps its right operand as written | EQ | grid "Bool" | **moved** in 4: `Operation::boolean_identity` over a `BooleanPair`; Core reads literals, identity, negation and nesting, and builds the negation as `xor(·, true)` |
| Leaf-set peel | `peel_bool`, `bool_leaves` (`spine.rs:147`, `:165`) | C, I | Syn | none | AGR | `spine::commutative_tests` | **moved** in 4: `same_leaves` |
| Tree normalization | `normalize_bool` (`reduce/intrinsic.rs:39`) | C | — | forces every leaf; a collection | EQ | grid associativity rows | 4; the forcing stays Core's |
| The truth table | `decide_bool` (`truth.rs:143`), `is_bool_connective` `:129`, `BOOL_ATOM_CAP = 8` | C, O | Syn, a comparison and its dual one atom | declined past eight atoms; charged before the first assignment | AGR | `truth_tests`, `laws_tests::every_truth_table_decision_*`, grid "Bool, at the table's cap" | **moved** in 4: `Formula`, `BOOL_ATOM_CAP`, `evaluation_size`, `agree`; Core reads and forces |
| Duals | `dual_comparison` `:223`, `dual_of_negated` `:196` | C, F, P, the table | total orders only; `Flt` excluded | none | EQ | grid negation rows | **moved** in 4: `Operation::negation`, read back through `Intrinsic::comparison`; key construction stays with the probes |
| Symmetric operations | `peel_symmetric` (`spine.rs:62`) | C, I | Syn | none | AGR | `spine::commutative_tests::peel_symmetric_*` | **moved** in 4: `Operation::commutes`; `Flt`'s equalities and `Bin`'s `eql` declared in 5b, so no case stays term-level |

### Bitwise operations and shifts

| Rule | Where | Callers | Atoms | Gate and charge | Strength | Evidence | Stage |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Nat/and`, `or`, `xor`: zero, idempotence, self-cancellation | `nat_bitwise_laws` (`laws.rs:116`) | F | Syn | none | EQ | grid "Nat bitwise and shifts" | **moved** in 3: `Operation::bitwise_identity` |
| Shift by zero, of zero | `nat_shift_laws` `:149` | F | — | none | EQ | the same | **moved** in 3: `Operation::shift_identity` |
| Left shift as a coefficient and as a power; right shift in two steps | `then_coefficient`, `then_power`, `then_split_shift` (`reduce/intrinsic/nat.rs:308`–`:375`), `reduce_nat_shl`, `reduce_int_shift` | F, both carriers | — | `shift_bound`, charged before the coefficient exists | EQ | `shift_tests`, grid shift rows | **moved** in 3: `power_of_two`; the rewrites are term construction and stay Core's |

### Words and positions

| Rule | Where | Callers | Atoms | Gate and charge | Strength and reconstruction | Evidence | Stage |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Prefix cancellation, runs merged, windows fused, regrouping | `peel_prefix`, `push`, `against_identity`, `peel_bin`, `peel_list`, `regroup`, the segment readers and `reassemble_*` (`spine.rs:405`–`:844`); the `BinEql` arm | C, I, F | chunks Syn; windows by length and position | fusion takes the second window's proof unchanged | EQ, EQV, IMP (a packed head or a positive residual against the identity); a list head never clashes | `spine::monoid_tests`, `aggregates::monoid_tests`, `laws_tests::every_bin_*`, `every_list_*` | **moved** in 5a: `Word::push`, `Word::strip_common_prefix`, `Stripped`; reading, rebuilding and regrouping stay Core's (`words`, `spine`) |
| Positions through windows and concatenations | `peel_position`, `rooted`, `same_position`, `operand_offsets`, `offsets_in`, `nat_equal` (`spine.rs:253`–`:363`) | C, I, W | roots Syn | no bound read | AGR | `spine::position_tests`, `laws_tests::every_position_*` | **moved** in 5a: `same_position`, `Word::offsets_of`; rooting stays Core's; numbers compared by `Nat::same` |
| Concatenation normalization | `normalize_concat` (`free_monoid.rs:703`), `FUSION_CAP = 64` | F | — | fusion past the cap declined; two payloads charged | EQ | `free_monoid_tests`, `cost_tests` | **moved** in 5b: `join`; building, the cap and the charges stay Core's |
| Measures and windows | `FreeMonoid::segments`, `measure`, `measured_window`, `window`, `bin_measure`, `concatenated` (`free_monoid.rs:513`–`:626`) | F (`len`, `get`, `slice`) | — | lazy; stops where the seam stops | EQ | `aggregates::monoid_tests` | **moved** in 5b: `total`, `locate`; the segment reading and `bin_measure` stay Core's |
| Homomorphism folds and the seam window | `reduce_homomorphism`, `bin_shape`, `list_shape`, `nat_sum`, `bin_piece`, `list_piece`, `element_of_run`, `seam_window` (`reduce/intrinsic/free_monoid.rs`) | F | — | charged per piece | EQ | `free_monoid_tests` | **moved** in 5b: `seam_window` over `Alphabet::difference`; the homomorphism's execution and the pieces stay Core's, and its declarations are stage 7's |
| `len` and `get` through `map`, `map` distributing, the identity `map` | the `ListLen`, `ListGet`, `ListMap` arms; `is_identity` (`reduce/intrinsic.rs:78`) | F | the binder check is term-level | the eta probe charged | EQ | grid "List" | 7, declared as homomorphisms with the law families the generator reads; `is_identity` and fold execution stay term-level |
| The packed-literal solving view | `packed_literal_view`, `known_len`, `split_against` (`curios-elab/src/convert/intrinsic.rs:200`–`:329`) | V | — | a literal against a spine of known segment lengths | EQV; IMP on a length clash | `tests::algebra::a_metavariable_is_solved_through_the_packed_literal_view` | **moved** in 5b: `split`; `known_len` and the proposals stay the elaborator's |

### Carrier conversions

| Rule | Where | Callers | Strength | Evidence | Stage |
| --- | --- | --- | --- | --- | --- |
| `Byte` and `Nat` invert each other | the `ByteToNat`, `NatToByte` arms | F | EQ | grid "Byte against Nat" | **moved** in 5b: `round_trip`, `Operation::undoes`, `Intrinsic::undone` |
| `Nat/to_int` as a semiring embedding, and its preimage | `int_of_nat` (`int.rs:280`), `int_preimage`; the `NatToInt`, `IntToNat` arms | F | EQ | grid "Int against Nat" | **moved** in 3: sums and products through the algebra, `Combination::natural`; the traversal stays Core's |
| Packed regrouping at the other grain and back | the `BinReinterp` arm | F | EQ | `aggregates::monoid_tests` grouping rows | **moved** in 5b: the round trip; the literal regrouping stays Core's |
| `Flt/of_le_bytes` of `Flt/to_le_bytes` | the `FltOfLeBytes` arm | F | EQ | `numeric::flt_tests` | **moved** in 5b: the round trip, one way |

Every other `Flt` fold, and `Int`'s bitwise operations and division, evaluate closed values only; that is `curios-num`'s and stays so.

### Atom identity as the call sites use it

- `Nat`: a summand is keyed on its monomial spine projected, and a monomial's factors are ordered by the structural hash of the unprojected term. `Int`: a monomial is keyed on its projected factor vector, sorted the same way. A quotient or remainder in Euclid's recombination is matched proof-insensitively. From reading, and not yet confirmed by a test: two monomials that differ only in the universe instance of a factor can sort differently and then fail to merge. That is incompleteness, in the declining direction.
- `Bool` leaves, truth-table atoms, word chunks and symmetric operands are compared syntactically, universe levels included. A comparison and its dual are one truth-table atom.
- The refinement stores key on the written spelling, with the dual and successor probes.

### Relations two carriers decide by different procedures

- The `<`/`<=` seam: since part 0 both carriers read it through `successor_comparison`, but `Int` alone then splits both sides by sign (`int_split_comparison`) and pulls widened naturals back to `Nat` (`nat_comparison_of_int`). Stage 8 replaces all three.
- Divisibility is one argument coded twice: `compare_nat` compares the two floors, `compare_int` the difference of the constants.
- Euclid's recombination at `Int` requires every sign to agree; at `Nat` there is nothing to check.
- A literal divisor's unit, self, floor and split laws exist at `Nat` only; `Int` division folds closed values alone.
- `Nat/and`, `or` and `xor` carry local laws and `peel_symmetric`'s commutativity; their `Int` twins carry neither, and the grid states no `Int` bitwise row. This is a law true at both carriers and decided at one, the gap the generated grid exists to expose; closing it is outside behavior preservation.
- The bounds oracle and domination exist for `Nat` alone, so an `Int` comparison reaches them only through the preimage pull-back.

### Seam call sites the part 0 change makes redundant

There are twenty calls of `Nat/Lt/le_succ_of_lt` and `lt_of_le_succ` in `/std`, not counting the two lemmas' own recursion, classified by shape:

- **Sixteen** are checked against a fixed expected type, so the lemma's argument alone now serves.
- **Four** feed a lemma whose implicit arguments are inferred from them — `WellFounded.crs:90`, `Nat/div_mod.crs:51`, `Nat/Lt.crs:92` and `:109` — and still need the lemma, or `@` arguments written.

A probe confirmed both shapes. The twenty sites were not verified one by one, and none has been removed.

### Evidence gaps and findings

- **Inversion.** Nothing at the analysis level puts the peel's outcomes to inversion. Its evidence is `tests::perimeter::index_tests` and the kernel's elimination tests. `a_shared_factor_leaves_conversion_a_residual_and_inversion_nothing` now pins the restriction where it is enforced today.
- **Kernel history.** No test routed a peel residual through the kernel's active `History`: `kernel/convert/recursion_tests` exercised history on recursive groups only. Closed in stage 6 by `a_peel_residual_is_compared_under_the_active_history`.
- **A lint allow.** `dominated` (`compare.rs:132`) carries `#[allow(clippy::too_many_arguments)]`, because its seven arguments are two (floor, inner, whole) triples. The comparison view removes the need for it in stage 3.
- **Solving through a law** (the stage 7 audit). A law decided by operand identity or by the truth table decides an equation between known atoms and solves no metavariable standing for one: commutativity through `peel_symmetric`, `&&` and `||` through their leaf sets, `Bool` distribution through the truth table. Incompleteness in the refusing direction, recorded by `tests::laws::audit::a_metavariable_is_solved_through_every_law_that_can_solve_it`, which holds every other declared law to solving.
- **Pinned by stage 1.** `tests::algebra` covers a law at an instance whose substitution changes its atoms, a metavariable solved through the cancellation, and one solved through the packed-literal view. `nat::tests` and `int::tests` cover read-back identity.

### Cost

**The instrument.** Two profile streams are written by the build scripts `cargo x clippy` runs with `--all-features`:

- `curios-prelude-archive/.artifacts/profile.tsv` for elaboration and erasure;
- `curios-prelude/.artifacts/profile.tsv` for certification. Its `recheck_module` span times the kernel's whole walk and distinguishes no judgment; that is [the certifier measured](../verdicts/01-measured-spec.md)'s.

**To retake.** Run `cargo x clippy` after the stage's change, then fold each stream with `cargo run --all-features --package curios -- profile <stream>`. The streams are instrumented debug build scripts, so read them this way:

- a duration is noisy and inflated;
- call counts and allocated megabytes are the stable figures;
- a stage's report names the span and the resource it compares.

**The baseline**, at `8a182528`:

| Stream | Span | Total | Calls | Allocated | Peak |
| --- | --- | --- | --- | --- | --- |
| elaboration | `elaborate_and_zonk_with_prelude` | 110.9 s | 2 | 36 724 MB | 447.3 MiB |
| elaboration | `erase_unit` | 13.1 s | 2 | 1 235 MB | |
| elaboration | `nat::cancel_common` | 5.08 s | 50 088 | 1 271 MB | |
| elaboration | `nat::sum_over_floor` | 2.94 s | 47 836 | 107 MB | |
| elaboration | `nat::multiply` | 1.28 s | 6 604 | 51 MB | |
| elaboration | `nat::linear` | 1.25 s | 150 697 | 89 MB | |
| certification | `recheck_module` | 42.1 s | 2 | 8 034 MB | 140.0 MiB |
| certification | `nat::cancel_common` | 2.53 s | 33 320 | 110 MB | |
| certification | `nat::sum_over_floor` | 1.13 s | 27 949 | 50 MB | |
| certification | `nat::linear` | 0.59 s | 95 056 | 52 MB | |
| certification | `nat::multiply` | 0.34 s | 2 472 | 16 MB | |
| certification | `truth::decide_bool` | 0.03 s | 41 | 2 MB | |

Nested spans overlap, so the rows do not add up.

**After stage 2.** Collection and cancellation now come from `curios-algebra`. Every call count other than `nat::linear`'s is unchanged, and so is every verdict: the old implementations were kept as a debug-build oracle through the prelude's elaboration and certification and the affected suites, and never disagreed.

| Stream | Span | Total | Calls | Allocated |
| --- | --- | --- | --- | --- |
| elaboration | `elaborate_and_zonk_with_prelude` | 111.0 s | 2 | 36 687 MB |
| elaboration | `nat::cancel_common` | 3.17 s | 50 088 | 128 MB |
| elaboration | `nat::sum_over_floor` | 3.15 s | 47 836 | 116 MB |
| elaboration | `nat::linear` | 0.52 s | 50 521 | 32 MB |
| certification | `recheck_module` | 42.1 s | 2 | 8 011 MB |
| certification | `nat::cancel_common` | 1.61 s | 33 320 | 81 MB |
| certification | `nat::sum_over_floor` | 1.24 s | 27 949 | 54 MB |

What moved, and why:

- **Cancellation allocates a tenth of what it did.** A summand is keyed once, as a handle, instead of being looked up by a linear scan over projected terms.
- **`nat::linear` has fewer calls.** Cancellation collects through the algebra directly rather than through it.
- **Collection allocates about a tenth more.** Each summand now carries a monomial vector, which is why `nat::sum_over_floor` rose from 107 MB to 116 MB.

Totals are within noise.

**After stage 3.** Distribution, Euclid's recombination, the comparison facts, the bounds, the literal-divisor split, the bitwise and shift identities and the embedding's arithmetic now come from `curios-algebra`, over operations `Intrinsic::algebra` declares. Every call count other than `nat::linear`'s is still the baseline's, and the eleven oracles — each confirmed live by a mutation it caught — never disagreed.

| Stream | Span | Total | Calls | Allocated |
| --- | --- | --- | --- | --- |
| elaboration | `elaborate_and_zonk_with_prelude` | 110.7 s | 2 | 36 685 MB |
| elaboration | `nat::cancel_common` | 3.13 s | 50 088 | 135 MB |
| elaboration | `nat::sum_over_floor` | 2.13 s | 47 836 | 103 MB |
| elaboration | `nat::multiply` | 1.23 s | 6 604 | 54 MB |
| certification | `recheck_module` | 41.4 s | 2 | 8 009 MB |
| certification | `nat::cancel_common` | 1.58 s | 33 320 | 86 MB |
| certification | `nat::sum_over_floor` | 0.78 s | 27 949 | 47 MB |

What moved, and why:

- **Recombination is cheaper.** It collects through the algebra without re-reading the sum through `nat::linear`, whose calls fell from 50 521 to 2 615. `nat::sum_over_floor` dropped a third in both checkers as a result.
- **Cancellation allocates about 5% more.** The atom table now keeps the term each atom stands for.

**After stage 4.** The Boolean laws, the leaf sets, the truth table's evaluation, the dual table and commutativity now come from `curios-algebra`, over the connectives `Intrinsic::algebra` declares at `Bool`. Five oracles, each confirmed live by a mutation it caught, never disagreed. Every call count and allocation total is stage 3's — `truth::decide_bool` still 25 calls and 2.1 MB in elaboration and 41 calls and 1.8 MB in certification — and the durations are within noise.

**After stage 5a.** The word's normal form, the prefix strip and its verdicts, where an operand begins and when two positions are one now come from `curios-algebra`. Three oracles — `peel_bin`, `peel_list` and `peel_position` against their old bodies, the old `nat_equal` given the numeric identity — each confirmed live by a mutation it caught, never disagreed. Rebuilt against the old, gated `nat_equal`, the oracle also stayed silent through the prelude: the widened identity changes no `/std` verdict. Every call count and allocation total is stage 4's — `nat::cancel_common` still 50 088 calls and 135 MB in elaboration and 33 320 calls and 86 MB in certification — and the durations are within noise.

**After stage 5b.** Locating a window, the seam walk, a concatenation's normal form, the packed-literal split and the round trips now come from `curios-algebra`, and `peel_symmetric` reads commutativity off declarations alone. Six oracles — the located window, the seam walk replayed over the measures the new walk took, the normal form, the split, `peel_symmetric`, and each round-trip arm — never disagreed, each confirmed live by a mutation it caught. Two of those mutations first went uncaught, because nothing reached their branches: a window located across operands, which only runs past `FUSION_CAP` produce, and a packed literal split with a trailing operand of unknown length, which the prefix strip decides first unless the head is symbolic. `reduce::intrinsic::free_monoid_tests::a_window_across_runs_past_the_fusion_cap_is_the_window_over_their_bytes` and `tests::algebra::a_metavariable_is_solved_as_the_rest_of_a_split_packed_literal` now reach them. Every call count is stage 5a's, and allocation totals moved by under 0.1% (elaboration 36 690 MB against 36 685). The durations were taken beside another session's build and are not comparable.

**After stage 6.** Both converters run `curios-analysis`'s `convert_intrinsics` and keep only their discharge. Nine mutations, one per discharge path, were each caught: the kernel's residual compared from a fresh history, its operands compared without stopping at the first failure, at `Type` rather than their declared types, or accepted across two operations, its result levels ignored; and the elaborator's residual dropped, its operands enqueued at `Type`, its result levels ignored, its packed view withdrawn. Three of them first went uncaught, because nothing tested those paths in either checker before this stage. The kernel's and the elaborator's result levels are now held by `two_polls_of_one_cell_at_two_ground_levels_do_not_convert`, and the elaborator's typed operands by `tests::perimeter::eta_tests::an_intrinsic_applied_to_two_proofs_converts_at_their_proposition`, a new row of the perimeter matrix both checkers accept. The widening fired 217 times in the prelude's elaboration — 207 on pairs identical once their solved metavariables were substituted, 10 by the cancellation — and 3 times in its certification. The `/std` archive is byte-identical with and without it, so it changes nothing the prelude elaborates or certifies. Every call count is stage 5b's except `nat::cancel_common`, which gains those 10 calls in elaboration (50 098) and 6 in certification (33 326). Allocations are unchanged: elaboration 36 684 MB, certification 8 009.5 MB, `convert::outcome` 67 708 calls and 2 329 MB.

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
| `align_comparisons`, `int_split_by_sign`, `nat_comparison_of_int` | The per-carrier alignment tables, replaced in stage 8 by one alignment over the canonical view. | Nothing: the refinement probes' dual and successor spellings are key construction and stay with the refinement stores. |
| `curios/src/tests/laws.rs` | Hand-stated held rows for laws a declaration now states, replaced in stage 7 by the generated grid. | The controls, the refused candidates with their reasons, and the rows for term-level laws. |

An old function name may remain as a small term-facing wrapper where it remains the right public interface. Its old mathematical body must be gone. Conversely, deleting an entire representation file is not a completion criterion. The inventory and caller audit establish that each law has one production owner.

## Verification

Verification is part of the implementation stages. The baseline is executable evidence gathered against the tree being migrated; this document's source inventory does not substitute for it.

- **Independent Algebra evidence.** Test algebraic operations with simple atoms and concrete valuations through `curios-num`, without importing Core or either checker. Check equivalent residuals in both directions, sufficient residuals in the implication direction, and impossibility against all sampled admissible valuations. Exercise coefficient multiplicities, signs, zero factors, Euclidean recombination, and packed/list distinctions. Handwritten controls remain necessary beside any declaration-driven cases.
- **Adapter evidence.** Check atom identity at universe and proof boundaries; retention of original terms when no progress occurs; reconstruction order and sharing; proof and element-type provenance; normalization retry behavior; and round trips between views and the term shapes they represent. A hash collision must not establish equality.
- **Differential migration evidence.** Compare old and new results, logical strength, reconstructed residuals, refusals, and solving outcomes on the same inputs. Stateful procedures run with isolated equivalent state, so the first run cannot solve a metavariable or warm a cache for the second. Any newly accepted or newly refused case is a finding to resolve against the behavior-preservation contract. Remove the temporary oracle when its replacement is established.
- **Both drivers and inversion.** Retain every held law and refusal control in the existing grids. Exercise solved-meta substitution, the packed-literal solving view, rollback and commitment, proof-typed congruence, recursive kernel history, and inversion's exclusion of sufficient product-factor conclusions. Include the outer Boolean path as well as intrinsic pairs.
- **Operational behavior.** Check fixed caps, refusal under exhausted budgets, charging before expensive work, lazy right operands, incremental window measurements, product-distribution gates, default-stack traversal, and sharing on repeated subexpressions. A symbolic list-head mismatch must never take the packed-literal clash path.
- **Judgment properties.** Carry the existing soundness evidence forward and add targeted symmetry, transitivity, and substitution cases, including substitutions that change atom structure and solved metavariables. Record the scope of these tests; a finite grid does not establish a general metatheorem or completeness claim.
- **Generated-grid evidence.** Every generated row is held through both checkers and against its carrier's semantics at closed values. The generator itself is tested: removing a carrier from a declaration removes its rows, and a declaration whose kind a carrier's procedure does not decide fails at that carrier rather than passing with the row absent. One mutation per declaration kind is caught.
- **The audit.** Freeness cases at every intrinsic case inversion distinguishes, and the symmetry, transitivity and substitution cases above run over the declared laws. The audit's record names what it covers and what it does not.
- **The contract and the view.** Each clause of the published contract is a held row and its complement a control. The view is checked against concrete valuations: two comparisons with equal views agree at every sampled valuation, and a view is independent of the spelling, association and order of the sums it reads.
- **Stage 8's differential.** Its only admissible differences are rows that become held because two comparisons of one relation now meet at a carrier where they did not; each is listed with its justification, and any other difference blocks the stage.
- **Cost and integration.** Measure prelude elaboration and certification before and after each Core or shared-engine stage, naming the measured stage and resource. Investigate allocation, repeated preparation, and build-time regressions before closing a stage. Use the repository's implementation validation gates and preserve the build dependency boundaries.

## Documentation and design record

The implemented mission and crate-local decisions belong in `curios-algebra`'s README; its algorithms and API contracts belong in rustdoc. Core documents the declaration, atom, demand, and reconstruction adapters. Analysis documents the shared procedure and its admissible results. Driver documentation explains the state each checker continues to own.

The cross-crate ownership decision and its alternatives belong in `documentation/design/`. The soundness entries are revised as each implementation moves, retaining their assumptions, evidence, and grades. Moving a rule into Algebra or running it through both checkers does not raise its evidence grade by itself. The binder-sensitive map check and fold equations retain explicit accounts of their special treatment.

The existing decisions about spelling, on-demand sums, refinement lookup, and proof preservation remain applicable during the consolidation. Update statements about where an algorithm lives when its migration lands; revise a behavioral decision only when its replacement is implemented and justified. Canonical refinement keys and a relational procedure cannot be cited as reasons to remove today's restrictions during extraction.

The records this delivery revises as each stage lands:

- [A law is decided where it neither respells nor invents](../../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md): a held law is a declaration's generated row, and what a lift owes is stated per declaration kind.
- [A comparison is spelled one way when it is stuck](../../design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md): the judgment's alignment becomes one procedure over the canonical view; the refinement probes keep their spellings.
- [A sum is merged when it is forced, not when it is built](../../design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md): where the merge's mathematics lives.
- [Open fold laws and the sum normal form](../../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md), [Intrinsic fold laws and the free-monoid peel](../../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md), [The bounds oracle and the division family](../../soundness/per-term-rules/the-bounds-oracle-and-the-division-family.md) and [The binary64 model and its NaN rule](../../soundness/per-term-rules/the-binary64-model-and-its-nan-rule.md): owners, the generated grid as evidence, and the mutations it runs.
- [Index inversion and K](../../soundness/per-term-rules/index-inversion-and-k.md): the restricted interface and the freeness audit.
- A new design decision: the carriers' algebra stays in conversion, with casts as its rejected alternative, replacing the reconciliation item in [the certifier's checked evidence](../verdicts/07-checked-evidence-spec.md).

`CLAUDE.md` gains the direct-dependency and no-`Term` invariants for Algebra, the rule that search stays outside the certifier's dependency closure, the ownership of declarations in Core and shared judgments in Analysis, and routing for changes to an algebraic structure, operation or declaration kind, the generated grid among what such a change must inspect. The roadmap records the foundation separately from the campaign's other parts. References in dependent specifications continue to identify the capabilities they actually require.

## Rejected for this delivery

- **Deferring the crate until after a Core-only consolidation.** The consolidation must establish and exercise the dependency boundary. Leaving mathematics tied to `Term` would defer the central extraction risk.
- **Moving the current files wholesale into Algebra.** Their term representation, reduction, and proof responsibilities belong in Core and the drivers. The crate must be usable and testable over independent atoms.
- **A general host trait that exposes the whole compiler.** It would allow the mathematical implementation to depend on conversion, solving, and reconstruction through callbacks. Narrow observations and resource accounting are sufficient for the intended boundary.
- **Treating every result as equality, clash, or an unqualified residual.** The distinction between equivalent and sufficient obligations is necessary for inversion and must be enforced by the interface.
- **Eager canonicalization as part of extraction.** It changes demand, spelling, and cost, and would require the canonical-key work excluded from the consolidation. The canonical linear view is not this: it rebuilds no term and keys nothing.
- **A permanent compatibility engine.** Two production implementations would preserve the ownership problem. Migration adapters are temporary; representation adapters remain because they have an ongoing responsibility.
- **Building unused future solvers or representation variants.** The initial interfaces are exercised by current consumers. Later capabilities extend those interfaces from measured requirements, which is why canonical refinement keys, polynomial unification proposals, Boolean normal forms and one internal sequence carrier are deferred on the roadmap to a consumer rather than scheduled here.
- **Stating each carrier's rows by hand for a declared law.** The convention the grid asks for is what the `<`/`<=` seam slipped past; generation makes the row at every carrier a consequence of the declaration.
- **Keeping an alignment per carrier after the migration.** Two procedures deciding one relation drift, which is how the seam was decided at `Int` and not at `Nat`; one alignment over the view decides it once.

## Completion criteria

- `curios-algebra` exists, depends directly on `curios-num` alone, names no Core or checker types, and is used by production reduction and both checkers.
- Every baseline inventory entry has a single mathematical implementation in Algebra or a documented term-level responsibility with its appropriate owner. All replaced mathematical bodies and permanent old-engine fallbacks are gone.
- Core owns term views, carrier-appropriate atom identities, demand, provenance, and reconstruction. Declarations expose only the implemented algebraic capabilities.
- Analysis owns the common algebraic comparison strategy and typed congruence preparation. Both drivers use it, including the outer Boolean path; neither repeats the algebraic chain.
- Equivalent and sufficient obligations are distinguished in the interfaces. Inversion can consume only admissible deductions, and a failed sufficient condition cannot become an algebraic clash.
- Elaboration owns proposal selection and assignment commitment. Kernel residual checking retains its active history. The certifier's dependency closure contains no elaboration search.
- Held laws, refusal controls, solving behavior, proof preservation, spelling stability, demand gates, and fixed limits retain their baseline behavior, except the rows stage 8 lists. Differential findings and cost changes have an explicit resolution.
- Every operation has a declaration, every declared law is a generated row held at every carrier its declaration covers, and one mutation per declaration kind is caught. The existing theory's audit is recorded with its scope.
- Each numeric carrier's atom identity and order are stated once, the canonical linear view serves the shared alignment, and the contract of what conversion decides is published with its rows.
- Independent algebra evidence, adapter evidence, both-driver coverage, and the implementation validation record are in place. Durable contracts and design decisions have moved to their authoritative documentation, the decision that the carriers' algebra stays in conversion among them.
- The campaign's later parts remain distinguishable from the completed foundation, and the retirement requirements below are satisfied.

## Retirement

Retire this specification when its implementation, verification, deletion audit, and durable documentation are complete. The campaign's later parts do not block retirement, and this file is not rewritten to hold their work.

Move the implemented contracts, decisions, rejected alternatives, and evidence to their authoritative homes: crate READMEs and rustdoc, cross-crate design decisions, the soundness perimeter, and the contributor invariants. Replace the roadmap's pending part 1 entry with a checked summary of the implemented capability.

Update parts 2 to 4, [the numeric laws](../numeric-laws-spec.md), [the certifier's checked evidence](../verdicts/07-checked-evidence-spec.md) and any other dependent specification to link to the implemented foundation's permanent documentation rather than this working document. Reassess their dependencies against what actually landed without treating extraction as delivery of a broader theory. Verify that nothing still references this specification's filename, then delete it.
