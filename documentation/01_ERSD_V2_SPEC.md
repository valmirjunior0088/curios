# Ersd v2

Working implementation specification for replacing the erased-term representation, the Core erasure boundary, the Ersd optimizer, the archived Ersd prelude prefix, and the Ersd-owned lowering into the landed Cont v2 interface. This document supersedes the previous specification of the same name: that specification's objective ("a single principled contract, consumed exhaustively") was refuted by measurement during its own implementation run, and the run it produced is preserved in history as a salvage quarry — nothing in it is normative. This specification derives Ersd's scope from the job the pipeline forces it to fill, and admits nothing else.

While this specification is active, it owns the intended architecture, migration order, and acceptance criteria. When the project lands completely, move durable local contracts into `curios-ersd`, `curios-core`, and `curios-prelude` crate and module documentation, update `AGENTS.md` and `ROADMAP.md`, verify that no document still depends on this filename, and delete this working specification.

## Purpose

Cont v2 is the structural half of the backend. Its animating decision is a delay: closure conversion is postponed until structural optimization is finished, so the optimizer sees direct calls instead of opaque closures. Ersd v2 is the symmetric semantic half: desugaring is postponed until semantic work is finished, so the semantic transformations see meaning instead of encodings.

Ersd is the erased program in flat, explicit form, and the three transformations the compiler can only perform while it holds that form: drop what will never run (pruning), run what is already decided (partial evaluation), and re-base what would otherwise exhaust the stack (worker/wrapper). It then compiles meaning into mechanism once, in one place — the lowering into Cont.

Each transformation has an empirical forcing proof that it can live nowhere else in the pipeline:

- **Pruning** needs top-level item granularity and effect summaries. Both dissolve into the entry function's initialization code at CPS conversion, so Cont's reachability pass cannot recover them: an unused-but-eager combinator web (the `Parse`/`Json`/`Http`/`Task` CAFs) is reachable-as-code from `main` even though running it is unobservable.
- **Partial evaluation** needs an interpreter over legible semantic data. Cont, holding the fully inlined and specialized `Fmt` combinator web, cannot collapse it — closed-term evaluation is not among its structural passes and is not expressible over CPS and a physical alphabet. The corpus makes this a class, not a case: a dependently-typed API's static argument was already evaluated by the elaborator at the type level, so its term-level collapse is always available.
- **Worker/wrapper** needs algebraic monoid laws attached to semantic identities Cont's alphabet no longer carries. It is a correctness transform, not an optimization: without it, deferred-context recursion (`… + 1`, `concat(rec(t), d)`) overflows the native stack on corpus-sized input.

Everything else Ersd might do is either Core's job (type theory), Cont's job (all structural and local optimization — folding, dead code, inlining, contification, specialization), or a representation guarantee's job (see the admission rule).

## Admission rule

This is the anti-creep clause, taken from the repository's own design practice, and it is falsifiable.

- **A representation guarantee beats an optimizer pass.** The rope representation makes naive accumulation and peel loops linear with no compiler recognition; the fold forms make primitive recursion a bounded loop with O(1) stack by construction; checked 32-bit arithmetic replaces range analysis. A pass is admissible into Ersd only when no representation guarantee can absorb its job. Today exactly the three transformations above qualify.
- **Every fact names its consumer.** Every stored field, derived analysis, verifier rule, behavior-contract entry, and printed detail must have a named consumer in this specification. A fact without a consumer is deleted, not kept for later.
- **No speculative surface.** Infrastructure is built when its consumer lands, sized by that consumer, and extended only when a new consumer arrives.

## Borders

### Erasure — the Core border

Erasure lives in `curios-core` (a lowering belongs to the crate holding the source representation) and is a **transcription**: it changes representation, not meaning, and makes no encoding decisions.

Erasure does exactly this:

- Sort-driven erasure: drop type- and prop-valued binders, fields, and arguments; discharge propositions; collapse a schema whose relevant width is one to the bare field (newtype collapse); map compiler-emitted literal certificates to their carriers.
- Semantic identity selection: emit the most precise Ersd form available — Bool switches, Nat dispatch versus Nat induction (read off Core's motive and induction-hypothesis structure), sequence folds, schema-carrying products, variants, and projections. This is transcription because the identity is already present in Core; erasure preserves it rather than encoding it away.
- Schema registration: structures and inductives register product schemas, variant families, and constructors once, from their declarations.
- ANF sequencing under the **operand law**: every source subexpression is erased to exactly one operand, bound as a statement in the innermost open block, in evaluation order; any structural reuse of a subexpression (a scrutinee's predecessor, a motive instantiation) references the bound atom and never re-erases the term. This law is what makes effect once-ness and ordering structural properties of the representation instead of per-pass discipline; the legacy `erase_aliased_match` machinery was a patch for a representation that lacked it, and must not be ported.
- Item ordering: top-level items are erased in dominance order so every reference is backward.

Erasure must **not**: choose carriers (Bool→Nat, Byte→Nat, Io→Bin), fix tag layouts, encode dispatch, desugar folds, or optimize anything. Every one of those decisions belongs to the lowering and appears exactly once, in the desugar table below.

Division of mechanics: `curios-core` classifies and traverses; the checked builder in `curios-ersd` owns representation construction (blocks, statements, interning, schema and recursion registration, finalization). The erasure source should read as "walk Core, classify, call the builder"; direct manipulation of Ersd internals from Core is boundary leakage.

Erasure reads; it should not re-derive. Elaboration has already computed every type erasure needs; re-entering `infer`/`reduce` during erasure and re-synthesizing primitive types are measured debt inherited from the legacy path. Reduce re-entry toward zero as the work proceeds; this is a tracked direction, not a day-one gate.

### Lowering — the Cont border

The landed `CpsModule` construction API and node alphabet are the frozen target. The lowering (`curios-ersd`, depending on `curios-cont`) owns the one-way door from meaning to mechanism: all desugaring and the CPS conversion, stated once, normatively:

| Ersd form | Cont encoding |
| --- | --- |
| Bool values and `SwitchBool` | Nat carrier `{0,1}`; `Switch` |
| Byte values and Byte operations | Nat carrier; Nat operations |
| Io constants and `IoEql` | `Bin` carrier; `BinEql` |
| `Product` / `Project` | `Tuple` / `TplGet` at the post-erasure field index |
| `Construct` / `MatchVariant` | `Tuple(tag, payload…)` with the constructor discriminant as tag; `TplGet(0)` + `Switch` + per-arm payload `TplGet` |
| single-relevant-field schema | the bare field (no tuple, no tag) |
| `SwitchNat` | `Switch` with literal cases and default |
| `FoldNat` | a synthesized bounded induction loop |
| `FoldSequence` | a synthesized right-fold loop over the grain's sequence primitives |
| sequence operations (including variadic concat/build) | `LetPrim` chains |
| `LetFunctions` / recursive groups | `LetFun` / `RecInit` preserving member scope and eager-init order |
| blocks and terminators | continuations; `Return` → apply the return continuation, `Exit` → `Exit`, `Unreachable` → `Unreachable` |
| `Cell` / `Foreign` / `Intrinsic` | their `return_to` node forms |

Encoding choices are lowering-internal and may evolve (see named futures: layout) without touching erasure, the representation, or the transformations. The representation's verifier rejects computed-only recursive cycles, so the lowering re-detects no recursion diagnostics.

## Representation

The erased program as flat, first-order data:

- Module-owned arenas addressed by typed `u32` IDs; deterministic traversal, mutation, and compaction; tombstoned removal without ID reuse.
- Ordered top-level items with an entry body — pruning's granularity and the record of eager initialization order.
- Blocks of single-operation statements over atomic operands, closed by explicit terminators — the operand law's home, and the reason effect ordering is statement order.
- The alphabet of erased Core's vocabulary: alias, saturated application, scalar operations, sequence operations, schema-carrying product/construct/project, variant match, Bool and Nat switches, Nat and sequence folds, cells, foreign calls, intrinsics; constants are the leaf scalar and packed-binary domain.
- First-class fold forms are the corpus's loop forms and carry an O(1)-native-stack guarantee by construction; general recursion is the only stack risk and is exactly worker/wrapper's domain.
- Functions store no capture lists; free values, uses, call graph, and recursive components are derived on demand and invalidated by mutation, never maintained as shadow state.
- Deterministic printing for the pipeline stages (`Stage::Ersd`, `Stage::ErsdOptm`) and for diffing; deep input diagnoses or prints on the default test-thread stack, never by raising it.
- First-order and serializable. This is the bootstrap seam property: the post-erasure representation is the planned interchange boundary between a future Curios frontend and the Rust backend, so it must remain exactly constructible, exhaustively validatable, and deterministically serializable. The versioned envelope codec itself is deferred until the bootstrap schedules it; this specification only forbids representation choices that would preclude it (hidden state, non-serializable facts, order-dependent identity).

During construction the new representation coexists with the legacy one inside `curios-ersd` and therefore lives in a nested module with temporary name disambiguation. That nesting is a migration artifact with a deletion date: the flip series flattens the representation to the crate root and removes the temporary prefixes.

## Verifier

The verifier checks exactly the language contract — no more, no less.

- Structural: ID existence and kind, lexical scoping and dominance, unique block ownership, statement/terminator well-formedness, arity agreement, schema and constructor agreement, exhaustiveness or default on matches, acyclic constants, no tombstone references.
- Semantic admission: recursive groups are accepted exactly as the language accepts them — function-only recursion, mixed function/computed knots with a consistent initialization order, and rejection of computed-only cycles. **Every rule is corpus-certified**: a rule that rejects a supported program is a bug in the rule. The first run's verifier over-rejected two supported value-recursion idioms (`Task/join_all`, self-referential lazy values); reconciling those rules against the corpus is part of landing the verifier, and unresolved over-rejection blocks the flip.
- Roles: always the finalize/test gate during construction and transformation; eventually the seam validator at the bootstrap boundary. Whether it runs on every production compile is a measured decision made at the flip, not an assumption.

## Effects and traps

One contract, stated once, consumed by pruning, dead-binding legality, and partial evaluation:

- **Ordering** is statement order. There are no motion passes; nothing reorders an observable relative to another observable, and pure computation moves freely because it has no observable order.
- **Once-ness** follows from the operand law: an effectful subexpression is bound once and referenced thereafter.
- **Deletion**: a binding may be removed for having an unused result only when its operation is total and unobservable. Traps are not dead code; cell operations, host calls, and known-trapping operations are never removed on unused-result grounds.
- **Residualization** (partial evaluation): an effect may survive into a residual only in tail position; non-tail evaluation never residualizes an effect; cell operations always bail (a cell operation's identity is its program point).
- **Dormancy**: constructing a function performs nothing. An effect inside a closure body is dormant until application; summaries attribute it to the call, not the construction. The scheduler corpus (`Task`) is written against this law.
- **Iteration**: an effectful fold step or `LstMap` mapper runs once per element — never hoisted, never folded, never deduplicated; behavior summaries compose the body's behavior into the fold or intrinsic.

The buried-effect behavior follows from this contract with no dedicated machinery: erasure's ANF unfolds a deeply nested effect to a statement at its evaluation position, and pure computation around it folds freely — the pure chain collapses and "hugs" the effect, preserving order, because the effect blocks only reordering and deletion, never value flow.

## The behavior oracle

One total function over the closed operation alphabet, in `curios-ersd`, with two halves and exactly three consumers:

- **Behavior**: operation → observable behavior (may trap, may diverge, may exit, host effect, state read, state write, observable allocation, callback). Consumed by pruning (eager-item taint), dead-binding legality, and partial evaluation (residual soundness). Per-function summaries are computed to a fixed point over the derived call graph: known calls compose, unknown calls are conservative, recursive components may diverge unless proven otherwise, `LstMap` composes its mapper.
- **Fold**: operation × constant operands → value, would-trap, or unknown. Consumed by partial evaluation. A known trap is distinct from both dead code and unknown: the operation stays as a residual, is never deleted, and never panics the compiler.

Arity and shape facts delegate to the representation's own definitions; the oracle restates nothing. There is no laws framework: an algebraic fact enters the oracle only when a named transformation consumes it (today: the monoid facts worker/wrapper rebases over).

## Numeric law

Core computes unbounded values during type-level reduction. Ersd and Cont assume exact `u32`, `i32`, and bit-preserving binary32 semantics: constants, the fold half of the oracle, specialization keys, and tests all use the full 32-bit domains. i31 is exclusively a physical representability question — unboxed immediate versus boxed — owned by Cont's Wasm boundary. No 31-bit fact may appear in Ersd's alphabet, oracle, or tests as anything other than "the boundary may box or trap; ask the boundary."

Two historical leaks of this law are on record and must not recur: the legacy Ersd constant folder transcribed 31-bit trap behavior into compile-time evaluation (upward leak), and the current emitter enforces the i31 carrier by trapping above it at runtime (downward leak — a known backend limitation, recorded as a Cont→Wasm obligation, not a weakening of this law). Consequence: the fold-versus-runtime differential matrix is exercised inside the range where the current backend is faithful; above-i31 behavior is the boundary's contract to fix, in its own project.

## The three transformations

### Pruning

Drop every top-level item the program neither reaches nor runs for observable effect. Roots are the entry body's transitive references plus every eager top-level binding whose initializer is observable under the oracle (trap, divergence, exit, host, state, observable allocation, or a call reaching one). Function construction is never observable; recursive groups are kept or dropped whole; statement order is preserved. Deterministic worklist, stable tie-breaks.

Gate: the eager-effect corpus tests (an unused effectful CAF is kept, the unused pure combinator webs are dropped) and a compile-time metric — the fixed prelude's dead slice never reaches the lowering, keeping the Cont optimizer's input proportional to the program actually written.

### Partial evaluation

Two drivers over one shared core (value domain with closures, interpreter, reifier, deterministic budgets):

- **Closed-term evaluation**: a call whose transitive dependencies resolve to constants and top-level definitions is executed at compile time; the result reifies to constants, construction statements, or a closure; a tail effect reifies to a residual instruction; budget exhaustion leaves the original term. Scope statement: this is the term-level shadow of type-level computation — a dependently-typed API's static argument (a format string, a literal certificate) was already evaluated by the elaborator, so the evaluator's success envelope is defined and predictable, not opportunistic.
- **Literal-spine specialization**: a recursive function applied to a constructor-shaped literal spine is unrolled one minted specialization per strictly smaller sub-spine, with memoization and hard budgets. The strict-subterm key is the termination argument; general partial-evaluation termination and generalization are out of scope.

Gates: the `Fmt` trio (constant arguments collapse to a single host call; runtime arguments specialize to a first-order chain; the partial-application residual), literal `Char`/`Str` certificate collapse, the effect-hugging program, an effectful-fold-body bail test, and budget determinism.

### Worker/wrapper

The monoid half only: a linear non-tail self-recursion whose deferred context composes over a registered monoid, with a pure context under the oracle, is rewritten to a tail-recursive worker threading an accumulator, behind a thin wrapper. The slice-cursor half of the legacy engine is explicitly omitted — the rope representation made suffix views O(1), absorbing its job (the admission rule in action). Sound-but-incomplete is the contract: outside the recognized envelope the transform is a no-op, never a miscompile.

This transform is a **flip precondition**. The first run deferred it as optional and paid with native-stack overflows on corpus-sized deferred-context recursion; "omission is a valid outcome" is recorded as refuted. Gate: the deferred-context corpus (`Nat/to_str`-shaped digit recursion, `Str` counting, `BigNat` bit-depth operations) runs on the default test-thread stack at corpus-sized inputs.

## Verification

- **Behavior identity on the full corpus.** The entire test suite runs through the new path and is the oracle — every runtime output compared. There is **no byte-identity gate against the legacy path anywhere**: byte-identity is only satisfiable by reproducing the legacy encoding decisions, which makes mimicry the definition of done and re-imports the old scope. A normalized IR diff against the legacy path is a debugging lens someone reaches for, never a gate.
- **Structural property gates.** The codegen structural suite (one natural loop for the hot kernel, no closures or indirect calls where calls are known, direct recursion, no dispatchers on ordinary programs) must hold through the new path — properties, not bytes.
- **Measured comparison at the flip.** Cont v2 §5 style: the pre-flip revision in a separate worktree, same corpus, compared on emitted size, node counts, compile time, and the runtime anchors. Every difference is explained or fixed; judgment is applied where judgment belongs.
- **Internal determinism is byte-strict.** Two compiles of the same input through the new path produce identical bytes at every stage. This is a self-property, not legacy-matching.
- **Deep input** stays on the default test-thread stack in every component — erasure, verification, printing, transformation, lowering. `RUST_MIN_STACK` is never used to hide a regression.
- The full repository gate (`make curios/runtime`, fmt, check, clippy `-Dwarnings`, workspace tests) passes before every commit.

## Delivery

Built on the pre-v2 baseline branch beside the untouched legacy path, which remains production until the flip. The first run's history is the salvage quarry: code is lifted where it implements a kernel component and passes this specification's gates, and salvage never imports surface whose consumer this specification does not name. Each phase is a checkpointed commit series with its exit gate green.

1. **Representation** — arenas, alphabet, builder, verifier (rules corpus-certified, including the value-recursion reconciliation), printer, derived analyses. Exit: representative modules build, verify, print, analyze, and reject malformed input deterministically, in isolation.
2. **Erasure** — the transcription defined above, under the operand law, prelude erased fresh. Exit: the fixed prelude and representative programs erase to verified modules; no stored captures; the once-ness tests pass; no stack regression.
3. **Lowering** — the desugar table and CPS conversion; the critical vertical. Exit: the full corpus runs through erase → lower → Cont → Wasm behind a test entry with behavior identity against production, and the structural property gates hold.
4. **Oracle and pruning** — behavior half, fixed-point summaries, the prune. Exit: pruning gates green; compile time proportional to the live slice.
5. **Partial evaluation** — shared core, both drivers. Exit: the partial-evaluation gates green.
6. **Worker/wrapper** — the monoid transform. Exit: the deferred-context corpus green on the default stack.
7. **The flip** — archive the prelude prefix in the new representation (landing with the flip so no fresh-re-erasure regression ships), repoint production and the `Stage::Ersd`/`Stage::ErsdOptm` observers, run the measured comparison, then delete in the same series: `curios-core/src/erase.rs` and `erase/`, the legacy `curios-ersd` representation/optimizer/lowering (`term`, `module`, `prim`, `optimize`, `into_cont`, legacy printing), the legacy archive shape, and every temporary test entry; flatten the representation to the crate root and drop the temporary prefixes. Exit: one representation from erasure through Wasm, the suite green, documentation updated, this specification deleted.

## Non-goals

- A laws framework, an exhaustively-consumed semantic contract, or any oracle entry without a named consumer.
- Local constant folding, CSE, code motion, loop-invariant hoisting, fusion, deforestation, inlining, or contification in Ersd — Cont and the representation guarantees own all of it.
- The slice-cursor transform, stored specialization facts (`candidate`-style annotations), or any maintained shadow analysis.
- A retained legacy path, in-tree oracle, compatibility facade, or selectable backend after the flip.
- Byte-identity gates against the legacy path.
- Changing Core's type theory, elaboration, or surface semantics; changing the landed Cont representation, the Wasm value layout, the host or runtime ABI; moving i31 above the Wasm boundary.
- Aggregate constant variants in the representation until the evaluator demonstrably needs to intern folded aggregates.
- Rescuing the totality tax of early-exit-flag folds (`take_while`) — accepted by the language's own design.
- General partial-evaluation termination or generalization — spine keys stay strict-subterm.

## Named futures

Recorded so their absence is a decision, not an oversight; each is its own project with its own specification and measured gate.

- **Layout handoff.** The semantic identities this representation preserves (scalar distinctions, schemas, constructors) are the information the cast-elimination project needs: a shape-carrying lowering feeding typed Cont values and per-schema Wasm heap types, gated on casts-per-operation over the probe set. Until that project exists, the desugar table stands; the identities' first consumer is partial evaluation, their second is this future.
- **Bootstrap envelope.** The versioned, validated Ersd byte codec at the Curios-frontend/Rust-backend seam. The representation keeps the seam property now; the codec lands when the bootstrap schedules it.
- **Full-u32 backend faithfulness.** The current emitter traps above the i31 carrier; making runtime `Nat`/`Int` faithful to the 32-bit law (boxing fallback or layout-informed encoding) is a Cont→Wasm obligation, naturally coupled to the layout project.
