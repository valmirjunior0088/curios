# Inference and unification

Working implementation specification for the remaining inference and unification work in `curios-elab`.

This document is the implementation handoff for six related items that all move the same subsystem: the metavariable solver, the postponement scheduler, and the witness-resolution keys that read solved metavariables. Their durable user-facing semantics belong in `SYNTAX.md`, elaborator invariants in `curios-elab` module documentation and tests, and cross-cutting rationale in `DESIGN.md`.

It consolidates what were three scattered plans — an unrefined unification umbrella, a refined lambda-inference handoff, and two limitations found while probing higher-kinded witness resolution. They are one document because they share a scheduler, a solver, and a failure mode: an elaboration that cannot proceed today parks, and every item below is either a rule that lets it proceed, or a diagnostic that explains why it did not.

## Covered roadmap items

- Pruning of out-of-scope metavariables
- η-equating metavariable heads
- Surfacing residual unification constraints (distinguishing postponed from rigid-mismatch diagnostics)
- Monomorphic, use-driven inference for unannotated lambda parameters
- Witness keying through a partially applied type constructor
- Right-biased partial imitation for flex-apply

## Status ledger

Verified against the tree rather than inherited from the superseded documents. Every citation is a file and line to re-read before starting, not a frozen API.

| Item | Landed | Remaining |
| --- | --- | --- |
| Pruning | The degenerate form only: a non-invertible spine entry makes the solution unable to depend on that slot, enforced by the scope check (`convert.rs:1385`, "pruning in its simplest form"), with `convert/tests.rs:1288` pinning it | Real pruning. `convert.rs:1327` postpones instead, and says so: "Postpone (the stand-in for pruning)" |
| η-equating metavariable heads | Nothing for this item. `eta_expand_neutral` (`convert.rs:1254`) is *type-directed* Π/Σ/struct eta, which is the separate, already-checked roadmap entry | The metavariable-head rule itself |
| Residual constraints | The checking-shaped half: a parked `Checking` obligation surviving every retry reports `Error::PostponedCheck` (`error.rs:245`) at the expression's own span, naming the expected type it waited on | The conversion-shaped residue. A parked `Conversion` goal still reports as a plain mismatch at its origin, distinguished only when it stands between witness holes |
| Lambda inference | The scheduling machinery, hardened in production: `ParkedGoal` freezes its birth frame and watch set (`context/solutions.rs:51`), `elaborate_apply` settles a whole telescope through `ParkedWork::Checking`, and drains run at item boundaries | The feature. `ParkedWork` (`context/solutions.rs:34`) has exactly `Conversion`, `Checking`, `Witness` — no inference-shaped or groundness-shaped variant — and `binding.rs:519` still rejects an unannotated domain outright |
| Witness keying | Keying works wherever the concept parameter reduces to a rigid head, including the higher-kinded case through `HeadKey::of_whnf`'s `Func` arm (`concept.rs:80`) | A partially applied family. `(A : Type) => Free(S, A)` leaves a stuck `Apply` under the binder, which the arm does not read |
| Partial imitation | Full-arity imitation, with pre-commit re-validation against the metavariable's frozen birth type (`convert.rs:1691`) | The under-applied case. `convert.rs:1646` blocks when `flex.params.len() != arity` |

The last two items are the newest and the least discussed, so they carry a reproduction each below. The first four are inherited scope whose superseded documents this file replaces.

## Part 1 — solver refinements

### 1.1 Pruning of out-of-scope metavariables

When solving `?m[σ] ≡ t`, a candidate solution `t` may mention metavariables whose contexts are wider than `?m`'s. Committing would let a solution escape the scope that justified it, so `convert.rs:1327` postpones the whole equation instead. Postponement is correct and incomplete: the goal wakes only if something else solves the offending metavariable, and if nothing does, an equation that had a legitimate restricted solution surfaces as a mismatch.

Pruning replaces the refusal with a restriction. Where a candidate mentions `?n` whose context exceeds `?m`'s, mint `?n'` at the intersection of the two contexts, solve `?n := ?n'` restricted to that intersection, and re-attempt the original equation against the pruned candidate. Where the intersection cannot support `?n`'s type — the type itself depends on a variable being pruned away — the equation is genuinely unsolvable and should fail rather than postpone.

Constraints this must respect:

- The existing scope check stays. Pruning narrows what a candidate mentions; it does not license a solution to mention what its birth frame never had.
- Solutions remain monotonic. A pruned metavariable is solved, not retracted, so no cache may assume an unsolved metavariable stays unsolved (the invariant `reduce.rs`'s memo already depends on).
- Pruning runs inside the same transaction as the solve it serves, so a rolled-back speculative branch unwinds the pruning with it.
- `convert.rs:1327`'s comment is the marker for the site to change, and its wording should stop advertising a stand-in once the real thing exists.

### 1.2 η-equating metavariable heads

Distinct from the type-directed eta already implemented. The rule wanted here equates a metavariable against an eta-expansion of itself — `?m ≡ λx. ?m(x)` — and, more usefully, lets `?m(x₁ … xₙ) ≡ t` be attacked by contracting the flex side when the arguments are exactly the binders in order.

`reduce_func_eta` (`reduce.rs:274`) already implements exactly that contraction for the *rigid* case, with the three guards that make it sound: the application is saturated by the binder count, each argument is precisely the corresponding binder, and the head does not mention any of them. The item is to make the solver reach for the same contraction when the head is a metavariable, rather than only when reduction happens to normalize into it.

Interacts with 1.4: both change what the solver does with a flex head against a spine, and 1.4's guard sits in the function 1.2 would extend the reach of. Land 1.2 first or land them together; landing 1.4 first means writing its guard twice.

### 1.3 Surfacing residual unification constraints

Half landed, and the landed half is the template. A `ParkedWork::Checking` obligation that survives the final drain reports `Error::PostponedCheck` at its own span, naming the expected type it was waiting on, instead of an unlocated `cannot infer`.

The remaining half is `ParkedWork::Conversion`. A parked conversion goal that never wakes currently surfaces as an ordinary type mismatch at its origin, which is honest but misleading: the two failures have different causes and different fixes. A rigid mismatch means the program is wrong; a postponed conversion means the program may be right and inference never gained the structure to decide.

The diagnostic should distinguish them, name the metavariables the goal was watching, and anchor at the origin term the `ParkedGoal` already carries (`context/solutions.rs:55` — "its span anchors the eventual error if the problem never resolves"). The existing witness-hole special case is a third state and should remain reachable rather than being folded into either.

This item is a prerequisite for 1.1 and Part 2 in practice, not in principle: both add ways for work to park, and both are much harder to debug against a diagnostic that cannot say whether the solver stalled or the program disagreed.

### 1.4 Right-biased partial imitation for flex-apply

**Reproduction.** With `induct Box(S : Type, A : Type) : Type | wrap(A) end`, a `!` on an action of type `Box(Str, Nat)` raises the goal `Monad(?M)` with the equation `?M(?A) ≡ Box(Str, Nat)`. Imitation blocks at `convert.rs:1646`:

```rust
let arity = rigid_args.len();          // params + indices
if flex.params.len() != arity { return self.block(context, goal); }
```

One binder against a two-argument rigid, so `?M` is never pinned and the witness never resolves.

**Rule.** When `flex.params.len() == k` and `k < arity == n`, commit the right-biased imitation:

```text
?m := λx₁ … xₖ. T(b₁, …, b_{n−k}, x₁, …, xₖ)
```

and equate the retained prefix pairwise as the saturated case already does.

**Why it is well-formed here.** A parameter telescope orders dependencies left to right — a later parameter may depend on an earlier one, never the reverse — so the fixed prefix `b₁ … b_{n−k}` can never mention the abstracted suffix. The candidate is always well-scoped. `k` is not guessed either: it is read from `?m`'s frozen birth type, which the arity check at `convert.rs:1658` already consults.

**Why it stays a guess, and why that is consistent.** `?m(x) ≡ Box(Str, Nat)` also admits `λx. Box(x, Nat)`; there is no most-general unifier. The existing design is already guess-and-block — `convert.rs:1582` states that "a rejected or postponed guess *blocks* the goal, never hard-fails it: refuting the imitation does not prove the equation unsatisfiable" — and the candidate is checked against the frozen birth type under the birth context before it commits (`convert.rs:1691`), so a wrong split is refused rather than landed. Right-biased is the conventional choice and the one kind-currying makes in Haskell, which is why `Monad (Either e)` works there.

**What must not change.** The `flex.params.len() == arity` path is untouched, so nothing that resolves today resolves differently. Incompleteness moves rather than disappearing: an equation whose intended solution abstracts a *prefix* will now be guessed wrong, blocked, and surface through 1.3's diagnostic, where today it blocks immediately. Determinism is preserved because the split is fixed.

### 1.5 Witness keying through a partially applied type constructor

**Reproduction.** `satisfy (@S : Type) => Monad((A : Type) => Box(S, A))` is refused with *witness cannot be keyed: its concept's parameter 1 reduces to `A => Box(S, A)`*. The same declaration with a unary family — `satisfy Monad((A : Type) => Uni(A))` — is accepted and runs.

**Cause.** Two mechanisms compose. Surface `Box(S, A)` lowers to `Apply(Box, [S, A])`, and weak-head reduction does not go under the `λA`, so the lambda body stays a stuck application. `HeadKey::of_whnf`'s `Func` arm (`concept.rs:80`) walks the telescope to its body and accepts only `InductType`, `StructType`, or a primitive former there; an `Apply` falls to `_ => None`, and `resolve.rs:651` raises the error.

The unary case is not an exception to this — it is `reduce_func_eta` firing first. `λA. Uni(A)` contracts to bare `Uni`, whose own whnf is the family node. Eta-contraction cannot fire on a partial application, which is why the two spellings diverge.

**Rule.** When the `Func` body is an `Apply`, key on that application's head. This makes registration and lookup agree, and both call sites (`resolve.rs:226` for the goal, `resolve.rs:651` for the declaration) go through the one function.

**Asymmetry worth knowing.** Only the registration side needs this. On the goal side, a committed imitation constructs its body through `Term::induct_type_at` (`convert.rs:1611`) — a materialized node — so once 1.4 lands, the solved `?M` keys through the existing arm unchanged. The two items are therefore independent fixes to opposite sides of the same wall, and 1.5 is independently useful: it admits the parametric witness declaration even before 1.4 makes `!` dispatch to it.

**Coherence.** `insert_witness` (`context/program.rs:154`) is strict-unique and a collision is a hard `DuplicateWitness`. Keying `λA. Box(Str, A)` as `Nominal(Box)` therefore collides with `λA. Box(Nat, A)`. This is the same no-overlap discipline the corpus already lives under: every parameterized-head witness in the prelude is written once and parametrically — `Show(Lst(A))`, `Show(Option(A))`, `Show(Result(A, E))` — with no ground-instance overlap anywhere. The intended spelling here is likewise one parametric witness, and the implementation should not relax uniqueness to accommodate ground instances.

**Open question the implementation must settle.** The failing diagnostic prints unsolved universe metavariables in the head (`Box.{?u264,?u265}`). Witness schemes have their own finalization route (`universe_solver.rs:1122`, `finalize_at_instance`), and whether keying on an `Apply` head reads a sound `UniverseInst` before levels are solved is not determinable by reading. Settle it with a test before relying on the rule.

## Part 2 — monomorphic, use-driven lambda inference

The largest item, and the one whose scheduling substrate is already in production. Preserved here in full from its superseded handoff.

### Goal

Permit an unannotated lambda whose body is temporarily blocked on a parameter type to receive enough type information from a later use in the same item.

```crs
let unwrap = (value) =>
  match value
  | some(x) => x
  | none() => 0
  end;
unwrap(Option/some(42))
```

The intended elaboration sequence is:

1. Give `value` a fresh metavariable domain `?A`.
2. Discover that the match cannot proceed while `?A` is unknown, and park inference of the match behind paired term and type placeholders.
3. Return a provisional lambda type `(?A) -> ?R`.
4. Elaborate `unwrap(Option/some(42))`, solving `?A := Option(Nat)`.
5. Wake the parked match, restore the lexical frame in which it was created, and infer it as `Nat`, solving both its term and type placeholders.
6. Zonk the enclosing item normally, with no unsolved metavariables remaining.

The feature is deliberately smaller than Hindley–Milner: a lambda may acquire constraints from later uses within the same enclosing item, but it is never generalized and inference never crosses an item boundary.

### Inherited safety discipline

The scheduling campaign that landed `ParkedWork::Checking` left a defect ledger, and it is this part's inherited discipline rather than background reading:

- A frozen frame is restored by reapplying only identities not already live. An intra-item wake under the older restoration doubled live binders, giving every metavariable born in the retry a non-linear identity spine that pattern inversion cannot invert.
- A placeholder solved ahead of its retry ends the obligation without re-elaborating. A second elaboration of a term whose lowering-minted holes are already birthed drops their spines.
- A rollback bracket may not contain retries. It would consume obligations whose solutions it then unwinds.

The authoritative description is `curios-elab/README.md`, "Postponement is a parked obligation, never a raw substitution".

### User-visible semantics

**Monomorphic inference.** Every unannotated lambda parameter receives one metavariable for the enclosing item. Uses constrain that single metavariable; they do not instantiate a generalized type scheme.

Two uses at one type succeed, inferring `id : (Nat) -> Nat`:

```crs
let id = (x) => x;
(id(1), id(2))
```

Two uses at different types fail, because they demand inconsistent solutions for the same monomorphic domain:

```crs
let id = (x) => x;
(id(1), id(true))
```

**The enclosing item is the inference boundary.** Constraints may flow through local definitions and later expressions in one item; they may not flow from a later top-level item into an earlier one. This matches the existing lifecycle — parked work is drained after each top-level item and after the body of a local binding region — so no module-wide inference phase is introduced.

**Unconstrained lambdas still fail.** An unannotated domain must be transitively ground by the end of its enclosing item. `(x) => x` remains an inference error, and the diagnostic must be anchored at the lambda parameter or its domain site rather than emitted later as an unlocated zonking failure.

**No constructor-name guessing.** An inductive match whose scrutinee type is unknown must wait for an actual type constraint. Inferring an inductive from arm tags alone is forbidden: tags are not globally unique, and guessing would make name resolution affect typing unpredictably.

**Primitive matches constrain their carriers eagerly.** Primitive match forms have an unambiguous carrier and should solve an unknown scrutinee type immediately rather than park — Boolean arms to `Bool`, numeric switch arms to `Nat`, bit and byte arms to their packed primitives, list-shaped arms to `Lst(?Element)` with the element type free to remain unknown.

### Core design

**Allow provisional lambda domains.** `elaborate_func_infer` rejects an unannotated domain that is still a metavariable (`binding.rs:519`), and the test is literally `matches!(…, Subterm::Metavar(_))` — a bare-outer-term check, not transitive groundness. Remove the early rejection and permit the inferred function type to carry the domain metavariable while the item is still elaborating.

This relaxation alone is insufficient: structural operations in the body must be able to suspend, and every unannotated domain needs an explicit end-of-item groundness obligation.

**Add parked inference.** A new obligation alongside the existing three, conceptually:

```rust
Inference {
    term: Term,
    blocker: Term,
    term_placeholder: MetavarId,
    type_placeholder: MetavarId,
}
```

`term` is the residual, partially rebuilt term to retry; `blocker` identifies the type information that prevented progress; the two placeholders stand for the elaborated core term and its inferred type. The existing frozen frame records the lexical environment in which all three are valid. The Rust spelling should follow the surrounding enum and ownership conventions rather than this sketch.

**Use paired placeholders.** When inference parks, create a fresh type placeholder `?T : Type` and a fresh term placeholder `?e : ?T`, then return `(?e, ?T)`. Both must be created inside the exact current frame, which is what allows an inferred result type to mention lambda parameters or other local assumptions. On retry: reduce the blocker enough to decide whether progress is possible; if still blocked, re-park without manufacturing a second pair; otherwise restore the frozen frame, infer the residual term, solve the type placeholder with the inferred type, and solve the term placeholder with the rebuilt term.

Solving these internal placeholders directly is justified by the same invariant retried checking uses — the replacement was elaborated under the placeholder's exact birth frame — and it avoids depending on general flex-flex orientation or on 1.1 landing first.

**Park the residual term, not the untouched source term.** Every structural park site must first infer or rebuild the portion already known, and store *that* in the obligation. This monotonicity rule is essential: retrying the untouched source term can allocate fresh implicit arguments, witness goals, or nested metavariables on every wake-up, leaving duplicate or orphaned obligations. A retry continues one elaboration; it does not start a parallel one.

**Add a groundness obligation.** Every unannotated lambda domain registers an obligation conceptually equivalent to `Ground { type_: Term }`, complete once the type is transitively ground, reusing or centralizing the transitive-groundness logic currently associated with application elaboration rather than re-testing the outer term. A survivor of the item's final drain reports `CannotInfer` at the lambda domain span. This is separate from parked body inference: `(x) => x` blocks on no structural operation, yet must still fail predictably.

**Watch transitive blockers.** Existing wake-up sets are organized around directly solved metavariables, but a blocker can be an alias or a type expression whose unsolved leaves are metavariables. New inference and groundness work should watch those transitively unsolved leaves, keeping the final drain as a safety net rather than the primary mechanism.

### Structural operations that must park

- **Inductive matching** (`elaborate_induct_match`) — if the rebuilt scrutinee type reduces to an unsolved metavariable, park the entire residual match; on retry, run the ordinary inductive lookup, motive construction, coverage checks, refinement handling, and branch elaboration. Do not pre-resolve tags.
- **Projection** (`elaborate_proj`) — if the rebuilt head type is an unsolved metavariable, park the residual projection, enabling `(pair) => pair.0`. Do not guess a tuple or record skeleton.
- **Application** (`elaborate_apply`) — if the rebuilt callee type is an unsolved metavariable, park the residual application, enabling `(f, x) => f(x)`. Do not solve the callee type to an invented function skeleton: Curios functions carry explicit, implicit, and witness plicities and may have dependent codomains, and guessing a skeleton commits to semantics the source did not supply.
- **Primitive matching** — before the generic path, recognize primitive arm shapes and unify the scrutinee with their known carrier, introducing a fresh element metavariable for `Lst(?Element)`.

### Retry and scheduler invariants

- A retry meeting the same unresolved blocker reuses the existing placeholders and obligation identity.
- A retry reaching a deeper blocker may create nested parked work but must not recursively spin during one wake-up cycle.
- Solving either placeholder obeys the ordinary occurs and scope checks for its birth frame.
- Parked inference may temporarily place placeholders inside definitions, but reduction continues to treat unsolved metavariables as stuck and solutions as monotonic.
- No normalization cache may assume an unsolved metavariable remains unsolved after an obligation wakes.
- The final successful module remains meta-free under existing zonking checks.
- Retrying under a frozen frame preserves representation-visibility islands, refinements, and witness scope exactly as immediate elaboration would.

### Conversion-oracle behavior

Some elaboration runs under an oracle or transaction where speculative obligations must not escape a rolled-back attempt. When parking is suppressed by that mode, structural inference must return the existing local inference or mismatch result rather than installing provisional placeholders. Delayed work must never outlive the conversion state whose assumptions justified it.

### Diagnostics

The primary diagnostic for an unresolved lambda domain is the existing `CannotInfer` family, anchored at the unannotated parameter. Ordinary errors discovered after a wake-up retain their original spans and categories — unknown or incomplete match arms, private representation access, witness ambiguity, type mismatch. A retry must not replace a useful structural error with a generic unsolved-metavariable error merely because the check happened later.

### Scope and soundness risks

- **Placeholder scope.** Both placeholders must be born under the lambda binder if their solutions may mention it. Creating them outside that frame either rejects valid dependent results or lets a local escape.
- **Refinements and witnesses.** A delayed match must see the same branch refinements as an immediate one, and delayed applications must neither duplicate nor silently discard witness resolution. Both paths need tests.
- **Representation visibility.** The frozen frame and item boundary must retain the active representation island, so delaying a projection or match cannot make private constructors or fields visible outside their origin scope.
- **Local definitions.** Definitions containing provisional placeholders are safe only if all outstanding work drains before the item leaves its elaboration boundary. No placeholder-backed definition may enter a later item's context.

## Ordering

The six items are not equally coupled, and two orderings are load-bearing rather than preferential.

1. **1.3 (residual constraint diagnostics)** first. It is cheap, half-done, and every later item adds ways for work to park. Debugging 1.1 or Part 2 against a diagnostic that cannot distinguish *the solver stalled* from *the program disagreed* is the avoidable cost here.
2. **1.5 (witness keying)** next. Independent of everything else, roughly five lines, and it settles the universe-instance question early on the cheapest possible surface. It also lands user-visible value alone: the parametric witness declaration becomes writable even before `!` dispatches to it.
3. **1.2 and 1.4 together.** Both change what the solver does with a flex head against a spine, and 1.4's guard sits inside the function 1.2 extends. Landing 1.4 first means writing that guard twice.
4. **1.1 (pruning)** after 1.2/1.4. Pruning is what lets the solver commit where it currently postpones, and it is easiest to evaluate once the imitation rules that feed it are settled.
5. **Part 2 (lambda inference)** last, in its own sequence: relax the domain rejection and add the groundness obligation; add paired placeholders and scheduler support against a narrow blocker; then application and projection parking; then inductive-match parking; then eager primitive carriers; then dependent, witness, refinement, privacy, and diagnostic coverage. This produces testable scheduler behavior before the richest match path depends on it.

Part 2 does not require 1.1: its placeholder equations are deliberately oriented so the placeholder is solved directly by a term elaborated in its own frame, minimizing dependence on unfinished flex-flex behavior. That orientation is a design constraint, not an accident, and should survive any reordering.

[`06_ANONYMOUS_MATCH_FUNCTION_SPEC.md`](06_ANONYMOUS_MATCH_FUNCTION_SPEC.md) depends on Part 2 landing but is not part of this document: it is `curios-text` parsing, printing, and lowering, and expects no `curios-elab` change of its own.

## Implementation map

The likely surface, to be re-read rather than trusted:

- `curios-elab/src/convert.rs` — imitation (1.4), metavariable-head eta (1.2), pruning at the solve site (1.1).
- `curios-elab/src/concept.rs` — `HeadKey::of_whnf`'s `Func` arm (1.5).
- `curios-elab/src/resolve.rs` — the two keying call sites (1.5).
- `curios-elab/src/context/solutions.rs` — new parked-work variants, blocker watchers, obligation bookkeeping (Part 2, 1.3).
- `curios-elab/src/typing.rs` — retry behavior, placeholder solving, wake-up policy, final draining diagnostics (Part 2, 1.3).
- `curios-elab/src/error.rs` — the conversion-shaped residual diagnostic (1.3).
- `curios-elab/src/elaborate/binding.rs` — provisional unannotated domains and groundness registration (Part 2).
- `curios-elab/src/elaborate/apply.rs` — application blocking and shared transitive-groundness support (Part 2).
- `curios-elab/src/elaborate/aggregate.rs` — projection blocking (Part 2).
- `curios-elab/src/elaborate/match_.rs` — inductive-match blocking and eager primitive carriers (Part 2).
- Test modules beside each, plus `curios/src/tests/` for cross-stage programs proving accepted terms compile and run.
- `documentation/SYNTAX.md`, `documentation/ROADMAP.md`, `documentation/DESIGN.md`, and affected module rustdocs once items land.

No core representation change is expected, and none of the six items adds a `HeadKey` or `WitnessKey` variant, so the prelude's rkyv archive format is unchanged. No erased IR, continuation IR, wasm, ABI, or runtime change is expected. Nothing here crosses the `curios-cert` seam: witnesses are ordinary definitions, resolution is elaboration-only, and the kernel rechecks a plain application either way.

## Acceptance tests

**1.1 Pruning** — an equation whose candidate mentions a wider-context metavariable is solved by restriction rather than postponed; an equation whose intersection cannot support the pruned metavariable's type fails rather than postponing; a rolled-back speculative branch unwinds its pruning; `convert/tests.rs:1288`'s existing degenerate case still holds.

**1.2 η metavariable heads** — `?m ≡ λx. ?m(x)` equates; `?m(x₁ … xₙ) ≡ t` is attacked by contraction when the arguments are exactly the binders in order; a non-binder or duplicated argument does not contract.

**1.3 Residual constraints** — a never-woken `Conversion` goal reports a postponement diagnostic naming its watched metavariables, distinct from a rigid mismatch; a genuine rigid mismatch still reports as a mismatch; the witness-hole case remains its own third state; the landed `PostponedCheck` behavior is unchanged.

**1.4 Partial imitation** — `?M(?A) ≡ Box(Str, Nat)` commits `λx. Box(Str, x)`; the retained prefix is equated pairwise; an ill-kinded split is refused by pre-commit re-validation rather than committed; a full-arity equation resolves exactly as it does today; a prefix-intended equation blocks and surfaces through 1.3.

**1.5 Witness keying** — `satisfy (@S : Type) => Monad((A : Type) => Box(S, A))` registers and keys on `Nominal(Box)`; a second ground witness at the same head is refused as `DuplicateWitness`; the unary eta-contracting path is unchanged; a witness registered before universe finalization keys soundly (the open question above).

**Part 2** — a local identity lambda fixed by a later `Nat` call; repeated uses at one type succeeding and incompatible uses failing monomorphically; a standalone `(x) => x` failing at its parameter span; an `Option` match inside a lambda inferred from a later call; a `Result` match and a user-defined indexed-inductive match retrying with correct refinements; Boolean, numeric, bit, byte and list primitive matches constraining their carrier immediately; a list match leaving its element type open and then either constrained or failed at the boundary; projection from an initially unknown parameter type; calling an initially unknown function parameter, including its plicities; a dependent inferred result mentioning a lambda parameter without escaping; nested structural blockers making progress without duplicate placeholders or infinite retry; witness goals created before or during a retry resolved once with source spans retained; representation privacy and match refinements identical before and after parking; no work surviving a top-level item boundary; oracle-mode elaboration leaking no parked obligations; successful zonking leaving no unsolved metavariables.

Where practical, scheduler tests should assert obligation counts or placeholder reuse, so a superficially successful program cannot hide duplicated delayed work.

## Non-goals

- Hindley–Milner generalization or let-polymorphism.
- Cross-item or module-wide inference.
- Inferring an inductive from constructor spellings alone.
- Inventing function, tuple, record, or inductive skeletons from an operation on an otherwise unconstrained value.
- Changing implicit or witness argument semantics.
- Relaxing witness uniqueness to admit overlapping ground instances.
- Full higher-order unification. Imitation stays a guess that blocks rather than a complete procedure, and the constant and projection solutions remain unproduced.
- Effect rows, or any second constraint domain beside the universe solver.
- A new core term, or a change to any downstream IR.
- Anonymous match-function syntax, specified separately.

## Effort estimate

Part 1 is a set of small-to-medium solver changes: 1.3 and 1.5 are each a focused change plus tests; 1.2 and 1.4 are moderate and share a surface; 1.1 is the largest of the four and the one with the most soundness exposure, since it touches the file carrying the most perimeter weight.

Part 2 is a medium-to-large core elaboration project on its own, roughly 250–450 lines of implementation and 300–500 lines of tests across 7–10 files. Its uncertainty lies less in the individual park sites than in preserving retry monotonicity, lexical scope, diagnostics, and solver transaction boundaries. A match-only proof of concept would be smaller but would leave ordinary lambdas inconsistent across match, projection, and application bodies, and should not be treated as the finished feature.

## Verification

After each item, run the repository's full done bar in order:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features
```

Because `curios-elab` is in the browser compiler's dependency graph, also run `make curios/web` with the exactly version-matched `wasm-bindgen-cli`.

Items touching `convert.rs` should additionally be weighed against `PERIMETER.md`: the conversion checker carries several probed rows, and a change to imitation or pruning is the class of change whose evidence those rows record.

## Retirement criteria

Each item is retired individually; the file is deleted when all six are.

- Durable user-facing semantics are recorded in `SYNTAX.md` — for Part 2, lambda inference semantics; for 1.4 and 1.5, whatever becomes writable that was not.
- Solver, parking, and retry invariants are recorded in the owning `curios-elab` module documentation and tests.
- Cross-cutting rationale — notably that imitation is a deliberate guess and that witness keying is head-only with uniqueness enforced — is recorded in `DESIGN.md` or `curios-elab/README.md` as appropriate.
- Remaining plans refer to landed elaborator behavior rather than this file.
- Each roadmap entry is a checked, unlinked summary, and no reference to this filename remains.
