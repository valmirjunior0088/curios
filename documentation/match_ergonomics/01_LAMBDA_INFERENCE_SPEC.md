# Lambda inference

Working implementation specification for monomorphic, use-driven inference of unannotated lambda parameters.

This document is the implementation handoff for the feature. Its durable user-facing semantics belong in `SYNTAX.md`, while elaborator invariants belong in `curios-elab` module documentation and tests.

## Status and relationship to anonymous match functions

This is the first of two related projects. It is independently useful and should land before the proposed `match =>` anonymous match-function syntax, so anonymous match functions inherit the same inference behavior as ordinary lambdas instead of introducing a syntax-specific typing rule.

The intended feature is deliberately smaller than Hindley–Milner inference: a lambda may acquire constraints from later uses within the same enclosing item, but it is never generalized and inference never crosses an item boundary.

The scheduling machinery this spec extends has since landed and hardened in production: `elaborate_apply` settles every argument in one telescope walk whose postponed introduction forms are `ParkedWork::Checking` obligations, so parked checking now runs over the whole fixed prelude rather than a rare corner. That campaign's defect ledger is this spec's inherited safety discipline: a frozen frame is restored by reapplying only identities not already live (an intra-item wake under the old restoration doubled live binders, giving every metavariable born in the retry a non-linear identity spine pattern inversion cannot invert); a placeholder solved ahead of its retry ends the obligation without re-elaborating (a second elaboration of a term whose lowering-minted holes are already birthed drops their spines); and a rollback bracket may not contain retries (it would consume obligations whose solutions it then unwinds). Drain survivors report a located `PostponedCheck` naming the expected type they waited on — the checking-shaped precedent for this spec's located-diagnostic requirement. The authoritative description is `curios-elab/README.md`, "Postponement is a parked obligation, never a raw substitution"; what remains here is the feature itself — the inference-shaped obligation with paired placeholders, provisional domains, the primitive-match carrier constraints, and the end-of-item groundness obligation.

## Goal

Permit an unannotated lambda whose body is temporarily blocked on a parameter type to receive enough type information from a later use in the same item.

For example, this should elaborate:

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

## User-visible semantics

### Monomorphic inference

Every unannotated lambda parameter receives one metavariable for the enclosing item. Uses constrain that single metavariable; they do not instantiate a generalized type scheme.

```crs
let id = (x) => x;
(id(1), id(2))
```

The example above succeeds with `id : (Nat) -> Nat`.

```crs
let id = (x) => x;
(id(1), id(true))
```

The second example fails because the two uses demand inconsistent solutions for the same monomorphic domain.

### The enclosing item is the inference boundary

Constraints may flow through local definitions and later expressions in one item. They may not flow from a later top-level item into an earlier item.

This matches the existing elaborator lifecycle: parked work is drained after each top-level item and after the body of a local binding region. No new module-wide inference phase is introduced.

### Unconstrained lambdas still fail

An unannotated domain must be transitively ground by the end of its enclosing item.

```crs
(x) => x
```

This remains an inference error. The diagnostic should be anchored at the lambda parameter or its domain site, not emitted later as an unlocated zonking failure.

### No constructor-name guessing

An inductive match whose scrutinee type is unknown must wait for an actual type constraint. The compiler must not infer an inductive solely from arm tags because tags are not globally unique and such guessing would make name resolution affect typing unpredictably.

### Primitive matches constrain their carriers eagerly

Primitive match forms have an unambiguous carrier and should solve an unknown scrutinee type immediately rather than park:

- Boolean arms constrain the scrutinee to `Bool`.
- Numeric switch arms constrain the scrutinee to `Nat`.
- Bit and byte arms constrain the scrutinee to their respective packed primitive types.
- List-shaped arms constrain the scrutinee to `Lst(?Element)`; the element type may remain unknown until another constraint solves it.

## Existing machinery to extend

The core elaborator already has the necessary scheduling model:

- `ParkedWork` in `curios-elab/src/context.rs` represents delayed conversion, checking, and witness obligations.
- A parked obligation freezes the assumptions, definitions, refinements, scrutinee and projection refinements, and witness binders needed to retry it in the original lexical environment.
- Solving a metavariable wakes obligations that directly watch it.
- Retried checking uses a fresh placeholder and later solves that placeholder with the rebuilt term.
- Final draining retries remaining work and reports obligations that cannot make progress.
- The module elaborator drains at item boundaries, providing the desired inference scope without a new global pass.

The project should extend this machinery rather than add a second scheduler.

## Core design

### Allow provisional lambda domains

`elaborate_func_infer` currently rejects an unannotated domain that remains a metavariable before the body can provide a constraint. Remove that early rejection and permit the inferred function type to contain the domain metavariable while the enclosing item is still being elaborated.

This relaxation alone is insufficient: structural operations in the body must be able to suspend, and every unannotated domain needs an explicit end-of-item groundness obligation.

### Add parked inference

Add an inference-shaped parked obligation alongside the existing checking-shaped obligation. The conceptual payload is:

```rust
Inference {
    term: Term,
    blocker: Term,
    term_placeholder: MetavarId,
    type_placeholder: MetavarId,
}
```

The exact Rust representation may follow the surrounding enum and ownership conventions, but it must preserve these roles:

- `term` is the residual, partially rebuilt term to retry.
- `blocker` identifies the type information that prevented progress.
- `term_placeholder` stands for the elaborated core term.
- `type_placeholder` stands for its inferred type.
- The existing frozen frame records the lexical environment in which both placeholders and the residual term are valid.

### Use paired placeholders

When inference parks, create a fresh type placeholder `?T : Type` and a fresh term placeholder `?e : ?T`, then return `(?e, ?T)` to the caller.

Both placeholders must be created inside the exact current frame. This is required for inferred result types that mention lambda parameters or other local assumptions.

On retry:

1. Reduce the blocker enough to determine whether progress is possible.
2. If it is still blocked, re-park without manufacturing a second pair of placeholders.
3. Restore the frozen frame.
4. Infer the residual term.
5. Solve the type placeholder with the inferred type.
6. Solve the term placeholder with the rebuilt term.

Directly solving these internal placeholders is justified by the same invariant used by retried checking: the replacement was elaborated under the placeholder's exact birth frame. This also avoids relying on general flex-flex orientation or metavariable pruning for an internal bookkeeping equation.

### Park the residual term, not the untouched source term

Every structural park site must first infer or rebuild the portion that is already known, then store that residual term in the obligation.

This monotonicity rule is essential. Retrying the untouched source term can allocate fresh implicit arguments, witness goals, or nested metavariables on every wake-up, leaving duplicate or orphaned obligations. A retry must continue the same elaboration, not start a parallel one.

### Add a groundness obligation

Every unannotated lambda domain should register an obligation conceptually equivalent to:

```rust
Ground {
    type_: Term,
}
```

The obligation is complete once the type is transitively ground. The implementation should reuse or centralize the transitive-groundness logic currently associated with application elaboration rather than testing only whether the outer term is a bare metavariable.

If the obligation survives the enclosing item's final drain, report `CannotInfer` at the lambda domain span.

The groundness obligation is separate from parked body inference. A body such as `(x) => x` does not itself block on a structural operation, but its parameter type is still underconstrained and must fail predictably.

### Watch transitive blockers

Existing wake-up sets are organized around directly solved metavariables. A blocker can instead be an alias or a type expression whose unsolved leaves are metavariables.

New parked inference and groundness work should watch the transitively unsolved metavariable leaves of the blocker. The final drain remains a safety net, but ordinary progress should wake obligations promptly rather than waiting until the item ends.

## Structural operations that must park

### Inductive matching

In `elaborate_induct_match`, if the rebuilt scrutinee type reduces to an unsolved metavariable, park inference of the entire residual match. Once the type is known, retry the ordinary inductive lookup, motive construction, coverage checks, refinement handling, and branch elaboration.

Do not pre-resolve tags or choose an inductive while the carrier is unknown.

### Projection

In `elaborate_proj`, if the rebuilt head type is an unsolved metavariable, park the residual projection. This enables code such as `(pair) => pair.0` to be constrained by a later call.

Do not guess a tuple or record skeleton from the projection alone in this project.

### Application

In `elaborate_apply`, if the rebuilt callee type is an unsolved metavariable, park the residual application. This enables a lambda such as `(f, x) => f(x)` to be constrained by a later use.

Do not solve the callee type to an invented function skeleton. Curios functions carry explicit, implicit, and witness plicities and may have dependent codomains; guessing a skeleton before those facts are known would commit to semantics that the source did not supply.

### Primitive matching

Before using the generic structural parking path, recognize primitive arm shapes and unify the scrutinee with their known carrier types. List matching may introduce a fresh element metavariable as part of `Lst(?Element)`.

## Retry and scheduler invariants

- A retry that encounters the same unresolved blocker reuses the existing placeholders and obligation identity.
- A retry that reaches a deeper blocker may create nested parked work, but it must not recursively spin during the same wake-up cycle.
- Solving either placeholder must obey the ordinary occurs and scope checks applicable to its birth frame.
- Parked inference may temporarily place placeholders inside definitions, but reduction must continue to treat unsolved metavariables as stuck and solutions as monotonic.
- No normalization cache may assume that an unsolved metavariable remains unsolved after an obligation wakes.
- The final successful module remains meta-free under the existing zonking checks.
- Retrying under a frozen frame must preserve representation-visibility islands, refinements, and witness scope exactly as immediate elaboration would.

## Conversion-oracle behavior

Some elaboration runs under an oracle or transaction where speculative obligations must not escape if the enclosing conversion attempt is rolled back.

When parking is suppressed by that mode, structural inference must return the existing local inference or mismatch result rather than installing provisional placeholders. The feature must not let delayed work outlive the conversion state whose assumptions justified it.

## Diagnostics

The primary diagnostic for an unresolved lambda domain is the existing `CannotInfer` family, anchored at the unannotated parameter.

Ordinary errors discovered after a wake-up should retain their original source spans and categories. Examples include unknown or incomplete match arms, private representation access, witness ambiguity, and type mismatch.

A retry should not replace a useful structural error with a generic unsolved-metavariable error merely because the structural check happened later.

## Scope and soundness risks

### Placeholder scope

The term and type placeholders for a parked expression must be born under the lambda binder if their eventual solutions may mention it. Creating them outside that frame would either reject valid dependent results or allow an escaping local variable.

### Metavariable pruning

General pruning of out-of-scope metavariables is not part of this project. Internal placeholder equations should therefore be oriented so the placeholder is solved directly by the term elaborated in its own frame, minimizing new dependence on unfinished flex-flex pruning behavior.

### Refinements and witnesses

A delayed match must see the same branch refinements as an immediate match, and delayed applications must neither duplicate nor silently discard witness-resolution work. Tests must cover both paths.

### Representation visibility

The frozen frame and item boundary must retain the active representation island. Delaying a projection or match must not allow private constructors or fields to become visible outside the scope where the expression originated.

### Local definitions

Definitions containing provisional placeholders are safe only if all outstanding work is drained before the item leaves its elaboration boundary. No placeholder-backed definition may enter a later top-level item's context.

## Implementation map

The likely implementation surface is:

- `curios-elab/src/context.rs`: inference and groundness parked-work variants, blocker watchers, frozen-frame payloads, and obligation bookkeeping.
- `curios-elab/src/typing.rs`: retry behavior, placeholder solving, wake-up policy, and final draining diagnostics.
- `curios-elab/src/elaborate/binding.rs`: provisional unannotated lambda domains and groundness registration.
- `curios-elab/src/elaborate/apply.rs`: application blocking and shared transitive-groundness support.
- `curios-elab/src/elaborate/aggregate.rs`: projection blocking.
- `curios-elab/src/elaborate/match_.rs`: inductive match blocking and eager primitive carrier constraints.
- `curios-elab/src/elaborate/tests.rs` and focused neighboring test modules: elaboration and scheduler coverage.
- `curios/src/tests/`: cross-stage programs proving that accepted inferred terms compile and run.
- `documentation/SYNTAX.md`, `documentation/ROADMAP.md`, and affected module rustdocs: durable documentation once the feature lands.

No erased IR, continuation IR, wasm, ABI, or runtime changes are expected.

Before implementation, re-read the listed core module documentation and the current versions of every target file; the specification describes responsibilities and invariants rather than freezing their present internal APIs.

## Suggested implementation sequence

1. Relax lambda inference and add the end-of-item groundness obligation; pin successful later use, unconstrained failure, and monomorphic conflict.
2. Add paired inference placeholders and scheduler support, initially exercised through a narrow synthetic or existing blocker path.
3. Add application and projection parking.
4. Add inductive-match parking without tag guessing.
5. Add eager primitive carrier constraints.
6. Add dependent, witness, refinement, privacy, integration, and diagnostic coverage.
7. Update durable documentation and then remove this handoff specification if desired.

This sequence produces testable scheduler behavior before the most semantically rich match path depends on it.

## Acceptance tests

At minimum, the implementation should pin the following cases:

- A local identity lambda is fixed by a later `Nat` call in the same item.
- Repeated uses at the same type succeed; uses at incompatible types fail monomorphically.
- A standalone unannotated identity lambda fails at its parameter span.
- An `Option` match inside a lambda is inferred from a later call.
- A `Result` match and a user-defined indexed-inductive match retry with the correct refinements.
- Boolean, numeric, bit, byte, and list primitive matches constrain their carrier immediately.
- A list match can leave its element type open temporarily, then either receive a later constraint or fail at the item boundary.
- Projection from an initially unknown parameter type succeeds after a later call constrains it.
- Calling an initially unknown function parameter succeeds after a later use constrains its full function type and plicity.
- A dependent inferred result may mention a lambda parameter without escaping its scope.
- Nested structural blockers make progress without duplicate placeholders or an infinite retry loop.
- Witness goals created before or during a retry are resolved once and retain their source spans.
- Representation privacy and match refinements behave identically before and after parking.
- Work cannot remain unresolved across a top-level item boundary.
- Oracle-mode elaboration does not leak parked obligations.
- Successful module zonking contains no unsolved metavariables.

Where practical, scheduler tests should assert obligation counts or placeholder reuse so a superficially successful program cannot hide duplicated delayed work.

## Effort estimate

This is a medium-to-large core elaboration project. A robust implementation is likely to require approximately 250–450 lines of implementation and 300–500 lines of tests across roughly 7–10 files. The uncertainty lies less in the individual park sites than in preserving retry monotonicity, lexical scope, diagnostics, and solver transaction boundaries.

A match-only proof of concept could be smaller, but it would leave ordinary lambdas inconsistent across match, projection, and application bodies and should not be treated as the finished feature.

## Non-goals

- Hindley–Milner generalization or let-polymorphism.
- Cross-item or module-wide inference.
- Inferring an inductive from constructor spellings alone.
- Inventing function, tuple, record, or inductive skeletons from an operation on an otherwise unconstrained value.
- Changing implicit or witness argument semantics.
- Introducing a new core term or changing downstream IRs.
- Adding anonymous match-function syntax; that is the follow-up project specified separately.

## Verification

After implementation, run the repository's full done bar in order:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features
```

Because `curios-elab` is in the browser compiler's dependency graph, also run the `curios-web` wasm32 build and its matching `wasm-bindgen --target web` step.

## Retirement criteria

- Before this specification is deleted, lambda inference semantics are recorded in `SYNTAX.md`, parking and retry invariants are recorded in the owning `curios-elab` module documentation and tests, remaining plans refer to the landed elaborator behavior rather than this file, the roadmap entry is a checked unlinked summary, and no reference to this filename remains.
