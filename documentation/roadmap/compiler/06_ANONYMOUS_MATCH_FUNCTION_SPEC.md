# Anonymous match functions

Working implementation specification for a surface form analogous to OCaml's `function`, expressed in Curios as `match =>`, together with the inference-position machinery that lets it be used where no expected function type supplies its scrutinee carrier.

This document is the implementation handoff for the feature. Its durable user-facing semantics belong in `SYNTAX.md`, lowering invariants in `curios-text` module documentation and tests, and scheduler invariants in `curios-elab` module documentation and tests.

## Design decision: the form carries the inference

On 2026-08-08, monomorphic use-driven lambda inference was cut from the inference-and-unification specification (retired the same day; its solver items landed — see `curios-elab/README.md`'s "Undecided conversions park" decision — and its corpus record lives in that campaign's history) as a general language feature and reframed as this form's machinery. The corpus measurement found no demand for it among `/std`'s explicit implicit arguments — prelude lambdas sit in checking position, where domains already flow from the expected type — and its only concrete consumer was this syntax. One *indirect* consumer class did surface: `/std/Toml`'s builders keep ~15 local-`let` annotations solely because their values are inference-position matches (`let permitted: Result({}, Str) = match Map/get(side, kp) …`), exactly the park-on-unknown-structure shape stage 2 specifies — dropping those annotations is a stage-2 acceptance probe.

The reframing changes the language story, not just the document layout. An unannotated lambda in inference position stays rejected, uniformly (`curios-elab/src/elaborate/binding.rs:722` refuses a bare-metavariable domain with `CannotInfer`); `match =>` is the one form that acquires use-driven inference, because the form itself declares that its single parameter is the scrutinee. The prior specification warned that implementing only match-shaped parking "would leave ordinary lambdas inconsistent across match, projection, and application bodies" — that worry assumed general lambda inference was the goal, and dissolves with it: there is no partially-inferring lambda feature to be inconsistent about, only a syntax form with defined behavior. This follows the same philosophy as the closed operator grammar: the form is fixed, and the form carries its semantics.

What the reframing cuts from the inherited plan, deliberately: projection parking (`(pair) => pair.0`) and application parking (`(f, x) => f(x)`) have no consumer once general lambda inference is off the table — nothing lowered from `match =>` produces either shape. Both are additive later on top of the machinery specified here, if demand for general inference ever materializes. When the feature lands, this rationale moves to `DESIGN.md`.

## Staging

The project lands in two independent stages:

- **Stage 1 — the syntax** (`curios-text` only): parsing, printing, and lowering. In checking position the form works through the ordinary elaborator, since the expected function type supplies the domain and the desugared match proceeds normally. In inference position the lowered lambda's metavariable domain hits the existing rejection, which must then anchor its `CannotInfer` at the `match =>` introducer; the diagnostic should suggest annotating or using the form where its type is known. Stage 1 is shippable alone and expects no `curios-elab` change.
- **Stage 2 — inference-position elaboration** (`curios-elab`): the parked-inference machinery that lets a later use in the same item pin the scrutinee carrier. Its diagnostic prerequisite already landed: a parked conversion surviving the drain reports as a postponement naming its watched metavariables (`Error::PostponedConversion`), so stage-2 parking failures will be attributable rather than surfacing as bare mismatches.

## Proposed syntax

```crs
match =>
| some(x) => x
| none() => default
end
```

The expression denotes a function of exactly one explicit argument. Applying it is equivalent to applying an ordinary single-argument lambda whose body is a headed match:

```crs
(value) =>
  match value
  | some(x) => x
  | none() => default
  end
```

The spelling deliberately extends the existing `match` family instead of reserving `function` as a new keyword.

## Grammar and disambiguation

The conceptual grammar is:

```text
match-function ::= "match" "=>" matrix-arm* "end"
matrix-arm     ::= "|" match-pattern "=>" term
```

Curios then has two visually distinct forms after the `match` keyword:

| Prefix | Meaning |
| --- | --- |
| `match <term>` | Ordinary headed pattern match |
| `match =>` | Anonymous one-argument match function |

The token after `match` is therefore a complete local discriminator. No whitespace-sensitive rule is required. `choose | test => body … end` is its own reserved keyword, disjoint from `match` entirely (see `SYNTAX.md`'s `choose` section) — it needs no disambiguation against this dispatch.

## Semantics

### One explicit argument

Every `match =>` expression constructs a function with exactly one explicit parameter. A tuple pattern matches one tuple-valued argument; it does not create a multi-argument function.

```crs
match =>
| (x, y) => x
end
```

The example above has one tuple argument and is equivalent to `(pair) => match pair | (x, y) => x end`.

Additional arguments are expressed by nesting ordinary lambdas or another match function.

### Ordinary matrix semantics

The arms have exactly the semantics of an existing headed match matrix:

- Constructor coverage and exhaustiveness rules are unchanged.
- Pattern refinements and dependent typing behavior are unchanged.
- A final wildcard arm has the same meaning and restrictions as in a headed match.
- Zero arms are legal exactly when an ordinary zero-arm match is legal for the inferred scrutinee type.
- Arm order does not introduce OCaml-style first-match row priority if Curios's existing matrix semantics do not provide it.

This is syntactic abstraction over a headed match, not a second pattern-matching language.

### No condition-ladder arms

`match =>` accepts matrix patterns only. It does not accept `choose`'s condition or binding forms, including `pattern = value` arms.

The separation is important: `choose` branches on independent conditions or bindings, whereas `match =>` partitions one future scrutinee.

### No explicit motive in the initial form

The initial syntax has no explicit motive slot. Users who need to spell a dependent motive can write the equivalent ordinary lambda and headed match:

```crs
(value) =>
  match value : motive
  | pattern => body
  end
```

A motive-less anonymous match function may still check against an expected dependent function type through the ordinary elaborator. The feature adds no special dependent-motive inference guarantee beyond that equivalence.

### Effects and postfix `!`

The anonymous matcher is a lambda boundary. Constructing it performs no branch effects; a postfix-`!` action inside an arm runs only when the function is applied and that arm is selected.

Lowering must use the same region-root behavior as an ordinary lambda body so do-notation cannot hoist an arm action outside the generated function.

### Direct invocation

Normal expression precedence applies. A directly invoked anonymous matcher should be written parenthesized unless the existing application grammar proves otherwise:

```crs
(match =>
 | some(x) => x
 | none() => 0
 end)(value)
```

The project should not add a special application precedence rule for this form.

## Surface AST

Retain the construct explicitly in the text AST rather than desugaring it in the parser. Every surface form is its own top-level `Subterm` variant (see `choose`'s own `Subterm::Choose` for the precedent this follows); there is no longer a `Match` enum to nest a new variant inside. A conceptual shape is:

```rust
struct MatchFunc {
    arms: Vec<MatrixArm>,
}

enum Subterm {
    …
    Match(Match),
    Choose(Choose),
    MatchFunc(MatchFunc),
    …
}
```

Names should follow the existing AST vocabulary rather than this sketch if they differ.

Keeping the sugar in the surface AST provides:

- Exact parser tests without manufacturing a source-level binder that the user never wrote.
- Canonical pretty-printing and parse-print round trips.
- Better source spans and diagnostics.
- One explicit lowering point where equivalence to a lambda can be audited.

No new core AST node is warranted.

## Parser design

The implementation belongs with the existing match parsers in `curios-text/src/parse/match_expr.rs`.

The parser should recognize the shared prefix `match =>` as the commitment point, then reuse the ordinary inductive matrix-arm parser, likely `parse_inductive_match_branch` or its current equivalent.

Only failure to see the complete `match =>` prefix may backtrack to the headed match alternative — `choose` is a distinct reserved keyword and is never a backtrack target here. Once the arrow has been consumed, a malformed arm or missing `end` must report an anonymous-match-function syntax error rather than silently reinterpret the expression as another match form.

A likely choice structure is conceptually:

```rust
parse_match_func()
    .or(parse_match())
```

(`parse_match` is the current name of the headed-only matrix parser in `curios-text/src/parse/match_expr.rs`.)

The exact placement of `catch` must follow the parser monad's current commitment conventions. The invariant, not the literal combinator sequence, is normative.

No lexer change or keyword addition is needed. In particular, `function` remains a legal identifier.

## Pretty-printing

The canonical printer should preserve the new form:

```crs
match =>
| pattern => body
end
```

It should not print a generated lambda parameter and headed match. Arm layout, indentation, multiline decisions, and `end` placement should reuse the headed matrix printer so the two forms remain visually parallel.

If the existing printer duplicates arm formatting between match variants, factoring a shared matrix-arm printer is in scope only as needed for this feature.

## Lowering

Lower entirely in `curios-text` to existing core constructs:

1. Mint a fresh internal binder with no surface spelling, carrying the `match =>` introducer's span.
2. Create the free core variable that refers to that binder.
3. Compile the arms with the ordinary headed matrix compiler using the variable as the scrutinee and no explicit motive.
4. Infer or create the lambda domain metavariable in the same order used by ordinary unannotated lambda lowering.
5. Wrap the compiled body with the existing explicit `Term::func` representation.

The lowering should share the headed matrix compiler entry point, such as `compile_matrix_headed`, rather than reproducing coverage, refinement, or motive logic.

For ordinary term lowering, arm leaves should use the existing `MatchCompiler::term` route. For region or do-notation lowering, they should use `MatchCompiler::region` so the generated lambda remains the region root.

Freshness and metavariable allocation order should match the explicit lambda spelling as closely as practical, especially if diagnostics or golden core prints expose generated identifiers.

In stage 2, the inference-position route must additionally let elaboration know the function came from `match =>`, so that only this form receives a provisional domain while ordinary lambdas keep the `binding.rs:722` rejection. The transient mechanism is the precedent — postfix `!` reaches elaboration as `Transient::Bang` — but the exact carrier should follow whatever provenance convention the tree has when stage 2 starts. In checking position the form elaborates exactly as the desugared lambda in both stages.

## Stage 2 — inference-position elaboration

### Goal

Permit an anonymous match function whose scrutinee carrier is temporarily unknown to receive enough type information from a later use in the same item:

```crs
let unwrap = match =>
  | some(x) => x
  | none() => 0
  end;
unwrap(Option/some(42))
```

The intended elaboration sequence:

1. Give the minted parameter a fresh metavariable domain `?A`.
2. Discover that the match cannot proceed while `?A` is unknown, and park inference of the match behind paired term and type placeholders.
3. Return a provisional function type `(?A) -> ?R`.
4. Elaborate `unwrap(Option/some(42))`, solving `?A := Option(Nat)`.
5. Wake the parked match, restore the lexical frame in which it was created, and infer it as `Nat`, solving both its term and type placeholders.
6. Zonk the enclosing item normally, with no unsolved metavariables remaining.

### User-visible semantics

**Monomorphic inference.** The minted parameter receives one metavariable for the enclosing item. Uses constrain that single metavariable; they do not instantiate a generalized type scheme. Two uses at one type succeed; two uses at incompatible types fail, because they demand inconsistent solutions for the same monomorphic domain.

**The enclosing item is the inference boundary.** Constraints may flow through local definitions and later expressions in one item; they may not flow from a later top-level item into an earlier one. This matches the existing lifecycle — parked work is drained after each top-level item — so no module-wide inference phase is introduced.

**Unconstrained matchers still fail.** The domain must be transitively ground by the end of its enclosing item. A standalone unconstrained `match => … end` remains an inference error, anchored at the `match =>` introducer rather than emitted later as an unlocated zonking failure.

**No constructor-name guessing.** An inductive matcher whose scrutinee type is unknown must wait for an actual type constraint. Inferring an inductive from arm tags alone is forbidden: tags are not globally unique, and guessing would make name resolution affect typing unpredictably.

**Intrinsic arms constrain their carriers eagerly.** Intrinsic arm shapes have an unambiguous carrier and should solve an unknown scrutinee type immediately rather than park — Boolean arms to `Bool`, numeric switch arms to `Nat`, bit and byte arms to their packed intrinsics, list-shaped arms to `List(?Element)` with the element type free to remain unknown.

### Core design

**Provisional domain, scoped to this form.** Only a function elaboration reached through the `match =>` provenance may carry a metavariable domain past the `binding.rs:722` rejection; every such domain registers an end-of-item groundness obligation, conceptually `Ground { type_: Term }`, complete once the type is transitively ground, whose survivor reports `CannotInfer` at the introducer span.

**Parked match inference.** A new obligation alongside the existing three, conceptually:

```rust
Inference {
    term: Term,
    blocker: Term,
    term_placeholder: MetavarId,
    type_placeholder: MetavarId,
}
```

`term` is the residual match to retry; `blocker` identifies the type information that prevented progress; the two placeholders stand for the elaborated core term and its inferred type. The existing frozen frame records the lexical environment in which all three are valid. The Rust spelling should follow the surrounding enum and ownership conventions rather than this sketch.

**Paired placeholders.** When inference parks, create a fresh type placeholder `?T : Type` and a fresh term placeholder `?e : ?T`, then return `(?e, ?T)`. Both must be created inside the exact current frame, which is what allows an inferred result type to mention the matcher's own parameter or other local assumptions. On retry: reduce the blocker enough to decide whether progress is possible; if still blocked, re-park without manufacturing a second pair; otherwise restore the frozen frame, infer the residual match, solve the type placeholder with the inferred type, and solve the term placeholder with the rebuilt term. Solving these internal placeholders directly is justified by the same invariant retried checking uses — the replacement was elaborated under the placeholder's exact birth frame — and deliberately avoids depending on pruning or flex-flex orientation.

**Park the residual term, not the untouched source term.** The park site must first infer or rebuild the portion already known, and store that in the obligation. Retrying the untouched source term can allocate fresh implicit arguments, witness goals, or nested metavariables on every wake-up, leaving duplicate or orphaned obligations. A retry continues one elaboration; it does not start a parallel one.

**Watch transitive blockers.** The blocker can be a type expression whose unsolved leaves are metavariables rather than a bare metavariable; the obligation should watch those transitively unsolved leaves, keeping the final drain as a safety net rather than the primary mechanism.

**On retry, run the ordinary match path.** The woken match runs the ordinary inductive lookup, motive construction, coverage checks, refinement handling, and branch elaboration. Do not pre-resolve tags.

### Inherited safety discipline

The scheduling campaign that landed `ParkedWork::Checking` left a defect ledger, and it is this stage's inherited discipline rather than background reading:

- A frozen frame is restored by reapplying only identities not already live. An intra-item wake under the older restoration doubled live binders, giving every metavariable born in the retry a non-linear identity spine that pattern inversion cannot invert.
- A placeholder solved ahead of its retry ends the obligation without re-elaborating. A second elaboration of a term whose lowering-minted holes are already birthed drops their spines.
- A rollback bracket may not contain retries. It would consume obligations whose solutions it then unwinds.

The authoritative description is `curios-elab/README.md`, "Postponement is a parked obligation, never a raw substitution".

### Retry and scheduler invariants

- A retry meeting the same unresolved blocker reuses the existing placeholders and obligation identity.
- A retry reaching a deeper blocker may create nested parked work but must not recursively spin during one wake-up cycle.
- Solving either placeholder obeys the ordinary occurs and scope checks for its birth frame.
- Reduction continues to treat unsolved metavariables as stuck and solutions as monotonic; no normalization cache may assume an unsolved metavariable remains unsolved after an obligation wakes.
- The final successful module remains meta-free under existing zonking checks.
- Retrying under a frozen frame preserves representation-visibility islands, refinements, and witness scope exactly as immediate elaboration would.

### Conversion-oracle behavior

Some elaboration runs under an oracle or transaction where speculative obligations must not escape a rolled-back attempt. When parking is suppressed by that mode, the matcher must return the existing local inference or mismatch result rather than installing provisional placeholders. Delayed work must never outlive the conversion state whose assumptions justified it.

### Diagnostics

The primary diagnostic for an unresolved domain is the existing `CannotInfer` family, anchored at the `match =>` introducer. Ordinary errors discovered after a wake-up retain their original spans and categories — unknown or incomplete match arms, private representation access, witness ambiguity, type mismatch. A retry must not replace a useful structural error with a generic unsolved-metavariable error merely because the check happened later.

### Scope and soundness risks

- **Placeholder scope.** Both placeholders must be born under the minted binder if their solutions may mention it. Creating them outside that frame either rejects valid dependent results or lets a local escape.
- **Refinements and witnesses.** A delayed match must see the same branch refinements as an immediate one, and witness goals created before or during a retry must resolve once, with source spans retained.
- **Representation visibility.** The frozen frame and item boundary must retain the active representation island, so delaying a match cannot make private constructors or fields visible outside their origin scope.
- **Local definitions.** Definitions containing provisional placeholders are safe only if all outstanding work drains before the item leaves its elaboration boundary. No placeholder-backed definition may enter a later item's context.

### Relationship to the inference specification

Stage 2 does not require pruning (which was itself resolved without implementation when its measured consumers turned out to be a solve-materialization defect, since fixed): its placeholder equations are deliberately oriented so the placeholder is solved directly by a term elaborated in its own frame, minimizing dependence on flex-flex behavior. That orientation is a design constraint, not an accident, and should survive any reordering. The residual-diagnostics prerequisite named under Staging has landed.

## Implementation map

Stage 1, the likely surface:

- `curios-text/src/term.rs`: the explicit surface AST variant and associated span behavior.
- `curios-text/src/parse.rs`: parser-module imports or dispatch wiring if required.
- `curios-text/src/parse/match_expr.rs`: `match =>` recognition, committed diagnostics, and shared arm parsing.
- `curios-text/src/print.rs`: canonical rendering and shared matrix-arm formatting.
- `curios-text/src/into_core/lowerer.rs`: dispatch from the new surface node.
- `curios-text/src/into_core/match_compile.rs`: reuse of headed matrix compilation and term/region entry points.
- Parser and printer test modules beside those components, plus `curios-text` lowering tests and `curios/src/tests/matching.rs` for semantic equivalence and cross-stage execution.

Stage 2, the likely surface:

- `curios-elab/src/elaborate/binding.rs`: the provenance-scoped provisional domain and groundness registration (the rejection at `:722` stays for ordinary lambdas).
- `curios-elab/src/elaborate/match_.rs`: match parking and eager intrinsic carriers.
- `curios-elab/src/context/solutions.rs`: the inference obligation variant, blocker watchers, and bookkeeping.
- `curios-elab/src/typing.rs`: retry behavior, placeholder solving, wake-up policy, and final-drain diagnostics.
- Test modules beside each, plus `curios/src/tests/` for cross-stage programs.

Both stages end at `documentation/SYNTAX.md`, `documentation/ROADMAP.md`, and affected module rustdocs. Before implementation, re-read `SYNTAX.md`, the module documentation, and the current versions of every target file; the specification describes responsibilities and invariants rather than freezing their present internal APIs.

## Diagnostics (stage 1)

- `match =>` followed by a malformed matrix arm should point into that arm and remain committed to the new form.
- A missing `end` should identify the anonymous match function as the unterminated construct.
- A `choose`-style condition or bind arm written after `match =>` should receive a matrix-pattern error rather than being reinterpreted as `choose`.
- Exhaustiveness, impossible-pattern, private-representation, and branch-type errors should be the same errors produced by the equivalent explicit lambda and headed match.
- An inference-position occurrence should report `CannotInfer` anchored at the `match =>` introducer, suggesting an annotation or a checking-position use, until stage 2 lands.

## Acceptance tests

Stage 1:

- The parser produces the explicit anonymous-match-function AST for inline and multiline forms.
- Parse-print-parse round trips preserve the form and its arms.
- `match <term>` and `match =>` remain unambiguous; `choose` needs no disambiguation against this dispatch, being a distinct reserved keyword.
- A malformed construct after the consumed arrow does not backtrack into another match parser.
- Zero-arm syntax parses and delegates legality to ordinary match elaboration.
- An annotated or expected function type checks an anonymous match function, including dependent expected types.
- An inference-position occurrence fails at the introducer with the annotate-or-check diagnostic.
- A tuple pattern denotes one tuple argument.
- A wildcard arm and nested patterns behave exactly as in a headed match.
- Coverage, impossible-pattern, refinement, and privacy errors match the explicit spelling.
- A higher-order function can receive an anonymous matcher as an argument; parenthesized direct invocation works.
- A postfix-`!` action inside an arm remains inside the generated lambda region and runs only for the selected arm.
- Compiling and running representative programs produces the same result as the explicit lambda-plus-match spelling; core prints show no new core construct.

Stage 2:

- An inductive matcher fixed by a later call in the same item; repeated uses at one type succeeding and incompatible uses failing monomorphically.
- A standalone unconstrained matcher failing at the introducer span.
- An `Option` matcher inferred from a later call; a `Result` matcher and a user-defined indexed-inductive matcher retrying with correct refinements.
- Boolean, numeric, bit, byte, and list intrinsic arms constraining their carrier immediately; a list matcher leaving its element type open and then either constrained or failed at the boundary.
- A dependent inferred result mentioning the matcher's parameter without escaping.
- Nested structural blockers making progress without duplicate placeholders or infinite retry; witness goals created before or during a retry resolved once with source spans retained.
- Representation privacy and match refinements identical before and after parking.
- An ordinary unannotated lambda in inference position still rejected — the provenance scoping is load-bearing, not incidental.
- No work surviving a top-level item boundary; oracle-mode elaboration leaking no parked obligations; successful zonking leaving no unsolved metavariables.
- Where practical, scheduler tests assert obligation counts or placeholder reuse, so a superficially successful program cannot hide duplicated delayed work.

## Suggested implementation sequence

1. Add the surface AST variant and parser with commitment tests.
2. Add canonical printing and round-trip tests.
3. Lower term bodies through the existing headed matrix compiler and wrap them in an ordinary function; pin the inference-position diagnostic.
4. Lower region bodies and pin postfix-`!` behavior.
5. Add checking-position inference, dependent/refinement, privacy, and cross-stage equivalence tests; ship stage 1.
6. Add the provenance-scoped provisional domain and groundness obligation against a narrow blocker.
7. Add paired placeholders, match parking, and scheduler support; then eager intrinsic carriers.
8. Add dependent, witness, refinement, privacy, and diagnostic coverage for the parked path.
9. Update durable syntax and roadmap documentation, then retire this specification.

## Non-goals

- General unannotated-lambda inference, including projection parking (`(pair) => pair.0`) and application parking (`(f, x) => f(x)`) — cut with the reframing recorded under Design decision, additive later if demand appears.
- Hindley–Milner generalization, let-polymorphism, or cross-item inference.
- Inferring an inductive from constructor spellings alone, or inventing function, tuple, record, or inductive skeletons from an operation on an unconstrained value.
- A `function` keyword or any new reserved word.
- OCaml-style ordered row priority distinct from Curios's current match semantics.
- Multiple implicit scrutinees or multi-argument pattern functions.
- `choose`-style condition or binding arms inside the new form.
- An explicit motive syntax in the initial version.
- Changing implicit or witness argument semantics, exhaustiveness, refinement, or representation-visibility rules.
- A new core, erased, continuation, or wasm node.

## Effort estimate

Stage 1 is a contained `curios-text` feature: parser, printer, lowering, and tests. Stage 2 is a medium `curios-elab` project — the match park site, the groundness obligation, and the scheduler additions, on machinery that already carries `ParkedWork::Checking` in production — smaller than the superseded general-lambda plan by the two cut park sites, with its uncertainty concentrated in retry monotonicity, lexical scope, and transaction boundaries rather than in the park site itself.

## Verification

After each stage, run the repository's full done bar in order:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features
```

Because `curios-text` and `curios-elab` are both in the browser compiler's dependency graph, also run `make curios/web` with the exactly version-matched `wasm-bindgen-cli`.

## Retirement criteria

- `match =>` grammar and semantics — including the inference-position behavior and its item boundary — are recorded in `SYNTAX.md`.
- Parsing, printing, and lowering invariants are recorded in the owning `curios-text` module documentation and tests; parking, retry, and provenance invariants in the owning `curios-elab` module documentation and tests.
- The design decision that the form carries the inference, and that general lambda inference was dropped for lack of demand, is recorded in `DESIGN.md`.
- Remaining plans refer to the landed syntax rather than this file, the roadmap entry is a checked unlinked summary, and no reference to this filename remains.
