# Anonymous match functions

Working implementation specification for a surface form analogous to OCaml's `function`, expressed in Curios as `match =>`, together with the inference-position machinery that lets it be used where no expected function type supplies its scrutinee carrier.

This document states responsibilities and invariants, not today's internal APIs — those move, and this feature has not started. Its durable user-facing semantics belong in `SYNTAX.md`, lowering invariants in `curios-text` module documentation and tests, and scheduler invariants in `curios-elab` module documentation and tests.

## Design decision: the form carries the inference

Monomorphic use-driven lambda inference was cut as a general language feature and reframed as this form's machinery. The corpus measurement found no demand for it among `/std`'s explicitly supplied implicit arguments — prelude lambdas sit in checking position, where domains already flow from the expected type — and its only concrete consumer was this syntax. One *indirect* consumer class did surface: `/std/Toml`'s builders keep local-`let` annotations solely because their values are inference-position matches, exactly the park-on-unknown-structure shape stage 2 specifies, so dropping those annotations is a stage-2 acceptance probe.

The reframing changes the language story, not just the document layout. An unannotated lambda in inference position stays rejected, uniformly — `curios-elab`'s `elaborate_func_infer` refuses a bare-metavariable domain with `CannotInfer`. `match =>` is the one form that acquires use-driven inference, because the form itself declares that its single parameter is the scrutinee. An earlier draft warned that implementing only match-shaped parking "would leave ordinary lambdas inconsistent across match, projection, and application bodies"; that worry assumed general lambda inference was the goal, and dissolves with it — there is no partially-inferring lambda feature to be inconsistent about, only a syntax form with defined behavior. This follows the same philosophy as the closed operator grammar: the form is fixed, and the form carries its semantics.

What the reframing cuts, deliberately: projection parking (`(pair) => pair.0`) and application parking (`(f, x) => f(x)`) have no consumer once general lambda inference is off the table, since nothing lowered from `match =>` produces either shape. Both are additive later on top of the machinery specified here. When the feature lands, this rationale moves to `DESIGN.md`.

## Staging

- **Stage 1 — the syntax** (`curios-text` only): parsing, printing, and lowering. In checking position the form works through the ordinary elaborator, since the expected function type supplies the domain and the desugared match proceeds normally. In inference position the lowered lambda's metavariable domain hits the existing rejection, which must then anchor its `CannotInfer` at the `match =>` introducer and suggest annotating or using the form where its type is known. Stage 1 is shippable alone and expects no `curios-elab` change.
- **Stage 2 — inference-position elaboration** (`curios-elab`): the parked-inference machinery that lets a later use in the same item pin the scrutinee carrier. Its diagnostic prerequisite already landed: a parked conversion surviving the drain reports as a postponement naming its watched metavariables, so stage-2 parking failures will be attributable rather than surfacing as bare mismatches.

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

The spelling deliberately extends the existing `match` family instead of reserving `function` as a new keyword, which stays a legal identifier. The token after `match` is a complete local discriminator — `match <term>` is the headed match, `match =>` this form — so no whitespace-sensitive rule is required, and no lexer change or keyword addition is needed. `choose` is its own reserved keyword, disjoint from `match` entirely, and needs no disambiguation against this dispatch.

## Semantics

**One explicit argument.** A tuple pattern matches one tuple-valued argument; it does not create a multi-argument function, so `match => | (x, y) => x end` is equivalent to `(pair) => match pair | (x, y) => x end`. Additional arguments are expressed by nesting ordinary lambdas or another match function.

**Ordinary matrix semantics.** The arms have exactly the semantics of an existing headed match matrix: constructor coverage and exhaustiveness rules unchanged, pattern refinements and dependent typing behavior unchanged, a final wildcard arm with the same meaning and restrictions, and zero arms legal exactly when a zero-arm match is legal for the inferred scrutinee type. Arm order introduces no OCaml-style first-match row priority, because Curios's matrix semantics do not have it. This is syntactic abstraction over a headed match, not a second pattern-matching language.

**No condition-ladder arms.** `match =>` accepts matrix patterns only, never `choose`'s condition or binding forms. The separation is the point: `choose` branches on independent conditions or bindings, whereas `match =>` partitions one future scrutinee.

**No explicit motive in the initial form.** A dependent motive is written as the equivalent ordinary lambda and headed match. A motive-less anonymous match function may still check against an expected dependent function type through the ordinary elaborator; the feature adds no dependent-motive inference guarantee beyond that equivalence.

**Effects and postfix `!`.** The anonymous matcher is a lambda boundary. Constructing it performs no branch effects; a `!` action inside an arm runs only when the function is applied and that arm is selected. Lowering must use the same region-root behavior as an ordinary lambda body, so sequencing cannot hoist an arm action outside the generated function.

**Direct invocation.** Normal expression precedence applies, so a directly invoked matcher is parenthesized. The project adds no special application precedence rule for this form.

## Surface AST, parsing, and printing

**Retain the construct explicitly in the text AST rather than desugaring it in the parser**, as every other surface form does. That buys exact parser tests without manufacturing a binder the user never wrote, canonical printing and round trips, better spans and diagnostics, and one explicit lowering point where equivalence to a lambda can be audited. No new core node is warranted.

The parser belongs with the existing match parsers. It should recognize the shared `match =>` prefix as the commitment point and then reuse the ordinary inductive matrix-arm parser. **Only failure to see the complete prefix may backtrack**; once the arrow is consumed, a malformed arm or missing `end` must report an anonymous-match-function syntax error rather than silently reinterpret the expression as another match form. The invariant is normative, not any particular combinator arrangement.

The canonical printer preserves the form rather than printing a generated lambda and headed match, reusing the headed matrix printer for arm layout, indentation, multiline decisions, and `end` placement so the two forms stay visually parallel. Factoring a shared matrix-arm printer is in scope only as needed.

## Lowering

Lower entirely in `curios-text` to existing core constructs: mint a fresh internal binder with no surface spelling carrying the introducer's span, refer to it with a free core variable, compile the arms with the ordinary headed matrix compiler using that variable as the scrutinee and no explicit motive, create the lambda domain metavariable in the same order ordinary unannotated lambda lowering uses, and wrap the compiled body in the existing explicit function representation.

The lowering shares the headed matrix compiler's entry point rather than reproducing coverage, refinement, or motive logic, and routes arm leaves through the term entry for ordinary lowering and the region entry for do-notation, so the generated lambda remains the region root. Freshness and metavariable allocation order should match the explicit lambda spelling as closely as practical, since diagnostics and golden core prints expose generated identifiers.

In stage 2 the inference-position route must additionally let elaboration know the function came from `match =>`, so only this form receives a provisional domain while ordinary lambdas keep `elaborate_func_infer`'s rejection. Postfix `!` reaching elaboration as a transient is the precedent, but the exact carrier should follow whatever provenance convention the tree has when stage 2 starts. In checking position the form elaborates exactly as the desugared lambda in both stages.

## Stage 2 — inference-position elaboration

**Goal.** An anonymous match function whose scrutinee carrier is temporarily unknown receives enough type information from a later use in the same item — `let unwrap = match => | some(x) => x | none() => 0 end; unwrap(Option/some(42))`. The minted parameter takes a fresh metavariable domain; the match parks behind paired term and type placeholders; a provisional function type is returned; the later application solves the domain; the parked match wakes, restores the lexical frame in which it was created, infers, and solves both placeholders; and the item zonks with no unsolved metavariables remaining.

**Monomorphic, and the item is the boundary.** The minted parameter receives one metavariable for the enclosing item. Uses constrain that single metavariable rather than instantiating a generalized scheme, so two uses at one type succeed and two at incompatible types fail. Constraints may flow through local definitions and later expressions in one item and may not flow from a later top-level item into an earlier one, which matches the existing lifecycle — parked work is drained after each top-level item, so no module-wide inference phase is introduced.

**Unconstrained matchers still fail.** The domain must be transitively ground by the end of its enclosing item, and a survivor reports `CannotInfer` at the `match =>` introducer rather than surfacing later as an unlocated zonking failure.

**No constructor-name guessing.** An inductive matcher whose scrutinee type is unknown must wait for an actual type constraint. Inferring an inductive from arm tags alone is forbidden: tags are not globally unique, and guessing would make name resolution affect typing unpredictably. **Intrinsic arms are the exception and constrain eagerly**, having an unambiguous carrier — boolean arms to `Bool`, numeric switch arms to `Nat`, bit and byte arms to their packed intrinsics, list-shaped arms to a list with the element type free to remain unknown.

**Paired placeholders, and park the residual rather than the source.** When inference parks, create a fresh type placeholder and a fresh term placeholder standing at it, both inside the exact current frame — which is what allows an inferred result type to mention the matcher's own parameter or other local assumptions. On retry, decide from the blocker whether progress is possible; if still blocked, re-park without manufacturing a second pair; otherwise restore the frozen frame, infer the residual match, and solve both placeholders. Solving these internal placeholders directly is justified by the invariant retried checking already uses — the replacement was elaborated under the placeholder's exact birth frame — and deliberately avoids depending on pruning or flex-flex orientation. The park site must store the portion already inferred, never the untouched source term: retrying the source can allocate fresh implicit arguments, witness goals, or nested metavariables on every wake-up, leaving duplicate or orphaned obligations. A retry continues one elaboration; it does not start a parallel one. The blocker can be a type expression whose unsolved leaves are metavariables rather than a bare metavariable, so the obligation watches those leaves transitively and the final drain stays a safety net rather than the primary mechanism. On retry the woken match runs the ordinary inductive lookup, motive construction, coverage checks, refinement handling, and branch elaboration — tags are not pre-resolved.

**Inherited safety discipline.** The scheduling campaign that landed checked-obligation parking left a defect ledger, and it is this stage's inherited discipline rather than background reading: a frozen frame is restored by reapplying only identities not already live, since doubling live binders gives every metavariable born in the retry a non-linear identity spine pattern inversion cannot invert; a placeholder solved ahead of its retry ends the obligation without re-elaborating, since a second elaboration of a term whose lowering-minted holes are already birthed drops their spines; and a rollback bracket may not contain retries, since it would consume obligations whose solutions it then unwinds. The authoritative description is `curios-elab/README.md`, "Postponement is a parked obligation, never a raw substitution".

**Scheduler invariants.** A retry meeting the same unresolved blocker reuses the existing placeholders and obligation identity. A retry reaching a deeper blocker may create nested parked work but must not recursively spin during one wake-up cycle. Solving either placeholder obeys the ordinary occurs and scope checks for its birth frame. Reduction continues to treat unsolved metavariables as stuck and solutions as monotonic, and no normalization cache may assume an unsolved metavariable stays unsolved after an obligation wakes. Retrying under a frozen frame preserves representation-visibility islands, refinements, and witness scope exactly as immediate elaboration would.

**Conversion-oracle behavior.** Where elaboration runs under an oracle or transaction whose speculative obligations must not escape a rolled-back attempt, the matcher returns the existing local inference or mismatch result rather than installing provisional placeholders. Delayed work must never outlive the conversion state whose assumptions justified it.

**Scope and soundness risks.** Both placeholders must be born under the minted binder if their solutions may mention it — creating them outside that frame either rejects valid dependent results or lets a local escape. A delayed match must see the same branch refinements as an immediate one, and witness goals created before or during a retry must resolve once with source spans retained. The frozen frame and item boundary must retain the active representation island, so delaying a match cannot make private constructors or fields visible outside their origin scope. And no placeholder-backed definition may enter a later item's context, which holds only if all outstanding work drains before the item leaves its elaboration boundary.

Stage 2 does not require pruning: its placeholder equations are deliberately oriented so the placeholder is solved directly by a term elaborated in its own frame. That orientation is a design constraint, not an accident, and should survive any reordering.

## Diagnostics

- `match =>` followed by a malformed matrix arm points into that arm and stays committed to the new form; a missing `end` identifies the anonymous match function as the unterminated construct.
- A `choose`-style condition or bind arm after `match =>` receives a matrix-pattern error rather than being reinterpreted as `choose`.
- Exhaustiveness, impossible-pattern, private-representation, and branch-type errors are the same errors the equivalent explicit lambda and headed match produce.
- An inference-position occurrence reports `CannotInfer` anchored at the introducer — suggesting an annotation or a checking-position use until stage 2 lands, and naming the unresolved domain afterwards.
- Ordinary errors discovered after a wake-up retain their original spans and categories. A retry must not replace a useful structural error with a generic unsolved-metavariable error merely because the check happened later.

## Acceptance

**Stage 1.** The parser produces the explicit AST for inline and multiline forms and round-trips through the printer; `match <term>` and `match =>` stay unambiguous; a malformed construct after the consumed arrow does not backtrack into another match parser; zero-arm syntax parses and delegates legality to ordinary elaboration; an annotated or expected function type checks the form, dependent expected types included; a tuple pattern denotes one tuple argument; wildcard and nested patterns behave as in a headed match; coverage, impossible-pattern, refinement, and privacy errors match the explicit spelling; a higher-order function can receive one as an argument and parenthesized direct invocation works; a `!` action inside an arm stays inside the generated lambda region; and compiled programs produce the same result as the explicit spelling with no new core construct in the print.

**Stage 2.** An inductive matcher fixed by a later call in the same item, with repeated uses at one type succeeding and incompatible uses failing monomorphically; a standalone unconstrained matcher failing at the introducer span; `Result` and user-defined indexed-inductive matchers retrying with correct refinements; intrinsic arms constraining their carrier immediately, with a list matcher leaving its element type open and then either constrained or failed at the boundary; a dependent inferred result mentioning the matcher's parameter without escaping; nested structural blockers making progress without duplicate placeholders or infinite retry; witness goals resolved once with spans retained; representation privacy and match refinements identical before and after parking; an ordinary unannotated lambda in inference position still rejected, since the provenance scoping is load-bearing rather than incidental; no work surviving a top-level item boundary and no parked obligation leaking from oracle-mode elaboration. Where practical, scheduler tests assert obligation counts or placeholder reuse, so a superficially successful program cannot hide duplicated delayed work.

## Non-goals

- General unannotated-lambda inference, including projection and application parking — cut with the reframing recorded above, additive later if demand appears.
- Hindley–Milner generalization, let-polymorphism, or cross-item inference.
- Inferring an inductive from constructor spellings alone, or inventing function, tuple, record, or inductive skeletons from an operation on an unconstrained value.
- A `function` keyword or any new reserved word.
- OCaml-style ordered row priority distinct from Curios's current match semantics.
- Multiple implicit scrutinees or multi-argument pattern functions.
- `choose`-style condition or binding arms inside the new form.
- An explicit motive syntax in the initial version.
- Changing implicit or witness argument semantics, exhaustiveness, refinement, or representation-visibility rules.
- A new core, erased, continuation, or wasm node.

## Verification

After each stage, run the repository's full done bar in order — [CLAUDE.md](../../../CLAUDE.md), "Before handing off code changes", which owns the command list. It was copied here verbatim once and had drifted from it within the day. Because `curios-text` and `curios-elab` are both in the browser compiler's dependency graph, also run `make curios/web` with the exactly version-matched `wasm-bindgen-cli`.

## Retirement criteria

- `match =>` grammar and semantics — including the inference-position behavior and its item boundary — are recorded in `SYNTAX.md`.
- Parsing, printing, and lowering invariants are recorded in the owning `curios-text` module documentation and tests; parking, retry, and provenance invariants in the owning `curios-elab` module documentation and tests.
- The design decision that the form carries the inference, and that general lambda inference was dropped for lack of demand, is recorded in `DESIGN.md`.
- Remaining plans refer to the landed syntax rather than this file, the roadmap entry is a checked unlinked summary, and no reference to this filename remains.
