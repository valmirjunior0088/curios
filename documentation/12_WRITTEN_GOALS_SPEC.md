# Written goals (`?label`) — design

This document specifies Curios's labeled written-goal form, complete goal reporting after successful elaboration, and the typed incomplete outcome shared by compilation and program analysis.

Written goals are a front-end capability rather than an interactive prover. They let a programmer or agent ask what belongs at several source locations, receive every answer one successful elaboration can establish, and then edit the source and analyze the new snapshot. The general `wonder` interface consumes these reports through its diagnostic model; see [13_WONDER_SPEC.md](13_WONDER_SPEC.md).

## Objective

A programmer or agent should be able to leave several explicitly labeled goals in a program and receive one deterministic batch containing every reached goal's label, source location, local scope, expected type, and inferred solution.

The workflow is:

```text
write `?label`
  → compile or request wonder diagnostics
  → inspect every reported scope, expected type, and solution
  → replace goals in source
  → analyze the new source snapshot
```

“Every goal” means every written goal reached by one otherwise-successful elaboration. Reporting useful typing information for syntax that parsing, lowering, or elaboration never reached would require general recovery and speculative contexts, which are outside this design.

## Current behavior

Today:

- Bare `?` parses to `Subterm::Goal` in `curios-text`.
- Lowering mints a fresh core metavariable tagged `MetavarOrigin::Goal`.
- Elaboration may solve that metavariable exactly like an inference hole.
- Zonk reports a written goal unconditionally, whether solved or unsolved, because writing a goal requests a report rather than silent substitution.
- `Error::Goal` carries the frozen local scope, expected type, optional solution, and the goal term's source span.
- Multiple goals parse and elaborate, but ordinary `Result` propagation makes zonk stop at the first goal it visits.

The existing distinction between written goals and silent compiler-generated inference holes is foundational. This design makes the written form labeled, makes reporting complete, and represents incompleteness separately from hard failure.

## Syntax

The only written form is:

```text
?label
```

The grammar is:

```text
goal  := "?" label
label := non-keyword identifier
```

The label is glued to `?`; whitespace is not permitted between them. It follows the ordinary single-identifier grammar and receives no goal-specific character restrictions.

Examples:

```text
?elementType
?step2
?_proof
```

Bare `?` is removed immediately rather than accepted through a warning period. Once the parser consumes `?`, it commits to the goal production and requires the glued label. Both `?` and `? label` produce a targeted parse diagnostic such as:

```text
written goals require a label; write `?name`
```

The surface representation is conceptually `Subterm::Goal(String)`. Its span covers the complete `?label` spelling, and the printer emits that spelling exactly.

## Meaning of labels

A label is required descriptive correlation metadata, not a semantic metavariable name.

- Every written occurrence mints its own metavariable.
- Repeating a label does not share a metavariable or add an equality constraint.
- Duplicate labels are legal, including within one declaration.
- Consumers filtering by label must accept that the result can contain several goals.
- Renaming a label changes report metadata but has no typing effect.

Required labels make every probe intentional and give humans and tools an authored name to display. Requiring uniqueness would instead create a new namespace with scoping rules across local declarations, mutual groups, entrypoint tails, and multiple source roots. Source location and snapshot-local occurrence identity already disambiguate duplicates, so uniqueness is not part of this feature.

Shared named metavariables would be a separate language feature with different typing and scoping obligations.

## Surface holes remain structural

Rejecting bare `?` means the surface printer must never use it to spell a silent inference hole.

The parsed surface AST contains `Subterm::Goal(String)` for written goals. Omitted syntax remains structural metadata such as `LetSignature::Name { type_: None, .. }`; lowering turns that absence directly into an unmarked core metavariable. Desugarings that need inference placeholders likewise mint unmarked core metavariables directly.

The current desugaring-only `Subterm::Hole`, whose printer also emits `?`, is removed from the printable term language. This keeps the invariant that every printed surface term is valid source and that every source `?` begins a labeled written goal.

## Core representation

Lowering turns each `Subterm::Goal(label)` into one fresh core metavariable with `MetavarOrigin::Goal { label }` and preserves the complete source span on its term occurrence.

The label rides with the metavariable origin through rebuilding, but it does not create typing constraints or replace the uniquely minted metavariable ID as occurrence identity. Silent metavariables retain their existing origins or remain unmarked.

## Registration at metavariable birth

Written goals are registered when elaboration births their metavariables. The elaboration context is the authoritative collection point because its metavariable store already owns:

- The metavariable ID.
- The local telescope frozen at birth.
- The expected type recorded at birth.
- The optional solution committed later by unification.

At the first birth of a goal metavariable, the context records a goal site containing its ID, label, complete source span, and deterministic registration order. Rebuilding the same metavariable is idempotent and must agree with the existing label and span.

The goal registry is keyed by metavariable ID. It therefore deduplicates naturally, remains valid if elaboration rewrites the containing term, and avoids an exhaustive post-hoc traversal over module items, registry telescopes, the entrypoint body, and any future core structure that can contain terms.

A goal that was parsed but never birthed because a hard failure stopped elaboration has no trustworthy scope or expected type and does not enter an incomplete batch.

## Collection semantics

Goal collection runs after elaboration and its ordinary parked and deferred obligations have completed successfully, while the context and metavariable store are still available. It runs before strict whole-program zonking.

The front end has the conceptual control flow:

```text
elaborate and finish obligations
  → hard failure: Error
  → registered goals exist: Incomplete(Vec<GoalReport>)
  → no registered goals: strict zonk
      → success: Clean(CheckedProgram)
      → failure: Error
```

Collection follows these rules:

1. Read every registered written goal in deterministic registration order.
2. Read its frozen scope, expected type, and optional solution from the metavariable store.
3. Reify those report terms tolerantly, substituting useful solutions without demanding a meta-free result.
4. Return `Incomplete` when at least one report exists and do not hand a module to erasure.
5. Run the existing strict whole-program zonk path unchanged only when no written goals exist.

Deterministic registration order follows deterministic elaboration order and never depends on hash-map iteration. An analysis consumer may reorder reports by its stable source identity and byte offset for source-oriented presentation, but it must use an explicit deterministic key.

## Tolerant report reification

Goal reports may legitimately mention unsolved metavariables, especially when a goal appeared in synthesis position or influenced surrounding inference. Strict zonk is therefore the wrong operation for report terms.

The report reifier follows a separate policy:

- Recursively substitute solved ordinary metavariables at the appropriate occurrence spine.
- Preserve written-goal metavariables as visible neutral terms.
- Preserve unsolved ordinary metavariables as visible neutral terms.
- Preserve spans and binding relationships needed for later rendering.
- Never discard all partial progress merely because one residual metavariable remains.

The current best-effort pattern of attempting strict zonk and falling back to the entire original term is insufficient: one unresolved subterm can hide unrelated substitutions that elaboration successfully committed.

Report reification is for observation only. It does not produce a checked term and cannot be passed to erasure.

## Typed checking outcome

Written goals are incomplete development state rather than ordinary compiler errors. The reusable checked front end returns a typed outcome conceptually equivalent to:

```rust
pub enum CheckOutcome {
    Clean(CheckedProgram),
    Incomplete(Vec<GoalReport>),
}

pub type CheckResult = Result<CheckOutcome, Error>;
```

`CheckedProgram` contains the strictly zonked, meta-free core module and entrypoint type needed by later compilation stages. `Incomplete` contains no checked program.

This separation gives each state one meaning:

- `clean`: elaboration and strict zonking completed with no written goals or hard errors.
- `incomplete`: elaboration completed and at least one written goal was registered, but strict zonking was deliberately not attempted.
- `error`: a hard failure prevented either clean checking or a completed goal-only result.

Consumers must inspect the typed outcome rather than infer incompleteness from an error string or special-case `Error::Goal`.

## Goal report

The compiler-core report contains only facts owned by elaboration:

```text
GoalReport
  internal metavar ID
  label
  source span
  frozen scope in binding order
  expected type
  optional solution
```

The metavariable ID is an internal correlation key for the duration of the check. It is not a public stable identifier and need not survive conversion into an analysis response.

`solution` is absent precisely when elaboration committed no solution for the written-goal metavariable. A residual metavariable inside a present solution does not make the solution absent; the tolerant reifier preserves that residual structure.

All scope names and displayed terms in one human report share a collision-aware pretty-name environment so a binder is spelled consistently throughout its scope, expected type, and solution.

One possible human rendering is:

```text
goal `?step`
  xs : Lst(A)
  ?step : Nat
  ?step = Lst/len(xs)
```

Each report retains its own source snippet. A batch renderer prints every report rather than returning after the first.

## Solved goals remain incomplete

A written goal is a request for information, not an ordinary inference hole. Even if elaboration determines a unique solution, its report remains present and the program remains incomplete until the written goal is removed or replaced.

This supports using a goal as an explicit query:

```text
?elementType : Type
?elementType = Nat
```

Silently substituting a solved written goal would erase the answer the author requested and allow deliberately unfinished source to compile.

## Error precedence and deferred validation

Parsing, loading, resolution, lowering, elaboration, witness resolution, conversion, and final parked-obligation failures remain fail-fast. A hard error takes precedence over any written goals registered before it because the interrupted elaboration did not establish a complete goal batch.

When elaboration succeeds and goals exist, strict zonking is deferred. A written goal in synthesis position can leave metavariables in its expected type or surrounding inferred structure; blindly suppressing only the goal node can misreport those dependencies as unrelated `CannotInfer` errors.

This means a genuinely unrelated residual inference error that only strict zonking would discover can remain hidden until the written goals are filled. Distinguishing independent residual errors from goal-dependent ones would require metavariable dependency provenance or a more general recovery system. That validation is intentionally deferred rather than approximated unsoundly.

## Compilation integration

`curios-pipeline` requests a `CheckOutcome` from the reusable front end.

- `Clean(CheckedProgram)` continues through erasure, Ersd, continuations, and wasm.
- `Incomplete(reports)` formats the complete report batch and stops before erasure.
- `Error` follows the existing hard-error path.

`run` and `compile` therefore continue rejecting every program containing a written goal, but they report all reached goals instead of only the first.

A dedicated typecheck-only CLI command is not required by this design. It can later consume the same outcome without changing goal semantics.

## `wonder` integration

The compiler converts core reports into durable, transport-neutral diagnostics before dropping the elaboration context. The analysis layer then adds facts it owns:

- Snapshot-local public goal IDs.
- Stable source identities and byte ranges within the snapshot.
- Enclosing source-item and semantic-symbol ownership when available.
- Display and canonical term renderings.
- JSON schema fields and pagination behavior.

The label is always a string in a valid written-goal diagnostic; it is never `null`. It remains metadata rather than identity, so a label filter may return several reports.

A `wonder` analysis reports goal diagnostics with kind `goal`, severity `incomplete`, and the analysis status `incomplete`. Query transport still succeeds when the analyzed program is incomplete.

Goal IDs and source ranges are snapshot-local. After an edit, a consumer reruns analysis rather than assuming the same label denotes the same scope, expected type, or occurrence.

## Untouched inference holes

Silent inference placeholders remain distinct from written goals:

- Omitted local annotations.
- Inserted implicit arguments.
- Inserted witness arguments.
- Match motives and other compiler-generated placeholders.
- Literal element types and similar inferred structure.

They carry their existing provenance and are substituted silently when solved. When unsolved in a goal-free program, strict zonking produces the specific hard diagnostic associated with their origin, such as an uninferred implicit or missing witness. They never enter the written-goal registry merely because they are metavariables.

## Non-goals

This design does not add:

- Shared named metavariables.
- Label uniqueness or label scoping rules.
- Tactics or interactive refinement commands.
- Automatic replacement of a solved goal.
- Public goal IDs stable across source edits.
- General multi-error elaboration or parser recovery.
- Proof that a goal-bearing program has no independent residual inference errors.
- A dedicated typecheck-only CLI command.
- Editor integration beyond the structured analysis report.

These capabilities can consume the typed outcome or goal reports later without changing the meaning of `?label`.

## Tests

The implementation is pinned at four levels:

- Parser and printer tests for required labels, glued spelling, full-span preservation, keyword rejection, targeted rejection of `?` and `? label`, and round trips.
- Lowering tests proving one fresh metavariable per occurrence, label threading, duplicate-label independence, direct lowering of structural omissions to silent metavariables, and the absence of a printable bare-hole term.
- Core tests for birth-time registration, idempotent rebuilding, solved and unsolved reports, scope preservation, tolerant reification, deterministic ordering, complete batching, elaboration-error precedence, goal-dependent type metavariables, and deferred residual-hole errors.
- Pipeline and analysis tests for clean, incomplete, and error outcomes; rejection before erasure; complete human batches; required non-null JSON labels; source ownership; and snapshot-local goal identity.

## Milestones

1. **Required labeled syntax.** Replace the written form with `?label`, remove bare `?`, keep structural omissions out of the printable term language, and thread labels and spans into core goal origins.
2. **Complete typed outcome.** Register goals at metavariable birth, add tolerant report reification and structured `GoalReport`, return clean versus incomplete outcomes, and route compilation only through a clean checked program.
3. **Analysis integration.** Convert reports into the shared diagnostic schema, assign snapshot-local IDs, correlate source ownership, and expose the complete batch through `wonder diagnostics`.

The first milestone is deliberately breaking: unlabeled written goals become parse errors immediately. The later milestones improve reporting and expose it to tools without adding tactics, recovery, or another goal language.
