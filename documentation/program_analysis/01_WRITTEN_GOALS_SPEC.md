# Written goals (`?`) — batched reports, readable display, and the printing substrate

This document specifies the completion of the written-goal workflow: one compilation reports every written goal it reaches, each report is identified by its source location and rendered in source-shaped spelling, and the printing layer gains the width-aware substrate that diagnostics, IR dumps, and a future formatter share.

It replaces the earlier labeled-goal design (`?label`, a typed incomplete checking outcome, and analysis-interface coupling). That design is superseded rather than deferred: the ideas that survive are restated here in their new form, and the rest are recorded as non-goals.

## Problem

Observed use of the current implementation shows three deficiencies:

1. **One goal per compilation.** Zonk fails at the first `Goal`-origin metavariable it meets, so a program holding several goals costs one full compile per goal, and the author watches the error walk forward one hole at a time.
2. **Reports spell elaborator internals.** A goal whose author-visible type is `Eq(0 + 0, 0 * 2)` reports as `Eq(@Nat, (witness2).0(0, 0), (witness4).0(0, 2))`: concept-dispatched operators surface as anonymous witness projections that no reader should have to decode.
3. **Display is all-or-nothing.** Report terms render through strict zonk with a whole-term fallback, so a single residual metavariable hides every substitution elaboration did commit.

Beneath the second and third deficiencies sits a structural one: every printer fixes its layout at document-build time, so large terms render as single lines (the Ersd printer today makes almost no layout decisions at all), and no mechanism lets one printer definition adapt between an inline diagnostic and a readable dump.

## Objective

A programmer or agent leaves `?` at several source locations, runs one compilation, and receives one deterministic batch containing every reached goal's location, local scope, expected type, and inferred solution — each spelled the way the author would write it. They edit the source and repeat.

“Every goal” means every written goal reached by one otherwise-successful elaboration. Reporting for syntax that parsing, lowering, or elaboration never reached would require general recovery, which stays outside this design.

## Current behavior

- Bare `?` parses to `Subterm::Goal`; lowering mints a fresh metavariable with `MetavarOrigin::Goal`; elaboration may solve it like an inference hole; zonk errors at the first one with `Error::Goal` carrying the frozen scope, expected type, optional solution, and span.
- `zonk_solved_term_metas` already implements tolerant materialization — it substitutes solved metavariables through their spines, preserves `Goal`-origin and unsolved metavariables as visible terms, keeps spans, and never fails — but the goal display path does not use it.
- An infix operator elaborates to `Apply(Proj(witness, index), [left, right])`, and the witness metavariable's `WitnessOrigin` carries the operator symbol and method name.
- The printer document is `Text`/`Concat`/`Indent`/`Deferred` with an iterative interpreter and an iterative `Drop`; no layout choice exists in the algebra.
- `parse_whitespace` consumes comments and discards them; `Span` carries its `Rc<Source>`.

## Syntax: unchanged

Bare `?` remains the only written-goal form. `?label` (glued) is unclaimed syntax today and remains a compatible future extension; nothing in this design needs it.

A goal's identity in reports is its source location — file, line, and column — the same coordinate every other diagnostic uses. Location is the label: it is unique by construction, requires no new grammar, no scoping rules, and no metadata-versus-identity distinction.

`Subterm::Hole` remains desugar-only, and the printer's `?` spelling for both forms is unchanged.

## Batched goal reports

The semantics is defined by equivalence: one successful elaboration reports exactly the set of written goals strict zonking would have encountered — all of them at once, instead of the first.

Collection runs after elaboration and its parked and deferred obligations complete, while the context is still available. It materializes committed solutions tolerantly over the whole module (`zonk_solved_term_metas` already recurses through solutions transitively and preserves `Goal`-origin metavariables by construction), then walks the result once, collecting `Goal`-origin metavariables in traversal order — items in order, then the entrypoint body — deduplicated by metavariable ID, the first occurrence's span winning. The order is deterministic by construction and follows source order in practice.

The outcome is one batched error — conceptually `Error::Goals(Vec<GoalReport>)` — carrying, per goal: the occurrence span, the frozen local scope in binding order, the expected type, and the optional solution.

- `run` and `compile` continue rejecting every goal-bearing program; a batch is a complete report, not a compilation product.
- A solved goal still reports: writing `?` requests the answer, and silently substituting it would erase the answer and let deliberately unfinished source compile.
- Hard errors keep full precedence. An elaboration a hard failure interrupted established no complete batch, so nothing of one is reported.

Each entry renders headed by its location (path, 1-based line and column, columns counting Unicode scalar values) with the ordinary caret snippet, followed by the existing turnstile idiom — hypotheses as `name : type` lines, `? : type` for the obligation, `? = term` when a solution landed. All names within one entry share the collision-aware pretty-name environment.

## Tolerant, denoised display

Two independent fixes to how report terms are spelled:

**Tolerant.** Scope, expected type, and solution render through the tolerant materializer instead of strict-zonk-or-original-term. Committed substitutions always appear; goal-origin and unsolved metavariables stay visible as neutral terms; one residual never discards unrelated progress.

**Denoised.** A display-only rewrite runs before witness solutions are spliced, folding `Apply(Proj(w, index), [left, right])` back into the core `Infix` node when `w` is a witness metavariable whose origin's function is an operator symbol — the origin already carries everything needed, so the fold works for solved and unsolved witnesses alike and no anonymous witness name ever reaches the report. `!=` reverse-maps its `BoolXor` wrapping. The folded term is for observation only and never re-enters checking: core `Infix` is elaboration-transient, and here it only ever meets the printer. Non-operator witness projections keep their current spelling; naming them through their concepts is a possible follow-up, not scoped here.

## The printing substrate

The layout mechanism is the Wadler document algebra, adopted into the existing printer document in `curios-base`. The engine stays IR-agnostic; each IR's `print.rs` expresses its own layout rules by where it places groups and lines. That placement is the per-IR customization — no style traits, no configuration, no per-IR engines.

- Two variants join the document: `Line` — a space (or nothing, or a mandatory break) when rendered flat, a newline plus indentation when broken — and `Group` — rendered flat when its flat spelling fits the remaining width, broken otherwise.
- Width is the mode. Rendering at infinite width keeps every group flat and reproduces today's output byte-for-byte; a finite width yields adaptive layout. There is no separate compact/expanded switch.
- The fits scan is iterative and materializing. It walks a group's document in flat spelling, counting characters against the remaining width, and stops at the first overflow or mandatory break. Forcing a `Deferred` to measure it replaces the node in place with the built document, so nothing is built twice and no `FnOnce` is lost; the scan never looks past one line width, so the extra materialization per decision is bounded.
- A group containing a mandatory break never renders flat, because the scan fails on the break. Break propagation is derived at the decision point, never cached at build time.
- The new variants join the iterative `Drop` dismantling, and every walk over the document — printing, measuring, freeing — stays iterative.
- Goal reports render at fixed width 100. The pipeline is pure and stays terminal-blind; no width detection exists anywhere.

Adoption is opportunistic and safe by construction: converting a printer (replacing a literal separator with a line inside a group) is output-neutral at infinite width, so the faithful `Display` paths and every test pinned to them are unchanged. The core term printer converts first, because it feeds diagnostics; the Ersd printer second, because its dumps currently render without line breaks; the remaining printers follow as need arises.

## Comment capture

A formatter that reprints a parsed module must not delete comments, so capturing them is this design's one piece of parser work. Comments die in exactly one place — `parse_whitespace` — and the capture happens there:

- `parse_whitespace` records each consumed comment's span in a per-parse side table; nothing else about parsing changes.
- A captured comment is a bare `Span`. `Span` carries its `Rc<Source>`, so the comment text is a slice of text already held, never a copy.
- The vehicle mirrors the packrat memo table's precedent: a thread-local map keyed by start offset, owned by `curios-text`, cleared at the start of each parse run, drained by the parse entry. `curios-base` is untouched.
- Offset keying makes recording idempotent under backtracking, and memoized jumps are harmless: the cache-miss run already recorded what the replay skips.
- The capture is sound because the whitespace parser never runs inside a string or character literal — literal interiors are consumed atomically by their own parsers — so every recorded span is a genuine comment of the winning parse.
- Comments surface as a parse product beside the module — conceptually `(Module, Vec<Span>)` — not as fields of the syntax tree. Structural equality, printing, lowering, and every existing test are untouched.
- Attachment — deciding which comment leads, trails, or sits inside which node — is a formatter decision and out of scope. Any attachment policy is expressible over the span table later.

## The formatter seam

The eventual formatter needs four things, and this design leaves each in a known state: expanded rendering (provided — finite-width printing over the surface printer, whose output round-trips already), comments (provided — the capture above), universal item source ranges (a separate substrate, deliberately unclaimed here), and trailing-comment re-emission in the `lineSuffix` style (future work, expressible over the algebra without changing it). Nothing in this specification obstructs any of the four.

## Non-goals

- Labels (`?label`), shared named metavariables, and label uniqueness or scoping.
- A typed incomplete checking outcome: goals remain an error, and `run`/`compile` reject them.
- Program-analysis integration, tactics, or automatic replacement of solved goals.
- `fill`, `conditionalGroup`, and any width-fitting refinement beyond greedy groups.
- Build-time break-propagation caching and terminal-width detection.
- Comment attachment policy and the formatter itself.
- Witness-projection naming beyond the operator fold.
- General multi-error recovery.

## Relation to the analysis specification

[02_WONDER_SPEC.md](02_WONDER_SPEC.md) presumes required labeled goals and a non-null label field in its diagnostic schema. Both premises are superseded by this document; the analysis specification absorbs the location-is-the-label model whenever it is next revised.

## Tests

- **Batching:** several goals report together in source order; solved and unsolved entries; a goal in synthesis position; a hard error preempting the batch; goals in the entrypoint tail; the per-entry location header.
- **Display:** committed substitutions appear even when residuals remain; the operator fold for each operator form including `!=`; folding with an unsolved witness; non-operator projections unchanged.
- **Printer:** grouped documents render identically to their ungrouped spelling at infinite width; fits boundaries (exactly fits, overflows by one); a mandatory break forces every enclosing group; the deep-document print and drop tests extended to the new variants; measurement forces each thunk at most once.
- **Comment capture:** leading, trailing, and interior comments; backtracking-heavy positions record once; a comment-free parse yields an empty table; literals containing `--` record nothing.

## Milestones

Three independent tracks; the suggested landing order is A1, A2, B1, B2, B3, B4, C1.

- **A1 — Batched goal reports** (`curios-elab`, `curios-pipeline`): tolerant collection, the batched error, location-headed rendering.
- **A2 — Operator denoise** (`curios-elab`): the display-only fold.
- **B1 — Engine** (`curios-base`): `Line`, `Group`, the materializing fits scan, the width-parameterized entry.
- **B2 — Core printer conversion** (`curios-core`): output-neutral at infinite width.
- **B3 — Goal reports at width 100** (`curios-elab`).
- **B4 — Ersd printer conversion** (`curios-ersd`): readable dumps.
- **C1 — Comment capture** (`curios-text`): the side table and parse product.

## Documentation

Each fact lands at its narrowest authoritative home as its milestone lands: the algebra and scan invariants in the printer module's documentation beside the existing depth-and-drop rationale; the cross-cutting layout decision and its rejected alternatives (per-construct visitor formatting, penalty-based line breaking, refusing width limits, cached break propagation) in `DESIGN.md`; goal-report behavior in the owning module documentation and tests.

## Retirement criteria

- Before this specification is deleted: goal-report behavior, the printing algebra, and comment capture are recorded in the owning crate and module documentation and tests; `DESIGN.md` records the layout decision; the roadmap subitem is a checked unlinked summary; the analysis specification no longer presumes labeled goals; and no reference to this filename remains.
