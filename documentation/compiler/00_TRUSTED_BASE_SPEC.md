# The trusted base

Working implementation specification for making the independent kernel load-bearing: enumerating what is trusted, closing every route by which the kernel certifies a false proposition, and turning it on so that the elaborator's rules leave the trusted base.

This effort does not add a rule to the language or change what any program means. It changes *who is trusted*. Today the compiler's verdict is the elaborator's alone; when this lands, a program is accepted only if two independently written checkers agree, and the trusted set is a table with a budget rather than a crate name.

This specification refines the independent-kernel item in [ROADMAP.md](../ROADMAP.md), which links here. When this work lands, fold the permanent boundary rules into the `curios-core` crate documentation, re-grade nothing in `DESIGN.md`'s perimeter table (see "What this may not claim"), update `ROADMAP.md`, and delete this working specification after no remaining document refers to it.

**Only the defunctionalized typing judgment is implemented.** It landed first and out of order, because the kernel aborted on real input and nothing downstream could be measured until it stopped. Every claim below was read out of the source and is cited to a line, or measured and cited to its measurement.

## Objective

Establish the following, and record it as what the compiler's acceptance means:

```text
A module is accepted only if the elaborator and the kernel both accept it.
The kernel decides from the finished terms alone.
```

`DESIGN.md` currently states the opposite, and states it correctly: *"An independent kernel is being built in `curios-core` and does not yet re-check anything, so it subtracts nothing from that base today."* `curios-elab/src/recheck.rs:25` says the same from the other side: *"Nothing in the pipeline calls this."*

**The elaboration rules leave the trusted base; the module plumbing does not.** `recheck_module`'s item walk and its dependency sort, `zonk_module`, the `Module`/`Item`/`Definition` types, and the archive round trip all stay — they decide *what* the kernel is asked about, which no amount of re-checking can validate. That residue is of order 1,800 lines, against 31,702 for the crate. An earlier revision of this document claimed the crate leaves entirely; it does not, and the narrower claim is the one to make.

The completed implementation must compile the existing `/sys`, `/syn`, `/std`, examples, benchmarks, and tests with `recheck_module` on the compile path and a kernel refusal failing the build.

## The trusted base, enumerated

A line is trusted when a bug in it can admit a program. That set is the call closure of the kernel's three entry points — `check_definition`, `check_rec_group`, `check_entrypoint` — and it does not coincide with any crate boundary.

| Trusted | Lines | Why |
| --- | --- | --- |
| `curios-core`, non-test | 15,065 | The kernel and everything it calls: `term.rs`, `scope.rs`, `reduce/prim.rs`, `universe.rs`, `spine.rs`, `free_monoid.rs`, `inductive.rs`, `structure.rs`, `polarity.rs`, `nat.rs`, `names.rs` |
| `curios-base` | 2,839 | `Flt`, `Int`, `PackedBin`, `Grain`, `NumOp`, `Scalar`, `Entropy`, the rotate helpers — the arithmetic every primitive fold performs |
| `curios-abi` | 952 | `ForeignFunction` and `WireType`, against which the kernel re-decides the foreign wire contract |
| `num-bigint` | external | `Nat::Succ(BigUint, Term)` — type-level natural arithmetic *is* `BigUint` arithmetic |
| `curios-elab` residue | ~1,800 | `recheck.rs`, `zonk.rs`, the module types, the archive round trip |

Total today, with the elaborator wholly trusted because its verdict is the only one anything acts on: `curios-core` 17,497 plus `curios-elab` 31,702 plus `curios-base` 2,839 plus `curios-abi` 952, and `num-bigint` besides. An earlier revision accounted 49,141 and omitted the last three. **A trusted base that has not been enumerated cannot be minimized**, and that omission is why the earlier line-count targets were unreachable.

Expected after this work: of order **21,000–22,000**. A ~55% reduction, and not a small kernel.

**The enumeration is the deliverable, not the estimate.** Add a test that computes the closure from those three entry points and asserts it against a recorded list with a line budget, so "did this change grow the trusted base" is a test result rather than an argument. Until that test exists the table above is a best reading, not a measurement — it was assembled by following imports, and the earlier omissions are what that method costs.

`print.rs` (1,349 lines) is in `curios-core` and is *not* trusted. `curios-core/src/lib.rs:27` already concedes this.

## Permanent design decisions

**Two independently written checkers is the objective.** Not a means to a smaller crate. `DESIGN.md`'s rationale stands: a second checker removes none of the perimeter's weaknesses and changes the cost of being wrong about one. The evidence that it earns its keep is already in hand — see "The elaborator is wrong where the kernel is right" below.

**Duplicate a check when the two sides see different inputs; share it when they do not.** This is the line, and it is new. Reduction, conversion, and the typing judgment stay duplicated: the elaborator sees metavariables, refinements, expected types, parked goals, and memoized derivations, while the kernel sees ground terms and holds no caches (`curios-core/src/kernel.rs:169`). Different inputs and different strategies are where a systematic mistake is both likely and costly, and where two verdicts are two samples.

Strict positivity, size-change totality, and index inversion are the other case. All three run post-zonk on final, meta-free terms — `curios-elab/src/positivity.rs:66` (*"Runs on zonked Core, so the telescopes it reads are final and meta-free"*) and the same for totality, whose flag is *"[w]ritten back … after zonking … the analysis needs final, meta-free terms"*. That is exactly the kernel's input. **Two runs of a total function on identical input is one sample, not two.** Duplicating them buys a diff test, which property-testing the single implementation buys more cheaply, and costs 2,865 trusted lines written twice.

`Reducer` is the precedent: `DESIGN.md` already draws this line for primitive folding — *"arithmetic over the representation and belongs here, while how far an operand reduces before a fold sees it is a strategy each side supplies for itself."* Positivity, totality, and inversion are algebra over the representation.

**The kernel recomputes rather than reading the elaborator's answers — for duplicated checks.** Amended. Once an analysis is shared there is no elaborator answer distinct from the kernel's: the stored vector is the output of exactly the code the kernel would run. What survives is an **integrity check on the archive** — recomputing catches a stored value that no run of the analysis would produce. That is worth having and the kernel already provides its equivalent for the term half by re-typechecking, but it must be argued as integrity, not as independence. The earlier instruction to re-derive rather than read `InductDecl::polarities` was justified on the wrong ground.

**Declaration acceptance is a typing rule, and belongs in the kernel.** `DESIGN.md` records the opposite — *"what it will not cover, by construction, is totality, positivity, and witness coherence, which are whole-module analyses rather than typing rules"* — and this specification reverses that for positivity and for totality. Two reasons. The kernel already has a module driver: `curios-core/src/kernel/module.rs` walks items in order and defines each as it goes, so "whole-module" does not describe a boundary it cannot cross. And positivity is not a whole-module analysis; it is a condition on accepting an `induct` or `struct` declaration, which every kernel in this family checks at that point.

**A kernel that uses a rule must check the rule's premise, and for totality this is forced rather than argued.** `DESIGN.md` establishes that definitional proof irrelevance is sound precisely because every `Prop` inhabitant is total, and that the conversion recurrence rule stands on aggressive (T). Both are the elaborator's fixpoint argument and neither transfers to a second checker that runs neither obligation. That was the earlier argument and it was sound but weak, because it made totality a question of coverage. It is not: `rec f : False = f` is two lines and the kernel certifies it. Given general recursion, **a totality analysis is unavoidably in the trusted base**, and the only open question is whether it is written once or twice.

**Incompleteness is the safe direction, with one stated exception.** `curios-core/src/kernel/convert.rs:46` already states this for conversion, and it governs every rule added here: a rule that refuses too much produces a disagreement, which is a signal; a rule that accepts too much is silent. The exception is `Cases::FreeMonoid { .. } => Ok(())` (`curios-core/src/kernel/infer.rs:506`), which is an *acceptance* hole and is the one place the invariant does not hold today. If a rule has to be weakened to make a real module pass, that weakening is a decision about the trusted base and belongs in `DESIGN.md`.

**Printing is not a rule and does not belong in the trusted crate.** The only thing anchoring it is `impl Display for Term`, needed by `KernelError`'s own `Display`.

## Non-goals

- A small kernel. Not because smallness is undesirable — it is the point of the enumeration — but because the measured floor is not small. Native inductive families, structures, a universe hierarchy, a primitive roster with folds, and an unavoidable termination analysis put any checker for this term language in the tens of thousands of lines. Every item below is justified against the budget in the table above.
- Removing native inductive types or the free-monoid carriers, or replacing them with an encoding cheaper to verify.
- Migrating from `Match` to generated recursors. It would delete coverage and inversion from the kernel and move the large-elimination guard from per-site to per-declaration, which is where it belongs — but the recursor generator is itself trusted, every backend stage would see a new elimination form, and the free-monoid carriers would need recursors too. The one benefit that is worth having independently is deciding the singleton condition once at declaration acceptance instead of per `Match`; take that without the migration.
- A fresh, fully explicit, metavariable-free IR for the kernel to check. Rejected in `DESIGN.md` and not reopened.
- Sharing the reduction driver. Rejected in `DESIGN.md`, and the rejection holds under this document's objective too: sharing would save **zero** trusted lines, because the kernel's reducer is trusted either way and the elaborator's copy is untrusted either way. It saves maintenance and costs the diff signal.
- Witness coherence and the orphan rule. Incoherence means two call sites resolve the same key differently and both elaborate to well-typed terms — confusion, not unsoundness.
- Cumulative inductive types. An open fork in `DESIGN.md`, deliberately not taken; **none** of the 90 measured refusals is inductive-parameter cumulativity, which is the evidence it should be decided against.
- Verifying the kernel itself, or any metatheory.

## What the kernel already checks

Stated because three rounds of investigation each began by claiming a hole that turned out to be covered, and because the work below is sized against this list.

**Elaboration-only syntax cannot reach it — but that guarantee is the elaborator's, not the kernel's.** `zonk_module` (`curios-elab/src/zonk.rs:127`) is a total traversal covering `items`, `body`, `type_`, and both registries including constructor telescopes and `result_sort`. Its `Subterm` match has `Infix`, `NumLit`, and `Metavar` as `unreachable!` arms, so a zonked module cannot carry any of the three, and `recheck_module` is only ever handed a zonked module. The kernel refuses all three in `infer`, in `sort_of`, and — since C4 — in conversion, whose metavariable arm now refuses anything it would have to look at, with reflexivity as the one sound admission. What still rests on the `curios-elab` pass is only what `infer` never reaches: the free-monoid arms.

**Constructor payload types are well-sorted, and the registry agrees with the bindings.** An `induct` declaration lowers to a `rec` group of ordinary definitions (`curios-elab/src/elaborate/module.rs:690-740`), so the type constructor and every value constructor are real module items that `check_definition`/`check_rec_group` walk. Sorting a constructor's declared function type sorts every payload domain, and `infer` checks a `Variant`'s payload against `declaration.instantiate(tag, params)` (`curios-core/src/kernel/infer.rs:255-295`), so the registry entry is cross-checked against the bindings in both directions.

**Nominal elimination is verified in full**, each arm at its own constructor's index targets, with the large-elimination guard deciding its singleton side condition rather than approximating it (`curios-core/src/kernel/infer/eliminate.rs`).

**The foreign wire contract is re-decided**, against `wire_term` rather than against the elaborator's record (`curios-core/src/kernel/infer/prim.rs:318-343`).

**Proof irrelevance, eta at Π and Σ, subsumption, and the recurrence rule** are all present in `curios-core/src/kernel/convert.rs`.

**The typing judgment runs on a bounded stack.** `infer` drives an explicit obligation stack: the child obligations of an application, a constructor, and a record are deferred rather than descended into, because those three instantiate their telescopes by substituting the child term rather than binding it, so every deferred obligation is checked in the context it was recorded in. Arms that open binders still recurse, which is the written-nesting bound `AGENTS.md` allows.

## What the kernel certifies today that it must not

Ranked by directness. This is the soundness statement, and it is the reason the work below is ordered as it is.

| | Route | Cite | Fixture |
| --- | --- | --- | --- |
| 1 | A `Switch`'s default and the free-monoid carriers' arms are typed by their bodies and never verified against the motive | `infer.rs` `check_cases` | the one *acceptance*-direction hole |

**The `rec` route closed at A4, and positivity at A3** — both entries preserved below the table in their landed form. What the kernel still takes on faith is narrower than either: see A4's residue note.

An earlier revision offered "defer both (T) and (V)" as *"a defensible trade-off — it is what ships soonest"*. It was not a trade-off about coverage; it was the difference between a sound rule set and an unsound one, and `rec f : False = f` — no primitive, no declaration, no elimination — was the two-line witness. The kernel now refuses it: a `rec` member whose declared type is a proof or yields a sort must descend, decided by the shared size-change engine.

**A second route was here and is now closed, by a typing rule rather than by an obligation.** `Prim::Exit` used to carry its result type as an operand, so `exit(@False, 0) : False`. It is now typed at `{}`; see "Totality of the erased program" in `DESIGN.md` for the decision and for why the two weaker designs fail. The short version is that a non-returning term is unsound exactly when it inhabits a type nothing total inhabits, and restricting *which* type `exit` may be given cannot fix that — any constructor-free `Empty : Type` eliminates into `Prop` unguarded, because `guard_large_elimination` returns at its first check and zero constructors leave no arms to verify. Removing the choice is what works. `/std/Never` went with it, having existed only to give `exit` a `Type`-sorted carrier.

Route 3 is not an unrecorded hole in the compiler. The elaborator enforces it: `add_declaration_sizing` (`curios-elab/src/elaborate/module.rs:95-140`) emits, per constructor, that each payload's level is `≤` the result level and each uniform parameter's is `≤` result + 1, under `UniverseConstraintKind::ConstructorSizing`, discharged by the universe solver. It sits in the perimeter table as `validate_universes`, graded *auditable only*. What is unrecorded is that the kernel does not duplicate it.

## What the kernel refuses that it must not

This is the completeness statement, and unlike the one above it is measured rather than enumerated.

`kernel_disagreements` in `curios/src/tests/kernel.rs` is an ignored inventory test that walks whole programs and tallies every refusal by class. It reports **90 of 1052 items refused**, identically across all three fixtures and identically in debug and release — the matching counts across profiles being the check that defunctionalizing the judgment was a restructuring and not a change of rule.

| Class | Count | Cause | Closed by |
| --- | --- | --- | --- |
| `Type.{u}` vs `Type.{w}` — two distinct rigid universe parameters | **25** | a scheme's own constraints are not **assumed** while it is checked generically | C1 |
| `Unclassified` | **16** | every one is the empty `Lst` literal | C3 |
| a zero level in an instance (`Async.{0,0,0}` vs `{u,v,w}`) | **13** | a use-site instance is not checked against its scheme's constraints | C1 |
| other mismatches | **36** | index inversion, the four syntactically-compared conversion positions, and an unknown remainder | A2, and see below |

**The 25-item cluster is the largest, and an earlier revision misidentified it.** That revision named the 13-item zero-level cluster as *"the largest identifiable cluster"* and specified only the use-site obligation: *"[v]erify at each `UniverseInst` that the stated levels satisfy the scheme's constraint set."* The 25-item cluster is the dual. `check_definition` checks a universe-polymorphic definition *generically*, at its own parameters, which is the right reading and the only one available — so the scheme's constraints must be **assumed** as hypotheses. `generalize` retains exactly the non-tautological constraints relating generalized parameters (`curios-elab/src/universe_solver.rs:1035-1049`); `Kernel` stores them per definition (`kernel.rs:163`) and never reads them, and `subsumes` decides `Type.{u} ≤ Type.{w}` with `Level::structurally_leq` (`infer.rs:555`), false for distinct rigid parameters with no hypothesis available. Every recorded constraint is therefore unusable.

That the mechanism is certain does not establish that it accounts for all 25; a probe on `/std/Map/get` would settle it, and should run before C1 is designed.

The same revision measured the recorded per-declaration constraints as 47 `SchemeInstantiation` and 13 `Cumulativity` with **zero** `ConstructorSizing` and zero `FieldSizing`, and read that as *"[t]here is nothing to discharge."* The measurement is right. Those 60 constraints are precisely the hypotheses the generic check needs; the reading was one-sided.

And the shape of the whole problem generalizes: **the elaborator satisfies a level condition by choosing levels that make it hold; the kernel is handed levels already chosen and must verify that they do.** Those are different operations and only the second is a check. It is why the elaborator can discharge sizing without leaving a trace, why the kernel's version cannot be assembled from what the elaborator left behind, and why `check_definition` computing a sort and comparing it to nothing is the same omission one level down.

**All 16 `Unclassified` items are the empty list literal** — `/std/Lst/nil`, `Lst/flatten`, `Map/entries|keys|values`, `Parse/many0`, `Parse/sep_by0`, `Toml/build/build_empty`, `Toml/decode/*`, `Toml/encode/*`, `http/get|post`. `Prim::Lst(Vec<Term>)` (`curios-core/src/prim.rs:135`) is the only `Lst` form that carries no element type; `LstType(Term)`, `LstLen(Term, Term)`, and `LstGet(Term, Term, Term)` all do. `infer/prim.rs:219-225` refuses rather than guesses, which is right.

No kernel work fixes this. `DESIGN.md` justifies the kernel being synthesis-directed on the grounds that *"a finished Core term has no omitted annotations to recover"*, and this is exactly an omitted annotation — so that premise is false for one term form. The fix is `Prim::Lst(Term, Vec<Term>)`, a representation change reaching erasure and `curios-ersd`.

**Conversion is incomplete in four named positions and no work item closed them.** `curios-core/src/kernel/convert.rs:46-60` names them: a stuck elimination's motive and arms, a `rec` group, and the arguments of a spine, which are compared at `Type` rather than at the types the head assigns. `recheck.rs` and `DESIGN.md` both repeat it. The earlier revision's acceptance gate was `kernel_disagreements` at zero, which those positions make unreachable. Either they get an item or the gate changes; this document takes the second option and states the gate as "zero refusals whose class is not a recorded conversion incompleteness."

## Defects found while writing this

Neither is trusted-base work. Both need owners.

**`Flt/min` and `Flt/max` disagree between compile time and runtime.** Demonstrated: with `nan = Flt/of_le_bytes(x\00\00\c0\7f)`, the definition

```text
let fold_says_one : Eq(@Flt, Flt/min(nan, 1.0), 1.0) = Eq/refl();
```

typechecks, while the same expression at runtime prints `NaN`. The type-level fold is Rust's `f32::min` (`curios-base/src/flt.rs:54-56`), which returns the non-NaN operand; the runtime is wasm `f32.min`, which propagates NaN (`curios-cont`'s `FltMin` → `Instr::F32Min`, opcode `0x96` at `curios-wasm/src/writer.rs:827`).

The acceptance is conclusive without a control: conversion compares `Flt` bitwise (`Flt { bits: u32 }` with derived equality, reaching `convert_prim`'s shape comparison), so an unfolded `Flt/min(nan, 1.0)` would be a stuck node and `refl` would be refused. Acceptance proves the fold fired and produced exactly `1.0`.

So the compiler proves definitionally an equation its own runtime falsifies. This is not a `False`-inhabitation route; it is narrower and more mundane — `Eq` stops meaning what it says. It also cannot be fully repaired by rewriting the folds, because wasm's NaN payload propagation is nondeterministic by specification while conversion is bitwise, so every NaN-producing operation has a set of legal runtime results and one compile-time answer. Work item D1 is the recommended response.

**The elaborator is wrong where the kernel is right.** `guard_large_elimination` (`curios-core/src/kernel/infer/eliminate.rs:55`, `:167`) decides the singleton side condition by whether the index targets *pin* a payload component. The elaborator's `singleton_eliminable` (`curios-elab/src/elaborate/match_.rs:526`) decides it with a syntactic occurrence test, and that is `DESIGN.md`'s one **open** forgery route. State this plainly in `DESIGN.md`: the kernel is already more correct than the elaborator on a live soundness hole, which is the strongest evidence the split earns its keep, and wiring it in is one of the two things that closes the route.

## The work

Four kinds, replacing the earlier nine steps. Only C writes a check that does not exist somewhere already.

### A — Relocate the shared analyses

Sixteen `Context` call sites across 2,865 lines. Measured, multiline-safe:

| | Lines | `fresh` | `unfold` | `assumption` | registry | `reduce_forced` | `convert_at` | driver-only |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `positivity.rs` | 833 | 3 | 1 | — | 2 | 2 | — | — |
| `totality.rs` | 1,797 | 4 | — | — | — | 2 | — | 5 |
| `invert.rs` | 235 | — | — | 1 | — | 1 | 1 | — |

**A1 — The seam.** `Reducer` exists and is already implemented on both sides (`curios-core/src/kernel/whnf.rs:37`, `curios-elab/src/reduce.rs:18`). Add two traits in `curios-core`:

```rust
pub trait Env {
    fn fresh(&self, hint: Option<&str>) -> Free;
    fn assumption(&self, name: &Free) -> Option<&Term>;
    fn unfold(&self, name: &Free) -> Option<&Term>;
    fn induct_decl(&self, name: &Global) -> Option<&InductDecl>;
    fn struct_decl(&self, name: &Global) -> Option<&StructDecl>;
}

pub trait Judge: Reducer + Env {
    fn convert_at(&mut self, type_: &Term, this: &Term, that: &Term) -> Result<bool, ReduceError>;
}
```

Consumers: `positivity: Reducer + Env`, `totality: Reducer + Env`, `invert: Judge`.

Every method is verbatim on both implementations. `fresh` — `kernel.rs:340` and `context.rs:479` have identical bodies, both minting `Free::local(index, hint)` from `curios_base::Entropy`, whose `fresh(&self)` uses a `Cell` (`entropy.rs:47-52`), so the trait may take `&self` and `Context::fresh`'s `&mut self` is incidental. `assumption` — `type_of` at `kernel.rs:309`, `context.rs:865`. `induct_decl`/`struct_decl` — `kernel.rs:253`/`:257` and `context.rs:1478`/`:1528`, identical signatures. `convert_at` — `kernel/convert.rs:78` and `curios-elab/src/typing.rs:34`, the latter a thin wrapper mapping `ReduceError` to `Error`, so `curios-elab`'s adaptation is *deleting* two wrappers (`typing.rs:26-45`). Put `ReduceError` on the traits and let each side map, which is `ReduceError`'s stated design (`curios-core/src/reduce.rs:9`).

**`unfold` means definitions only, on both sides.** This is the one method where the existing implementations differ: `Kernel::value_at` resolves definitions (`kernel.rs:358`), while `Context::var_reduct_at` resolves refinements *then* definitions (`context.rs:1266-1282`). Adding a refinement-free lookup to `Context` is the second half of `raw_var_reduct` — four lines — and it makes the two implementations semantically identical rather than merely agreeing in one position.

The alternative, mapping to `var_reduct_at`, requires the invariant *"positivity never runs with refinements in scope"* to hold forever in `curios-elab` or the two checkers silently diverge in the **unsafe** direction: `blocked()` (`positivity.rs:609-623`) walks reachable bodies at `Polarity::Mixed`, `Mixed` fails `accepting()`, so resolving fewer names rejects less. Definitions-only removes the invariant instead of depending on it.

That the change is behavior-preserving rests on two arguments. `Context::refine` has exactly two non-test callers — `typing.rs:419` inside `refine_head`, and `elaborate/match_.rs:883` — both match-scoped and inside `with_frame`, while `check_positivity` runs at module finalization (`elaborate/module.rs:1285`). And independently: `blocked()` looks up free variables of declaration telescopes, which are freshly-minted parameter binders and globals, never arm-local scrutinee heads, and `Entropy` never reuses an index — so a stale refinement could not be looked up even if one existed. Land a one-time assertion confirming the refinement store is empty there; do not make it a standing guard.

Map `unfold` to `value_at`, **not** `value`: the latter filters `universes.parameter_count == 0` and would silently stop unfolding polymorphic definitions in `blocked()`, the same unsafe direction.

**`Judge` is worth its own trait despite having one consumer**, because it records a trust concession in the type system. `Env` means no judgment is borrowed and one sample suffices. `Judge` means a judgment *is* borrowed, which is a real concession — see A2. When inversion later moves to emitted certificates, `Judge` disappearing is the visible signal the concession is gone, and having exactly one consumer is what makes that a one-line change.

**A2 — Relocate `invert.rs`, and make the arm rule apply what it computes. Landed.** The relocation alone did not move the count, and the diagnosis it forced is recorded in `DESIGN.md`'s index-inversion entry: the walk stopped a rung *earlier* than inversion, at the scrutinee refinement the elaborator holds in a store and drops from the emitted term. What landed is the rule stated directly — `check_arm` specializes the context by the most-general solution of `actual indices ~ case targets`, both directions through the shared unifier (the outer-variable direction being the same call with its sides swapped), substituted into body and expectation and shadowed into the affected locals under the existing `mark`/`retract` bracket. Measured: **90 of 1050 refusals became 79**, `/std/Nat/Lte/trans` and `/std/Str/utf8/drop_valid`/`take_valid` among the cleared, no regressions. The convoy-widening alternative was measured first and rejected: it cleared `trans` and broke obligation (V) for `drop_valid`, because a convoy re-binds a hypothesis while size-change measures descent against original parameter identities.

Coverage landed with it: with no catch-all, every constructor with no arm must be `Invert::Impossible` at the scrutinee's indices, and an undecided case is refused rather than passed. Measured: the whole prelude passes with **zero** new refusals — the count holding is the correct signature for closing an *acceptance* hole, and the unit fixture `an_undecided_absent_arm_is_refused` is the evidence the check does anything. Certifying-table route 4 is closed.

**Sharing inversion is weaker than the alternative, and the alternative is deferred deliberately.** If `invert.rs` pins a binder wrongly, a shared implementation means the kernel checks the arm at a wrong expected type and accepts it — one sample, and soundness-critical. The stronger design is for the elaborator to emit certificates the kernel type-checks: substitute the pinned solutions into the emitted arm rather than applying them as `context.refine` (`match_.rs:879-885`), and emit an absurdity witness per pruned arm rather than `continue` (`:780`). That leaves `invert.rs` fully untrusted at zero trusted lines, because the kernel would validate a *term* with machinery it already has.

Most of that design is already built: `seed_motive` (`match_.rs:1004-1065`) generalizes ambient hypotheses into a Π-motive and `rebuilt` (`:935-940`) re-applies the eliminator to them, so the convoy pattern is already elaborated and already emitted. What is missing is only the two products above. It is deferred because it changes emitted terms — reaching erasure, `curios-ersd`, and `curios-cont` — and because it must preserve the `Eq : Prop` definitional-K argument that `Invert::Impossible` leans on (`invert.rs:50`). Record it as the named successor to A2, not as an abandoned option.

**A3 — Relocate `positivity.rs`. Landed.** The analysis lives in `curios-core/src/positivity.rs` as `positivity_vectors`, generic over `Env` — which grew `fresh`, `unfold` (definitions-only on both sides; the elaborator gained a refinement-free `definition_body` lookup so no invariant about the refinement store is load-bearing), and the two registry methods. The cut fell exactly at `reject` as planned, and better than planned: the analysis turned out to be *infallible* — no arm ever constructed an error, `forced` swallows reduction failures, and `reject` returned its `Error` as an `Ok` value — so the moved code carries no error plumbing at all and returns `Result<vectors, NotPositive>` with the structured refusal (`name`, `part`, `type_`, `polarity`) the elaborator's five-line wrapper renders. The pure-closure tests moved with it, plus two kernel-driven fixtures: the four-line `Bad` route refused, a strict self-occurrence admitted.

**The kernel passes the full spliced module**, so `Vectors::at`'s archive fallback never fires and every vector is recomputed. Measured: the whole prelude passes with zero `NotPositive` refusals and the refusal set byte-identical to the C1 baseline — the acceptance-direction signature again, with the hand-built fixtures as the evidence the gate does anything.

**The driver ordering is load-bearing, and getting it wrong was measured before it was reasoned.** Declaration acceptance — positivity and sizing both — must run *after* the item walk, not before: registry telescopes mention top-level definitions (type aliases, the type constructors' own `rec` groups), and those names are only defined as the walk proceeds; a pre-pass refused 51 declarations as `Unclassified` because their telescopes named definitions that did not exist yet. The walk's define-even-on-refusal semantics are what make the post-walk position sound — by the entrypoint, the environment is complete regardless of verdicts.

**A4 — Relocate `totality.rs`. Landed, with the cost fork it predicted now measured and open.** The cut fell at `group_totality` exactly as mapped: the size-change engine — `Member`, `Walk`, `close`, the `Matrix`/`Shape`/`Size`/`Relation`/`Guard`/`Carriers`/`Tag` data, the three strategy-free bounds, and `yields_a_sort` — moved to `curios-core/src/totality.rs`, generic over `Env` and infallible; the driver (`classify_module`, `record_totality`, the (T)/(V) position checks, `mentioned`, and `Erased`) stayed, with the engine's twelve algebra tests moving and the three driver tests staying. `SCHEMA` bumped to 20 with the `Totality` move.

**The kernel's obligation is local and self-derivable, not (T)/(V).** At both `rec` sites — `check_rec_group` for top-level groups, `infer`'s `Rec` arm for nested ones — a member whose declared type is `Prop`-sorted or yields a sort requires `group_totality == Total`, refusing as `NotDescending` otherwise. That closes `rec f : False = f` and its type-level twin `rec Bad : Type = Bad` (both fixtures landed, plus the control: a non-descending *value* recursion stays legal). **The residue, stated honestly:** partiality reached *indirectly* — a total-looking proof that calls a partial definition — is (T)/(V)'s reach analysis, which reads the elaborator's settle records and stays elaborator-only. The kernel's gate covers the group's own descent, which is every route with a two-line witness; the reach-based remainder is what the kernel still takes on the elaborator's word, and it belongs in `DESIGN.md`'s account of the second opinion's coverage.

**The cost measurement did exactly what it was mandated to do.** Whole-prelude inventory: **16.6s before the gate, 164s after** — a 10× blowup, refusals unchanged at 38 with zero `NotDescending`, so the gate is correct and expensive. The cost is not repeated reduction within a walk — a per-walk memo on `Env::force` was implemented, measured at no effect, and reverted — but first-reduction cache misses across the session: the elaborator runs this same engine cheaply through its session-wide reduction cache, which the kernel forgoes *by design* (`kernel.rs:169`). The fork this leaves for step E: a kernel-side reduction cache (semantically transparent for closed heads over an append-only definition store, but against the crate's written no-cache rule), the archive-verdict pattern (record the kernel's own verdicts at archive build; needs new archive plumbing), or accepting the cost. Not resolved here; E cannot land without resolving it.

The sophistication of these analyses is corpus-forced, not discretionary, and cannot be traded away for a smaller trusted base. `totality.rs:19-24`: `/std/BigNat/add/raw` descends on *either* of two `Bits` arguments depending on the arm, `add/raw_assoc` over three, `add/raw_comm` needs the mutual closure across two members, and *"[a] rule keyed to one designated argument rejects all of them, and a fold cannot express them either."*

### B — Close the certifying holes

**B1 — Type `exit` at `{}`, and remove `/std/Never`. Done.** `Prim::Exit` carries only its code; the kernel's rule is `check(code, Nat)` with no side condition. Recorded in `DESIGN.md` under "Totality of the erased program", with the argument for why restricting the result type instead cannot work. The `SCHEMA` bumped to 19 and `soundness.rs`'s exit fixture was deleted rather than retargeted, because the program it asserted is now refused during elaboration.

**B2 — Everything else in the certifying table falls out of A**: route 1 from A4, route 2 from A3. Route 4 (coverage) closed at A2, as measured there.

### C — Write what does not exist

**C1 — Universe constraint entailment, in both directions. Landed.** The probe confirmed the hypothesis exactly: the recorded per-definition constraints are precisely the hypotheses the generic check needs — `/std/Map/get` records the chain its refusal wanted, `/std/Async/Future/register` needs one transitivity step. The kernel now assumes the item's own constraint set at each declaration boundary (`Kernel::assume_universes`, reset like the budget), decides `≤` and level equality under it (`entails` in `universe.rs`: sound, deliberately incomplete — left maxima decompose exactly, atoms chain through hypothesis uppers with offset shifts, a cycle guard and a fuel bound refuse rather than diverge), and *discharges* every instance against its scheme (`Kernel::check_instance`, wired at `UniverseInst` synthesis, `RecMember` instantiation, and every nominal instance in `sort`, `infer`, and projection). Measured: **79 of 1050 became 38** — both the 25-item bare-parameter class and the 13-item zero-level class cleared entirely, and the obligation direction produced **zero** new refusals: every instance in the prelude satisfies its scheme under entailment. A refused instance is `KernelError::UniverseInstance`, and a constraint level naming a parameter the instance does not supply is refused rather than kept, since an unsubstituted scheme parameter would be misread as an ambient one — the accepting direction.

**C2 — Constructor sizing. Landed.** `check_induct_decl`/`check_struct_decl` re-derive the `ConstructorSizing` and `FieldSizing` inequalities from each declaration's telescopes — each `Type`-sorted domain must sit at or below the family's declared level, one rung higher for the uniform-parameter prefix, decided by `level_leq` under the declaration's own universe hypotheses; a `Prop`-sorted result imposes no condition, `Prop` being impredicative with the large-elimination guard as its soundness story. Nothing recorded could have been discharged instead, because the solver makes a sizing constraint true by *choosing* the result level and then drops it as a tautology. The driver runs both checks over the full registry — *after the item walk*, for the ordering reason recorded under A3. Measured (after that fix): every declaration in the prelude passes, refusal count unchanged — the correct signature for closing a certifying route, with the `Bad : Type 0 | mk(x : Type 0)` fixture (unspellable in surface syntax) as the evidence the check does anything. Still open from the old residue list: constructor tag distinctness, index-telescope arity, registry-versus-binding completeness, and deciding the singleton side condition once at declaration acceptance rather than per `Match`.

**C3 — Give the empty `Lst` literal an element type.** `Prim::Lst(Term, Vec<Term>)`, matching every other `Lst` form. Reaches erasure and `curios-ersd`; sized honestly as a cross-stage change, not a kernel patch. Sixteen refusals.

**C4 — Make the elaboration-only exclusion the kernel's own. Landed, with one deliberate narrowing.** `convert` refuses a metavariable in any comparison that would have to look at one — including two with *equal* ids, previously accepted as convertible. Two admitted stances are recorded in place rather than changed: `compare`'s syntactic fast path passes a metavariable against itself, soundly, because reflexivity decides nothing about the unknown; and `whnf` keeps treating one as a stuck neutral, because reduction is not an admission point — the only ways a term is admitted are `infer` and conversion, and both refuse. The earlier instruction to change `whnf`'s arm is withdrawn: it would need a new `ReduceError` variant in the shared vocabulary to express a refusal reduction cannot act on. `KernelError::NotCore` stays an error rather than an assertion: the input contract is `&Module`, not "a zonked `&Module`", and that assumption has been violated in practice — `curios/src/tests/kernel.rs` records the period during which the tests read `Stage::Core` and fed an un-typechecked module to the kernel. A refusal is what made that diagnosable; an `unreachable!` would have aborted.

**C5 — The `Switch` default and the free-monoid carriers' arms.** The one acceptance-direction hole.

### D — Cut what cannot admit a program

Last, and by visibility before relocation, because A through C change what is left. The in-flight `pub` → `pub(crate)` narrowing is the right instrument and should continue: relocation cannot start until nothing outside uses an item, and demote-and-compile is how that gets known. `curios-core` has exactly **one** consumer, `curios-elab`, which is the single gate to push against — an earlier revision justified keeping the builder clusters as *"call-site convenience for `curios-text`'s lowering"*, which is wrong on its face, since `curios-text` reaches them through `curios-elab`'s re-exports.

**D1 — Make `Flt` opaque at the type level.** The mechanism exists: `ReduceError::EffectAtTypeLevel` already refuses `Exit`, `Foreign`, and `Cell` (`reduce/prim.rs:1715-1739`). Constant folding moves to `curios-ersd`, where partial evaluation already lives and is untrusted. This is the recommended response to the divergence above, and its justification is correctness rather than line count — though ~64 fold arms and `curios-base/src/flt.rs` leaving the trusted base is a real side effect. The cost is a language decision: `refl : Eq(@Flt, 1.0 + 1.0, 2.0)` becomes unprovable. `/std/Flt.crs` is a two-line re-export facade with no lemmas and every prelude use of floats is codec work, so the corpus cost is zero.

The discipline this establishes and should be written into the `curios-core` documentation: **a primitive needs a fold in the kernel only if a type or a proof can depend on its value.** Everything else is constant folding and belongs downstream. Fold arms by family today, for sizing the rest of the sweep: `Nat` 98, `Int` 64, `Flt` 64, `Bin` 50, `Bool` 34, `Lst` 24, `Byte` 14, and the already-opaque `Handle`, `Cell`, `Foreign`, and `Exit` at 4, 4, 1, and 1. `Int`'s status is undecided — `/std/BigInt.crs` has 56 `Int` references and whether any sits in a proof has not been traced.

**D2 — `print.rs`, in two parts.** Lines 33-350 are the source-style-name machinery — two thread-locals, `display_names`, `build_rename`, `build_shorten`, `with_pretty_names`, `with_short_names`, `collect_labels` — which alpha-rename core's gensyms back toward what the user wrote. Diagnostic presentation; moves with no argument. Whether the faithful printer follows is a separate decision, because moving it means dropping `impl Display for KernelError` and rendering kernel refusals from `curios-elab/src/error.rs` instead. Take that decision explicitly rather than by default.

**D3 — The builder clusters and the strays.** `curios-core/src/prim.rs`'s 68 one-line `impl Into<Term>` wrappers (~650 lines), of which the kernel names three; `curios-core/src/term.rs`'s `induct_type_at`/`struct_at`/`struct_entries` family and the whole match-builder set (~600 lines), none of which the kernel references, and most of `term/tests.rs` with them; `transparent_alias_target`, `direct_type_alias_target`, `HeadTag`, `head_key`, and the ten solver-shaped predicates in `universe.rs` the kernel never calls (~150 lines). An extension trait in `curios-elab` holds the builders identically.

**D4 — `Metavar`, `Infix`, and `NumLit` out of `Subterm`.** Deferred, and a legibility item rather than a soundness one once C4 lands: what it buys is making the excluded state unrepresentable instead of merely refused. `metavar` has 160 occurrences downstream and `goal` 313, which is the cost. A `Subterm<X>` parameterization is the shape if it is ever taken.

### E — Turn it on

`recheck_module` runs in the pipeline and a refusal fails the compile. Worth nothing before A and C, because a checker that has to be bypassed is worth nothing.

## Sequencing

1. A1, then A2, then re-run `kernel_disagreements` and record the new counts before planning further.
2. A3, with the amended principle written into `DESIGN.md` in the same commit.
3. Measure A4's cost, then A4 in its own commit with the `SCHEMA` bump.
4. ~~B1~~ done, ahead of A: it was self-contained and it shrinks the surface every later item walks.
5. C1 after a probe on `/std/Map/get`. C2, C3, C4, C5 in any order.
6. D throughout, by visibility first.
7. E last.

`positivity.rs`, `totality.rs`, `invert.rs`, and `elaborate/match_.rs` are outside the current in-flight naming work, so A does not collide with it. Two conventions that work has settled and this document follows: unit tests live in `foo/tests.rs` beside a `#[cfg(test)] mod tests;` declaration, and the pipeline entry points are `curios-pipeline/src/compile.rs` and `stage.rs`.

## Measurements

Re-taken from the worktree, not estimated. Re-run the inventory after every item; **an item that does not move a class count has not been shown to do anything.**

Crate sizes: `curios-core` 17,497 (15,065 non-test), `curios-elab` 31,702, `curios-base` 2,839, `curios-abi` 952.

Kernel refusals: **90 of 1050 items**, identical across the `trivial`, `arithmetic`, and `literal` fixtures and across debug and release. By class: 25 bare universe-parameter mismatch, 16 `Unclassified` (all empty `Lst`), 13 zero-level-in-instance, 36 other.

After A4's descent gate: **still 38 of 1050**, zero `NotDescending` — every proof-typed and type-yielding `rec` in the prelude descends — at **16.6s → 164s** for the inventory, the open cost fork recorded under A4.

After A3's shared positivity (and the declaration-pass reordering it forced): **still 38 of 1050**, byte-identical to the C1 baseline — both declaration gates pass the whole prelude.

After C2's declaration sizing: **still 38 of 1050** — every prelude declaration satisfies the size condition, and the evidence the check does anything is its hand-built fixture, as with coverage.

After C1's constraint entailment: **38 of 1050** — the 25-item bare-parameter class and the 13-item zero-level class both cleared, the obligation direction refused nothing, and one item progressed to the empty-`Lst` class (now 17). The remainder is that class plus a ~21-item tail of projection-shaped inversions and the four syntactically-compared conversion positions.

After A2's specialization rule: **79 of 1050**, no regressions — cleared: `/std/Nat/Lte/trans`, `/std/Vec/rest`, `/std/Str/utf8/drop_valid`, `take_valid`, `bad_uninhabited`, `/std/BigNat/cmp` and `add`/`succ` equality lemmas, and one `Bool`-head case (`xor3_inj_mid`); two items progressed to later refusals within themselves, which the per-item tally renders as a changed class rather than a clear.

The count was 90 of 1052 before B1, and the two fewer items are the deleted `Never` group. **The refusal count did not move, and should not have**: B1 closed a route the kernel wrongly *accepted*, and the inventory only counts refusals. The standing rule — an item that does not move a class count has not been shown to do anything — governs the *refusing* half of the work, sections A and C. For the certifying half, the evidence is a fixture that stops being accepted, and for B1 it is a program that stops elaborating.

Shared-analysis dependency surface: 16 `Context` call sites across 2,865 lines, per the table in A.

Positivity's fixpoint domain: of order 100 declarations, against 1,052 items.

Primitive fold arms by family, per D1.

Expected movement: out of `curios-core` ~3,100 lines under D, in ~2,865 under A plus ~600 under C, landing the crate near where it started while nearly all of it becomes trusted-and-load-bearing rather than partly trusted-and-inert.

## Verification

**Every route in the certifying table becomes a rejection test**, using the `assert!(crate::run_text(…).is_err())` idiom from `curios/src/tests/soundness.rs` and `curios/src/tests/positivity.rs`, but asserting against the *kernel's* verdict rather than the compiler's — a fixture the elaborator accepts and the kernel must refuse. At minimum: `rec f : False = f`; `exit(@False, 0)`; `induct Bad | c(f : (Bad) -> False) end`; a constructor whose payload sits at or above its own result level; a `UniverseInst` at levels violating its scheme's constraints; an elimination with an arm removed.

Some fixtures cannot be surface programs. The sizing one is among them: an earlier revision gave it as `induct Bad : Type 0 | mk(x : Type 0) end`, which does not parse, since levels have no surface syntax. The nearest writable program, `induct Box : pub Type | mk(x : Type) end`, is *correctly accepted* — the solver assigns `Box : Type 1` and `x : Type 0`, which is the choosing-versus-verifying distinction showing up in the test plan. Where no source text reaches a rule, the fixture constructs the `Module` directly, in the style of the hand-built fixtures already in `curios-core/src/kernel/*/tests.rs`, and the perimeter entry stays *auditable only* rather than being re-graded.

**Acceptance tests pin what must keep working.** The whole prelude passes the kernel; `/std/Nat/Lte/trans` passes after A2; the two currently-ignored tests `a_trivial_program_rechecks` and `arithmetic_rechecks` lose their `#[ignore]` at A2 and gate E.

**The gate for E is zero refusals whose class is not a recorded conversion incompleteness**, not zero refusals. `convert.rs:46-60` names four positions compared syntactically and nothing here closes them; an unconditional gate would be unreachable, which is how an acceptance criterion quietly becomes a formality.

**Unit tests** for each new judgment in `curios-core/src/kernel/*/tests.rs`, beside the existing `convert/tests.rs`, `infer/tests.rs`, `sort/tests.rs`, and `whnf/tests.rs`. **Property tests** for the relocated analyses, because sharing removes the disagreement signal for them.

The full gate applies, in order, with the suite run once into a file and inspected there:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features > /tmp/curios-tests.txt 2>&1
```

E additionally requires a compile-time measurement: the kernel re-checks every item of every module, so `make curios/profile CURIOS_PROFILE_SOURCE=programs/hello_curios.crs` before and after is what says whether turning it on is affordable.

## Risks

**Turning the kernel on doubles the checking work per module.** Partly measured. The kernel holds no caches by design (`kernel.rs:169`) and the elaborator's memoization is precisely what it declines to share, so the second walk cannot be made cheap the way the first was. A3 settles positivity's share by counting the fixpoint domain; A4's is the open one. The fallback is neither a flag nor a weakening but the pattern `positivity.rs:66-76` already uses — compute at archive-build time, read at replay — which under the amended principle reads the kernel's own answer.

**The kernel refuses something real and the reflex is to weaken it.** `recheck.rs:19` already states the rule: a disagreement is a question with two answers. E creates schedule pressure the earlier items do not.

**Sharing removes the disagreement signal for the analyses it shares.** Once positivity, totality, and inversion are one implementation, `kernel_disagreements` reports zero for those classes *by construction*. That is a loss, not a win, and the evidence for those analyses being right has to come from probes and property tests instead. This is the price of the share/duplicate line and should be recorded as such rather than discovered later.

**A2 borrows a judgment.** Shared inversion means a mispinned binder is accepted by both checkers. The certificate design named in A2 removes that concession; until it lands, inversion is the one place the second opinion does not apply, and it should be listed in `DESIGN.md` alongside what else the kernel takes on faith.

**The archive is an integrity boundary, not a soundness one.** The kernel re-typechecks every term it is handed, so a corrupted term is refused. A corrupted *derived value* — a polarity vector, a totality flag — is refused only where the kernel recomputes. A3 recomputes; A4 may not. Whichever way A4 lands, write down which values the kernel accepts from the archive without recomputing.

## What this may not claim

**No perimeter entry in `DESIGN.md` may be re-graded on account of this work.** `DESIGN.md` is careful that a second checker removes none of the perimeter's weaknesses and only changes the cost of being wrong about one. What changes is that each entry acquires a second implementation to disagree with, and the disagreement count is the evidence. That is a weaker claim than it will be tempting to make at E.

**`curios-elab` does not leave the trusted base**, only its rules do. See "Objective".

**The two checkers are not independent on positivity, totality, or inversion.** By design, per the share/duplicate line.

## Retractions

Recorded because the retractions are the useful part, and because all of them have the same shape: a gap inferred by reading one crate and reasoning about what must be missing, where following the construction one stage further — or taking one direct measurement — would have settled it.

**The validation pass at the kernel's boundary does not exist and should not be written.** `DESIGN.md` promised one. `zonk_module`'s total traversal already delivers the exclusion, and a second pass would duplicate a traversal to re-derive a guarantee that holds. What survived is C4: the guarantee is currently *the elaborator's*, and two arms in the kernel accept what the rest of it refuses.

**`declare_induct` and `declare_struct` are unchecked inserts, and the conclusion drawn from that was false.** The inserts are unchecked; the kernel does not thereby certify inductive declarations wholesale, because an `induct` lowers to a `rec` group of ordinary definitions, so payload sorting and registry-versus-binding agreement both fall out of the item walk. What survived was one clause — the size condition — which is not a declaration-checking problem at all but the universe constraint problem of C1.

**The functions named as the recursion cycle to defunctionalize appear zero times in the stack that overflowed.** An earlier revision named `compare`, `sort_of`, `whnf`, and `reduce_prim`, measured from watermarks that counted `infer` entries and attributed the span between them to the whole cycle. A backtrace at judgment depth 300 showed 300 `infer` frames and 298 `check` frames and nothing else. Frame size was also a distraction: debug costs ~21.5KiB per level against release's 2.05KiB and the prelude needs 102 levels, so splitting arms into `#[inline(never)]` functions would have cleared the threshold and left depth data-bound.

**Sharing the reduction driver does not become attractive under a trusted-surface objective.** An intermediate revision of this analysis claimed it did. It saves zero trusted lines: the kernel's reducer is trusted either way and the elaborator's copy is untrusted either way. `DESIGN.md`'s rejection stands under both objectives.

**`Exit` is not the largest hole and restricting a primitive does not close the class.** An intermediate revision called it the finding that reordered everything. `rec f : False = f` is smaller, needs no primitive, and cannot be closed by any typing-rule refinement — which is what makes totality forced rather than optional.

**The 13-item zero-level cluster is not the largest identifiable class**, and the constraint measurement that closed the sizing fork was read one-sidedly. See "What the kernel refuses that it must not".

**No typing-level restriction on `exit` closes its route — but removing its choice of result type does.** An intermediate revision of this analysis reached the first half and stopped there, having only considered restrictions on *which* type `exit` may be given. Every such restriction fails to the same counterexample: a constructor-free `Empty : Type` eliminates into `Prop` unguarded, so confining `exit` to relevant types still admits `Exit(Empty, 0)`. What that argument does not rule out is fixing the result at a single inhabited type, which is what landed. The near-miss is worth recording: the general claim was drawn from three failed attempts at one *shape* of fix, and stated as though it covered every shape.

**A measurement was recorded that was never taken.** The sizing item's first revision said "measured: every declaration in the prelude passes, refusal count unchanged" on the strength of a full-suite run — but the inventory is an `#[ignore]`d test the suite never executes, and when it was actually run, 51 declarations were being refused by an ordering defect the false claim had hidden. The instruction this adds to the standing one: a green suite is not the inventory, and a claim of "measured" must name the command that measured it.

**A single-line regex undercounted the shared-analysis dependency surface.** `rg -o 'context\.[a-z_]+'` missed `context` and `.induct_decl` split across lines by rustfmt, which hid `Vectors::at`'s fallback onto archive-carried polarity vectors — the finding that settled how A3 should be driven and that reframed the recompute principle. The measurement was rerun with `-U` and a multiline pattern.

The instruction all of these produce is the same one the totality work reached: **measure with `kernel_disagreements` before designing for a gap**, because the classes are countable and the count is what says which gaps matter — and check that the instrument counts what it claims to.
