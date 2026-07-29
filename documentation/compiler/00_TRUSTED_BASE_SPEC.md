# The trusted base

Working implementation specification for making the independent kernel load-bearing: moving out of `curios-core` everything that cannot admit a program, moving into it every rule that can, and turning it on so that `curios-elab` leaves the trusted base.

This effort does not add a rule to the language, change what any program means, or make the kernel small. It changes *who is trusted*. Today the compiler's verdict is the elaborator's alone; when this lands, a program is accepted only if two independently written checkers agree.

This specification refines the roadmap item at [ROADMAP.md](../ROADMAP.md) line 96, which should link here. When this work lands, fold the permanent boundary rules into the `curios-core` crate documentation, re-grade the perimeter table and the "What is not checked" list in `DESIGN.md`, update `ROADMAP.md`, and delete this working specification after no remaining document refers to it.

**Step 7 is implemented; everything else here is not.** It landed first and out of order, because the kernel aborted on real input and nothing downstream could be measured against a real module until it stopped. The measurements below are computed from the worktree; every claim about what the kernel does or does not check was read out of the source and is cited to a line.

## Objective

Establish the following, and record it as what the compiler's acceptance means:

```text
A module is accepted only if the elaborator and the kernel both accept it.
The kernel decides from the finished terms alone.
```

`DESIGN.md` currently states the opposite, and states it correctly: *"An independent kernel is being built in `curios-core` and does not yet re-check anything, so it subtracts nothing from that base today."* `curios-elab/src/recheck.rs:25` says the same from the other side: *"Nothing in the pipeline calls this."*

The completed implementation must compile the existing `/sys`, `/syn`, `/std`, examples, benchmarks, and tests with `recheck_module` on the compile path and a kernel refusal failing the build.

## What the trusted base is today

`curios-core` is 17,428 lines and `curios-elab` is 31,713. **Both are trusted, all 49,141 lines**, because the elaborator's verdict is the only one anything acts on and the kernel is inert.

That produces the measurement problem this specification has to settle before any sequencing makes sense. Moving code out of `curios-core` shrinks a crate that is not yet load-bearing and subtracts nothing from what is trusted. Moving checks into the kernel and turning it on is what subtracts — and it *adds* lines to `curios-core`.

The expected net is out ≈ 3,100 lines and in ≈ 2,000–3,500 excluding totality, so `curios-core` lands between 17,000 and 20,000 lines: roughly where it started. What changes is that nearly all of it becomes trusted-and-load-bearing rather than partly trusted-and-inert, and that 31,713 lines of `curios-elab` stop being trusted at all.

**The target is the trusted surface, not the line count.** Every step below is justified against that and not against the size of the crate.

## Permanent design decisions

**Declaration acceptance is a typing rule, and belongs in the kernel.** `DESIGN.md` records the opposite — *"what it will not cover, by construction, is totality, positivity, and witness coherence, which are whole-module analyses rather than typing rules"* — and this specification reverses that for positivity and reopens it for totality. Two reasons. The kernel already has a module driver: `curios-core/src/kernel/module.rs` walks items in order and defines each as it goes, so "whole-module" does not describe a boundary it cannot cross. And positivity is not a whole-module analysis in the first place; it is a condition on accepting an `induct` or `struct` declaration, which every kernel in this family checks at that point.

**A kernel that uses a rule must check the rule's premise.** `DESIGN.md` establishes that definitional proof irrelevance is sound precisely because every `Prop` inhabitant is total — obligation (V)'s job — and that the conversion recurrence rule stands on aggressive (T). The kernel uses irrelevance (`curios-core/src/kernel/convert.rs:160`) and it uses the recurrence rule. The fixpoint argument that licenses both is an argument about *the elaborator's* pipeline; it does not transfer to a second checker that runs neither obligation. A kernel that declines to check totality is applying two rules on the elaborator's word, which is the exact dependency the split exists to sever.

**The kernel recomputes; it does not read the elaborator's answers.** Where a check's result is already recorded on a registry entry — `InductDecl::polarities`, the `UniverseContext` constraint sets — the kernel must derive it again from the telescopes. Those fields are the elaborator's verdict and they ride the prelude archive. Reading them would let a bad archive through a checker whose entire purpose is not to trust `curios-elab`.

**Incompleteness is the safe direction, and it stays that way.** `curios-core/src/kernel/convert.rs:46` already states this for conversion. It governs every rule added here: a rule that refuses too much produces a disagreement, which is a signal; a rule that accepts too much is silent, which is what the second opinion exists to prevent. If a rule has to be weakened to make a real module pass, that weakening is a decision about the trusted base and belongs in `DESIGN.md`.

**Printing is not a rule and does not belong in the trusted crate.** `curios-core/src/lib.rs:27` already concedes this. The only thing anchoring it is `impl Display for Term`, needed by `KernelError`'s own `Display`.

## Non-goals

- A small kernel. `DESIGN.md` settles this: native inductive families, structures, a universe hierarchy, and a primitive roster with folds make any checker for this term language thousands of lines rather than hundreds.
- Removing native inductive types, or replacing them with an encoding cheaper to verify.
- A fresh, fully explicit, metavariable-free IR for the kernel to check. Rejected in `DESIGN.md` and not reopened here; see "The boundary is already enforced" below for why the alternative it named turned out to be unnecessary too.
- Sharing the reduction driver between the two checkers. Rejected in `DESIGN.md`; the duplicate reduction is the point.
- Witness coherence and the orphan rule. Incoherence means two call sites resolve the same key differently and both elaborate to well-typed terms — confusion, not unsoundness, and not a typing rule.
- Cumulative inductive types. An open fork in `DESIGN.md`, deliberately not taken, and this work is the evidence it should be decided against.
- Verifying the kernel itself, or any metatheory. The model that definitional proof irrelevance needs is out of scope here as it was for the totality work; `DESIGN.md` records it under "What is still missing is the model, not the reasoning".
- Reducing the line count of `curios-core`.

## What the kernel already checks

Stated because two rounds of investigation each began by claiming a hole that turned out to be covered, and because the steps below are sized against this list.

**Elaboration-only syntax cannot reach it.** `zonk_module` (`curios-elab/src/zonk.rs:127`) is a total traversal covering `items`, `body`, `type_`, and both registries including constructor telescopes and `result_sort` (lines 144–190). Its `Subterm` match has `Infix`, `NumLit`, and `Metavar` as `unreachable!` arms (`curios-elab/src/zonk.rs:895`, `:896`, `:1106`). A zonked module cannot carry any of the three, and `recheck_module` is only ever handed a zonked module.

**Constructor payload types are well-sorted, and the registry agrees with the bindings.** An `induct` declaration lowers to a `rec` group of ordinary definitions (`curios-elab/src/elaborate/module.rs:690–740`), so the type constructor and every value constructor are real module items that `check_definition`/`check_rec_group` walk. Sorting a constructor's declared function type sorts every payload domain, and `infer` checks a `Variant`'s payload against `declaration.instantiate(tag, params)` (`curios-core/src/kernel/infer.rs:190–212`), so the registry entry is cross-checked against the bindings in both directions.

**Nominal elimination is verified in full**, each arm at its own constructor's index targets, with the large-elimination guard deciding its singleton side condition rather than approximating it (`curios-core/src/kernel/infer/eliminate.rs`).

**The foreign wire contract is re-decided**, against `wire_term` rather than against the elaborator's record (`curios-core/src/kernel/infer/prim.rs:321–343`).

**Proof irrelevance, eta at Π and Σ, subsumption, and the recurrence rule** are all present in `curios-core/src/kernel/convert.rs`.

## The boundary is already enforced

`DESIGN.md` rejects a separate kernel IR and says the three elaboration-only constructors are *"better excluded by a validation pass at the kernel's boundary, which is a few lines and is checkable."* That pass was never written, and it should not be: `zonk_module`'s total traversal already delivers the exclusion, and a second pass would duplicate a traversal to re-derive a guarantee that holds.

`KernelError::NotCore` stays an error rather than becoming an assertion. The kernel's input contract is `&Module`, not "a zonked `&Module`", and that assumption has already been violated in practice — `curios/src/tests/kernel.rs:16–21` records the period during which the tests read `Stage::Core`, which the pipeline emitted before elaboration, and fed an un-typechecked module to the kernel. `NotCore` is what the kernel said about it, and a refusal is what made the thing diagnosable. An `unreachable!` there would have aborted instead.

## What moves out of `curios-core`

Every item below was selected mechanically: for each `pub fn` in the crate, whether anything under `curios-core/src/kernel/` references it. The kernel is the only part that can admit a program, so a public function the kernel never names is carried in the trusted crate for a downstream consumer's benefit.

| Module | `pub fn` | Named by the kernel | Not named |
| --- | --- | --- | --- |
| `print.rs` | 5 | 0 | 5 |
| `prim.rs` | 69 | 3 | 66 |
| `term.rs` | 90 | 37 | 53 |
| `scope.rs` | 45 | 21 | 24 |
| `universe.rs` | 22 | 12 | 10 |
| `inductive.rs` | 10 | 5 | 5 |
| `polarity.rs` | 4 | 1 | 3 |
| `structure.rs` | 3 | 1 | 2 |

**`print.rs`, 1,349 lines, zero kernel references.** Move in two parts. Lines 33–350 are the source-style-name machinery — two thread-locals, `display_names`, `build_rename`, `build_shorten`, `with_pretty_names`, `with_short_names`, `collect_labels` — which alpha-rename core's gensyms back toward what the user wrote. That is diagnostic presentation and moves with no argument. Whether the faithful printer follows is a separate decision, because moving it means dropping `impl Display for KernelError` and rendering kernel refusals from `curios-elab/src/error.rs` instead.

**The `impl Prim` constructor helpers, about 650 lines.** `curios-core/src/prim.rs:167–946` is 68 one-line `impl Into<Term>` wrappers — `int_add`, `flt_sqrt`, `cell_get` — of which the kernel names three. Fields are public, so these are call-site convenience for `curios-text`'s lowering and the elaborator's neutral rebuilds. The trusted content of the module is the `Prim` enum and the five internal methods from line 771.

**The `Term` builder cluster, about 600 lines.** `curios-core/src/term.rs:828–1443`: the `induct_type_at`/`struct_at`/`struct_entries` family and the whole match-builder set — `induct_match`, `induct_match_default`, `bool_match`, `nat_match`, `lst_match`, `bin_match`, `switch_scoped`, and every `_scoped` variant. None is referenced by the kernel. An extension trait in `curios-elab` holds them identically, and most of `curios-core/src/term/tests.rs` (566 lines) follows them out.

**Strays with no in-crate use, about 150 lines.** `transparent_alias_target` and `direct_type_alias_target` (`term.rs:474`, `:510`) are the elaborator's alias-unfolding heuristics living in the representation. `HeadTag` and `head_key` likewise. Ten solver-shaped predicates in `universe.rs` that the kernel never calls — `is_tautology`, `cancel_offset`, `is_closed`, `constant_part`, `identity_instance`, `from_constraints` — are luggage the universe solver left behind when it moved to `curios-elab`.

**`Metavar`, `Infix`, and `NumLit` out of `Subterm`, about 350 lines direct.** Deferred, and it is a legibility item rather than a soundness one: the exclusion already holds, so what this buys is making the excluded state unrepresentable instead of merely unreachable. `metavar` has 160 occurrences downstream and `goal` 313, which is the cost.

**`polarity.rs` does not move.** It has one kernel reference today and is otherwise the elaborator's analysis result stored on a core declaration, which reads as misfiled. Under step 3 below the kernel becomes its second consumer and its placement becomes correct.

## What moves into the kernel

Ordered by what each one lets through today.

**The universe constraint set is discarded, not discharged.** `curios-core/src/scope.rs:337–358` instantiates a scheme by pure substitution with no constraint check, and `RecGroup::instantiate_universes` (`curios-core/src/term.rs:2497–2523`) verifies instance arity and then sets `context: UniverseContext::empty()`. A grep for `constraints` across `curios-core/src/kernel/` returns nothing. So a polymorphic definition's declared constraints are decoration at every use site the kernel sees.

This is also the kernel's only available route to the **constructor size condition**, which is what keeps inductives from re-admitting the paradox the hierarchy exists to exclude. The elaborator enforces it: `add_declaration_sizing` (`curios-elab/src/elaborate/module.rs:95–140`) emits, per constructor, that each payload's level is `≤` the result level and each uniform parameter's is `≤` result + 1, under `UniverseConstraintKind::ConstructorSizing`, discharged by the universe solver. The kernel neither re-derives nor discharges them, and `check_definition` (`curios-core/src/kernel/module.rs:45`) calls `sort_of(kernel, type_)?` and **discards the result** — it computes a sort and compares it to nothing. `induct Bad : Type 0 | mk(x : Type 0) end` is therefore certified by the kernel with `Bad : Type 0` while `Bad` contains `Type 0`.

The size condition is not an unrecorded hole in the compiler. It sits in the perimeter table as `validate_universes`, graded "auditable only". What is unrecorded is that the kernel does not duplicate it.

**Strict positivity is not checked.** `check_positivity` runs at `curios-elab/src/elaborate/module.rs:1285`; `curios-core/src/kernel/` contains no occurrence of the word and zero references to `Polarity`. `DESIGN.md`'s own four-line exploit — `induct Bad | c(f : (Bad) -> False) end` — is well-typed by every rule the kernel has, so the kernel certifies it.

**Coverage is not verified.** `check_induct_arms` (`curios-core/src/kernel/infer/eliminate.rs:47–72`) iterates the arms that are present and never asks whether the absent ones were legitimately absent. An elimination missing an arm, with no catch-all, is a well-typed stuck term inhabiting the motive — an inhabitant of `False` with no proof for the missing case. The perimeter grades Coverage *probed*, but that grade is the elaborator's.

**Index inversion does not exist, and it is where the walk stops.** `/std/Nat/Lte/trans` refines `b` to `b2 + 1` in one arm and `b3 + 1` in another, and its recursive call is well-typed only given `b2 ≡ b3`. `curios-elab/src/invert.rs` supplies that in 235 lines; the kernel has no equivalent, so it refuses. Nothing downstream of this can be validated against a real module until it lands, and writing it independently — rather than sharing the elaborator's — is what keeps the split meaningful.

**A `Switch`'s default and the free-monoid carriers' arms are unchecked.** Documented in place at `curios-core/src/kernel/infer.rs:370`: those arms are typed by their bodies and never verified against the motive, because their binders would have to be typed against the carrier's own successor structure.

**Small declaration residue.** Constructor tag distinctness, index-telescope arity, and whether every registry constructor has a corresponding binding. Perhaps 100 lines, and it belongs with positivity rather than as its own piece.

**Totality is not checked, and the kernel uses two rules that need it.** See "Permanent design decisions" above. Note also that because an inductive's type bindings lower to a `rec` group, `DESIGN.md`'s third route — `rec Bad : Type = Sink(Bad)` — is structurally the same shape as a type-constructor binding, so positivity over registry entries does not reach it however well it is implemented.

**The kernel aborts on real input.** A debug build overflows partway through `/std/Toml`, because judgment depth scales with a `Str` literal's *length* — 103 nested judgments at 40 bytes, 324 at 160, 494 at 640 — as `infer` and `check` descend a certified-UTF-8 chain two frames per link. `AGENTS.md` forbids exactly this shape, and a checker that aborts cannot be made load-bearing whatever else it decides. Step 7 is the fix and records the backtrace that identifies the chain.

## Implementation steps

Steps 1, 2, and 3 are independent of step 4 and deliver whether or not index inversion is quick.

**1. Move what cannot admit a program.** `print.rs` lines 33–350, the `impl Prim` constructor helpers, the `Term` builder cluster, and the strays — as specified above. No behavior changes, no rule changes, and `curios-core` becomes readable as the statement of the rules it is meant to be. Take the `Display for KernelError` decision explicitly rather than by default.

**2. Discharge universe constraints, and derive constructor sizing.** Verify at each `UniverseInst` that the stated levels satisfy the scheme's constraint set, and stop discarding the constraints in `RecGroup::instantiate_universes`. Then re-derive the `ConstructorSizing` inequalities from each declaration's telescopes and check them against the level algebra.

**The fork this step used to record is settled, and one arm of it is empty.** The question was whether to **re-derive** the inequalities from the telescopes or **discharge** the set already recorded on the declaration's `UniverseContext`, and the answer was argued on trust: only the first is a second opinion. Measurement makes it forced instead. Across the whole prelude — 32 inductive and 42 structure declarations — the recorded per-declaration constraints are 47 `SchemeInstantiation` and 13 `Cumulativity`, and **zero `ConstructorSizing` and zero `FieldSizing`**. There is nothing to discharge, and nothing to cross-check a re-derivation against either.

Constructor telescope metas *are* in the declaration's interface (`curios-elab/src/elaborate/module.rs:817`), so the visibility this step worried about is not what drops them. `generalize` (`curios-elab/src/universe_solver.rs:999`) drops a constraint that mentions no generalized meta, and again if it is a tautology — and a sizing constraint is exactly the thing the solver makes true by *choosing* the result level, so once solved it is a tautology and is discarded. It was never carried because it was never in doubt.

That is the shape of the whole step, and it generalizes past sizing: **the elaborator satisfies a level condition by choosing levels that make it hold; the kernel is handed levels already chosen and must verify that they do.** Those are different operations, and only the second is a check. It is why the elaborator can discharge sizing without leaving a trace, why the kernel's version cannot be assembled from what the elaborator left behind, and why `check_definition` computing a sort and comparing it to nothing is the same omission one level down.

**3. Strict positivity in the kernel**, plus the declaration residue from the previous section. Recompute the polarity vectors from the telescopes; do not read `InductDecl::polarities`. This is a clause of accepting a declaration and should land in the same place as the residue, not as a separate pass.

**4. Index inversion**, written independently of `curios-elab/src/invert.rs`. The largest single piece, and the blocker for everything measured against a real module.

**5. Coverage**, which depends on step 4 because legitimate absence has two sources — a catch-all, and a constructor pruned by inversion because it cannot produce a value at the scrutinee's indices.

**6. The `Switch` default and the free-monoid carriers' arms.**

**7. Defunctionalize `infer` and `check` onto an explicit frame stack.** The typing judgment must run on the default test-thread stack at a term nesting bounded by input rather than by written nesting, per `AGENTS.md`.

**This step named the wrong functions, and the correction is the useful part.** It used to say the cycle to make iterative was `compare`/`sort_of`/`whnf`/`reduce_prim`, and to measure the depth watermark against the figures in `curios/src/tests/kernel.rs`. A backtrace captured at judgment depth 300 says the deep stack is:

```text
300  curios_core::kernel::infer::infer
298  curios_core::kernel::infer::check
  1  curios_core::kernel::module::check_entrypoint
  1  curios_elab::recheck::recheck_module_verdicts
```

and nothing else — **zero frames of `compare`, `sort_of`, `whnf`, `reduce_prim`, `convert`, or `subsumes`.** Every function this step used to name is absent from the chain it exists to shorten. The watermark measurements that produced those names counted `infer` entries and attributed the span between them to the whole cycle; the cycle was never on the stack.

The real shape is two frames per level and nothing else. `check(t, T)` is `infer(t)` followed by `subsumes`, and `infer` on an application checks each argument, so a right-nested chain — one link per byte of a literal's UTF-8 derivation — descends `infer → check` per link. The judgment is structurally recursive over a term whose nesting is data-driven, and the only thing bounding it is the step budget, which permits about 1,000,000 where the stack affords roughly 95 levels in a debug build and 1,000 in release. A step budget is not a stack bound and was never going to be one.

**Frame size is a distraction, and rejecting it is what makes this the fix at the source.** Debug costs about 21.5KiB per level against release's 2.05KiB — the ordinary unoptimized-frame penalty — and the prelude needs 102 levels, so debug misses by roughly ten percent. Splitting `infer`'s arms into separate `#[inline(never)]` functions would clear that, and it would be wrong: it moves a threshold and leaves depth data-bound, so a deeper term fails again and the work bought nothing but headroom.

**The precedent is in the sibling checker, for the same terms.** `curios-elab` hit exactly this and defunctionalized its `elaborate → elaborate_apply → check` cycle onto a frame stack, for literal spines, for this reason. `curios-core` itself does the same everywhere else a walk meets data depth — `traverse_rewrite_spine` for `Apply`/`Variant` spines, `Term`'s worklist equality, `Node`'s iterative dismantle. The kernel's judgment is the one place in the crate that hand-rolls native recursion over a data-deep structure. `DESIGN.md` forbids sharing the driver and that stands: the technique transfers, the code must not.

Success criterion, already baselined: `kernel_disagreements` completes in a **debug** build, and judgment nesting stops appearing in the stack span.

**8. Totality, at a scope to be decided.** Three options, and this specification recommends the middle one.

Full (T) and (V) is the complete answer and the largest piece: `curios-elab`'s implementation is 1,797 lines plus 331 of tests, and a kernel version is 800–1,200 before the position analysis that decides *where* a `Partial` classification is a rejection.

(V) alone checks the premise proof irrelevance actually needs. It is the smaller of the two, and `DESIGN.md` establishes that (V) needs no walk — a term is a proof exactly when its type is a proposition, which the kernel already decides through `sort_of`. (T)'s aggressive reading depends on the elaborator's settle records, which a second checker cannot inherit and would have to re-derive incompletely, and it backs only the recurrence rule.

Deferring both leaves the kernel applying irrelevance and the recurrence rule on the elaborator's word. That is a defensible trade-off — it is what ships soonest — but it must be written into `DESIGN.md` as what the second opinion does not cover, rather than left implied.

**9. Turn it on.** `recheck_module` runs in the pipeline and a refusal fails the compile. This is the step that subtracts `curios-elab` from the trusted base, and it is worth nothing before steps 4 through 7, because a checker that has to be bypassed is worth nothing.

## Measurements

Computed from the worktree at the time of writing, not estimated.

Crate sizes: `curios-core` 17,428 lines, `curios-elab` 31,713, both trusted, 49,141 total.

Kernel-unreferenced public API, per module, in the table under "What moves out of `curios-core`". The aggregate: 168 of the 248 `pub fn` in the crate's non-kernel modules are never named by the kernel.

Expected movement: out ≈ 3,100 lines; in ≈ 2,000–3,500 excluding step 8, or 3,000–5,000 including it.

Kernel judgment depth on a `Str` literal, from `curios/src/tests/kernel.rs`: 103 nested judgments at 40 bytes, 324 at 160, 494 at 640. A 2 MiB thread runs out at roughly 120 nested judgments in a debug build.

**The baseline.** `kernel_disagreements` in `curios/src/tests/kernel.rs` is an ignored inventory test that walks whole programs and tallies every refusal by class. Run it after every step and record the counts here.

It has to be run in release, and that is step 7's defect rather than a property of the test: a debug build aborts partway through `/std/Toml`. The depth is identical in both profiles and only the frame size differs, so release is a way to keep measuring until the abort is fixed — not evidence that it is tolerable. Until step 7 lands, every count below is taken in a configuration the compiler is not allowed to require. Its own documentation states why: *"The kernel is incomplete in known places, so a walk over a real module stops at the first of them and says nothing about what lies past it."* A step that does not move a class count has not been shown to do anything.

Before step 2, over the whole standard library: **1,048 items, 90 refused — 74 `Mismatch`, 16 `Unclassified`.** Two cautions on reading it. `Unclassified` is a class the first-error walk never surfaced, so the inventory is already worth more than the walk it replaced. And the count is per *item*: an item stops at its own first refusal, so this classifies what is missing and does not size what is left.

Of the 74 mismatches, 18 involve a zero universe level in an instance — `Async.{0,0,0}` against `Async.{u,v,w}`. That is a grep rather than a classification and should be treated as a signal, but it is the largest identifiable cluster and it points at step 2, which this document otherwise justifies by argument alone. **None** of the 90 is inductive-parameter cumulativity, which is the evidence the non-goal above claims.

## Verification

**Every hole named above becomes a rejection test**, using the `assert!(crate::run_text(…).is_err())` idiom from `curios/src/tests/soundness.rs` and `curios/src/tests/positivity.rs`, but asserting against the *kernel's* verdict rather than the compiler's — a fixture the elaborator accepts and the kernel must refuse. At minimum: `induct Bad | c(f : (Bad) -> False) end` for positivity; a constructor whose payload sits at or above its own result level, for constructor sizing; a `UniverseInst` at levels violating its scheme's constraints; and an elimination with an arm removed, for coverage.

The sizing fixture is one of the hand-built ones. An earlier revision gave it as `induct Bad : Type 0 | mk(x : Type 0) end`, which does not parse — levels have no surface syntax, so `Type 0` is rejected at `Type`. The nearest writable program, `induct Box : pub Type | mk(x : Type) end`, is *correctly accepted*: the solver simply assigns `Box : Type 1` and `x : Type 0`, which is the choosing-versus-verifying distinction under step 2 showing up in the test plan.

Those fixtures cannot be written as surface programs in every case. Where no source text reaches the rule — which `DESIGN.md` says is permanently true of the universe hierarchy — the fixture constructs the `Module` directly, in the style of the hand-built fixtures already in `curios-core/src/kernel/*/tests.rs`, and the perimeter entry stays *auditable only* rather than being re-graded.

**Acceptance tests must pin what must keep working.** The whole prelude passes the kernel; `/std/Nat/Lte/trans` passes after step 4; the fixtures in `kernel_disagreements` reach zero refusals. The two currently-ignored tests `a_trivial_program_rechecks` and `arithmetic_rechecks` lose their `#[ignore]` at step 4 and are the gate on step 9.

**Unit tests** in `curios-core/src/kernel/` for each new judgment, beside the existing `convert/tests.rs`, `infer/tests.rs`, `sort/tests.rs`, and `whnf/tests.rs`.

The full gate applies, in order, with the suite run once into a file and inspected there:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features > /tmp/curios-tests.txt 2>&1
```

Step 9 additionally requires the release run of `kernel_disagreements` at zero, and a compile-time measurement: the kernel re-checks every item of every module, so `make curios/profile CURIOS_PROFILE_SOURCE=programs/hello_curios.crs` before and after is what says whether turning it on is affordable.

## Risks

**The kernel refuses something real and the reflex is to weaken it.** `curios-elab/src/recheck.rs:19` already states the rule: a disagreement is a question with two answers, and if a rule has to be weakened to make a real module pass, that weakening is a decision about the trusted base and belongs in `DESIGN.md`. The risk is that step 9 creates schedule pressure the earlier steps do not.

**Turning the kernel on doubles the checking work per module.** Unmeasured. The kernel holds no caches by design (`curios-core/src/kernel.rs:169`), and the elaborator's memoization is precisely what it declines to share, so the second walk cannot be made cheap the way the first was. If it proves unaffordable, the fallback is a flag rather than a weakening — but a kernel that is off by default is back to subtracting nothing.

~~**Step 7 may not be a local fix.**~~ **It is local, and narrower than this risk feared.** The worry was that the depth belongs to how a certified-UTF-8 chain is *represented* rather than to how it is walked, so no rewrite of the walkers would settle it. A backtrace settles it the other way: the chain is `infer`/`check` alone, in one file, and defunctionalizing them bounds the depth whatever the representation does. Representation still matters for the reduction *cost* — the budget spend is linear at roughly 43 steps per byte, capping a literal near 23KiB against the default 1,000,000 — but that is a separate question from the stack, and it is not what makes the kernel abort. The standing part of the risk is the rule it cited: `AGENTS.md` forbids hiding any of this behind `RUST_MIN_STACK`.

**Step 8 is a re-litigation of a written decision.** `DESIGN.md` says totality is out of the kernel by construction. Reopening it without settling it leaves the specification and the design document in contradiction, which is worse than either answer. Take the decision at step 8 and record it there in the same commit.

**The prelude archive may be a hole that none of these steps closes.** The archive carries `polarities` and each declaration's `universe_context`, and a replayed prefix's verdicts were settled when the archive was built. If the kernel does not re-check a replayed prefix, steps 2 and 3 have a gap at the archive boundary regardless of how well they are implemented. This was not settled during investigation and should be settled before step 2.

## Left open

~~**Whether the `ConstructorSizing` constraints are visible per declaration.**~~ **Settled: they are not, in any declaration in the prelude.** Recorded under step 2, where it closes the fork rather than deciding it.

~~**Whether a replayed prelude prefix is re-checked by the kernel.**~~ **Settled: it is.** `recheck_module` walks `module.items`, and at a replay `elaborate_and_zonk_with_prelude` splices `prelude.items` ahead of the user suffix, so the module the kernel receives is the whole program. The inventory above walked all 1,048 items with refusals landing inside `/std/Async`, `/std/Map`, and `/std/Toml`. Steps 2 and 3 are worth their full value at the archive boundary. The narrower hazard survives and is already step 3's instruction: the archive-carried *verdicts* — `polarities`, `universe_context` — are read rather than recomputed.

**`Prop` non-informativeness for structures and concepts.** A perimeter entry, currently enforced at `curios-elab/src/elaborate/match_.rs:523`, and plausibly a clause of `StructDecl` acceptance in step 3. Not traced far enough to say whether it separates cleanly, and deliberately not folded into step 3 on a guess.

**What the second opinion is worth once it runs.** `DESIGN.md` is careful that a second checker removes none of the perimeter's weaknesses and only changes the cost of being wrong about one. No perimeter entry may be re-graded on account of this work; what changes is that each entry acquires a second implementation to disagree with, and the disagreement count is the evidence. That is a weaker claim than it will be tempting to make at step 9.

## State of the worktree

**Step 7 is done and committed.** `infer` now drives an explicit obligation stack: the child obligations of an application, a constructor, and a record are deferred rather than descended into, because those three instantiate their telescopes by substituting the child term rather than binding it, so every deferred obligation is checked in the context it was recorded in. Arms that open binders still recurse, which is the written-nesting bound `AGENTS.md` allows. `kernel_disagreements` now completes in a **debug** build and reports the same 90 of 1,048 it reports in release — the identical count across profiles being the check that this was a restructuring and not a change of rule.

Nothing else in this document is implemented. Steps 1, 2, 3 remain independent and available; step 2 has its fork closed and both of its prerequisites settled, and it has the largest measured class behind it.

**One change outside this document's scope moved ground under it.** `/syn/Str` now proves a string literal valid by computation — `of_scan_eq(b, refl_scan(b))`, constant size — instead of by a `Utf8` derivation with one link per byte. That was forced by the same shape step 7 was: five separate defects, including both erasure obligations and three in the printer, traced to a derivation whose depth was the literal's length. Three consequences for this work. The kernel re-checks a different, far smaller term for every literal. `SCHEMA` moved to 18, so the archive rebuilds. And `/std/Str/utf8`'s lemmas are untouched, because the bridge rebuilds a derivation by reduction wherever one is actually eliminated — which is why the reflection was done as a bridge rather than by restating `Valid`.

Two claims were made during the investigation that produced this document and then retracted against the source, and both are recorded because the retractions are the useful part.

The first was that the validation pass `DESIGN.md` promised at the kernel's boundary does not exist and should be written. It does not exist, and it should not be: `zonk_module`'s traversal already delivers the guarantee. The claim came from grepping `curios-core` for the check and finding nothing, without asking whether some pass upstream had already made it unnecessary.

The second was that `declare_induct` and `declare_struct` are unchecked inserts and therefore the kernel certifies inductive declarations wholesale. The inserts are unchecked and the conclusion is false: an `induct` lowers to a `rec` group of ordinary definitions, so payload sorting and registry-versus-binding agreement both fall out of the item walk. What survived was one clause — the size condition — which is not a declaration-checking problem at all but the universe constraint problem of step 2, and which promoted step 2 from a small hygiene fix to the item carrying the paradox guard.

A third belongs with them, from implementing step 7. The functions this document named as the cycle to make iterative — `compare`, `sort_of`, `whnf`, `reduce_prim` — appear **zero** times in the stack that overflows. They were named from watermark measurements that counted `infer` entries and attributed the span between them to the whole cycle; a backtrace showed 300 `infer` and 298 `check` frames and nothing else. All three errors have the same shape as the ones the totality work produced before it: a gap inferred by reading one crate and reasoning about what must be missing, where following the construction one stage further — or taking one direct measurement — would have settled it. The instruction that follows is the same one that document reached: measure with `kernel_disagreements` before designing for a gap, because the classes are countable and the count is what says which gaps matter.
