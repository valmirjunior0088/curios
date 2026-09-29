# curios-cert

The Curios certifier: the kernel deciding, from a finished term alone, whether the elaborator's output is well-typed — reduction, sort, conversion, the typing judgment, nominal elimination, subsumption, the whole-module walk that applies all of it, the erasure obligations, and the level entailment oracle, which is this kernel's alone rather than shared. The rules *both* checkers run — index inversion and the singleton determination walk, strict positivity, size-change totality — live one crate down in `curios-analysis`, behind the `Env`/`Judge` seam; `curios-core` owns what a term *is*; this crate owns what one *means*. The two-checker decision and its rationale are cross-cutting and stay in [An independent kernel re-checks what the elaborator accepts](../documentation/design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md); what the kernel covers at any moment belongs to [roadmap.md](../documentation/roadmap.md); local architecture belongs to the crate rustdoc.

## Design

### The trusted base is a crate boundary

**Decision.** The trusted base is this crate's dependency closure, checkable with `cargo tree -p curios-cert -e normal`, rather than the call-closure of the checking entry points inside a larger crate. It reaches `curios-analysis`, `curios-core`, `curios-utilities`, `curios-num` and `curios-abi` — the rules and the representation — plus `curios-archive`, `curios-print` and `curios-profile`, which serialize, format and time rather than decide. It reaches nothing of the elaborator; the dependency never reverses, so the kernel cannot consult a metavariable store, a refinement layer, or a cached elaboration, and sharing `curios-core` is sharing the representation, never a judgment. In the other direction `curios-elab` takes this crate as a **dev**-dependency only, which does not propagate — the property `curios-prelude-archive`'s build script rests on, since a build script reaching the kernel re-elaborates the whole standard library on every certifier edit.

**Rationale.** A call-closure is enumerated by tracing and drifts silently as code moves; a crate boundary is enforced by the compiler and read off the manifest. It also makes "not trusted" structural: builders, printers, and elaboration conveniences physically cannot sit inside the base, where before they had to be kept out by inventory. This supersedes the earlier rejection of a third crate, amended in [An independent kernel re-checks what the elaborator accepts](../documentation/design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) where it was recorded: the rejection predated the kernel outgrowing its host — the shared analyses, the `Env`/`Judge` seam, and the evaluation memos made the base a substantial, nameable thing whose boundary deserved enforcement.

**Rejected.** Sharing the representation through `curios-utilities`, which would put the term language in a crate whose purpose is stage-independent utilities — `curios-core` remains the representation's owner and this crate builds on it.

### The judgments flatten onto the root

**Decision.** The crate is a flat module space: `curios_cert::Kernel`, `curios_cert::convert`, `curios_cert::check_definition`. In `curios-core` the kernel kept a `kernel::` namespace because its judgments name the same things the elaborator names its own; here the crate name is the disambiguator, and `curios_cert::convert` against the elaborator's bare `convert` reads exactly as the second opinion it is.

### Incompleteness is the safe direction

**Decision.** A rule that refuses too much produces a disagreement between the two checkers, which is a signal; a rule that accepts too much is silent. Every judgment in this crate is written to that asymmetry: where a check cannot yet decide something, it refuses rather than guesses.

**Rationale.** The two-checker split only catches a systematic mistake if a wrong rule shows up as a disagreement. An overly permissive rule hides in the corpus passing, exactly like the single-checker baseline this crate exists to improve on; an overly strict one is visible the moment a real program hits it.

### The kernel memoizes its own evaluation, and a memo changes only resource verdicts

**Decision.** The kernel carries evaluation memos — weak-head memos for terms, and the types inferred for local-free terms — following the precedent of Lean's trusted `type_checker`. Every reduct entry records the binder identities its computation minted, and they are always replayed: a hit mints exactly what a recomputation would have, so every later identity lands where it would have. An inferred-type hit replays nothing and hands back the type alone. No hit is charged steps, and every table is cleared wherever the budget is restored. So switching the memos off may move an exhaustion point and the identities minted after an inferred-type hit, and can move nothing else; `kernel_memo_parity` holds the semantic half to account, and `Kernel::uncached` exists so it can.

**Rationale.** A metavariable heap or refinement store *injects* answers a term alone could not produce; a memo replays the kernel's own pure function of `(term, definitions)`, computed once. Refusing even that costs a whole-prelude re-check by a large factor, spent re-deriving the same prelude spines for every recursive group the totality gate walks; the `memos` module carries the figure.

Charging a hit what a memo-free evaluator would have spent is a different thing from charging what the kernel did, and it was the second that the budget is supposed to bound. Recorded costs compound — a subterm hit twice per level makes the charge exponential in a structure the memos evaluate linearly — so a budget was declared exhausted after a small fraction of it had been spent, and this kernel refused programs the elaborator accepted within the budget the compile path hands them both. The `spend` module states the compounding with its figure, and `curios`' `kernel_memo_charge_measurements` holds the per-rung floors that say what it cost a user. A free hit can only reduce what a judgment spends, so it can only turn an exhaustion refusal into an acceptance; a semantic refusal does not move, because it does not depend on the budget. Clearing the tables at the declaration boundary is what makes the free hit deterministic rather than order-dependent, and it measured free. A name-keyed table of definition unfolds used to outlive the declaration, charged on a hit what its first computation had spent; that computation could take free hits of its own, so which declaration unfolded a name first decided what every later one was charged. Once the closed machine read bodies directly the table held 209 entries over the whole standard library, and it was removed rather than priced exactly — [No memo outlives the declaration that filled it](../documentation/design/toolchain/no-memo-outlives-the-declaration-that-filled-it.md) carries the rule both checkers follow and what pricing it exactly cost.

The inferred-type table gives up the identity replay because the replay would count what the table avoids. It exists to type a reduct — a graph whose tree can be exponential in its depth — once per node, and the identities a recomputation mints are counted per path: replayed, a `Str/split_once` claim stated in a type exhausted the 32-bit identity space in under two seconds. Freshness is the one property a judgment reads off an identity, and the counter never falling keeps it without the replay; the `spend` module states what that concedes.

### A later walk reads the certifier's own totality record, never elaboration's stamp

**Decision.** `certify_module` returns, beside its verdicts in one `Rechecked`, a record of each definition it judged and that definition's totality, closed over everything it mentions (`curios_core::Certification`). Only this walk makes one. It is filed with the unit it classifies, and `Globals::of` and `Globals::mount` take it: where it covers its unit, its classifications seed the non-total set obligations (T) and (V) close from; where it names fewer definitions than the unit holds — an empty one, for an environment built by hand, included — the unit's items are held unclassified and `partial_definitions` classifies them from their terms exactly as it classifies an item it judges. Elaboration's stamp on a definition is read only on an item the walk judges, and only to compare it with the walk's own closed verdict. Each entry also holds what judging the definition read of other items (`curios_core::Reads`): each type, universe scheme or registry entry it consulted and each body it asked for, noted where the kernel consults its environment and taken at each item's end. That is the kernel's half of the item graph a recompile is to invalidate along, which no walk reads yet. A registry entry is accepted as part of its type former, whose entry holds what that acceptance read; positivity, one judgment over the whole declaration set, is attributed to no item. `curios-pipeline`'s `certification_tests::the_prelude_reads_along_the_graph_its_definitions_reach` holds the prelude's reads to the graph elaboration built: every name a definition mentions where the kernel types is read, and nothing is read that the definition's item does not reach.

**Rationale.** What a proof reaches runs out of the environment as readily as out of the module being walked, so the environment's classification of what is already in scope decides (V) for every proof that reaches it. It used to be the stamp elaboration writes, cross-checked once, when the unit was filed — which made the transitive half of every carried verdict the elaborator's conclusion, and the one cross-check the belief rested on once compared the wrong half of it. Reading the certifier's own conclusion removes the input rather than guarding it. The record is keyed by nothing of its own: it is part of the unit, so it is found under the unit's address, which names the compiler whose certifier made it. The perimeter entry is `documentation/soundness/what-the-kernel-consults/the-certifiers-totality-record.md`.

**Rejected.** Keeping the stamp and strengthening its cross-check at filing: the check would still be the only thing between a later walk and another component's verdict. Reading a record one name at a time: a record naming fewer definitions than its unit holds was not made by a walk over that unit, so none of its entries is known to be the closure it claims. Refusing a unit whose record does not cover it: re-deriving costs the reading walk that unit's termination analysis and admits nothing, so such a record stays a cost rather than becoming a fault every hand-built environment must avoid. An optional record: a compiled unit always carries one, so absence is only ever a hand-built environment's, and an empty record already says it. A digest of the certifier beside the record: the unit's address already names the binary.

### A group's calls are the ones the kernel types

**Decision.** The kernel does not look for a group's recursive calls with a traversal of its own. `check_group` opens a frame for the group whose bodies it checks, and every application the walk types whose head is one of the group's members is recorded as a call and graded at once by `curios-analysis`'s shared `grade`, under the size context the arms around it built: the solutions each arm's case value and index inversion put in force, its payload binders, the nonzero fact a boolean or switch arm establishes — read before the arm assumes its case equation, which would reduce the comparison to the arm's own literal — and each binder of a lambda applied on the spot standing for its argument. A member named anywhere but at an application's head is a call with no arguments, graded unknown. Once every body is checked, the shared `decide` closes the group's calls; the verdict is the local gate's and the one obligations (T) and (V) read for the group, and a group the walk never typed has none and reads `Partial`. A group is typed with the binders around it opened while a term holds it closed, so its verdict reaches what encloses it where it was typed: a count of the groups that do not descend, compared around each position and each definition, with a memo hit counting again what its term's first typing closed (`kernel::calls`).

**Rationale.** A traversal separate from typing fails open: a position it never visits contributes no edge, and a group with no edges is accepted. The kernel types every position it accepts, so calls taken from its typing fail closed wherever it types. Separate call inspection is where Rocq's guard checker was found unsound in February 2026 — `find_uniform_parameters` looked at self-calls and missed the calls between bodies ([rocq#21682](https://github.com/rocq-prover/rocq/issues/21682)) — while Agda's [`TermCheck`](https://agda.readthedocs.io/en/latest/language/termination-checking.html) and Idris 2 extract calls from terms after typing, and Lean 4's kernel checks no termination at all, compiling recursion to recursors. Grading and closure stay shared, so the two checkers still grade a call alike and differ only in how they find it; the elaborator keeps the discovery walk, rebuilt over the same two functions.

**What it types.** A call is recorded wherever the kernel types, and it types everything it accepts: a type is typed as written (`infer_type`), so an argument a redex would drop or an arm a known scrutinee rules out is typed with the rest. A nominal value's parameters are typed as an occurrence's are. Counting them without typing them was the one position a call could stand in unseen (`kernel::calls::tests::a_call_in_a_nominal_values_parameter_is_recorded`), and it certified a record literal and a constructor application at an ill-typed parameter as well (`recheck::occurrence_tests::a_nominal_value_types_its_parameters`); both are mutation-checked against typing them.

**Rejected.** Having the kernel check that discovery is complete — record the member calls it typed and refuse a group where discovery missed one. It closes the fail-open case in a fraction of the code, but the calls would still come from discovery and be graded under discovery's context, so the kernel's verdict would rest on a traversal it does not own. Keeping discovery for the kernel as the elaborator keeps it: that is the case the perimeter recorded as the one analysis whose blindness admits.

**Evidence.** `kernel::calls::tests` puts four groups with a type-yielding member, which must descend, to the local gate, each mutation-checked against the one piece of recording it depends on: a call at the caller's own argument is refused, and recording no applied call accepts it; descent through a `Nat` arm, which refining the scrutinee from the typing's spelling `pred + 1` loses; descent through a lambda applied on the spot, which typing its binders as fresh loses; and descent through a guard behind a definition, which reading the guard after the case equation loses. `recheck::proposition_tests::a_wait_inside_a_proof_is_refused_with_no_definition_to_blame` holds an inline group's verdict reaching the proof around it, and fails when a group that does not descend closes uncounted. A differential against discovery over the same opened group in the same scope agreed on all 951 groups of `/sys` and `/std` — 870 total, 81 partial — and on all 55 across the 26 programs of `programs/`, taken 2026-09-29. To retake it, have `close_group` also run `curios_analysis::group_totality(self, group)` with the recorder set aside and `curios_profile::note!` both verdicts, then read the notes out of `curios-prelude/.artifacts/profile.tsv` after `cargo x clippy` and out of each program's `curios --profile <PATH> wonder diagnostics` stream, attributing each to the `certify_declaration` span it falls inside.

### A type is accepted by typing it, and reduction is total on arbitrary terms

**Decision.** Three mechanisms, in this order, each doing a job the others structurally cannot.

**Reduction is total on arbitrary terms.** `whnf` never asserts on a shape a caller could hand it — an application that does not saturate its lambda, an elimination arm that does not match its payload, a projection out of range all go *stuck* rather than aborting. This is not defence in depth: `recheck_module_verdicts` takes a `Module` from anywhere and is documented as walking to the end with each verdict independent of the others, so an abort takes every other verdict with it, and reduction that declines to fire can never admit anything.

**A type is accepted by `infer_type`, not by `Sort::of`.** The kernel used to have two ways to accept a type, and only one of them was a judgment. `Sort::of` classifies a term structurally without typing it, so a declared type reached reduction, conversion and erasure having been *read* rather than *checked* — the root of the motive clause, the β step, the elimination arm and its recursive twin. `infer_type` types the term and destructs its type as a sort, which is Coq's `infer_type`/`type_of_case`; Lean enters through `inferType`; Agda carries the sort on the type so a type in hand is one that was checked. `Sort::of` survives as the fast path with a precondition, the role Coq gives `Retyping.get_sort_of`, and is reached only where typing has already run.

`infer_type` once reduced a type before typing it, which is how reduction came to meet untyped terms *by construction*: the elaborator let an occurrence's level float above its argument's, so `List.{v,w}(Waker)` typed as its former's promised `Type v` where its reduct sat at the `Type 0` the constructor size condition needs. The elaborator now settles that level at its argument's (`curios-elab`'s `UniverseSolver::finalize`), so `infer_type` types the spelling and the two land in one sort. Totality stays reduction's precondition rather than its complement: conversion compares unfoldings untyped, through `ground`, and a walk that aborts takes every other verdict with it.

**Counts are checked at the boundary, because typing never sees them.** An occurrence's parameters and indices, a value's parameters, a constructor tag's uniqueness, a plicity vector's parallelism: no typing rule reads a length, so no ordering discipline will ever catch these. They are checked where the declaration is consulted, and removing them leaves a malformed occurrence *certified* rather than merely aborting — the permissive failure, not the loud one.

**Rationale.** Six defects arrived through the gap between "the kernel reads a field" and "something established the field". Two produced level capture and a bypassed large-elimination guard; four aborted the walk. Fixing them one guard at a time closed instances, never the class. The split above is what makes each class impossible rather than caught: shapes by typing, counts by the boundary, and neither able to abort.

### Level entailment is forward reasoning to a least model

**Decision.** Whether a declaration's assumed constraints force `lower ≤ upper` is decided forward: from the facts `upper` states, every hypothesis `L ≤ U` whose upper side is bounded fires at the largest shift it is bounded by, raising its lower side, until a pass raises nothing; the answer is whether that least model bounds `lower`. It is Bezem and Coquand's decision procedure for the semilattice with an inflationary successor (*Loop-checking and the uniform word problem for join-semilattices with an inflationary endomorphism*, TCS 913, 2022), with one rule the naturals add — a parameter carries its offset, so `h + k` bounds the constant `k` — and the bound on the least model's values (their Corollary 4.2) is where it refuses a set holding a loop. Satisfiability reads the same model from the other end: started with every head at the largest offset any upper side carries, a set has a least model exactly when it holds no loop, which is exactly when it has a model in the naturals (their Corollary 3.5) — so the certifier's second opinion on a context is decided in polynomial time and in both directions, where a budgeted search over the disjunctions a right-hand maximum reads as used to refuse when it ran out.

**Rationale.** Its predecessor searched backward: one atom of `lower` at a time, through the first hypothesis mentioning it, into every part of that hypothesis's upper side, with a path guard against cycles, fuel against growth, and nothing remembered between branches. That is exponential in the hypotheses' width, and the standard library found the width: `/std/Try`'s lift between two `Try`s assumes 27 constraints whose sides are maxima of up to seventeen parameters, and the certifier spent 9.2 of the prelude's 9.6 seconds of entailment on its 46 level questions — the costliest, five seconds, asking for a bound the hypotheses state verbatim. Forward, the hypothesis stating it fires in the first pass. The procedure also accepts every goal the search accepted, since each of the search's three rules is a step of the forward derivation, and the true chains fuel used to cut: a differential over 30 052 goals found none the search accepted and this refuses, and 360 the other way, each held sound by the brute-force sweep the soundness perimeter names.

**Rejected.** Memoizing the backward search: a proven subgoal can be cached, but a refuted one was refused for the path it was reached on — the cycle guard and the fuel both depend on it — so the cache is either unsound to consult or ad hoc, and the search stays exponential where no subgoal repeats. Normalizing the constraint set before searching, splitting each left maximum into its parts and dropping the parts its right side already bounds: it narrows each branch and leaves the search. Rocq's incremental model, which maintains the least model as constraints arrive: the kernel's hypotheses are fixed for the whole of a declaration, so a model per question costs a pass or two and needs nothing kept between them.

## Measuring the certifier

**The instrument.** `cargo x clippy` builds `curios-prelude` with `--all-features`, and its build script certifies the fixed prelude under a record stream filed at `curios-prelude/.artifacts/profile.tsv`; the elaboration's stream is `curios-prelude-archive/.artifacts/profile.tsv`. The walk carries three kinds of span:

- **per item:** `certify_declaration`, grouped by `Item::describe` as the elaborator's `declaration` span is, so one item's cost in each checker is found under one key;
- **per stage of the walk:** `universe_verdict`; `partial_definitions` and `check_positions` for obligations (T) and (V); `check_entrypoint`; `check_induct_decl` and `check_struct_decl`; and the shared `positivity_vectors`. What no span covers — the residue and escape checks — is `recheck_module`'s own self time;
- **per judgment:** `convert`; `reduce` and `reduce_forced`, reduction entered from outside reduction, the second past its memo so a hit costs no row; `Sort::of`; `entails`; and the shared analyses' `grade`, once per recursive call the walk types, `decide`, once per group it closes, and `invert_with`. Typing — what each arm enters into the size context a call is graded under included — is `certify_declaration`'s self time.

**To retake.** Run `cargo x clippy`, then fold the stream with `cargo run --all-features --package curios -- profile curios-prelude/.artifacts/profile.tsv`. Read the self columns: a fold attributes each nanosecond and each byte to the innermost span entered, so judgment rows that re-enter one another add up where their inclusive totals do not. The stream is an instrumented debug build script's, so a duration is inflated — the spans themselves cost the walk about ten seconds — while call counts and allocation are the stable figures. An item's cost in the kernel is read beside its cost in the elaborator by summing each stream's per-item span by group and joining the two:

```sh
per_item() { awk -F'\t' -v span="$1" '$1=="D"{n[$2]=$4} $1=="S"&&n[$3]==span{g="";for(i=5;i<=NF;i++)if(substr($i,1,6)=="group=")g=substr($i,7);of[$2]=g} $1=="E"&&($2 in of){at[$2]=$4} $1=="X"&&($2 in at){t[of[$2]]+=$4-at[$2];delete at[$2]} END{for(g in t)printf "%.1f\t%s\n",t[g]/1e6,g}' "$2" | sort -t$'\t' -k2; }
join -t$'\t' -1 2 -2 2 <(per_item declaration curios-prelude-archive/.artifacts/profile.tsv) <(per_item certify_declaration curios-prelude/.artifacts/profile.tsv) | sort -t$'\t' -k3 -rn | head
```

The columns are the item, its elaboration milliseconds and its certification milliseconds. `Item::describe` names every witness of a module alike, so a module's witnesses share one row.

**The baseline**, at `19809241`, over `/sys` and `/std`:

| Span | Calls | Total | Self | Allocated | Self allocated |
| --- | --- | --- | --- | --- | --- |
| `recheck_module` | 2 | 52.4 s | 1.0 s | 8 139 MB | 119 MB |
| `certify_declaration` | 2 421 | 49.3 s | 11.1 s | 7 742 MB | 1 653 MB |
| `reduce_forced` | 96 579 | 19.9 s | 6.5 s | 1 772 MB | 509 MB |
| `Sort::of` | 147 378 | 16.1 s | 8.4 s | 3 348 MB | 1 909 MB |
| `convert` | 101 155 | 14.3 s | 3.3 s | 2 571 MB | 350 MB |
| `entails` | 42 824 | 9.6 s | 9.6 s | 2 220 MB | 2 220 MB |
| `machine::reduce_closed` | 85 622 | 6.5 s | 5.5 s | 878 MB | 849 MB |
| `partial_definitions` | 2 | 1.1 s | 0.8 s | 86 MB | 39 MB |
| `check_positions` | 2 423 | 0.48 s | 0.47 s | 88 MB | 87 MB |
| `group_totality` | 1 146 | 0.37 s | 0.18 s | 52 MB | 27 MB |
| `check_induct_decl` | 64 | 0.18 s | 0.06 s | 57 MB | 19 MB |
| `check_struct_decl` | 94 | 0.15 s | 0.05 s | 24 MB | 8 MB |
| `positivity_vectors` | 2 | 0.10 s | 0.03 s | 20 MB | 2 MB |
| `reduce` | 3 063 | 0.10 s | 0.05 s | 17 MB | 8 MB |
| `invert_with` | 4 726 | 0.03 s | 0.03 s | 1 MB | 1 MB |
| `universe_verdict` | 2 605 | 0.02 s | 0.02 s | 3 MB | 3 MB |

The `nat::*` and `truth::decide_bool` rows beside these are the algebra's, whose record is its own.

The heaviest items in the kernel, beside their elaboration:

| Item | Elaboration | Certification |
| --- | --- | --- |
| the witnesses in `/std/Try` | 1 210 ms | 9 504 ms |
| `/std/http/Url/lit` | 57 ms | 1 224 ms |
| `/std/Flt/significant` | 201 ms | 1 073 ms |
| the witnesses in `/std/Tuple` | 1 546 ms | 1 033 ms |
| `/std/Flt/to_str` | 1 537 ms | 808 ms |
| `/std/Tui/input/decode` | 787 ms | 787 ms |
| `/std/Flt/of_decimal_in` | 295 ms | 623 ms |
| `/std/Cli/step` | 915 ms | 622 ms |

**What it showed first.** `entails` is almost wholly one witness: 9.2 s of its 9.6 s is `/std/Try`'s lift between two `Try`s, 46 level questions under 27 assumed `max(…) ≤ max(…)` constraints, each answered `true` after a depth-first search whose dead ends it never remembers — the costliest of them asking for a bound the hypotheses state verbatim.

**After entailment went forward** ([the decision above](#level-entailment-is-forward-reasoning-to-a-least-model)), retaken the same way, with the calls unchanged:

| Span | Before | After |
| --- | --- | --- |
| `recheck_module` | 52.4 s, 8 139 MB | 38.9 s, 6 058 MB |
| `entails`, self | 9.6 s, 2 220 MB | 1.9 s, 139 MB |
| `convert` | 14.3 s, 2 571 MB | 5.8 s, 633 MB |
| the witnesses in `/std/Try` | 9 504 ms | 229 ms |

A program declaring the same lift between two `Try`s on its own, compiled by `wonder stage core-elab -` under `--profile`, went from 15.0 s to 4.6 s, its kernel walk from 11.0 s to 0.44 s, and its 628 entailments from 10.6 s to 43 ms.

**After the kernel recorded its own calls**, retaken the same way: the walk no longer asks the shared discovery for a group's calls, so `group_totality` has no row in its stream, and the two halves of the shared analysis it drove take its place.

| Span | Calls | Total | Self | Allocated |
| --- | --- | --- | --- | --- |
| `group_totality`, before | 1 146 | 0.37 s | 0.18 s | 52 MB |
| `grade` | 450 | 0.17 s | 0.01 s | 24 MB |
| `decide` | 951 | 0.01 s | 0.01 s | 0.5 MB |
