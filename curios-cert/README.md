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

**Decision.** The kernel carries evaluation memos — a per-definition unfold memo, weak-head memos for local-free terms, and the types inferred for local-free terms — following the precedent of Lean's trusted `type_checker`. Every reduct entry records what its computation consumed, in budget steps and minted binder identities, and the identities are always replayed: a hit mints exactly what a recomputation would have, so every later identity lands where it would have. An inferred-type hit replays neither and hands back the type alone. The *steps* are charged for a name-keyed unfold hit and not for a term-keyed one, and the term-keyed tables are cleared wherever the budget is restored. So switching the memos off may move an exhaustion point and the identities minted after an inferred-type hit, and can move nothing else; `kernel_memo_parity` holds the semantic half to account, and `Kernel::uncached` exists so it can.

**Rationale.** A metavariable heap or refinement store *injects* answers a term alone could not produce; a memo replays the kernel's own pure function of `(term, definitions)`, computed once. Refusing even that costs a whole-prelude re-check by a large factor, spent re-deriving the same prelude spines for every recursive group the totality gate walks; the `memos` module carries the figure.

Charging a hit what a memo-free evaluator would have spent is a different thing from charging what the kernel did, and it was the second that the budget is supposed to bound. Recorded costs compound — a subterm hit twice per level makes the charge exponential in a structure the memos evaluate linearly — so a budget was declared exhausted after a small fraction of it had been spent, and this kernel refused programs the elaborator accepted within the budget the compile path hands them both. The `spend` module states the compounding with its figure, and `curios`' `kernel_memo_charge_measurements` holds the per-rung floors that say what it cost a user. A free hit can only reduce what a judgment spends, so it can only turn an exhaustion refusal into an acceptance; a semantic refusal does not move, because it does not depend on the budget. Clearing the term-keyed tables at the declaration boundary is what makes the free hit deterministic rather than order-dependent, and it measured free. The name-keyed table keeps both its charge and its longer life: it is what makes certifying a whole module affordable, and a charged hit costs what recomputing would.

The inferred-type table gives up the identity replay because the replay would count what the table avoids. It exists to type a reduct — a graph whose tree can be exponential in its depth — once per node, and the identities a recomputation mints are counted per path: replayed, a `Str/split_once` claim stated in a type exhausted the 32-bit identity space in under two seconds. Freshness is the one property a judgment reads off an identity, and the counter never falling keeps it without the replay; the `spend` module states what that concedes.

### A type is accepted by typing it, and reduction is total so that it can be

**Decision.** Three mechanisms, in this order, each doing a job the others structurally cannot.

**Reduction is total on arbitrary terms.** `whnf` never asserts on a shape a caller could hand it — an application that does not saturate its lambda, an elimination arm that does not match its payload, a projection out of range all go *stuck* rather than aborting. This is not defence in depth: `recheck_module_verdicts` takes a `Module` from anywhere and is documented as walking to the end with each verdict independent of the others, so an abort takes every other verdict with it, and reduction that declines to fire can never admit anything.

**A type is accepted by `infer_type`, not by `Sort::of`.** The kernel used to have two ways to accept a type, and only one of them was a judgment. `Sort::of` classifies a term structurally without typing it, so a declared type reached reduction, conversion and erasure having been *read* rather than *checked* — the root of the motive clause, the β step, the elimination arm and its recursive twin. `infer_type` reduces, types, and destructs the result as a sort, which is Coq's `infer_type`/`type_of_case`; Lean enters through `inferType`; Agda carries the sort on the type so a type in hand is one that was checked. `Sort::of` survives as the fast path with a precondition, the role Coq gives `Retyping.get_sort_of`, and is reached only where typing has already run.

The reduction inside `infer_type` is what makes the two mechanisms interlock: typing a declared type must reduce it first, so reduction meets untyped terms *by construction*, which is why totality is its precondition rather than its complement. It is also load-bearing on its own — `List.{v,w}(Waker)` types as its former's promised `Type v` unreduced, and as the minimal `Type 0` the constructor size condition needs once reduced.

**Counts are checked at the boundary, because typing never sees them.** An occurrence's parameters and indices, a value's parameters, a constructor tag's uniqueness, a plicity vector's parallelism: no typing rule reads a length, so no ordering discipline will ever catch these. They are checked where the declaration is consulted, and removing them leaves a malformed occurrence *certified* rather than merely aborting — the permissive failure, not the loud one.

**Rationale.** Six defects arrived through the gap between "the kernel reads a field" and "something established the field". Two produced level capture and a bypassed large-elimination guard; four aborted the walk. Fixing them one guard at a time closed instances, never the class. The split above is what makes each class impossible rather than caught: shapes by typing, counts by the boundary, and neither able to abort.

## Measuring the certifier

**The instrument.** `cargo x clippy` builds `curios-prelude` with `--all-features`, and its build script certifies the fixed prelude under a record stream filed at `curios-prelude/.artifacts/profile.tsv`; the elaboration's stream is `curios-prelude-archive/.artifacts/profile.tsv`. The walk carries three kinds of span:

- **per item:** `certify_declaration`, grouped by `Item::describe` as the elaborator's `declaration` span is, so one item's cost in each checker is found under one key;
- **per stage of the walk:** `universe_verdict`; `partial_definitions` and `check_positions` for obligations (T) and (V); `check_entrypoint`; `check_induct_decl` and `check_struct_decl`; and the shared `positivity_vectors`. What no span covers — the residue and escape checks — is `recheck_module`'s own self time;
- **per judgment:** `convert`; `reduce` and `reduce_forced`, reduction entered from outside reduction, the second past its memo so a hit costs no row; `Sort::of`; `entails`; and the shared analyses' `group_totality` and `invert_with`. Typing is `certify_declaration`'s self time.

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
