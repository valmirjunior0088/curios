# Algebra, part 2: a bound that follows from the facts in scope is proved by the elaborator

Working specification for discharging an obligation the automatic fill leaves open when it follows, by linear arithmetic, from the facts in scope: the elaborator finds a certificate and writes it as an ordinary proof, which both checkers recheck. Nothing it adds is trusted, no judgment changes, and a failure is today's refusal with a better report.

It builds on the canonical linear view and the contract of what conversion decides, both published in `curios-core`'s `linear` module by [part 1](../../design/toolchain/one-crate-owns-the-carriers-algebra-and-the-checkers-share-its-strategy.md). [Part 3](03-declared-operations-spec.md)'s operations join its fragment as they are declared. It is independently implementable and retirable.

## What this builds on

- **The fill.** `trivially_inhabited` (`curios-elab/src/elaborate/metavar.rs`) answers a bound whose type reduces to `Bool/True` with its constructor, and nothing else. `insert_auto_argument` (`elaborate/apply.rs`) tries it at insertion, with the arm's refinements live, and writes the inhabitant into the arm. `attempt_discharge` retries a parked bound under the refinements its slot was born under, as re-validation judges a solution, so a guard of the arm the call sits in holds on retry too. A bound whose proposition is itself still a metavariable at insertion is parked like any other and retried once the proposition is known, because `Sort::of_in` (`curios-elab/src/convert/sort.rs`) classifies an unsolved metavariable by its type ([A bound is stated in a decided proposition and discharged by reduction](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)).
- **What a hole may mention.** A metavariable's record freezes the telescope it was born under, and a solution's free variables lie in it (`context/solutions.rs`). The refinement stores (`context/frames.rs`) hold each guard's scrutinee with its case value and each refined variable with its constructor, live only inside their arm.
- **What conversion decides.** The contract in `curios-core`'s `linear` module, beside the `LinearView` it is stated over: a `Nat` or `Int` comparison whose canonical view reduces to a constant is decided in both checkers, over the atom identity that module states, and two comparisons whose views agree once aligned are one proposition at every carrier.
- **The decided propositions.** `Nat/Lt`, `Nat/Le` and their `Int` twins are `Bool/Holds` of a comparison ([A bound is stated in a decided proposition and discharged by reduction](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)). `/std/Nat/Le`, `/std/Nat/Lt` and their `Int` twins carry the order lemmas the standard library composes by hand today, and `Bool/holds_of_eq` turns a decided equation into the decision.
- **The prior art.** Lean discharges an index's bound when `xs[i]` is written, with an extensible tactic that tries `omega` — Pugh's omega test without its dark and grey shadows, reporting a possible counterexample when it fails. Mathlib's `linarith` finds a certificate with an untrusted oracle and checks the combination by normalization. Coq's `lia` checks certificates with reflexive checkers proved correct once (Besson, 2006). Coq's `Program` generates obligations during elaboration, solves what it can and leaves the rest to the user (Sozeau, 2006). The design below is `linarith`'s, with conversion as the normalizer.

## The gap

**A bound that follows from the facts in scope is refused.** `List/get(xs, i)` under `p: Nat/Lt(i, m)` and `q: Nat/Le(m, List/len(xs))` reports `nothing discharged Holds(Nat/lt(i, List/len(@Nat, xs)))`, and so does the same call under a guard `i < m` with only `q` in scope. The author writes the bridge — `Nat/Lt/of_lt_le`, `Nat/Le/trans`, an `Eq/subst` along the equation a hypothesis holds — and passes it with `@`.

**Most of the standard library's hand-written arithmetic is that bridge.** Outside `Nat/Le` and `Nat/Lt`, the calls to their order lemmas are linear consequences of facts in scope, except the few inside `Nat/div_mod`'s two proofs, which multiply by a variable divisor. About half of the library's `Eq/subst` transports carry a linear fact across an equation a hypothesis or a proof field holds, as `/std/Vec`'s `get` does along its `counted` field. A chain ends in an `@` bound, in a contradiction arm eliminated by a zero-arm match, or in a proof that hands its fact to another lemma — the last most often, inside `Str`, `Str/Valid`, `Str/At`, `Char` and `Bytes` — which is why the entry points below reach beyond the `@` bound. Stage 1 retakes the census on the tree the work starts from.

**The escape hatch is the cost.** [A bound is stated in a decided proposition and discharged by reduction](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md) sends a fact the reducer cannot reach to a proof passed as `@`, and rejected "adding a general arithmetic decision procedure first" on the premise that the facts cancellation does not settle "want structural induction rather than a solver". The census contradicts the premise: what cancellation leaves is mostly linear arithmetic over facts already in scope.

## The contract

**What is filled.** A proposition-typed implicit the fill leaves open is filled when it follows from the admitted facts in the supported fragment, within the procedure's cap. The fill is an ordinary term, elaborated as written code is and rechecked by both checkers.

**What is not changed.** No conversion rule, no refinement, no solving choice. The procedure assigns only the hole it was asked about, and only a term whose type is the hole's.

**What failure is.** Today's refusal, naming the facts the procedure considered and, where the search produced one, an assignment of the atoms that satisfies them and falsifies the goal.

**What it claims.** Soundness is the checkers': a wrong certificate is a term that does not check, and the procedure's mistakes are refusals. Completeness is stated per fragment: a consequence of the admitted facts over the rationals is found within the cap, with a strict integer fact strengthened to its successor; a consequence that holds only over the integers and needs a cut is refused, as `linarith` and `omega` without its shadows refuse it. The fill itself stays what [its documentation](../../../curios-elab/src/elaborate/metavar.rs) says it is — not proof search, a unique answer — and this procedure is a separate, fallible step after it, with this contract.

## When it runs

After `trivially_inhabited` answers nothing and the bound waits on no metavariable, at the two points the fill is asked:

- **At insertion**, with the arm's refinements live: guards are facts, and the proof is written into the arm itself.
- **At a parked bound's retry**, under the refinements its metavariable was born under: the arm's guards are facts for a bound born in the arm, and only the telescope's for one born outside it, because re-validation judges the fill under exactly those.

Not at the end of an item, as Lean runs tactic blocks after everything else: an arm's guards are refinements here rather than binders, so a procedure run after the arm has closed would not see them, where Lean's `if h : c` puts the guard in the context. The procedure runs only where the elaborator would otherwise report the bound, so a program accepted today does not reach it; stage 1 measures that on the corpus.

## The facts

- **Hypotheses** in the hole's telescope whose types reduce to a decided comparison, a conjunction or range of comparisons (`Nat/in_range` among them), or `Eq` at `Nat` or `Int`.
- **Proof fields** of struct-typed hypotheses, one level down: `v.counted`, `hi.ok`.
- **Local definitions**, unfolded as the kernel sees them, so a fact stated over a `let` meets a goal stated over its value.
- **Guards**: the arm's at insertion, and at a retry the ones the hole was born under, which is what re-validation judges its solution by. A comparison scrutinee with its case value — an `==` scrutinee's true arm as the equation it gives, a range check's true arm through `Le/of_in_range` — and a refined variable with the constructor it was refined to, which every statement read reduced sees.
- **The negated goal**, for the refuting form below.

Each fact is read through part 1's view. A fact the view cannot read is not admitted, and the report says which facts were left out.

## The fragment

- Linear `Nat` and `Int` arithmetic with literal coefficients over part 1's atoms, each `Nat` atom non-negative and `Nat/to_int` read as the embedding.
- Truncated subtraction by the case split `omega` makes — `b <= a`, where `b + (a - b) = a`, and `a < b`, where `a - b <= 0` — opened only where the search without it finds an assignment, and read as facts where a guard or conversion already decides the case.
- `/` and `%` by a literal at `Nat`, through the quotient's bounds `k * (x / k) <= x < k * (x / k) + k`. Conversion reads `k * (x / k) + x % k` as `x` wherever one side holds both, so Euclid's equation is no fact, and a sum holding a remainder beside its quotient's multiple is not the sum of the views: every fact over `x % k` is raised by the multiple on both sides, which conversion reads as one over `x`, and a goal over a remainder is proved by refuting its negation, raised the same way. At `Int`, conversion decides Euclid's identity and neither of a remainder's bounds, so a quotient by a literal is no fact there.
- [Part 3](03-declared-operations-spec.md)'s `min`, `max`, `abs` and `sign`, by the case split their definitions state, as each is declared.
- In stage 6, products of two facts for a variable multiplier — what Mathlib's `nlinarith` adds, and what `Nat/div_mod`'s two proofs need.
- Outside: induction; a recursive function's value beyond the atom it is; and rewriting under a function by an equation, as `pending(r, …)` against `pending(r′, …)` with `r = r′` asks, which is congruence rather than arithmetic.

## The search

Fourier–Motzkin elimination or the simplex method over the rationals finds non-negative multipliers for the admitted facts and the negated goal whose combination is a false constant. The search is deterministic, so the same program finds the same certificate on every machine, and it is capped by a count of its own, stated in the theory's units and never the host's. Exhausting the cap refuses, as the reduction budget does, and never selects anything. The search lives outside the certifier's dependency closure, on the elaborator's side, and reads facts only through Core's views.

## The proof

**A certificate is a sum of facts.** Each fact enters as a `Nat/Le` or `Int/Le`:

- a strict fact as its successor, through the seam part 1 reads alike at every carrier;
- the false arm of a comparison through its dual;
- an equation as the two `Le`s it gives;
- a fact scaled by a literal through `Le/mul_mono_r`.

One lemma, `Le/add`, adds two of them. Conversion's cancellation does the rest: a combination whose sides differ by a constant reduces to `Bool/True` or `Bool/False`, which is part 1's published contract.

**The direct form.** When the combination's view is the goal's plus a constant `s >= 0`, the sum with the literal fact `0 <= s` is the proof. `Nat/Le/trans` is one `Le/add`, because `a + b <= b + c` cancels to `a <= c`, a bound fed from `Nat/Lt(i, m)` and `Nat/Le(m, len)` is one `Le/add` of the strict fact's successor with the second, and `a <= c + 5` from `a <= b` and `b <= c` is that sum with `0 <= 5`.

**The refuting form.** Otherwise the proof splits on the goal's own comparison, spelled as the refinement it will be keyed on. The true arm is `Bool/True/qed()`. The refuting arm eliminates, with a zero-arm match, the sum of the facts with the negated goal, whose type reduces to `Bool/False` — `a <= b` from `2 * a <= 2 * b` scales the negated goal `b + 1 <= a` by two and leaves `2 <= 0`. Both forms check against the compiler this campaign starts from, with `Le/add` written from `Nat/Le`'s `trans` alone, whose cancellation does what `add_mono_l` would.

**A case split** over a truncated subtraction is written as the match on its guard, `b <= a`, each arm's proof in either form over the facts that case adds; nested, one level per split the certificate opened.

**Vocabulary.** The names the procedure writes — `Le/add`, the scaling and bridge lemmas, `Bool/holds_of_eq`, `Eq/refl`, and the entry points below — are `SyntaxRegistry` slots beside `ProofSyntax`'s, filled by `curios-prelude-archive`. Inside `/std`, the procedure stays off in an item compiled before its vocabulary is elaborated, which gets today's behavior; the scheduler gains no edge, unlike a derived witness body, whose vocabulary it must order before the witness.

**Reflection** — a checker written in Curios and proved sound once, each proof then its application to a closed certificate with `Bool/True/qed()` for the check — is deferred. It trades a one-time proof for smaller terms, and Chaieb and Nipkow measured it one to two orders of magnitude faster in Isabelle (2008). A proof size or check time measured in stage 7's sweep is what reopens it.

## The entry points

- **An omitted `@` bound**, as today.
- **A tautology conversion decides.** `Bool/holds_of_eq(e, Eq/refl())` where `e` against `true` holds by conversion's probe-side decisions but not by reduction — `b || Bool/not(b)`. [A law is decided where it neither respells nor invents](../../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md) rejected reducing a tautology to `true` because a truth table in the fold costs a whole-tree walk at every reduction; asked once where a bound was left open, it costs one query.
- **A contradiction arm.** A `/std` producer whose one bound is `Bool/False`, eliminated by the arm's own zero-arm match. [A contradiction is eliminated by its match, not a library function](../../design/language/a-contradiction-is-eliminated-by-its-match-not-a-library-function.md) keeps elimination the match's, and a producer is not an eliminator. Its proposition is fixed, so it is filled at insertion, where the guards are live.
- **A stated proposition.** `proved(@P: Prop, @p: P) -> P`, for a fact a term needs that no bound demands. The proposition is written or pinned by the expectation; when it is pinned later, the bound is parked and retried with the telescope's facts.

## Reports

A refused bound's report lists the facts the procedure considered, each with where it came from — a hypothesis, a field, a guard — the facts it could not read, and, when the search produced one, an assignment of the atoms satisfying the facts and falsifying the goal. A written goal `?` over a bound shows the same.

## Settled decisions

- **The search** is integer Fourier–Motzkin behind one interface, from the admitted facts to non-negative multipliers or a counterexample: rows scaled to integers and divided by their gcd, the multipliers tracked per derived row, the counterexample read by back-substitution. Its cap is a count of derived rows. Mathlib's `linarith` defaults to a simplex oracle and keeps Fourier–Motzkin as the one "sometimes faster on small states" that "cannot handle large problems" ([`Mathlib.Tactic.Linarith.Frontend`](https://florisvandoorn.com/carleson/docs/Mathlib/Tactic/Linarith/Frontend.html)). The census finds small states. Stage 6 measures the product rows, and a row that exhausts the cap replaces the engine with a Bland's-rule simplex over integer tableaus behind the same interface.
- **The procedure** is `curios-elab`'s `entailment` module: `facts`, `search`, `proof` and `report`. No crate of its own, since nothing else consumes it yet.
- **The vocabulary** is a `SyntaxRegistry` group, `EntailmentSyntax`, beside `ProofSyntax`. A proof form is written only where every name it applies is assumed in the context, which is how an item of `/std` compiled before its vocabulary keeps today's behavior.
- **The names.** Sums are `Le/add` at `/std/Nat/Le` and `/std/Int/Le`: `a <= b` and `c <= d` give `a + c <= b + d`. The `False` producer is `/std/Bool/False/refuted`, in a `Bool/False` module that mirrors `Bool/True`. `proved` is top-level in `/std`, beside `print`. A further lemma the procedure writes is housed under the proposition it concludes: `Le/of_eq` for an equation, `Lt/of_not_le` for a `<=` guard's false arm, `Le/sub_zero` for a truncated subtraction's truncating case — `a <= b` gives `a - b <= 0` — and `Le/mul` for stage 6's products — `a <= b` and `c <= d` give `a * d + b * c <= a * c + b * d`.

## Cost

Prelude elaboration and certification are read from the two profile streams `cargo x clippy` writes, since it builds with `--all-features`: `curios-prelude-archive/.artifacts/profile.tsv` for elaboration and erasure, and `curios-prelude/.artifacts/profile.tsv` for certification. Each is folded with `cargo run --all-features --package curios -- profile <stream>`. The build scripts are instrumented debug builds: a duration is noisy and inflated, while call counts and allocated megabytes are the stable figures. A stage's record names the span and the resource it compares.

| After | Elaboration allocated, `elaborate_and_zonk_with_prelude` | Certification allocated, `recheck_module` | `entailment::entail` calls in elaboration |
| --- | --- | --- | --- |
| The baseline, `bb28989d` | 34 123.3 MB | 5 003.0 MB | — |
| Stage 2 | 34 111.8 MB | 5 002.4 MB | 0 |
| Stage 3 | 34 169.3 MB | 5 007.2 MB | 0 |
| The rebased base, `4976ac03` | 61 795.5 MB | — | — |
| Stage 4 | 61 893.6 MB | 5 011.4 MB | 0 |
| Stage 5 | 61 911.4 MB | 5 012.0 MB | 0 |

Stage 2 moves neither figure beyond noise. The procedure is never asked while the prelude elaborates, as the premise count said it would not be. Stage 3's growth is the six lemmas it adds to `/std` — `Le/add`, `Le/of_eq` and `Lt/of_not_le` at each carrier — elaborated and certified, and not the procedure, which is still never asked. The branch was then rebased onto `main` at `4976ac03`, whose own prelude elaboration allocates 81% more than `bb28989d` did. That rise is `main`'s, measured on its own stream, and stage 4 is compared against it: its `Bool/False` module and `proved` add 98 MB.

## Census

Taken at `bb28989d` over `curios-prelude-archive/std`, outside `Nat/Le`, `Nat/Lt`, `Int/Le` and `Int/Lt`. A chain is one bridge expression at one site, however many lemmas it nests. `Le/refl` (15 calls) is not a bridge, since conversion decides `x <= x`, and neither are the `try` decision procedures (23).

| Endpoint | Chains | Where | Stage that covers them |
| --- | --- | --- | --- |
| An `@` bound | 19 | `Char` 4, `Tui/input` 3, the three `drop`s of `List`, `Bits` and `Bytes`, `Handle` 2, `Str/At` 2, `Str`, `Int`, `Flt`, `Toml/build`, `http/Url` | 3; the `drop`s and three of `Char`'s at 5 |
| A contradiction arm | 22 | `Str/Valid` 13, `Str` 7, `Nat/div_mod` 2 | 4; `Str/Valid`'s at 5, `Nat/div_mod`'s at 6 |
| A fact handed on — an explicit argument, a field, a `let`, a body | 34 | `Str` 14, `Str/At` 10, `Char` 4, `List` 3, `Nat/div_mod`, `WellFounded`, `Char/Valid` | 4, through `proved`; truncated subtraction's at 5 |
| An equation goal, by `antisym` | 3 | `Bytes`, `Str`, `Int` | none: an equation is not a bound |

Of the 45 `Eq/subst` transports, 19 carry a linear fact across an equation a hypothesis, a field or an `==` guard holds: `Str/Valid` 12, the three `drop`s, `Bytes` 2, `Vec` and `Str/At` 1 each. The other 26 rewrite an equation goal or rewrite under a function, which is congruence.

**The shapes.** Every fact is linear over atoms once truncated subtraction and division by a literal are read, except `Nat/div_mod`'s two products. Facts come from hypotheses, including those a constructor pattern binds (`some(inside)`); from proof fields one level down of hypotheses and of `let`- and pattern-bound structures (`have.progress`, `marker.room`, `range.low`); from guards; and from refined variables (`match k | j + 1`), with a slack the direct form closes by a literal fact. Four readings the list under *The facts* implies are named here because the census meets them:

- a guard whose scrutinee unfolds to a comparison or a range check, as `is_upper(c)` does, gives its fact through the unfolding;
- a range check, as a hypothesis or as a guard's true arm, gives its two bounds through `Le/of_in_range`;
- an `==` guard's true arm gives an equation, which is how `Str/Valid` reads a quotient that its guard pinned;
- a proof field two levels down, `first.past.within` in `Str`'s `occurrence`, is not read, so that site keeps its chain.

A range check's false arm is a disjunction and is outside the fragment: `Str/Valid`'s `step_of_not_continuing` keeps its bridge.

**The procedure runs on nothing the corpus accepts.** The two points it will run at were counted over the prelude's elaboration and over every program under `programs/`. The first is insertion, where the fill answered nothing for a proposition that waits on no metavariable. The second is a parked bound's retry, once the bound waits on nothing. Oracles were counted apart from ordinary elaboration. No count fired anywhere, and a control program with a refused bound fired the insertion count once.

**To retake the premise count.** The measurement adds and then removes temporary instrumentation:

1. In `insert_auto_argument`, after `waiting` is computed, put `curios_profile::sample!("premise::insertion", 1)` under `proposition && !waiting`. In `attempt_discharge`, put `curios_profile::sample!("premise::retry", 1)` after the `waits_on_metavariable` return. Name each `…_in_oracle` instead where `Context::parking_suppressed()` answers `true`.
2. Run `cargo x clippy` and grep `curios-prelude-archive/.artifacts/profile.tsv` for `premise::`. A sample name is declared in the stream on its first event, so no match means no event.
3. Build with `cargo build --all-features --package curios` and run `target/debug/curios --profile <file> wonder diagnostics <program>` for each program, grepping the same way.
4. Run the spec's first row on standard input as the control.

**To retake the census.** Search `curios-prelude-archive/std` with `rg -n "\b(Nat/|Int/)?(Le|Lt)/[a-z_]+\b" -g '*.crs' -g '!Nat/Le.crs' -g '!Nat/Lt.crs' -g '!Int/Le.crs' -g '!Int/Lt.crs'` and with `rg -n "Eq/subst\("`. Read each site and classify it by its endpoint, the bridge's outermost consumer, and by the shape of every fact the chain reads.

## Stages

Each lands alone, with its rows stated first.

1. **The census and the grid.** Retake the census on the tree the work starts from. State each shape it found as a row the procedure must fill, at `Nat` and `Int`, and each non-target as a control it must refuse. Measure that the procedure runs on nothing the corpus accepts.
2. **Tautologies conversion decides.** The `holds_of_eq` entry, with the plumbing every later stage uses: the hook at both points and the vocabulary slots.
3. **Linear facts, direct sums.** Hypotheses, proof fields, local definitions and guards at insertion; the direct form; omitted `@` bounds; and the report, which from here on has facts to name.
4. **Refutations and the other entry points.** The refuting form, the `False` producer and `proved`.
5. **Defined operations.** Truncated subtraction, `/` and `%` by a literal, and part 3's operations as each lands.
6. **Products.** Pairwise products of facts for a variable multiplier.
7. **The standard library's sweep.** Replace the chains the procedure covers, keep the lemmas it writes and those callers still use for themselves, remeasure the census, and record proof sizes and check times.

## Verification

- **Its own grid**, in the manner of `curios`'s `tests::laws`: rows the procedure must fill, at `Nat` and `Int`, and controls it must refuse — a nonlinear consequence before stage 6, a fact out of scope, a guard's fact at a retry the hole may travel from, and an exhausted cap.
- **Both checkers** recheck every filled row. A certificate corrupted one multiplier at a time fails to check, each corruption refused.
- **Determinism.** The same program files the same proof on two runs and two platforms.
- **Mutation.** Removing one admitted fact from a filled row refuses it; removing the procedure refuses every row.
- **Cost.** Prelude elaboration and certification are measured before and after each stage, naming the stage and the resource.

## Documentation and design record

- A new design decision: an obligation that follows from the facts in scope is proved by the elaborator, with the rejections below.
- [A bound is stated in a decided proposition and discharged by reduction](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md): the escape hatch, the section on the refinement store as a lookup rather than a theory — hypotheses now reach a bound through a proof rather than through the store — and the rejection of a general arithmetic decision procedure, whose premise the census contradicts.
- `trivially_inhabited`'s documentation: the fill remains a unique answer, and the procedure beside it has its own contract.
- [A contradiction is eliminated by its match, not a library function](../../design/language/a-contradiction-is-eliminated-by-its-match-not-a-library-function.md): a producer of `Bool/False` is not an eliminator.
- `syntax.md`'s account of bounds and of the `/` and `%` preconditions, and `/std/Nat/Le`, `/std/Nat/Lt` and their `Int` twins' documentation for the vocabulary.
- No perimeter entry: nothing is trusted that was not, and the design decision says so.

## Rejected

- **Hypotheses in conversion.** The Calculus of Congruent Inductive Constructions sends "the set of user hypotheses available from the current context" to its decision procedure inside conversion (Blanqui, Jouannaud and Strub, 2007). Its metatheory is established for the weak recursor — "Including strong elimination rules invalidates this argument" — and with strong elimination "it is necessary to block the congruence below the strong recursor in order to avoid lifting an incoherence from the object level to the predicate level". `Bool/Holds` is a large elimination, so the block would sit under every decided bound; without it, a context whose hypotheses contradict can type terms that do not normalize. A mistake there is a false definitional equation; a mistake here is a term that does not check.
- **Reflection first.** A one-time proof for smaller terms, before any measurement says terms are too large.
- **An external solver.** Linear arithmetic needs no SMT solver, and a dependency outside the workspace for it would buy nothing the search does not do.
- **A tactic syntax.** [Syntax forms are closed](../../design/language/syntax-forms-are-closed-semantics-extend-by-witness.md); the entry points are `/std` declarations with `@` bounds.
- **Running last.** It loses the guards, which are refinements rather than binders.
- **A heuristic search.** Acceptance would then depend on the heuristic. The procedure is complete for its fragment within its cap, and its cap is deterministic.

## Completion criteria

- An omitted bound that follows from the admitted facts in the fragment is filled at insertion and at retry, and the fill is rechecked by both checkers.
- The tautology entry, the `False` producer and `proved` reach the same procedure.
- The fragment includes truncated subtraction, `/` and `%` by a literal, part 3's operations as declared, and pairwise products.
- A refusal names the facts considered and, where one exists, a counterexample.
- The procedure's grid, determinism, mutation and cost records are in place, and the standard library's covered chains are gone.

## Retirement

Move the contract to `curios-elab`'s documentation, the decision and its rejected alternatives to a design decision, the procedure's grid to the test suite beside the law grid, and the vocabulary's contracts to the `/std` modules that hold it. Replace the roadmap entry with a checked summary, update the dependents — [the numeric laws](../numeric-laws-spec.md) among them — to the permanent record, verify that nothing references this filename, and delete it.
