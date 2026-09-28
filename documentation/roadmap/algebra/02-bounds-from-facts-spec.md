# Algebra, part 2: a bound that follows from the facts in scope is proved by the elaborator

Working specification for discharging an obligation the automatic fill leaves open when it follows, by linear arithmetic, from the facts in scope: the elaborator finds a certificate and writes it as an ordinary proof, which both checkers recheck. Nothing it adds is trusted, no judgment changes, and a failure is today's refusal with a better report.

It builds on [part 1](01-one-owner-spec.md)'s published view and contract and may begin once part 1's third stage lands. [Part 3](03-declared-operations-spec.md)'s operations join its fragment as they are declared. It is independently implementable and retirable.

## What this builds on

- **The fill.** `trivially_inhabited` (`curios-elab/src/elaborate/metavar.rs`) answers a bound whose type reduces to `Bool/True` with its constructor, and nothing else. `insert_auto_argument` (`elaborate/apply.rs`) tries it at insertion, with the arm's refinements live, and writes the inhabitant into the arm. `attempt_discharge` retries a parked bound with every refinement withheld, because its fill is a metavariable solution that may travel past the arm it was minted in. A bound whose proposition is itself still a metavariable at insertion is parked like any other and retried once the proposition is known, because `Sort::of_in` (`curios-elab/src/convert/sort.rs`) classifies an unsolved metavariable by its type ([A bound is stated in a decided proposition and discharged by reduction](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)).
- **What a hole may mention.** A metavariable's record freezes the telescope it was born under, and a solution's free variables lie in it (`context/solutions.rs`). The refinement stores (`context/frames.rs`) hold each guard's scrutinee with its case value and each refined variable with its constructor, live only inside their arm.
- **What conversion decides.** Part 1's published contract, in `curios-core`'s `linear` module beside the `LinearView` it is stated over: a `Nat` or `Int` comparison whose canonical view reduces to a constant is decided in both checkers, over the atom identity part 1 states, with the `<`/`<=` seam read alike at every carrier.
- **The decided propositions.** `Nat/Lt`, `Nat/Le` and their `Int` twins are `Bool/Holds` of a comparison ([A bound is stated in a decided proposition and discharged by reduction](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)). `/std/Nat/Le`, `/std/Nat/Lt` and their `Int` twins carry the order lemmas the standard library composes by hand today, and `Bool/holds_of_eq` turns a decided equation into the decision.
- **The prior art.** Lean discharges an index's bound when `xs[i]` is written, with an extensible tactic that tries `omega` — Pugh's omega test without its dark and grey shadows, reporting a possible counterexample when it fails. Mathlib's `linarith` finds a certificate with an untrusted oracle and checks the combination by normalization. Coq's `lia` checks certificates with reflexive checkers proved correct once (Besson, 2006). Coq's `Program` generates obligations during elaboration, solves what it can and leaves the rest to the user (Sozeau, 2006). The design below is `linarith`'s, with conversion as the normalizer.

## The gap

**A bound that follows from the facts in scope is refused.** `List/get(xs, i)` under `p: Nat/Lt(i, m)` and `q: Nat/Le(m, List/len(xs))` reports `nothing discharged Holds(Nat/lt(i, List/len(@Nat, xs)))`, and so does the same call under a guard `i < m` with only `q` in scope. The author writes the bridge — `Nat/Lt/lt_of_lt_le`, `Nat/Le/trans`, an `Eq/subst` along the equation a hypothesis holds — and passes it with `@`.

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
- **At a parked bound's retry**, with every refinement withheld: only the telescope's facts, because the fill is a solution that may travel past the arm.

Not at the end of an item, as Lean runs tactic blocks after everything else: an arm's guards are refinements here rather than binders, so a procedure run after the arm has closed would not see them, where Lean's `if h : c` puts the guard in the context. The procedure runs only where the elaborator would otherwise report the bound, so a program accepted today does not reach it; stage 1 measures that on the corpus.

## The facts

- **Hypotheses** in the hole's telescope whose types reduce to a decided comparison, a conjunction or range of comparisons (`Nat/in_range` among them), or `Eq` at `Nat` or `Int`.
- **Proof fields** of struct-typed hypotheses, one level down: `v.counted`, `hi.ok`.
- **Local definitions**, unfolded as the kernel sees them, so a fact stated over a `let` meets a goal stated over its value.
- **Guards**, at insertion only: a comparison scrutinee with its case value, and a refined variable with the constructor it was refined to.
- **The negated goal**, for the refuting form below.

Each fact is read through part 1's view. A fact the view cannot read is not admitted, and the report says which facts were left out.

## The fragment

- Linear `Nat` and `Int` arithmetic with literal coefficients over part 1's atoms, each `Nat` atom non-negative and `Nat/to_int` read as the embedding.
- Truncated subtraction by the case split `omega` makes, and `/` and `%` by a literal through the quotient and remainder they denote, whose defining equation and bound conversion already decides.
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

One lemma, `add_le_add`, adds two of them. Conversion's cancellation does the rest: a combination whose sides differ by a constant reduces to `Bool/True` or `Bool/False`, which is part 1's published contract.

**The direct form.** When the combination's view is the goal's, the sum is the proof. `Nat/Le/trans` is one `add_le_add`, because `a + b <= b + c` cancels to `a <= c`, and a bound fed from `Nat/Lt(i, m)` and `Nat/Le(m, len)` is one `add_le_add` of the strict fact's successor with the second.

**The refuting form.** Otherwise the proof splits on the goal's own comparison, spelled as the refinement it will be keyed on. The true arm is `Bool/True/qed()`. The refuting arm eliminates, with a zero-arm match, the sum of the facts with the negated goal, whose type reduces to `Bool/False` — `a <= c + 5` from `a <= b` and `b <= c` leaves `6 <= 0`. Both forms check against the compiler this campaign starts from, with `add_le_add` written from `Nat/Le`'s `trans` and `add_mono_l`.

**Vocabulary.** The names the procedure writes — `add_le_add`, the scaling and bridge lemmas, `Bool/holds_of_eq`, `Eq/refl`, and the entry points below — are `SyntaxRegistry` slots beside `ProofSyntax`'s, filled by `curios-prelude-archive`. Inside `/std`, the procedure stays off in an item compiled before its vocabulary is elaborated, which gets today's behavior; the scheduler gains no edge, unlike a derived witness body, whose vocabulary it must order before the witness.

**Reflection** — a checker written in Curios and proved sound once, each proof then its application to a closed certificate with `Bool/True/qed()` for the check — is deferred. It trades a one-time proof for smaller terms, and Chaieb and Nipkow measured it one to two orders of magnitude faster in Isabelle (2008). A proof size or check time measured in stage 7's sweep is what reopens it.

## The entry points

- **An omitted `@` bound**, as today.
- **A tautology conversion decides.** `Bool/holds_of_eq(e, Eq/refl())` where `e` against `true` holds by conversion's probe-side decisions but not by reduction — `b || Bool/not(b)`. [A law is decided where it neither respells nor invents](../../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md) rejected reducing a tautology to `true` because a truth table in the fold costs a whole-tree walk at every reduction; asked once where a bound was left open, it costs one query.
- **A contradiction arm.** A `/std` producer whose one bound is `Bool/False`, eliminated by the arm's own zero-arm match. [A contradiction is eliminated by its match, not a library function](../../design/language/a-contradiction-is-eliminated-by-its-match-not-a-library-function.md) keeps elimination the match's, and a producer is not an eliminator. Its proposition is fixed, so it is filled at insertion, where the guards are live.
- **A stated proposition.** `proved(@P: Prop, @p: P) -> P`, for a fact a term needs that no bound demands. The proposition is written or pinned by the expectation; when it is pinned later, the bound is parked and retried with the telescope's facts.

## Reports

A refused bound's report lists the facts the procedure considered, each with where it came from — a hypothesis, a field, a guard — the facts it could not read, and, when the search produced one, an assignment of the atoms satisfying the facts and falsifying the goal. A written goal `?` over a bound shows the same.

## Stages

Each lands alone, with its rows stated first.

1. **The census and the grid.** Retake the census on the tree the work starts from. State each shape it found as a row the procedure must fill, at `Nat` and `Int`, and each non-target as a control it must refuse. Measure that the procedure runs on nothing the corpus accepts.
2. **Tautologies conversion decides.** The `holds_of_eq` entry, with the plumbing every later stage uses: the hook at both points, the vocabulary slots, the report.
3. **Linear facts, direct sums.** Hypotheses, proof fields, local definitions and guards at insertion; the direct form; omitted `@` bounds.
4. **Refutations and the other entry points.** The refuting form, the `False` producer and `proved`.
5. **Defined operations.** Truncated subtraction, `/` and `%` by a literal, and part 3's operations as each lands.
6. **Products.** Pairwise products of facts for a variable multiplier.
7. **The standard library's sweep.** Replace the chains the procedure covers, keep the lemmas it writes and those callers still use for themselves, remeasure the census, and record proof sizes and check times.

## Verification

- **Its own grid**, in the manner of `curios/src/tests/laws.rs`: rows the procedure must fill, at `Nat` and `Int`, and controls it must refuse — a nonlinear consequence before stage 6, a fact out of scope, a guard's fact at a retry the hole may travel from, and an exhausted cap.
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
