# A proposition is sized as a type is, and its proof is never read

Working specification for the smaller `Prop`: a proposition takes a universe level and is sized exactly as a type is, it reaches data only where it is empty or where its indices pin every payload of its one constructor, and a match that takes it there fires without reading the proof. It closes the open ticket in [the board's totality part](../../../xboard/src/board/totality.rs), and it replaces the reservation this directory held for K-like reduction.

It amends [`Prop` is strict, proof-irrelevant, and definitionally K](../../design/theory/prop-is-strict-proof-irrelevant-and-definitionally-k.md), whose strictness, irrelevance and K all stand. What an arm learns of its scrutinee is untouched: [case equations](../../design/soundness/case-equations-and-their-key.md) stay as they are.

## Where it stands

| Part | State | Where it is written |
| --- | --- | --- |
| `Prop` is one sort with no level, and a function type into it is a proposition whatever it quantifies over | To change | `Subterm::Prop` (`curios-core/src/term/subterm.rs`), `func_sort` (`curios-cert/src/kernel/sort.rs`) |
| A `Prop`-sorted family, and a `Prop`-sorted payload of any family, carry no size condition | To change | `check_signature` (`curios-cert/src/kernel/module.rs`), the telescope's bound in `curios-elab/src/elaborate/module.rs` |
| A proposition eliminates into data where it is empty or has one constructor whose every payload is a proof or pinned by the indices | To change | `guard_large_elimination` (`curios-cert/src/kernel/infer/eliminate.rs`), `singleton_eliminable` (`curios-elab/src/elaborate/match_.rs`) |
| That elimination reduces by forcing the proof to its constructor | To change | the `Match` arm of `whnf` (`curios-cert/src/kernel/whnf.rs`), `reduce_match` (`curios-elab/src/reduce.rs`), the machine's `Cases::Induct` step (`curios-core/src/machine.rs`) |
| An application of a constructor's payload is graded a child of the constructor | Stands, on an argument to restate | `curios-analysis/src/totality.rs` |
| `/std` recurses on an accessibility proof where a measure descends | To change | `curios-std/src/WellFounded.crs` and its callers |

What `/std` uses of all this, counted over `/sys` and `/std` by the protocol under *Verification*:

- A proposition is eliminated into data by 12 typings: one of `Eq`, in the definition of `/std/Eq/subst`, and eleven of `Accessible`, in `/std/WellFounded/recurse` and in the `Nat/Gcd` and `Nat/Coprime` lemmas whose terms hold it unfolded.
- Every one of the 77 calls of `Eq/subst` outside the test units has a motive that returns a proposition.
- A function type into `Prop` ranges over a domain above level zero at 660 typings, and in every one the domain is a type variable or a value of one: a level types each of them where it stands.

## The gap

**A closed proof of `False`.** `induct Bad: Prop | c(f: (A: Prop) -> (A) -> A) end` holds a payload that quantifies over the sort `Bad` is in, since nothing sizes a proposition, and the payload's type names `Bad` nowhere, so strict positivity has nothing to refuse. The size order reads `f(Bad)(b)` as a child of `c(f)`, on the premise that a payload of a foreign type cannot hand back a value of the family; at the identity it hands back `b`, and `let bad(b: Bad) -> False = match b | c(f) => bad(f(Bad)(b)) end` is judged to descend. The same declaration in `Type` is refused by its levels.

**A checker reduces proofs.** Eliminating `Accessible` or `Eq` into data forces the proof, so a proof body is never opaque ([A declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md)), the evaluator a type runs still demands a proof where an elimination reads one ([The closed machine binds a proof unreduced](../../design/compilation/the-closed-machine-binds-a-proof-unreduced.md)), and with an impredicative `Prop` a cast between two propositions can unfold without end, which the budget refuses.

**The algebra and a cast disagree.** `Eq/subst(P, p, v)` along a hypothesis `p: Eq()(a + b, b + a)` is stuck, where conversion already holds `P(a + b)` and `P(b + a)` one type: the cast is the identity to every judgment but reduction.

## Prior art

- **A levelled, predicative, irrelevant `Prop`** is Agda's `Prop ℓ`: `Prop ℓ : Set (lsuc ℓ)`, a datatype in `Prop ℓ` takes constructor arguments at or below `ℓ`, and a proposition is matched into `Set` only by the absurd pattern ([Agda, *Prop*](https://agda.readthedocs.io/en/stable/language/prop.html)). Its documentation recommends stating a proposition as a recursive function, which is what `/sys/Bool/Holds` is.
- **Structural recursion and impredicativity are incompatible** where a payload's application counts as a descent: Agda 2.6.4 stopped letting a constructor store a value larger than its own sort, forced by an index or not, for that reason ([changelog](https://hackage.haskell.org/package/Agda-2.6.4/changelog)).
- **Which propositions may reach data** is Gilbert, Cockx, Sozeau and Tabareau's question (POPL 2019): every argument the indices do not force is a proof, the constructors' result types are orthogonal, and recursion is syntactically guarded. `Acc` fails the last, since irrelevance makes its unfolding hold for any proof and an open term unfolds without end.
- **A pinned singleton that fires where its indices convert** is Rocq's `Definitional UIP`: an `SProp` inductive with one constructor taking no non-parameter argument is not squashed, and a match on it reduces where the motive at the scrutinee's indices converts with the motive at the constructor's, reduction depending on conversion ([Rocq reference manual, *SProp*](https://rocq-prover.org/doc/v9.3/refman/addendum/sprop.html)). It documents non-termination with an impredicative *relevant* sort, which Curios does not have.
- **Abel and Coquand** (LMCS 16(2:14), 2020) lose normalization from three ingredients together: an impredicative `Prop`, an equation between two propositions, and a cast that fires without inspecting its proof. Their term is a proof, and they leave open whether a predicative theory, or one that never reduces an embedded proof, keeps normalization.
- **Accessibility in a strict universe** with a definitional unfolding (Felicissimo, Leray, Pujet, Tabareau, Tanter and Winterhalter, LICS 2026) is consistent and canonical on closed terms, and its checker may diverge on open ones.

## Decisions

- **`Prop u : Type (u + 1)`, written `Prop` and inferred as `Type` is.** `Prop u ≤ Prop v` and `Prop u ≤ Type v` where `u ≤ v`. A function type into `Prop v` sits at the join of its domains' levels and `v`, a `Prop`-sorted domain counting at its own level. `Holds`, `True` and `False` sit at level zero, and `Eq(@A)` at `A`'s.
- **A proposition is sized as a type is, with no exemption.** A declaration at `Prop u` bounds every payload and field at `u` — a proof at the level of the proposition it proves, and a payload an index pins like any other — and its parameters one rung higher.
- **A proposition reaches data two ways**: by the zero-arm match on an empty one, and by a match on one whose single constructor has every payload pinned by its index targets. A constructor that holds a proof no index pins is taken apart only into a proposition.
- **That match fires without reading its proof.** Its arm names no payload, each pinned binder written as the index that pins it. A judgment's reduction types the scrutinee by lookup, unifies the indices of that type with the constructor's targets, and puts each position inversion leaves open to its checker's conversion: where every one holds the match is its arm's body, and otherwise it is stuck. Plain reduction asks nothing and fires where inversion alone settles every position. The closed machine takes a one-arm, default-free match whose body names no binder for that body, its scrutinee unevaluated.
- **`Prop` stays below `Type`**, so one `Eq/subst` serves both a proposition and a type as its motive.
- **A measure descends on a bound.** `/std/Nat/descend(measure, P, step, s)` recurses structurally on a number the measure fits under, the step handed a hypothesis that may recur at any state whose measure is below, and `WellFounded` leaves the library with its last consumer.
- **The descent rule stands.** With every payload sized, a payload's application is below its constructor in rank, which is the premise the rule states.

## Open questions

- **What the elaborator refuses the ticket's programs by.** The kernel's refusal is `Error::Oversized`; the elaborator's is read off `cargo xboard` once the bound is recorded.
- **What keeps the subject of `a_proof_is_not_reduced_to_compare_it_with_another`.** Its fixture is Abel and Coquand's term, which no longer types. A predicative program on which a checker that reduces proofs first runs out of steps keeps the test; if none is found the test goes with the hazard.
- **A scrutinee no lookup types.** A β-redex or a `let` is stepped until one does; whether any other shape is met is counted over `/std` and the corpus before the old path is removed.
- **Hand-built fixtures whose arm names a pinned payload.** The kernel refuses one once stage 4 lands, so each such fixture is respelled or becomes the refusal's test.

## Stages

Each is one commit on its own check, and none waits for a later one.

1. **`Prop` takes a level.** The `Prop` node carries a `Level` through every walk that reads one; a written `Prop` mints a level as a written `Type` does; both checkers' sorts, their function-type rule, subsumption and the sizing of a declaration take the level; the stored unit's schema tag moves. The ticket turns fixed in this commit, each witness naming its refusal. Check: `cargo xboard`; the suites of `curios-core`, `curios-analysis`, `curios-cert` and `curios-elab`; `curios`'s `tests::laws`.
2. **`/std` recurses on a bound.** `Nat/descend` lands and every caller of `WellFounded/recurse` moves to it; `Str/At`'s `forward` and `backward` are stated over a measure, their accessibility proofs deleted; `WellFounded.crs` is deleted. Check: `cargo xtask clippy`, `cargo xtask test curios-std`, and `descend`'s reported type read back.
3. **A proposition reaches data only where empty or wholly pinned.** One classifier in `curios-analysis` beside `pinned_by_targets`, called by both guards; the clause admitting a proof payload goes, and erasure's branch for an unpinned payload with it. Check: the elimination, erasure and board suites.
4. **The match fires without reading its proof.** The elaborator writes the arm free of its binders and the kernel's guard refuses one that is not; the rule enters both reducers through one function of `curios-analysis` and the closed machine by its own; nothing forces a proposition's proof any longer. Check: rows for a cast between algebra-equal ends, a cast along a hypothesis whose ends differ and a closed cast along a lemma, put to each checker alone; `kernel_memo_parity`; the respelling grid.

## Verification

- **The board.** `cargo xboard` shows the ticket `HOLDS` and none regressed. The two neighbouring routes — a large payload pinned by an index, and a proof payload of a data family reached through a squash — are tests of `curios-cert`, each refused.
- **The census**, `counted`, taken at `dae89d7ca` and retaken after stage 4. In a scratch copy of the tree, three places of `curios-cert` append a line to a file named by an environment variable, each with the item the walk is judging (`item.describe()`, set where `recheck` enters an item): `func_sort`, where it answers `Prop` and a domain's level is not zero, with the levels; `guard_large_elimination`, where it admits an empty or a singleton family, with the family's name; and `curios-prelude`'s build, with each root's item count in place of its panic. Building `curios-prelude` then certifies `/sys` and `/std` once. After stage 4 the second kind of line names only families whose every payload is pinned, and the first kind is gone, a function type into `Prop` being sized by its domains. The motives of `Eq/subst` are read off the sources: the first argument of each call under `curios-std/src` outside `tests/`, by what its body returns.
- **Figures the change may move**, named and not retaken here: the budget `/std` consumes in elaboration and in certification, `curios-prelude-archive`'s `universe_parameter_census`, and the least budgets `curios/src/tests/reduction.rs` holds.
- **The full gate**, once, after stage 4.

## Design decisions this overturns or corrects

- [`Prop` is strict, proof-irrelevant, and definitionally K](../../design/theory/prop-is-strict-proof-irrelevant-and-definitionally-k.md): the decision gains the level and the two ways into data; "K-like reduction … is not a rule" and "Lean's position rather than Rocq's" are reversed, and its rejections of Rocq's K criterion and of restricting relevant elimination become what is decided. Its rejection of recording the scrutinee's instance on the `match` stands: the instance is read by lookup.
- [A universe level is implicit, cumulative, and settles by where it stands](../../design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-stands.md) and [Subsumption is a relation, not a traversal order](../../design/theory/subsumption-is-a-relation-not-a-traversal-order.md): `Prop` joins the hierarchy, below the `Type` of its level.
- [An arm is checked in a context specialized by index inversion](../../design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md): the guard's side condition, and an arm that names no pinned payload.
- [Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md) and `curios-analysis`'s `totality` rustdoc: the descent through a payload rests on sizing, and the recursion principle `/std` offers is a bound.
- [The closed machine binds a proof unreduced](../../design/compilation/the-closed-machine-binds-a-proof-unreduced.md) and [A declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md): no elimination demands a proof, so a proof body is read by no judgment.
- [Strict positivity modulo polarity](../../design/theory/strict-positivity-modulo-polarity.md), [A call fills one parameter group](../../design/theory/a-call-fills-one-parameter-group.md), [An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), `curios-std/README.md` and `documentation/syntax.md`: each names impredicativity, the singleton walk, `Accessible` or `WellFounded` where the rule has moved.

## Rejected

- **One small `Prop`, statements over a large type left outside it**: `Accessible`'s payload quantifies over its carrier, so the family could not be a proposition at a large one, and exempting a proof payload from sizing to admit it is the hole.
- **Impredicativity kept, the descent narrowed to a payload whose declared type ends in its family**, as Rocq's guard has it: it closes the ticket and leaves the unbounded cast to the budget, for a capability no declaration of `/std` uses.
- **No data cast along an equation**, Rocq's and Agda's default: a value of a family indexed by an abstract type cannot be rebuilt, so two paths over any node type could not be joined after a test proved their ends one, and a value recovered through a run-time comparison of two type descriptions would be copied, where the cast erases to its argument.
- **A cast primitive in `/sys`**, with `Eq` moved beside it: one family where the rule states every such family, and a registry slot for what a guard and a reducer already say.
- **Firing wherever the family is a pinned singleton, with no question of its indices**: sound on a closed term, and on an open one it turns a `P(x)` into a `P(y)` on a hypothesis, which conversion's typed rules do not survive.
- **A mark on the `match` telling the machine its scrutinee is a proof**: a one-arm match whose body names no binder is its body on a closed total term whatever its scrutinee's sort, so the machine needs to be told nothing.
- **`Accessible` kept in data with a definitional unfolding**: every recursion `/std` makes descends on a number, and the unfolding is one only the budget stops on an open term.
- **`Accessible` kept for proofs alone**: nothing in `/std` proves by it once its callers recurse on a bound; it returns with a consumer whose relation has no numeric rank.

## Completion and retirement

When stage 4 lands, the rules go to the decisions listed above and to the rustdoc of the code that enforces them, `/std`'s recursion principle to `curios-std/README.md`, the Theory lines of `documentation/roadmap.md` become checked summaries, and this file is deleted.
