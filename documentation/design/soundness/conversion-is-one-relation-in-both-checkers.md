# Conversion is one relation in both checkers

**Decision.** A conversion verdict is a function of the two terms and their type. It follows neither the checker that asked, nor the path that checker took to the question, nor the position the pair sits in, nor the spelling a term reached it in. The two checkers never ask the same goals of one program — the elaborator takes an annotation as a step where the kernel compares the first type with the last, compares a part where it is written where the kernel compares the whole it sits in, and checks a definition before the kernel unfolds it — so what makes a program that elaborates certify is that the relation each decides is closed under its laws, and that it is the same relation:

| Law | The difference between the walks it absorbs |
| --- | --- |
| Transitivity | A step through an annotation, against the first type compared with the last |
| Congruence | A part compared where it is written, against the whole compared where it sits |
| Stability under respelling | A term compared before a definition is unfolded, against after |
| One set of rules, tried in one order | — |

**The laws are held by an audit**, `curios`'s `tests::laws`:

1. **A row is a program.** Both sides elaborate at one type before either checker compares them, the invariant every conversion in a compilation stands on, so no row asks what no program can.
2. **A row's verdict is derived from its seed.** A seed is one equation a rule decides, or a near miss beside it (`structural`). A held seed reversed, chained through a shared side, substituted at a compound term (`audit`) or placed under a child a term former holds (`contexts`) is held; a near miss under a context that keeps its hole is refused. No cell states a verdict by hand.
3. **Each checker is asked by itself.** The elaborator's answer is the row's proof, elaborated and stopped short of the kernel; the kernel's is a claim put to it alone, built from terms the elaborator made for a statement that holds whatever the row's side. Neither is the other's oracle.
4. **A refusal by a spent budget is not a refusal.**
5. **A row a checker puts on the other side than derived is listed where the audit runs** (`parted`), with what each checker says and the finding that holds why, held equal to what the run finds. The table is empty.
6. **The positions are held by a lint.** Where each context's hole landed is read back from the term the elaborator built, against a `match` over Core's formers with no wildcard.

**The rules have one statement and one order in both checkers:**

- **Two calls of one definition, by their spines, first**: on the pair as posed, whatever spells its sides, through a curried spine. It keeps two calls folded; no verdict follows it.
- **A goal's type read forced**: a member of a declaration group and a call of a recursive definition are opened before any rule reads the type's shape.
- **Eta by the goal's type, ahead of structure**: both sides applied at a function type, both projected at a Σ, whatever their shapes.
- **Two neutrals where eta cannot be fired, by the shape of their type**: at a nominal struct, and at the type a lookup gives both where a child is compared with none, two neutrals convert where their type has one inhabitant — a proposition, the empty Σ, a Σ or a struct of such, a function into one — and by their heads and spines otherwise. Neither is expanded against the other.
- **Eta by a literal, where no type directs it**: taken against every term whose head shows no type, and refused at a goal whose type is a former other than the literal's own.
- **A child at the type its position has**: a spine's arguments under every head a lookup types, a family's parameters and indices, a struct's fields and a constructor's payload, an intrinsic's operands; and a binder at the type its position gives it, an arm's at its constructor's telescope and a motive's at its family's indices.
- **A lookup types every side that is no literal of a function or a record**: a neutral by its head, a stuck elimination by its result at its scrutinee, a constructor's value by its declaration. It is a lookup, a substitution and a reduction, never a conversion.

The classifiers the two share — what a type former is, which heads show a type — are stated once, in `curios-core`, each a `match` a new former has to answer. Whether a type has one inhabitant is read by each checker with its own reduction and its own sort judgment, and the audit holds the two readings equal. The reading ends at a struct that reaches itself: one met again answers no where its declaration names itself, by its own fields or through another struct's, and is judged again where it is nested in its own parameter. That question is asked of the declarations as they are written and stated once, beside the positivity analysis that owns the fact; its answer ends a walk early and admits nothing.

And the solver's re-validation of a candidate has three answers — it fits, it does not, it cannot be judged yet — so that what it commits does not follow the order parked work is retried in.

**Rationale.**

- **Equal verdicts on equal goals do not make the checkers agree on a program**, because they are not asked equal goals. A law is what carries a verdict from one walk's goal to the other's, so a law that fails in one checker is a program one accepts and the other refuses, and every disagreement found is one law failing in one checker.
- **A near miss is what an admitting flaw needs, and no accepted program holds one.** A corpus that compiles is true equations, which certification re-decides already; the audit's refused rows are the population the kernel is otherwise never asked about, the elaborator refusing them ahead of it.
- **Two checkers wrong alike agree.** A pair of verdicts says which is wrong only against a statement of the rule, which is the seed.
- **The kernel gains a rule rather than the elaborator losing one** where the rule is sound by the invariant irrelevance already stands on: conversion is asked about two terms of the goal's type.
- **Eta between two neutrals decides nothing the shape of their type does not.** Where a field or the codomain has a second inhabitant, the two sides' projections or applications are equal only where their heads are. So two neutrals are never expanded against each other, a struct needs no eta by its type, and what a type directs between two neutrals can be asked of a type that was looked up.
- **A rule that poses a goal about the two sides is not asked of a looked-up type.** Eta at one projects both sides, their heads are a child with no type, the lookup types them again, and the goal is one in progress, which the recurrence rule assumes. Asked there, two variables at a record with a relevant field converge. Reading the type poses no such goal.
- **The typed rule and the looked-up rule read one type.** A lookup forces the type it reads, so a goal's type read in weak-head form parts a pair where it is typed from the same pair where it is looked up: two variables at a struct declared in a group are refused under `Eq` and accepted in an arm's tuple.
- **A lost type is recovered, not worked around.** Two calls that differ in a proof part once reduction unfolds them, because the proof becomes a scrutinee compared with no type. Keeping the calls folded everywhere is one cure and typing the scrutinee is the other; the second makes the unfolded forms converge whatever spelling survived, and leaves the spine rule a matter of cost.
- **An arm's body needs no type of its own.** With its binders at their types, two neutrals in it are decided by the type a lookup gives them, a literal against a neutral by the literal, and two literals by their parts; and an arm is typed under its case equations, which conversion does not assume.
- **Neutrality is no part of eta.** A literal's eta holds for every inhabitant, and what licenses it is that the two sides have one type. Where the goal states a type that is checked; where it states none a term's shape is all there is, and a shape tells only the terms whose head shows their type.
- **One order, where two orders that agree on every goal they share still part on a program.**
- **Undecided is not refused.** A check that would have parked has judged nothing; read as a refusal, it makes a program's fate the order its parked work woke in.

**Rejected.**

- **Replaying a corpus's goals in the other checker.** It needs a tap in one checker and the transport of a scope the other is built never to be told, and it holds no near miss.
- **A grid of terms built by hand in `curios-elab`'s tests.** Each cell states its verdict by hand, nothing keeps a cell well typed, and it reaches no `/std` type.
- **The other checker as the oracle.**
- **A law of history**, a verdict asked again after other questions. What the kernel remembers of a comparison is a verdict that assumed no goal in progress, kept for one declaration with no closure taken over the pairs ([`curios-cert`'s README](../../../curios-cert/README.md#the-kernel-memoizes-its-own-evaluation-and-a-memo-changes-only-resource-verdicts)); a row here is a declaration of its own, so no row is asked of what another left.
- **No unit eta**, as Rocq requires a record to have a relevant field: a literal against a neutral without two neutrals against each other is itself not transitive, and removing both refuses programs that compile.
- **The positivity pass's own answer to whether a declaration reaches itself.** It is the fact, and both checkers compute it — after the unit's items, whose conversions are what would have read it.
- **Refusing every struct met again**, and **judging one again only at a smaller instance**: the first follows which name wraps a unit, the second how a reducer spells an instance.
- **Eta at a looked-up type**, and **two neutrals at a struct compared by their projections**: the first is assumed by the recurrence rule, the second does not end at a struct that reaches itself, and neither decides what the type's shape does not.
- **Reading a type's shape at every goal**, ahead of eta by the type: a second statement of what eta derives at a function and a record type.
- **Typing an arm's body at its motive.** It brings an arm's case equations into conversion, for no verdict.
- **Unfolding lazily in both reducers**, comparing unreduced spellings by shape before any definition is opened: a redesign of both reducers for a verdict the lookup already gives.
- **Settling a candidate in re-validation**, and **postponing by the candidate's shape**: the first commits a lambda's written shape before the drain, the second repeats the list of forms the parking sites already are and misses the same candidate one level down.
- **A relevance mark on a binder**, so that irrelevance is decided with no type, as Rocq's kernel decides it: a second statement of what the type says.
- **A table on the elaborator's context for the binders conversion opens**, **assuming them there**, and **a binder's type stored on the arm**: a second scope with a removal rule of its own, a cache write at every binder, and what Rocq's case node dropped for the declaration's.
