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
- **The same member of two recursive groups, by the groups as binders, before either is unfolded**: member for member their types and their bodies under one shared set of binders, bare or at the head of two calls, whose spines follow ([Conversion recurrence](conversion-recurrence.md)). It unfolds no member, and a pair it does not hold goes on.
- **A goal's type read forced**: a member of a declaration group and a call of a recursive definition are opened before any rule reads the type's shape.
- **Eta by the goal's type, ahead of structure**: both sides applied at a function type, both projected at a Σ with a field, whatever their shapes.
- **Two neutrals at a proposition, and by nothing else their type says**: at a goal whose type is a proposition, and at the proposition a lookup gives both where a child is compared with no type, two terms convert unread. At every other type two neutrals convert by their heads and spines: the Σ with no field has no eta, a nominal struct has none by its type, and neither is expanded against the other. Two eliminations that state the instance they eliminate are compared by it, the instances first and the scrutinees at the instance, with no lookup.
- **Eta by a literal, where no type directs it**: taken against every term whose head shows no type, refused at a goal whose type is a former other than the literal's own, and fired by no literal that holds no field of data — the tuple with no field, and a struct literal whose every field is a proof.
- **A child at the type its position has**: a spine's arguments under every head a lookup types, a family's parameters and indices, a struct's fields and a constructor's payload, an intrinsic's operands; and a binder at the type its position gives it, an arm's at its constructor's telescope and a motive's at its family's indices.
- **A lookup types every side that is no literal of a function or a record**: a neutral by its head, a stuck elimination by its result at its scrutinee, a constructor's value by its declaration. It is a lookup, a substitution and a reduction, never a conversion.
- **Two folded calls every rule above leaves apart, by one unfolding of both, last**: their two heads a pair unfolded against itself once, and apart where it is met again inside that unfolding, whatever its spines ([Conversion recurrence](conversion-recurrence.md)).

The classifiers the two share — what a type former is, which heads show a type — are stated once, in `curios-core`, each a `match` a new former has to answer.

And the solver's re-validation of a candidate has three answers — it fits, it does not, it cannot be judged yet — so that what it commits does not follow the order parked work is retried in.

**Rationale.**

- **Equal verdicts on equal goals do not make the checkers agree on a program**, because they are not asked equal goals. A law is what carries a verdict from one walk's goal to the other's, so a law that fails in one checker is a program one accepts and the other refuses, and every disagreement found is one law failing in one checker.
- **A near miss is what an admitting flaw needs, and no accepted program holds one.** A corpus that compiles is true equations, which certification re-decides already; the audit's refused rows are the population the kernel is otherwise never asked about, the elaborator refusing them ahead of it.
- **Two checkers wrong alike agree.** A pair of verdicts says which is wrong only against a statement of the rule, which is the seed.
- **The kernel gains a rule rather than the elaborator losing one** where the rule is sound by the invariant irrelevance already stands on: conversion is asked about two terms of the goal's type.
- **Eta between two neutrals decides nothing irrelevance does not.** Where a field or the codomain is no proposition, the two sides' projections or applications are equal only where their heads are. So two neutrals are never expanded against each other, a struct needs no eta by its type, and what a type directs between two neutrals can be asked of a type that was looked up.
- **That a type has one inhabitant is read off its sort, never off its shape.** A proposition is one by its sort, which a substitution keeps. The type with no field, a record of such, a struct of proofs and a function into any of them are so by their shape, which a substitution makes true of a type it was not true of: `F(c)` that is `{}` at `true` and `Nat` at `false` has two neutrals apart while `c` is open and one inside the arm that fixes it. A verdict read there is kept by whatever remembers a term past the arm, a guard's reduced spelling among them, and proves `False` where `c` is open again. A type meant to have one inhabitant is declared a proposition.
- **A literal with no field of data has no eta, so conversion stays transitive.** Its walk compares nothing and would answer `true` of any neutral, so two neutrals would be one through the literal where their heads keep them apart, and which atoms a fold classes together would follow the order they were met in.
- **A rule that poses a goal about the two sides is not asked of a looked-up type.** Eta at one projects both sides, their heads are a child with no type, the lookup types them again, and the goal is the one in hand, posed again without end. Reading the type poses no such goal.
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
- **A law of history**, a verdict asked again after other questions. What the kernel remembers of a comparison is a verdict that left no pair of calls apart inside its own unfolding, kept for one declaration with no closure taken over the pairs ([`curios-cert`'s README](../../../curios-cert/README.md#the-kernel-memoizes-its-own-evaluation-and-a-memo-changes-only-resource-verdicts)); a row here is a declaration of its own, so no row is asked of what another left.
- **Unit eta, and two neutrals made one by the shape of their type**: the type with no field, a record or a struct of such, a function into one, read off the goal's type and off a looked-up one. The shape is no function of the term a verdict is remembered under, as the rationale states. Rocq forbids the definitional unit and requires a record to have a relevant field for what reading a type in conversion costs; here the reading was paid for and the verdict was the hazard.
- **The unit literal's eta kept, against a neutral and not between two.** `x` is `()` and `()` is `y` where `x` is not `y`.
- **Eta at a looked-up type**, and **two neutrals at a struct compared by their projections**: the first poses the goal in hand again without end, the second poses their heads again, which only two equal heads pass, and neither decides what irrelevance does not.
- **Typing an arm's body at its motive.** It brings an arm's case equations into conversion, for no verdict.
- **Unfolding lazily in both reducers**, comparing unreduced spellings by shape before any definition is opened: a redesign of both reducers for a verdict the lookup already gives.
- **Settling a candidate in re-validation**, and **postponing by the candidate's shape**: the first commits a lambda's written shape before the drain, the second repeats the list of forms the parking sites already are and misses the same candidate one level down.
- **A table on the elaborator's context for the binders conversion opens**, **assuming them there**, and **a binder's type stored on the arm**: a second scope with a removal rule of its own, a cache write at every binder, and what Rocq's case node dropped for the declaration's.
