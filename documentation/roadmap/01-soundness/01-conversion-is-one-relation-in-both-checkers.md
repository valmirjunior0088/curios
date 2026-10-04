# Conversion is one relation in both checkers

Working specification for holding conversion to its laws at every rule it has, with each checker asked by itself. A conversion verdict is a function of the two terms and their type: it follows neither the checker that asked, nor the path that checker took to the question, nor the position the pair sits in. The two checkers never ask the same goals of one program, so what makes a program that elaborates certify is that the relation each decides is closed under its laws, and that it is the same relation. The audit states the laws as rows, derives each row's verdict from the equation it was built from, and reads each checker's answer apart from the other's.

It is independent of every other spec; [Checked evidence and trusted reasoning](03-checked-evidence.md) names it in its verification. Its first four stages change no checker, and its fifth changes both.

## What this builds on

- **Two checkers, one shared layer.** Each runs its own conversion over the representation, the carriers' algebra and the chain both run when two intrinsics meet ([An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md)).
- **The law grid.** `curios`'s `tests::laws` states every family `curios-algebra`'s law table declares, at every carrier declaring it (`generated`), and by hand the laws no family states, the controls and the refused candidates (`written`). A held row is an `Eq/refl()` proof compiled through both checkers (`closes`); every row is also a written goal read back through the `? ≈ Eq/refl()` candidate line (`misplaced`).
- **The audit of the theory.** `tests::laws::audit` puts every declared law reversed, chained through a shared side and substituted at compound terms, and holds constructors free modulo the theory: the conditions [The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) takes from Coq Modulo Theory.
- **One fixture states a path.** `written`'s `a_step_taken_through_an_annotation_is_one_the_kernel_takes_in_one` holds that a step the elaborator takes through a `let`'s annotation is one the kernel takes comparing the first type with the last ([An atom is one where conversion says so](../../design/arithmetic/an-atom-is-one-where-conversion-says-so.md)).
- **Two doors read one checker.** `curios-pipeline`'s `typecheck_with_prelude` lowers and elaborates a program and stops short of the kernel, handing back the program it built. `recheck_with_prelude` puts a program to the kernel alone with the prelude in scope, which is how `xboard` puts a module built by hand.
- **Where each checker fires eta.** The elaborator fires it by a side's shape, at any goal type: a lambda, a tuple literal or a struct literal against anything (`eta_expand_func`, `eta_expand_tuple`, `eta_expand_struct`, `curios-elab/src/convert.rs`), and by the goal's type only where both sides are neutral (`eta_expand_neutral`). The kernel fires Π and Σ eta by the goal's type (`compare`, `curios-cert/src/kernel/convert.rs`), a nominal struct's by the literal against a neutral (`struct_eta`), and contracts `(x) => f(x)` to `f` in `whnf`. A child the kernel has no type for is compared at `Type` ([Eta and untyped child positions](../../design/soundness/conversion/eta-and-untyped-child-positions.md)).

## The gap

**The checkers walk one program by different goals.** The elaborator takes an annotation as a step where the kernel compares the first type with the last; it compares a part where it is written where the kernel compares the whole it sits in; it checks a definition before the kernel unfolds it. Equal verdicts on equal goals do not make the two agree on a program. Each law of the relation absorbs one of those differences, and each place the checkers part is a law failing in one of them:

| Law | The difference it absorbs | Where it fails |
| --- | --- | --- |
| Transitivity | A step through an annotation against the first type compared with the last | The elaborator at a record of units; the truth table's cap in both, by design ([What conversion still decides by spelling or by cap](../04-arithmetic/01-decided-by-spelling-or-cap.md)) |
| Congruence | A part compared where it is written against the whole compared where it sits | The kernel, at Π and Σ eta under a child it compares at `Type` |
| Stability under respelling | A term compared before a definition is unfolded against after | A case equation's key, which [What conversion still decides by spelling or by cap](../04-arithmetic/01-decided-by-spelling-or-cap.md) holds |
| One set of rules | — | Unit eta, which the kernel lacks; the elaborator's eta at a goal type the literal contradicts ([the findings](00-findings.md)) |

**Congruence fails in the kernel.** Record eta holds at the goal, under a variable head, under a constructor payload and under a list element, in both checkers. Under a stuck `match`'s arm the elaborator accepts it and the kernel refuses:

```
use /std/{Eq, Nat, Bool};
let arm(b: Bool, p: {Nat, Nat})
    -> Eq()(match b: (_) => {Nat, Nat} | true => (p.0, p.1) | false => p end,
            match b: (_) => {Nat, Nat} | true => p | false => p end)
    = Eq/refl();
```

`run` stops on `the kernel refused /arm`. So do the same pair under a `Nat` match's arm, under a projection of the two matches, as the bodies of two lambdas in an arm, and `(x: Nat) => g(x + 0)` against `g` in an arm at `(Nat) -> Nat`. `(x: Nat) => g(x)` against `g` there is accepted, by the contraction, and so is a struct literal against its neutral.

**Transitivity fails in the elaborator.** At `{a: {}, b: {}}`, `r` against `((), ())` and `((), ())` against `s` each fit `Eq/refl()`, and `r` against `s` does not: the literal is expanded by its shape, and two neutrals' projections are compared at `Type`.

**The kernel is never asked about a row the elaborator refuses.** The grid's refused rows and controls are read through the candidate line, which is the elaborator's answer and stops before the kernel, and a board test of a refusal asserts the elaborator's diagnostic. A kernel that held a control would pass every test, and it is the direction no program can show, since the kernel is asked only once the elaborator accepts.

**The audit holds the theory's laws alone.** It has no row under a context, and none for a rule no law table states: beta, delta, zeta, iota, eta, irrelevance.

## Prior art

- **Lennon-Bertrand** ([*What does it take to certify a conversion checker?*, FSCD 2025](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.FSCD.2025.27)). "In Rocq and Lean, conversion does not maintain any type information, and Agda's conversion, while primarily type-directed, similarly uses term-directed η-expansion of functions." An untyped checker replaces the type-directed rule by term-directed ones, with "no η-rule when the two sides are neutrals"; a definitional unit type and strict propositions "completely wreck completeness of neutral comparison" and need the type.
- **Abel and Coquand** ([*Untyped algorithmic equality for Martin-Löf's logical framework with surjective pairs*, Fundamenta Informaticae 77(4), 2007](https://research.chalmers.se/en/publication/175708)). An untyped βη-equality test, eta for functions and for pairs fired by a side's shape, is complete for the judgmental equality on well-typed terms.
- **Lean 4.33.1** ([release notes](https://lean-lang.org/doc/reference/latest/releases/v4.33.1/)). The kernel's `is_def_eq` is not transitive, and a cache that closed it transitively made "a query's result depend on the order of earlier queries", from which a crafted input derives `False`; the fix makes `is_def_eq` "again a function of its two arguments".
- **Lean issue 2258** ([*DefEq transitivity failures for unit-like eta*](https://github.com/leanprover/lean4/issues/2258)). `p` against the literal and the literal against `q` hold, and `p` against `q` does not: the failure above, open there.
- **The Lean Kernel Arena** ([arena.lean-lang.org](https://arena.lean-lang.org/)). Several kernels put to one battery of proofs to accept and proofs to reject, each answering accept, reject or decline, a declined test counting as neither.
- **Rocq's `SProp`** ([reference manual](https://rocq-prover.org/doc/v9.3/refman/addendum/sprop.html)). Each binder caches whether its type is a strict proposition, so conversion decides irrelevance with no type, and a mark elaboration left wrong makes a conversion fail.

## Decisions

Taken before stage 1, each with its reason, so a stage meets none of them as a fork.

1. **The audit extends `tests::laws`, and a row is a program.** Both sides of a row elaborate at one type before either checker compares them, which is the invariant every conversion in a compilation stands on, so no row asks what no program can.
2. **A row's verdict is derived from its seed.** A seed is one equation a rule decides, or a near miss beside it. A held seed reversed, chained, substituted or placed under a context is held; a near miss under a context that keeps its hole is refused. No cell states a verdict by hand, and neither checker is the other's oracle.
3. **Each checker is asked by itself.** The elaborator's answer is the candidate line. The kernel's is a claim put to it alone: the row's statement and a proof by reflexivity at its left side are both elaborated, the statement's lambda becomes the claim's type and the proof's body the claim's, and `recheck_with_prelude` judges it. Every term in the claim is one the elaborator built.
4. **A refusal by a spent budget is not a refusal.** A near miss is refused as a mismatch, and a row a checker exhausts on is its own class.
5. **A row the checkers part on is listed where the audit runs**, in one table naming its finding, held equal to what the run finds: a row that starts parting fails, and so does one that stops while it is still listed. The finding's text is [the findings](00-findings.md)'; a row driven to `/std/Bool/False` is a ticket in `xboard/src/board/conversion.rs`.
6. **The positions are held by a lint.** Where each context's hole landed is read back from the term the elaborator built, and the formers are matched with no wildcard, so a former added to Core states its contexts before the audit compiles. The lint holds formers and kinds of elimination; which child of a former a context targets is the context's to state.
7. **Three fixes land with the audit.** The kernel fires Π and Σ eta by the literal against a neutral, the set `struct_eta` takes, under the invariant `struct_eta` states. Unit eta lands as [the findings](00-findings.md) state it. The elaborator's tuple and struct eta is gated by the goal type, as they state it.
8. **What needs a type the position does not carry stays a stated row.** Unit eta and irrelevance between two neutrals, in a stuck elimination's arm or under a projection's head, and two neutrals at a nominal struct compared by their projections, stay rows both checkers refuse, under an open roadmap line of their own.
9. **Out of the audit.** A case equation's key is [What conversion still decides by spelling or by cap](../04-arithmetic/01-decided-by-spelling-or-cap.md)'s. A level is solved by the elaborator and judged by the kernel, which is not one question, so rows keep their levels ground. Recurrence stays with `kernel::convert::recursion_tests` unless a source program reaches it.

## Open questions

- Whether the neutrals `struct_eta` takes are the whole set the kernel's Π and Σ eta need. A row that wants a stuck elimination as the neutral is reported before the set grows.
- Whether a source program reaches the recurrence rule, so that it has a seed.

## Stages

1. **Both verdicts on every row the grid states.** The kernel is asked alone about each generated and written row. Check: every held row is held by it, the reader's control; every refused row is refused by it as a mismatch; the table of parted rows equals what the run finds.
2. **Seeds for the rules no law table states**: beta, delta, zeta, iota at each elimination, Π eta, Σ eta, struct eta, unit eta, irrelevance — a held seed against a canonical neutral, and a near miss. Check: every seed is on the side each checker puts it, asked alone.
3. **Every seed under every position**, one context per child a former holds, with the lint of decision 6. Check: a held seed holds under every context in both checkers; a near miss stays refused under a context that keeps its hole; every context's hole lands under the former it names.
4. **The other laws over every seed**: reversed, chained through a shared side, substituted at a compound term. Check: each generated row holds in both checkers, or is listed as parted.
5. **The three fixes**, each emptying its rows of the table and deleting its finding. Check: the fix's own tests in the crate it changes, mutation-checked; the programs it accepts as tests in `curios/src/tests/board/`; `cargo xboard`; `/std` certifies.
6. **Landing.** The decision record, the entries and the roadmap.

## Verification

- Every program this specification names as refused runs, and every near miss beside it is still refused, by both checkers.
- The table of parted rows holds only rows decision 8 names.
- `cargo xboard` reports the conversion part as it stood, and `/std` certifies.
- A fix may move what certifying a unit costs; `curios-prelude-archive`'s `stored_prelude_measurements` states the protocol that retakes it.

## Rejected

- **Replaying a corpus's goals in the other checker.** A program that compiles holds true equations, and certification re-decides them already; a near miss is what an admitting flaw needs, and no accepted program holds one. A replay also needs a tap in one checker and the transport of a scope the other is built never to be told — a metavariable, a refinement layer, a parked goal.
- **A grid of terms built by hand in `curios-elab`'s tests.** Each cell would state its verdict by hand, nothing would keep a cell well typed, and it reaches no `/std` type.
- **The other checker as the oracle.** Two checkers wrong alike agree, and a pair of verdicts says which of them is wrong only against a statement of the rule.
- **A law of history**, a verdict asked again after other questions. The kernel keeps no cache of conversions: its history is built per comparison and left on return, and its memos are cleared at each declaration (`recheck_module_verdicts`), with `kernel_memo_parity` holding the memos themselves.
- **A relevance mark on a binder**, so that irrelevance is decided with no type: [An atom is one where conversion says so](../../design/arithmetic/an-atom-is-one-where-conversion-says-so.md) rejects it as a second statement of what the type says.

## Retirement

Record the decision and its rejected alternatives in a design decision under `documentation/design/soundness/`; restate [Eta and untyped child positions](../../design/soundness/conversion/eta-and-untyped-child-positions.md) for the rules the fixes land; name the audit in [An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) among what holds the two checkers to each other; replace the roadmap entry with a checked summary and an open line for decision 8's rows, with this file rewritten to that remainder; verify that nothing references a filename this one has had.
