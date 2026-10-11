# An elimination in a goal is typed alike by both checkers

Working specification for what the kernel does with an arm no reduction takes. It is held to [An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), which has everything the elaborator invents reach the certifier as an ordinary term; to [Case equations and their key](../../design/soundness/case-equations-and-their-key.md), which has what one checker accepts under an arm accepted by the other; and to [An arm is checked in a context specialized by index inversion](../../design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md). The elaborator writes types it derived by putting a value in a variable's place and types none of them, so the three hold only where the kernel types every such type, and it refuses two kinds.

## What this builds on

- **A derived type is written, and nothing types it but the kernel.** A `let` with no annotation is written at the type its value was inferred at, a lambda checked at a function type takes the domain it is checked at, and a match with no motive checked at a type stores that type as its goal (`MatchResult::Ambient`, `curios-core/src/term/shape.rs`). Each is an instance of a type that was typed where a reader wrote it: a result at a call's arguments, or a goal at an arm's case (`MatchResult::at`). The kernel types a type as written, never its reduct ([`curios-cert`'s README](../../../curios-cert/README.md)), so it types each of them.
- **An arm is typed under what its case says of the scrutinee.** A variable scrutinee is substituted. An expression scrutinee stands as an equation, recorded only where it names a local (`curios_analysis::records_case_equation`): the kernel's memos keep a local-free term's reduct for the declaration, and an equation about one would leave an entry resting on an equation the arm's exit retracts. An equation in force that an inner arm's solution leaves naming no local is dropped the same way (`Scope::restate`, `curios-cert/src/kernel/scope.rs`).
- **A nominal arm is typed under the solution of its index equations**, and one whose targets clash with the scrutinee's indices is typed as written, with nothing solved (`check_arm`, `curios-cert/src/kernel/infer/eliminate.rs`). The same answer of the shared unifier excuses that arm's absence.
- **One kind of arm is typed by no rule**: one of a case split whose scrutinee already is a literal or a constructed value of another case (`never_taken`). A fold is never asked, its reduct holding the fold at the tail.
- **The elaborator already computes whether an arm is dead, to word a refusal.** Where an arm's body is refused, `from_arm` (`curios-elab/src/elaborate/match_.rs`) reduces its guard and reports "this arm is never taken" where the guard is always another case (`unreachable_arm`) or the arm's case contradicts a guard around it (`contradicted_guard`, `curios-elab/src/typing.rs`). It reads nothing of the index equations.
- **What the certifier's rules are held to** by the first decision above: every way it accepts is a rule that is sound alone, so the order rules are tried in decides what a verdict costs and never which way it goes; a rule that does not apply leaves nothing behind; a budget changes a verdict only into a refusal; a grid or a differential holds it to a statement independent of it.
- **A motive whose body is a match with no motive of its own takes the sort its arms decide**, since a motive's sort became a quality (`2acea664b`): the `In` inversion this specification once held is accepted in both spellings, the inner match's goal stored as `Prop`. No fixture holds it, and `/std` still writes the motive it no longer needs, `match m: (_) => Prop`, five times (`List/Member.crs`, `List/Pairwise.crs`, `List/sort_by.crs`).
- **Peers.** A restriction on which equations an arm assumes must be stable under substitution, or a reduction takes a typed term to one that is not (Burke, [Local Rewriting in Dependent Type Theory](https://github.com/NathanielB123/fyp/blob/main/submitted.pdf), §5.3.2). Altenkirch's smart case normalizes a scrutinee where its equation is assumed, and one that normalizes to the other constructor makes the context inconsistent, under which every two terms convert ([The case of the smart case](https://people.cs.nott.ac.uk/psztxa/talks/shonan11.pdf)). Lean keeps a term its kernel accepts beside the reduct it refuses as a pair of kernel tests ([`subject-reduction-reduct`](https://arena.lean-lang.org/test/corner-cases/subject-reduction-reduct/)).

## The gap

Each program of the first four paragraphs elaborates and is refused by the kernel, with no position, at `ca95d7595`. The first three are over `let g(c: Bool, @ok: Holds(c)) -> Nat = 1;`.

**An arm under a scrutinee that computes to another case is typed with no equation.** `let h(c: Bool) -> Holds((match c | true => g(c, @True/qed()) | false => 0 end) <= 1) = match c | false => True/qed() | true => True/qed() end;` is accepted, and so is `let x = h(false);`, the arm under a literal being typed by no rule. `let used() -> Nat = let x = h(Bool/not(true)); 0;` stops on ``the kernel refused /used: expected `Holds(Bool/not(true))`, found `True` ``: the type written for `x` matches on `Bool/not(true)`, which names no local, and the proof under its `true` arm rested on that arm's equation. The same instance is refused where it is written as a lambda's domain, in `let k(c: Bool) -> (x: Holds((match Bool/not(c) | true => g(Bool/not(c), @True/qed()) | false => 0 end) <= 1)) -> Nat = match c | false => (x) => 0 | true => (x) => 1 end;`, and as an inner match's goal, in `let f(c: Bool, k: Nat) -> Holds((match Bool/not(c) | true => g(Bool/not(c), @True/qed()) | false => 0 end) <= 1) = match c | false => True/proved() | true => match k | 0 => True/qed() | _ + 1 => True/qed() end end;`, which reports the goal as no valid motive. A goal matching on `0 < n`, under a match on `n`, is the arithmetic shape. A local function elaborated again holds the same `let` and is refused the same way.

**So is an arm under an equation its own solution closed.** With `let h2(c: Bool, d: Bool) -> Holds((match Bool/not(c) && d | true => (match d | true => g(Bool/not(c) && d, @True/qed()) | false => 0 end) | false => 0 end) <= 1)`, accepted, `let x = h2(true, d);` is refused: the inner arm solves `d`, the outer equation restated under it names no local, and it is dropped.

**So is an arm whose proof spells the scrutinee another way.** With `h3` as `h2` without its inner match, its proof at `g(d && Bool/not(c), @True/qed())`, `let x = h3(true, d);` is refused: the equation is recorded, its scrutinee `Bool/not(true) && d` naming `d`, and the proof's spelling reduces to `false` without meeting it. Where the proof spells the scrutinee as the match does, the instance is accepted.

**An arm its scrutinee's indices rule out is typed as written.** Over `induct Tag: (Nat) -> Type` with `zero(): (0)` and `other(n: Nat): (n + 1)`, and `let needs_zero(n: Nat, @ok: Holds(n == 0)) -> Nat = 7;`, the declaration `let h(n: Nat, t: Tag(n)) -> Holds((match t | zero() => needs_zero(n) | other(_) => 0 end) <= 7) = match t | zero() => True/qed() | other(_) => True/qed() end;` is accepted, and `let used(t: Tag(1)) -> Nat = let x = h(1, t); 0;` stops on ``the kernel refused /used: expected `Holds((/std/Nat/witness@/std/ops/Eql(Nat)).0(1, 0))`, found `True` ``. No case equation is involved: `Tag/zero()`'s target `0` clashes with the index `1`, and the arm typed under `n := 0` is typed with nothing solved.

**A goal's occurrence of an expression scrutinee is replaced only where no solved metavariable hides it.** `at` replaces the occurrences the goal spells as the match does, and a scrutinee written through an operator holds its witness as a metavariable solved and not yet substituted, which the goal's occurrence does not: under `let g2(a: Nat, b: Nat, @ok: Holds(a <= b)) -> Nat = 1;`, with `let f(a: Nat, b: Nat, k: Nat) -> Holds((match a <= b | true => g2(a, b) | false => 0 end) <= 1)` matched on `a <= b`, an inner match's goal in either arm still matches on `a <= b`, and the program is accepted. Which occurrences an arm specializes follows the order two metavariables were solved in.

Nothing is admitted by any of it. A reader meets a kernel refusal with no position for a program the elaborator accepted, and what repairs it is a motive written where none is needed.

## Decisions

**The kernel types every instance of a type it types.** What it accepts at a variable it accepts at every value put in that variable's place. That is the property the elaborator writes derived types by, and the statement the harness of *Verification* holds the arm rules to.

**The kernel types no arm that no reduction takes**, and knows one in three ways:

- its scrutinee already is a literal or a constructed value of another case, as today;
- its case's targets clash with its scrutinee's indices, by the shared unifier's answer, which excuses that arm's absence;
- an equation it assumes computes to another case than the equation states: its own scrutinee's, or one in force that its solution restated.

The first two are known before the arm is typed and cost nothing. The third costs a reduction, so it is asked only where the arm was refused: the arm is typed as it is today, and where that refusal is no spent budget, each equation the arm assumed has its scrutinee reduced, outside the arm's own equation, and put to the unifier against its case. A definite clash, and the arm is not typed. Every program the kernel accepts today is accepted by the same work, and no term of one is run to type it.

**It is sound for the reason `never_taken` gives.** A scrutinee that reduces to a value of another case, under the equations around the arm, is that value wherever the arm is reached, so reduction opens another arm and conversion compares none of this one. The two ways an arm is accepted are each sound alone, and which is tried first decides what a verdict costs.

**A refused walk leaves nothing behind.** What the kernel records while it types — calls, positions, the count of groups that do not descend, the recorder's two flags — is taken back where a walk that may be refused is refused, as scope and the equations in force already are. An arm that is excused is then one that was never typed.

**A spent budget is never excused, and a fold is never asked.** A refusal for the budget propagates from the arm and from the reduction that would excuse it. Every arm of a fold is typed, a value of either case taking its identity arm in the end.

**The elaborator types an arm a reader wrote.** It has to elaborate one to emit it, so a proof written in a dead arm is refused where it is written, with the note it gets today. That note and the kernel's rule read one predicate of the shared layer, and the note is given for an arm the indices rule out as well, with the two types that clash and the word that the arm may be left out.

**`at` reads the scrutinee and the goal with their solved metavariables substituted**, so the occurrences an arm specializes are the ones the two terms hold, whichever was solved first.

## Stages

Each lands alone, on its own check.

1. **The fixed entry's fixture.** The `In` inversion in both spellings, and the same shape with data in its arms refused as the elimination it is, in `curios/src/tests/matching/motive_tests.rs`; the five motives dropped from `/std`. May move: the stored prelude's size and the figures read off its image.
2. **The harness** of *Verification*. Landed: over the prelude it reports no instance, as it stands and with the kernel typing every arm alike, so it holds the prelude to the property as it grows and shows nothing of the rule.
3. **An arm its indices rule out is not typed.** `check_arm` keeps its arity, mark and proof-binder checks and types nothing more where the unifier answers a clash. Check: a kernel fixture mutation-checked against typing the arm, and the `Tag` program run, with `h(0, t)` beside it.
4. **A refused walk leaves nothing behind.** A bracket of the kernel's beside `Kernel::scoped`, landed with the rule of stage 5 that calls it, a bracket with no caller being code nothing reads. Check: a differential in the kernel's tests, where a walk refused inside the bracket and a judgment after it record what the judgment records alone.
5. **An arm refused under an equation that computes to another case is not typed.** First the predicate in `curios-analysis`, read by `never_taken` and by the elaborator's two notes; then the rule at the three sites an arm taken once is typed. Tried at `ca95d7595` in a scratch copy and not landed, with nothing taken back from a refused arm: the programs of the first three paragraphs above run, and `/std` certifies, 2,964 items, with no arm excused and no scrutinee met that names no local and is no value. Check: the programs above as fixtures run by both checkers, the two written dead arms still refused with their notes, kernel fixtures for a restated equation, a scrutinee naming a local that computes, a spent budget and a fold, each mutation-checked, and the harness at none.
6. **The elaborator says an arm its indices rule out cannot be reached.** Check: a fixture asserting the note, and the finding deleted.
7. **`at` reads substituted terms.** Tried with the equation rule and not landed: `/std` elaborates to another image, 40,161,328 bytes of `std.rkyv` against 40,122,800, counted at `ca95d7595`, and certifies with no arm excused. Check: the operator program's inner goals specialized in both arms, read off the `core-elab` rung. May move: the figures read off the image, and a budget floor a test holds.

## Verification

- **The harness.** For each root of the prelude, the kernel that certified it (`curios_cert::recheck_module_measured`) is asked to type (`curios_cert::infer`, the budget restored and the definition's universes assumed) every instance of every definition's declared type at one parameter: a parameter at `Bool` or `Nat`, each of the literals `false`, `true`, `0` and `1` and a closed spelling that computes to each, the binder dropped. A declaration that leaves a sort to its caller is put at each. A refusal that is no spent budget is a violation. It lives beside `kernel_memo_parity` in `curios-prelude-archive`. No declared type of the prelude holds an arm that rests on a parameter's case, so what makes the rule's absence show is the next item.
- Every program under *The gap* is a fixture of `curios`'s `tests::matching::instance_tests`, run and not only elaborated, beside the value that leaves its arm the one taken; and the arm rule's own fixtures in `curios-cert` are each checked against the mutation that removes what they hold.
- `cargo xtask lint`, which certifies `/std`, at every stage, and `cargo xboard report` after the last.

## Rejected

- **Computing a scrutinee before its arms are typed.** It decides the same arms and runs every closed scrutinee a program holds, which may be the program, where nothing needed the answer.
- **Computing only within a type**, the typing memo keyed by it: it makes a typing follow where it was made, and reaches neither an equation a solution closed nor an arm the indices rule out.
- **Taking decided arms where `at` substitutes.** `let x = h(Bool/not(true));` calls it nowhere.
- **Recording an equation for a scrutinee that names no local.** The written spelling would answer, and a spelling that reduces without meeting it would not, so the instance is refused all the same; and a local-free term's remembered reduct would have to follow the equations in force.
- **Writing a derived type reduced.** It loses the spelling a report shows, costs a normal form per annotation, and leaves an arm that is dead only under a guard around it.
- **The elaborator typing what it derives**, refusing where the kernel does: both checkers would refuse a program that is well typed.

## Non-goals

Whether an arm's equation is the kernel's to hold at all, or a hypothesis the arm binds and the elaborator proves from: it has a specification of its own once this one's stages have landed.

## Completion and retirement

- Every program under *The gap* runs, and the harness reports no violation over the prelude.
- The property is stated in [An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), and the rule in [Case equations and their key](../../design/soundness/case-equations-and-their-key.md), the index-inversion decision and `curios-cert`'s README, each restated with the stage that changes it.
- This file is deleted once the roadmap's line is its checked summary.
