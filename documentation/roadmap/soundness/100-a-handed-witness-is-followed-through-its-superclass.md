# A handed witness is followed through its superclass

**[Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md) grades a call in a callback where its callee applies it, so that "a witness handed on inside another's application, `List`'s witness applied to `Option`'s applied to a member, is graded where each applies what it is handed"; a container's witness of a concept with a superclass publishes nothing of its premise, and a family nested through it has no law known to terminate; its next step is stage 4, a ready delivery.** A summary follows a value through four constructs: a parameter handed whole to a callee, a parameter handed on at its own place to a member, a member that escapes, a call of each of its parts claiming nothing past its arguments, and a field lambda, a part of its own. A path rooted at a parameter is followed where the construct reads the parameter, and a definition applied where it reads a member, escaping as a member does and held at a field as a field lambda is, so a witness over `use Ord(T)` alone is graded as one of a concept with no superclass is; what is left is the premise written for it before it was.

## What is wrong today

Each is probed at `8796393a0`.

- **The premise is written for it in eleven witnesses of `/std` and two other places.** `Ord`'s witnesses of `List`, `Option`, `Result` and the tuples of one to eight fields each take `Eql` of an element type beside `Ord` of it, with a note saying why: resolution takes a premise that matches before a premise's superclass ([Concepts resolve with global coherence](../../design/surface/concepts-resolve-with-global-coherence.md)), so the witness's own superclass is built from a parameter handed whole, which leaves `Ord`'s premise followed. `documentation/language.md` writes it in its derived `Ord(Tree(A))`, and `curios/src/tests/derive/ord.rs` in `Box`'s and `Rose`'s.
- **A program's witness meets it.** Over `induct Box(A: Type): Type | box(A) end` and `induct Forest: Type | none() | some(Box(Forest)) end`, each with its derived `Eql`, `satisfy (@A: Type, use Ord(A)) => Ord(Box(A)) { .. }` is accepted, and `satisfy Ord(Forest) { .. }` after it is refused, `a type in /witness~/std/Ord/Ord(/Forest) reaches the witness of Ord for head Forest, which is not known to terminate`; written `(@A: Type, use Eql(A), use Ord(A))` both are accepted. Nothing tells the witness's author which premise the family three declarations later needed.

## What it simplifies

- **`/std`'s eleven premises** and their notes go, and so do the three outside `/std`: each witness is over `use Ord(T)` alone, its superclass resolved through it.
- **The premise is no rule a program's author has to know.** A witness over `use Ord(A)` alone serves a family nested through it, written or derived, and so does one of any concept whose superclass a container builds from its element's.

## Goal

- **Every `Ord` witness is over `use Ord(…)` alone**, in `/std`, in `documentation/language.md` and in the tests, its superclass resolved through its premise.

## Settled

- **The peers leave it to the author, or reduce at the use.** Lean's derived `BEq` of a nested inductive is `partial` ([Zulip](https://leanprover-community.github.io/archive/stream/113488-general/topic/Defining.20.60BEq.60.20for.20nested.20inductive.20type.html)), its deriving handler for `DecidableEq` refuses one ([Zulip](https://leanprover-community.github.io/archive/stream/113488-general/topic/DecidableEq.20for.20nested.20inductive.20types.html)), and its derived `LawfulBEq` is refused there ([Zulip](https://leanprover-community.github.io/archive/stream/113489-new-members/topic/Adding.20an.20instance.20of.20LawfulBEq.20to.20my.20nested.20inductive.20type.html)); Idris's totality checker cannot see through `map` and leaves the author `assert_smaller` ([tutorial](https://idris-community.github.io/idris2-tutorial/Tutorial/Folds/Totality.html)); Agda and Rocq accept the recursion written out as a function of its own beside the type ([Agda's list](https://lists.chalmers.se/pipermail/agda/2019/011227.html)), and Rocq's guard checker reads a nested fixpoint by reducing the callee's body where it is met, a rule its maintainers keep relaxing ([TYPES 2025](https://msp.cis.strath.ac.uk/types2025/abstracts/TYPES2025_paper15.pdf)). A summary each definition publishes once is the reading neither needs: the decision's rejection of a method lifted beside its witness stands.

**Rejected.** The premise written wherever a witness is over a concept with a superclass, what `/std` does today: an author learns of it from a refusal at another declaration.

## Open

- **Whether a field read as a value is a use.** A witness whose superclass reads a field of its premise's that is no function, `mark = Mark/mark(@T) + 1` over `concept Mark(A: Type): pub Type { mark: Nat }`, names the premise where nothing applies it, and a concept with that superclass stays refused under these rules. No concept of `/std` has one; filed at retirement unless a stage meets one.

## Stages

**4. `/std` drops the premise.** The eleven witnesses, `documentation/language.md`'s example and `curios/src/tests/derive/ord.rs`'s `Box` and `Rose`. Check: `/std` builds and its tests pass; `a_derived_witness_proves_the_orders_laws` holds a family nested through a witness over `use Ord(A)` alone.

## Verification and retirement

Each stage's check, as a test of one name put to both checkers (`curios/src/tests/rules/totality_nested.rs`, and `curios/src/tests/derive/ord.rs` for stage 4), then `cargo xtask compiler lint`, `cargo xtask jig lint`, the suite and `cargo xtask docs build`. The decision's *A call in a callback is graded where its callee applies it* and *A call is read off a path* are restated with the rules as they land. The figures the stages may move are the runaway table (`curios/src/tests/instrumented/runaways.rs`) and what `/std`'s build spends in `group_applications` and `definition_applications`. Once the four stages have landed, this file is deleted, the open question filed first.
