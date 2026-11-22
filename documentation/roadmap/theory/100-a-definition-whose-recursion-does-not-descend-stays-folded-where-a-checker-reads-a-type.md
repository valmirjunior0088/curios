# A definition whose recursion does not descend stays folded where a checker reads a type

**Adopted by the user on 2026-11-22; not refined yet.** Obligation (T) of [Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md) refuses a type that reaches a definition not known to terminate, reading the reach by mention, where a checker unfolds only what reduction forces; this specification narrows (T) to what a checker can unfold, by keeping folded in both checkers a definition whose own recursion does not descend. Its design is open, under *Open questions*; its consumers are `/std/Io/all` and `/std/Try/all`, below.

## What is wrong today

**A type naming a loop it never applies is refused, and the verdict follows the spelling.** (T) reads its reach by mention (`curios-elab/src/totality/reach.rs`), and a definition that mentions a partial one is partial itself. With `let spin(n: Nat) -> Io(Nat) = Io/bind(Io/yield(), (()) => spin(n));`, the parameter type `Vec(Io(Nat), List/len(List/map([1, 2, 3], spin)))` is refused ("it reaches `spin`"), though `List/len` over a map reduces to its list's length "whatever `f` does" (`curios-core/src/reduce/intrinsic.rs`); the same type formed by instantiating `counted(work: List(Io(Nat)), v: Vec(Io(Nat), List/len(work)))` at `List/map([1, 2, 3], spin)` is accepted. Over `concept Joint(T: Type): Type { Answers: Type, run(T) -> Io(Answers) }` witnessed at `{Io(A), Io(B)}` with `Answers = {A, B}` and `run(t) = Io/both(t.0, t.1)`, a call of `let all_of(@T: Type, use Joint(T), actions: T) -> Io(Joint/Answers(@T))` is refused ("it reaches `/witness~/Joint({_, _})`"), where the field the type reads is `{A, B}`.

**`Io/all` and `Try/all` answer a `List` that states no length**, though it is the length of the list they were given. `all(@A: Type, actions: List(Io(A))) -> Io(Vec(A, List/len(actions)))` elaborates — `promised` and `awaited` build the count as they recurse, `Eq/subst` carrying it to the result — but is refused at every caller that maps an action that loops, a send, a receive, a read, `/std/Tui`'s `running` and a fail-fast `Try/all` among them. A caller reading the answers of a list written out by position takes an `Option` for each, or matches a case the list cannot be in, where the counted signature lets `List/get` read each answer off the vector's list, its bound discharged by the vector's count where the call is.

## What is known

- **One of the decision's two reasons for the aggressive reading no longer holds.** It says the precise reading "buys nothing (V) does not already refuse"; it buys the programs above. The other, that a total type-level function applied to a partial value reties the knot (`Shape(inf)`), stands against reading what a type scrutinizes, not against holding a loop folded.
- **The peers keep such a definition folded.** [Lean](https://lean-lang.org/doc/reference/4.26.0/Definitions/Definitions/): a `partial def` is an opaque constant, never δ-reduced; [Agda](https://agda.readthedocs.io/en/latest/language/termination-checking.html): a `NON_TERMINATING` function does not reduce during type checking; [Idris](https://arxiv.org/abs/1610.07978): a partial function is not evaluated during type checking. Neither checker unfolds a definition whose own recursion does not descend, an inline group's included, so `Shape(inf)` and `Bad` stay folded and no knot is tied. Lean asks an opaque constant's type to be inhabited because a proof could use it, which is (V)'s job here.
- **The reading this points at**: (T) narrows to the declaration being judged, whose verdict comes only after its elaboration, and a definition that merely mentions such a root unfolds as any other.
- **What it touches**: both checkers' reduction, the verdict the totality analysis publishes, the decision, and the knot tests of `curios/src/tests/rules/totality_partiality.rs`.

## Open questions

- Whether (V) reaches through a proof's type arguments, as a bound discharged over a mapped list asks it to.
- What the decision's "the expression would have returned" says of an erased type naming a folded loop.
- How a group's own members unfold while it is elaborated, before its verdict is known.

## Verification and retirement

Check, once refined: the three refused programs above accepted, in both checkers; `Shape(inf)`, `Bad` and the rest of `totality_partiality.rs`'s knots still refused; `Io/all` and `Try/all` restated at `Io(Vec(A, List/len(actions)))`, with a test reading the answers of `Io/all` over a list written out through `List/get` of the vector's list. Once it lands, the decision is restated to the narrowed (T), its **Rejected** paragraph gaining the aggressive reading, and this file is deleted.
