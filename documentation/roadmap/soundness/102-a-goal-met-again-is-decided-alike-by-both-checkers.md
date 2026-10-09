# A goal met again is decided alike by both checkers

**Not refined yet.** This specification reserves one key for a goal met again in a comparison, so that an equation the elaborator accepts by recurrence is one the kernel answers. It is not an implementation plan.

It is held to [Conversion is one relation in both checkers](../../design/soundness/conversion-is-one-relation-in-both-checkers.md), where a verdict follows neither the checker that asked nor the path it took to the question, and to [Conversion recurrence](../../design/soundness/conversion-recurrence.md), where a goal met again in the conversion history is assumed.

## What is missing

**The two recurrence keys decide two relations, and the kernel does not answer where the elaborator accepts.** With `let f(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => f(k) + 2 end;` and `let h(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => 2 + h(k) end;`, `let same(n: Nat) -> Eq()(f(n), h(n)) = Eq/refl();` elaborates, and the kernel's comparison of the two calls unfolds a round after a round: the arms compared, their sums put to the intrinsic chain, the two calls it leaves compared in turn. The elaborator's key renames the binders its conversion minted and records no local context, so `f(k) ≟ h(k)` under an arm's fresh `k` is the goal met again and is assumed; the kernel's key records the context, one binder longer each round, so no round meets a goal again ([Conversion recurrence](../../design/soundness/conversion-recurrence.md)). The same two functions written with one text are accepted by both. Nothing is admitted by it: the kernel refuses by its budget or, by [A comparison costs what it walks](100-a-comparison-costs-what-it-walks.md), does not answer; but [Conversion is one relation in both checkers](../../design/soundness/conversion-is-one-relation-in-both-checkers.md) holds a verdict to follow no checker. **Uncertain** as to the cause, which is read off the decision and one stack sample of the kernel (`ground_cases`, `convert_intrinsic` and `History::enter` in a cycle), not stepped.

## Refinement

- **Which key both checkers take.** Not chosen between two: a goal is met again only with its context, in both checkers, so the elaborator refuses and the equation is a lemma by induction; or both take a goal up to the binders its own comparison opened, which is an induction principle in the kernel and needs its argument written before it is a rule. Check: the program refused by the elaborator or accepted by both, within seconds at the default budget, and a row of the law audit that holds whichever is chosen.
