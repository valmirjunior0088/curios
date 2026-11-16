# A witness whose method reaches itself descends through that method

**The benefit.** A witness whose method recurses on its argument is total, and a type may reach it: a derived `Eql`, `Ord` or `Spell` on any recursive type, the first witnesses a program over a tree derives, and a hand-written `satisfy` whose method calls itself on a payload. Today every such witness is refused wherever a type reaches it, and `/std` writes each recursive method of its own as a definition beside its witness to stay clear of the refusal (`List.crs`'s `eql = eql`). The project has decided to adopt it.

**Not refined yet.** Two designs are named under *Refinement*, and neither is drawn: choosing one is the refinement.

## What is missing

**A recursive witness is graded as a call of the witness with no arguments.** A witness whose method reaches itself elaborates to a group whose member's body is the concept's literal, `/witness~/Count(/Tree) = /Count { (t) => match t | leaf(n) => n | node(l, r) => (W.0)(l) + (W.0)(r) end }`, so size-change counts no parameter of it: `curios-analysis`'s `Member::of` peels a member's leading lambdas, and the method's lambda is a field of the literal. The recursive call `(W.0)(l)` is a projection of the member applied, graded as the member named bare, a call with no arguments, and the group's self-call is a matrix with no diagonal to descend on, so the witness is not known to terminate, in both checkers alike. A witness under a telescope, `(@A, use Show(A)) => Show(Tree(A))`, hands its own parameters on unchanged and closes to no decrease either.

So `Holds(Count/count(t) == 0)` over a written `satisfy Count(Tree)` whose `count` recurses on the children is refused, and so is `Holds(a == b)` over `satisfy Eql(Tree) { .. }` for a recursive `Tree` ("it reaches `/witness~/std/ops/Eql(/Tree)`, which is not known to terminate"), though each method descends on its argument.

## What is known

- **The rule is the totality decision's.** [Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md) grades a recursive group by size-change over its members' calls, and states nothing of a call reached through a member's projected method; a witness whose method hands back its own argument does not descend, and stays refused whichever design lands.
- **Both checkers judge it, and both have to move together.** The elaborator's discovery walk and the certifier's call recorder count a member's parameters the same way (`Member::of`, `record_call`), and a change on one side alone is a disagreement the board's probe shows, which fails closed where the certifier is the narrower and is a ticket where it is the wider.
- **`/std`'s workaround is one of the designs, by hand.** A recursive method written as a definition beside its witness is graded as any recursive definition is, which is why the library's own recursive witnesses are admitted today.

## Refinement

- **The design, one of two.** (a) Size-change reads a member whose body is a concept literal of lambdas as one member per method: a call `(W.i)(args)` is graded as a call of method `i` against its lambda's parameters, in `Member::of`, `group_applications` and both checkers' call recording, so the trusted base changes and a summary names a method's parameters. (b) Elaboration writes a witness whose method reaches itself as a group of its methods as definitions, each calling the others by name and the witness's literal naming them, as `/std` does by hand, so the certifier sees ordinary recursive definitions and the trusted base does not change; the cost is in elaboration, and in what the methods' definitions are named and how a report spells them.
- **Check.** Both programs above pass both checkers; a witness whose method hands back its own argument stays refused by both; and each of `/std`'s recursive methods written beside its witness is either written as one witness again, in its own change, or kept, with the reason stated where it stands.

## Retirement

Once the design has landed, its rule stated where the totality decision states how a group's calls are graded, and nothing references this specification, it is deleted.
