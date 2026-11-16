# A call in a callback is graded where its callee applies it

This specification describes what remains of a capability that has landed in part. A recursion through a callback a definition applies is graded where the definition applies it ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md)): a tree's `has_zero` recursing through `List/any(children, (c) => has_zero(c))` is total (`curios`'s `a_call_in_a_callback_descends_where_its_callee_applies_it_below`), and so is one through `/sys/List/map`, `/sys/List/fold`, a definition of no group, or a definition that hands its callback on to another that applies it, and `/std/Json/encode`, whose object arm recurses on a field of each pair it maps. What is still graded as fresh is a derived witness on a type whose children sit in a container.

## What is missing

**A call through a callee's field.** A derived `Eql` on `induct Rose | rose(Nat, List(Rose))` compares the children through `/std/List/witness~/std/ops/Eql(List)(Rose, W).0(x4, x6)`: `List`'s witness applied, projected to its method and applied. A summary states how a definition's leading lambdas apply its parameters, and the method is a lambda field of the record those lambdas answer, so nothing is filed of what it applies — though the method, which hands its `use` parameter on to `List/eql`, applies it as `List/eql`'s summary states.

## Stages

1. **A call through a callee's field.** A summary is kept for each part of a definition, as for its leading lambdas, and a path rooted at a definition is graded by the summary of the part it reaches. Check: a derived `Eql` on `Rose`, reached by a type, passes both checkers, with a refusal where a container's method applies the witness at its own argument; the record's format raised where a summary's archived form moves.

## Retirement

Once the stage lands, the totality decision states it. Then this specification is deleted, and the soundness findings' entry on a call read off a path names the premises it added: a projection below its record, and a summary composed through a callee's.
