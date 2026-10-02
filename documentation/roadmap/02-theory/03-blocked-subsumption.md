# A subsumption blocked on a metavariable waits as a subsumption

**Not refined yet.** This specification reserves the elaborator's completeness gap at a blocked subsumption. It is not an implementation plan.

## What is missing

[Subsumption is a relation](../../design/theory/subsumption-is-a-relation-not-a-traversal-order.md), decided structurally in both checkers. When the elaborator's `subsume_telescope` meets an unsolved metavariable it abandons the walk and hands the pair to conversion, which is stricter: the function-type case fires only on two rigid Πs, and `Context::park` freezes the frame. So a program the relation admits, whose types become known only later in the item, is refused. The refusing direction: nothing unsound follows.

## Previously discussed

A `ParkedWork` variant carrying the relation, so a blocked subsumption parks as a subsumption and resumes as one.
