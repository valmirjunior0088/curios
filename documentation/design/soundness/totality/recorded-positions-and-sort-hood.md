# Recorded positions and sort-hood

**Assumes.** Every `Prop`-typed and every type-position term an item's check meets is recorded, classified while its binders are still in scope.

**Evidence.** Argued. This is the kernel's output rather than an input — nothing in it decides whether a term is well typed — and it is what (T) and (V) are seeded from, so a position it fails to record is one those obligations never see.

**A position is classified when recorded, never afterwards.** A position's type mentions binders the item opened, which retract when the item's check returns, so a later pass could not ask for their sorts and could only fail, silently. Classifying at every record site works because of three properties: which half a type's positions belong to is remembered beside the kernel's sorts and under their lives ([The evaluation memo](../conversion/the-evaluation-memo.md)), so the cost is one question per type for as long as what its sort was read off stands, and a type whose sort an arm's equation decides is classified under each arm's own; a classification that could not be decided is kept and surfaced with the drain, since a recording site returns nothing; and the walk is re-entrancy guarded, since deciding a position's erased half types terms of its own.
