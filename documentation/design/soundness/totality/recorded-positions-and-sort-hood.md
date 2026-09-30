# Recorded positions and sort-hood

**Assumes.** Every `Prop`-typed and every type-position term an item's check meets is recorded, classified while its binders are still in scope.

**Status.** **argued**. This is the kernel's output rather than an input — nothing in it decides whether a term is well typed — and it is what (T) and (V) are seeded from, so a position it fails to record is one those obligations never see.

**A position is classified when recorded, never afterwards.** A position's type mentions binders the item opened, which retract when the item's check returns, so a later pass could not ask for their sorts and could only fail, silently. Classifying at every record site works because of three properties the component holds itself: sort-hood is memoized per distinct type, so the cost is one question per type; a classification that could not be decided is kept and surfaced with the drain, since a recording site returns nothing; and the walk is re-entrancy guarded, since deciding a position's erased half types terms of its own. The memo's keys name the item's own binders, so it is cleared with the drain; nothing tests that the clearing and the item boundary cannot come apart.
