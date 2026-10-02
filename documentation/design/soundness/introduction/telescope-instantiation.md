# Telescope instantiation

**Assumes.** A dependent telescope is opened at actuals already checked, in order, and plicity is part of a function type's identity.

**Evidence.** Probed incidentally, never directly. One assumption serves five typing rules — an application's arguments, a projection's field, and the three literal forms — and four conversion sites: binder comparison, tuple eta (opened at the left side's projection), nominal argument comparison (at the left instance's actuals) and struct eta. Each opens the rest of a telescope at what it has already accepted, and is sound only because the earlier positions were established first. No fixture states the shared rule, so a fifth site is held to it only by review.

The plicity clause is the half able to admit: conflating an explicit binder with an implicit one would apply a value through the wrong convention, and erasure masks by plicity, so the two differ in what survives. Conversion's function and function-type arms and subsumption each guard it ([Plicity is part of function identity](../../types/plicity-is-part-of-function-identity.md)).
