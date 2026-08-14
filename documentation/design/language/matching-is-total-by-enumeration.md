# Matching is total by enumeration

**Decision.** Every match must cover its scrutinee. Arms enumerate constructors without row priority, nested patterns compile through the pattern matrix by full enumeration, and an omitted arm is legal only when index inversion proves it impossible.

**Rationale.** Arm order never changes meaning, coverage is a checked property rather than a runtime default, and impossibility is discharged by the type system instead of an unreachable-arm convention.

**Rejected.** First-match-wins row priority with catch-all defaults.
