# A motive is a term, not a grammar

**Decision.** A match motive is a term checked against the eliminator's motive type, `(ī : Ī(p̄)) -> I(p̄, ī) -> Sort`: it binds one name per index and then the scrutinee. Parameters are never abstracted, and the eliminated family is never written.

**Rationale.** The motive is morally a lambda, so making it one answers every question a bespoke grammar answers badly. Binder annotations are ordinary types in ordinary positions, so plicity is expressible — a family declared `induct Eq(@A : Type)` is annotated `Eq()(s, t)`, as everywhere else — and whether a position binds is decided by the syntax, not by whether a name resolves. A parameter abstracted in the motive is abstracted to the same term in the match's type and in every arm, so dropping them costs nothing. A malformed motive is a type error at the motive, not a silent reinterpretation. With no constant rung there is no ambiguity to resolve — a constant motive may itself be a Π type that checks both ways — and the parser calls `parse_term` without backtracking, since `|` is no infix operator and a motive ends at the first arm.

**Rejected.**

- **A three-rung ladder** — constant, scrutinee-bound, and an annotated type-pattern over the inductive's flat parameter-then-index slots.
- **Eliminators as first-class functions** taking the motive as an argument, which would need a generated matcher per match site. `Match` stays an intrinsic node whose arms carry per-constructor index refinement, and a checked-term motive keeps that door open.
