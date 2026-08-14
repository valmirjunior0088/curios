# Concepts resolve with global coherence

**Decision.** Ad-hoc polymorphism is concepts and witnesses. Witness resolution consults one program-wide table under global coherence checks, and anonymous witnesses fill structure the goal already determines.

**Rationale.** Coherence makes the chosen witness a fact about the program rather than about the scope of the call site, so moving code or reorganizing imports cannot silently change which witness runs.
