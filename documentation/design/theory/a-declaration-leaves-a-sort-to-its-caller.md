# A declaration leaves a sort to its caller

**Decision.** A sort is a quality beside its level: `Prop`, `Type`, or one its declaration leaves to each caller, written `Sort`. A top-level `let`, `induct` or `struct` writes `Sort` in its signature, each written one a parameter of the declaration as each written `Type` is a level of it, and two the declaration requires equal are one. An occurrence of the declaration states a quality for each beside its levels, by inference and with no syntax of its own. The elaborator checks the declaration once with those qualities unknown, each of its readers answering for an unknown one as it answers for a quality nothing has solved: no proposition for irrelevance or for eta, a kept slot for erasure, no elimination of a proposition a type would refuse, and the termination a proof owes. The kernel judges the declaration once at every assignment of `Prop` and `Type` to its qualities, on the declaration read at the assignment, and judges no term that holds an unknown one; a declaration leaves at most eight. A family's own sort is `Prop` or `Type`. `Sort` is refused where no caller states it, in a body, a local definition, a `concept` and a `satisfy`, and where the declaration's own terms decide it, and a tuple type over two of a declaration's qualities is refused. `/sys` declares each former and operation over a `Sort` wherever its row calls an operand a type, and `/std` writes `Eq` with its lemmas, `Eq/subst`, `Nat/descend` and `Option` over one.

**Rationale.** With no proposition admitted where a type is wanted ([Subsumption is a relation, not a traversal order](subsumption-is-a-relation-not-a-traversal-order.md)), a definition that serves both sorts is written once for each, and nothing a caller does chooses between two bodies that are one text. A parameter states what the two copies share: the definition is one term, a use picks the sort as it picks a level, and being a proof stays a fact about a type under every substitution, since a use at `Prop` and a use at `Type` are two instances and no one type is both.

The kernel reads which of the two a sort is at every rule that tells a proof from data: irrelevance, eta, the elimination guard, the obligation an erased position carries. Each rule has its own safe answer for a sort that states neither, and an argument that the answers agree would be the kernel's to carry. Judged at each assignment, the declaration is a term at one sort wherever a rule reads it, so the rules stand as they are, a store keyed by a term holds each term at one sort, and what is accepted is what would be accepted of the declarations written out, one for each assignment. That is two to the count of qualities judgments, each under the whole budget: the bound keeps the count small, and the word keeps it chosen, a declaration that writes no `Sort` being judged once.

The elaborator checks once because it reports to a reader who wrote one declaration. Its answers for an unknown quality cost a refusal where the declaration is written and never an acceptance: what it accepts the kernel judges at each assignment. A value at an unknown sort is a proof at one use, so it owes the termination a proof owes ([Totality of the erased program](../soundness/totality-of-the-erased-program.md)), and it is data at another, so it is given no irrelevance.

A family's own sort stays `Prop` or `Type`, so the rules read off a declaration, inversion's guard, the pinned payload and the elimination guard ([`Prop` is strict, proof-irrelevant, and definitionally K](prop-is-strict-proof-irrelevant-and-definitionally-k.md)), read a literal, and no relation between two unknown sorts has to say what one eliminates into. A quality is a parameter of a top-level declaration alone and is counted from no binder: a recursive group inside a term binds none, and reading a declaration at an instance states its qualities through every group it holds.

Erasure keeps a slot for what a caller's variable types, a function a parameter for each binder and a tuple a field for each member, so a proof handed to a definition generic in a sort fills the slot a type's value would, and no runtime shape follows the sort.

A `Sort` the declaration decides is refused because a caller could state the other sort and be answered by a term checked at this one; one in a body or a local definition has no caller to state it, every use standing inside one declaration.

**Rejected.** `Prop u ≤ Type v`, which the subsumption decision rejects.

A kernel that judges an unknown sort, as Rocq's does: every reader of a sort takes an answer for one not known, safe in a direction of its own, where judging each assignment takes none. The two checkers share the term representation, so it stays reachable.

A copy of the declaration for each assignment, named before the kernel: the kernel is untouched, and every stage after it, every registry slot and every report learns an instance's name.

Every `Type` in an input position a caller's choice: no word is added, a signature no longer says whether it takes a proposition, and the kernel judges a declaration at two to the count of the types it writes.

A box the elaborator inserts where a proposition meets a `Type`: two proofs under a box stand apart wherever neither is a literal, and every use at a proposition runs through a wrap and an unwrap no source wrote.

Irrelevance by where a value is used, with no sort of propositions: a function has no irrelevant result, and a proposition is neither declared nor quantified over.

A quality carried on a term metavariable, or on the level's: a reader of a type needs its level at once where its quality may wait, and a proposition's level and a type's are identified wherever they are compared.

A family whose own sort is a caller's: what one sort eliminates into another is then a relation between two unknowns, which every rule read off a declaration would consult.

A recursive group inside a term that binds a quality of its own: a quality is counted from no binder, so one the group bound is read as the enclosing declaration's wherever a term holds both.

A `Sort` in a `concept` or a `satisfy`: a witness is found by the key its signature spells, and what a key at an unknown sort matches is not designed.

A tuple type over two of a declaration's qualities, taken for a type: it is a proposition at the assignment that states `Prop` for both, where the elaborator's one check would have read it as data.
