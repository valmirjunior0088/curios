# A group instance keeps its levels beside its group

**Not refined yet.** This specification reserves holding a recursive group at a universe instance as the group and its levels, as a global at an instance is held, where today the instance is the group with its levels substituted through every member. It is not an implementation plan: it waits on the design under *Refinement*.

## What is missing

**An instance is known by the term it is.** `Caches::opened_instance` (`curios-elab/src/context/caches.rs`) files each instance it opens under the instantiated group, its levels substituted through every member, beside the group and the levels it came from; the record is "the one way an instance is opened, so that every one a term comes to hold is known for the instance it is" (`Context::opened_instance`, `curios-elab/src/context.rs`), and a term that names a group's members again reads an instance by that record alone (`Context::member_names`). An instance whose levels are rewritten after it was opened — a level solved and zonked through it, or identified with another — is another term, known to no record, so its projection stands as a copy of the group where its member's name would.

**Over `/std` at `68ca73258`**, 23 answers of the reduction table's second door are such a projection: `/sys/Option.{Type; ?u}(/std/Command/Pipe)` and the like over `/sys/Option`'s, `/std/Test/Test`'s and a `/std/Map/internal/Node` group, where reducing the asked spelling, the same group at other levels, leaves the group standing; the two convert, and with the door removed seven `/std` bodies move (`/std/Nat/Scale/unique`, `/std/Nat/Gcd/step`, `/std/Nat/DivMod/div_lt_of_lt_mul`, `/std/Nat/DivMod/le_div_of_mul_le`, `/std/Nat/DivMod/unique`, `/std/Nat/Coprime/quotients`, `/std/Map/bits/within`). Counted by reducing every door answer again with the door bypassed, in a copy of the tree. The procedure that proves a bound meets a reading of the kind: once `/std/Nat/log2/log2`'s head has been reduced, `Nat/log2(n)` reads as the call's body with a copy of the group inside it, so it asks the names a report gives an atom only once its search is over (`Facts::beneath`, `curios-elab/src/entailment/facts.rs`).

## What is known

- **A global at an instance holds its levels apart.** `Instance { head, universes }` keeps a definition shared and its levels a vector, so a rewrite of the levels rewrites the vector, and what the instance is stays readable. A group's instance substitutes the levels into every member's body instead, which no record can follow through every rewrite.
- **Which rewrite makes the unrecorded spellings is not known**, and need not be: held apart, no rewrite of the levels makes another group.
- **What the record and the door answer.** Counted at `f0712f8e4`, by counters a scratch copy kept at `Context::member_names`, `Context::instance_of` and the second door, taken at each budget sample over `cargo xtask profile-prelude elaboration`. Asked for the names a group's members go by, the record finds them 291 times and none 270 times, where the group stands as its copy; asked for an instance, it answers 262 times and misses 8. The second door is asked 708,804 times, finds a stored spelling 385,850 times and serves 379,856 of them, declining the 5,994 whose levels or instances do not carry. Which of the served answers are a group's copy, the reading above, was not counted again.
- **Both checkers hold groups.** The kernel reads a recursive member as its group projected (`curios-core`'s `Rec`), so the representation is the trusted base's and both checkers change with it.

## Refinement

- **The design.** A group at an instance as the group and a universe vector, unfolded to its members' bodies where a reduction asks, in `curios-core` and both checkers; the instance record and the second door each kept only where a measurement still finds what they answer.
- **Check**: two spellings of one instance, one opened and one rewritten, reduce to one spelling; the counts above retaken; `/std`'s image.
