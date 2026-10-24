# Boolean and bitwise terms meet in a normal form

**Not refined yet.** This specification reserves conversion reasoning about relationships among comparison atoms and further natural bitwise laws, past today's local identities and the decision diagram's agreement. It is not an implementation plan. It waits for a consumer that needs what it decides, and refinement establishes its fragment, algorithm contract and acceptance criteria before any implementation.

## What is missing

Two `Bool` terms with a connective between them are compared by a decision diagram over their atoms, a rung of `curios-analysis`'s conversion chain, each comparison known by what it states and its negation as the same atom negated, and beside it conversion holds local identities alone: no relationship among comparison atoms and no bitwise law past those identities is decided.

## What is known

- **Previously discussed.** Algebraic normal form for natural bitwise operations.

## Refinement

- Which representations serve which operations, how arithmetic relationships enter the Boolean procedure, and which complexity limits keep work predictable.
- Agreement, impossibility and failure to decide need separate contracts; disagreement over independent opaque atoms is not automatically a realizable counterexample.

It retires when conversion decides the relationships among comparison atoms and the bitwise laws refinement chooses, each verdict under the contract it states.
