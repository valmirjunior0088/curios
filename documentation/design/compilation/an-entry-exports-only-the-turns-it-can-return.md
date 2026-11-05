# An entry exports only the turns it can return

**Decision.** A module registers its suspension row as a representation identity (`curios_cps::Module::suspension`), whether or not its entry can return a value of it. Whether it can is read off return flow: `curios-cps`'s `row_flow` computes the rows that may reach each value as a may-set, conservative over opaque boundaries, recursion and return positions, and `entry_suspension` answers the suspension row where an entry result may carry it and nothing where return flow excludes it. The emitter reads that answer where the optimized CPS graph is lowered to its machine IR, after every optimizer rewrite and caching nothing across them (`curios-emit`'s `machine.rs`), and exports accordingly: an entry whose results exclude the row exports `ENTRY` alone and answers its value directly, and one whose results may include it exports `ADVANCE` beside `ENTRY`, both answering turns, the two names `curios-abi` states and both hosts drive ([`curios-native`'s README](../../../curios-native/README.md#a-program-is-advanced-turn-by-turn-and-the-host-waits-only-between-turns)). The row roster stays complete, a suspension row whose constructions the optimizer removed included; the dispatch types a turn needs are required by the ABI selected, and a surviving call or closure requires its own.

**Rationale.**

- **The lowering introduces suspension handling the optimizer removes**, and a suspension returned inside a program need not escape its entry. Return flow answers the host-boundary question, where the row's registration answers representation's: constructors, projections and the verifier read one identity, and the entry one capability.
- **One interpretation of opaque boundaries.** The row analysis that simplifies row tests is the one that selects the entry's ABI, so a call through a closure, a foreign row and a recursion are read alike by both.
- **A foreign row's asynchronous reply is part of its contract.** The guest code that turns a later reply into a suspension is a construction like any other, so a host that answers at once establishes nothing about another valid host, and the ABI is chosen for every host ([A host operation is a foreign declaration whose types are its whole contract](../runtime/a-host-operation-is-a-foreign-declaration-whose-types-are-its-whole-contract.md)).

**Rejected.**

- **Selecting the turn ABI from the row's registration**, which keeps an advance a residual entry cannot use.
- **Clearing the row identity where no construction survives**, which conflates representation with capability.
- **Searching for a surviving suspension construction**, which takes a value consumed inside the program for one escaping its entry.
- **A cached flag on the mutable CPS graph**, which needs invalidation at every rewrite.
- **Assuming the host's own operations answer at once**, which changes the foreign contract.
- **A second effect analysis beside row flow**, for the same question.
