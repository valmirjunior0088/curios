# A command compiles only as far as its answer needs, and a project keeps what it compiled

**Decision.** Every command drives the pipeline to the nearest point its answer can be read from — the judged units for `lint` and a `wonder` query, the payload for `run` and `compile`, the pages for `document` — and a project keeps what it compiled on the way: each unit compiled whole is filed in the store, so the next invocation of any command starts from there rather than cold. A unit compiled over a stored baseline is filed on [A stored unit is a baseline for an item-level recompile](../architecture/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md)'s terms. Text an editor holds unsaved is compiled over its filed baseline and kept for the session, never filed. A loose file and standard input file nothing: neither has a project, hence no store ([An argument names one subject, and each command states what it accepts](an-argument-names-one-subject-and-each-command-states-what-it-accepts.md)).

**Rationale.**

- **A question that files nothing starts cold.** Every unit no build has filed is compiled again by every `lint`, every `wonder` query and every server session that reaches it, so the commands asked most often pay the most.
- **Filing is idempotent.** The store addresses a unit by what it was compiled from ([Cached verdicts](../soundness/admission/cached-verdicts.md)), and a slot is one file renamed into place, so two commands filing one unit write the same bytes, and a store that cannot be written costs the reuse and never the answer.
- **Text on disk, not every keystroke.** A server compiles on every edit, and what it compiles from unsaved text the next keystroke replaces; filing it would write a slot per edit for text that may never be saved. Text on disk is what the next command reads, so it is what is worth keeping.
- **The nearest point, not the furthest.** A payload is native code the runtime compiles, and the crates a question runs in link no runtime ([`curios-wonder`'s README](../../../curios-wonder/README.md)), so a question stops at the judged unit and a build goes on to its product.

**Rejected.**

- **A question that files nothing**, which spares a server a slot per keystroke at the price of starting every question cold; filing text on disk alone spares it that without the price.
- **Filing unsaved text**, a slot per keystroke for text the next keystroke replaces and nobody may save.
- **A question that builds past its answer** — a payload for a `wonder` query — which needs a runtime a question does not link and spends compile time no answer reads.
