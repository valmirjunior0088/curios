# A witness reads its own superclasses as the table's

[Concepts resolve with global coherence](../../design/surface/concepts-resolve-with-global-coherence.md) holds that "a superclass is the table's, whatever a field's type names", and that it reads alike through the table and through a witness (`a_superclass_reads_alike_through_the_table_and_through_a_witness`). Inside a witness's body, read through the witness's own entry, it now reads so once the witness's turn finds it known to terminate, a lone witness being a group of one (`elaborate_module_let`, `curios-elab/src/elaborate/module.rs`), so a written or derived law over its superclasses that recurses through the entry converts (`curios`'s `tests::concepts::recursion` and `tests::derive::key`). What is left is the documents that state the rule, and one that rests a rejection on the gap the reading closed.

## What is wrong today

- **The rule's owners do not state the reading inside a body.** The coherence decision's superclass paragraph, `documentation/language.md`'s witness section and [A witness body may be written by the compiler](../../design/surface/a-witness-body-may-be-written-by-the-compiler.md)'s `Hash/Key` bullet say nothing of a witness read through its own entry, nor that a key derives over a family that holds itself.
- **A rejection rests on the closed gap.** [Equality is decided where a decider exists and is `Eq` everywhere, joined by one law](../../design/theory/equality-is-decided-where-a-decider-exists-and-is-eq-everywhere-joined-by-one-law.md) rejects an order's tie stated decided because "inside the witness of a family nested through a container that superclass is the one of the witness under definition, so the container's `==` written in the body and the one in the law's type do not convert there".

## Goal

A witness that resolves through its own entry is a group of one while its body elaborates: its entry is its slot, read through once its turn finds it known to terminate, as every member of a group is read in both checkers. A law stated over a superclass converts with the registered witness's method wherever the body reads it through the entry, and a key derives over a family that holds itself, directly or through another key's type. No rule of the certifier changes.

## Settled

- The rule is the turns': one reading of a member under way, a lone witness's and a group member's, the elaborator's and the certifier's alike. A member not known to terminate stands for nothing, its superclass slots included.
- Rejected, a field rule, under which a projection of the entry at a field whose term names no member reads as that term whatever the recursion: beyond the turns it admits only a witness whose recursion does not descend, which no lawful witness is, at the cost of a rule the certifier must admit, argued under `documentation/design/soundness/`, and a reading of a lone witness apart from its group's.
- Rejected, a derived body recursing through a local function of its own, as Lean's deriving handlers define the bare functions in a `mutual` block and build the instances from them: it leaves the written key refused, gives derivation two recursion shapes and refuses a payload nested through another key's type. A derived witness keeps one shape: every derived law recurses through its own entry.
- `own_methods_read` keeps its reader: a body that reads its own methods only where they name no member names itself nowhere and is no knot at run time, which the turns do not decide.
- A superclass slot's resolution never passes through the entry: a concept is not its own superclass, and a premise is strictly smaller than its witness's head, so the loop GHC's superclass invariant refuses cannot be written. A slot that names a member is withheld with it.
- The equality decision's rejection of a decided tie is dropped with the reason, not restated.

## Verification and retirement

Verified by the programs of `curios`'s `tests::concepts::recursion` and `tests::derive::key` that read a witness through its own entry, each certified as it runs, and `/std` built. Retired once the documents above are restated: the coherence decision's superclass paragraph to the reading inside a body, `documentation/language.md`'s witness section to a lone witness reading its own entry as a group's member does, the derivation decision's `Hash/Key` bullet to a key over a family that holds itself, and the equality decision's rejection of a decided tie dropped.
