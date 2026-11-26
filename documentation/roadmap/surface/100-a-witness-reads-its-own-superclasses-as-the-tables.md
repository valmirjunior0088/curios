# A witness reads its own superclasses as the table's

[Concepts resolve with global coherence](../../design/surface/concepts-resolve-with-global-coherence.md) holds that "a superclass is the table's, whatever a field's type names", and that it reads alike through the table and through a witness (`a_superclass_reads_alike_through_the_table_and_through_a_witness`). Inside a witness's own body, read through the witness being declared, it does not: a law stated over the witness's superclasses is refused where its proof recurses through the witness, written or derived.

## What is wrong today

- **A written key recursing through its own entry is refused.** Over `induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end` with `Eql`, `Ord` and `Hash` derived, `let inner: Eq()(Ord/ord(l, r), Bytes/ord(Hash/hash(l), Hash/hash(r))) = Hash/Key/mono(l, r);` inside `satisfy Hash/Key(Tree) { mono = … }` is refused at `Hash/Key/mono(l, r)`, which resolves to the entry being declared: expected `Ord/ord(@Tree, use x0.1, x1, x2)`, the entry's own superclass slot, against the registered `Ord(Tree)`'s comparison (probed at `77ed86f49`). A written key recurses today only through a lemma of its own, as `Nat/hash/mono` does.
- **A derived key over such a family is refused alike.** `satisfy Hash/Key(Tree) { .. }` is refused with a type mismatch at `Hash/Key/then`'s `part`: a recursive payload's part is `Hash/Key/mono` at the witness's own entry, stated over that entry's superclasses, while the comparison the derived `Ord` writes reads the registered `Ord(Tree)` (probed at `0583a8573`).
- **The rule is stated, applied after the refusal.** `own_methods_read` (`curios-elab/src/elaborate/module/own_method.rs`) replaces a read of the witness's own field that mentions the witness nowhere by that field's term, "the witness unfolded once and projected", but over the zonked body; while the body elaborates, the witness is its group's local (`Context::within_group`, `curios-elab/src/context/scope.rs`), and conversion reads a projection of it as stuck. A superclass slot is such a field: resolution fills it before the body is written, with another witness.
- **A rejection rests on the gap.** [Equality is decided where a decider exists and is `Eq` everywhere, joined by one law](../../design/theory/equality-is-decided-where-a-decider-exists-and-is-eq-everywhere-joined-by-one-law.md) rejects an order's tie stated decided because "inside the witness of a family nested through a container that superclass is the one of the witness under definition, so the container's `==` written in the body and the one in the law's type do not convert there".

## Goal

Inside a witness's body, a projection of the witness's own entry at a field whose term names no member of its group is that term, in the elaborator's conversion and in the certifier, so a law stated over a superclass converts with the registered witness's method wherever the body reads it, a recursion through the entry included, and a key derives over a family that holds itself, directly or through another key's type.

## Settled

- The rule is `own_methods_read`'s, read where conversion meets the projection rather than after the body is zonked.
- It is sound because the field's term names no member of the group: the equation holds of the group's fixpoint whatever the recursion builds, and the term it reduces to cannot enter the recursion again. It rests on resolution filling a superclass slot before the body is written and never through the entry itself, the loop GHC refuses where an instance's superclass would be solved through its own dictionary; the rule checks that the field names no member rather than assuming it.
- Agda reads a field of an instance defined by copatterns as its clause under that projection, and OCaml's `module rec` types a self-reference by a signature whose manifest parts are known: each reads what is fixed of a recursive definition before its body, and nothing the body builds.
- A derived witness keeps one recursion shape: every derived law recurses through its own entry, the key's as `Eql`'s, `Ord`'s and `Hash`'s do; no derived body writes a lemma of its own.

## Open

- How the certifier reads it: its group's local carrying the terms of the fields that name no member, or a projection of a member read off the member's body under the same check. The certifier is never handed such a body today, elaboration refusing first.
- Whether `own_methods_read`'s rewrite of a method that names no member then has a reader left beside the run-time knot it removes.
- The decision the certifier's rule is argued in, under `documentation/design/soundness/` as a rule that can admit a term.

## Stages

1. **The certifier reads a member's field that names no member as its term.** Check: a recheck test of a group whose member, a concept literal, states a law over its own superclass slot and is accepted, beside a twin whose slot names a member, refused; each mutation-checked. Its decision written.
2. **The elaborator's conversion reads it alike.** Check: the written key over `Tree` above, accepted under one test name in both checkers, in `curios/src/tests/derive/key.rs`.
3. **A key derives over a family that holds itself.** Check: `satisfy Hash/Key(Tree) { .. }` over the `Tree` above and over a family whose payload is a `Box(Tree)`, each walking a map in `Ord` order, in `curios/src/tests/derive/key.rs`.

## Verification and retirement

Verified by stage 3's derivations and stage 2's written key in both checkers, and `/std` built unchanged. Retired once the three stages land: the coherence decision's superclass paragraph restated to the reading inside a body, the equality decision's rejection of a decided tie restated without the reason this removes, and [A witness body may be written by the compiler](../../design/surface/a-witness-body-may-be-written-by-the-compiler.md) stating that a key derives over a family that holds itself.
