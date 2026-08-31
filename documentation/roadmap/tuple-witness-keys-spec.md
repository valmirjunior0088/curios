# A tuple type is a witness key

## Status

Nothing is built. The ground needs no preparation: labels are already part of a tuple type's identity (`curios-core/src/term/shape.rs`, `TupleType`'s `Eq`/`Hash`; `documentation/syntax.md`, "Tuple types"), conversion already compares tuple types label by label (`curios-elab/src/convert.rs`, `compare_tuple_type`), the witness table already keys on a tuple of heads and displays them, and the orphan check already answers "owned by nobody" for a head that is not a name (`curios-elab/src/context/program.rs`, `mount_of_head`). What is missing is one arm.

Today, against the tree's compiler:

```text
satisfy Show({Nat, Bool}) { … }
→ witness '/witness@0' cannot be keyed: its concept's parameter 1 reduces to {Nat, Bool}
    every parameter's head must be an inductive, a struct, or an intrinsic type

Show/show((1, true))
→ no witness of Show({Nat, Bool}) found
```

## Mission

A witness may be keyed on a tuple type. `satisfy (@A: Type, @B: Type, use Show(A), use Show(B)) => Show({A, B}) { … }` registers, `Show/show((1, true))` resolves to it, and `Show(List({Nat, Bool}))` reaches it through the `List` witness's premise with nothing written at the call. The standard library then gives the tuple shapes programs write the witnesses their carriers already have.

The objective is the ability to *declare* such witnesses, in the language, one shape at a time. It is not arity-generic tuple programming, not derivation, and not a change to how witnesses are found: a tuple type becomes one more thing a parameter can be rigidly headed by, and every other rule — regularity, coherence, the orphan rule, deferral — applies to it unchanged.

## Design

### The one decision: a tuple type's rigid head is its shape

Every witness is keyed on the rigid head of each concept parameter, and what sits under the head is unification's business at resolution time (`curios-elab/src/resolve.rs`, `register_witness` and step 3 of `resolve_witness`). For `List(Nat)` the head is `List` and `Nat` is checked after lookup. A tuple type `{x: Nat, y: Bool}` has no name to be headed by, so the question is what part of it plays the role `List` plays: rigid, cheap to read off a weak-head normal form, and exactly the part conversion does not delegate to the components.

That part is the **shape**: the label at each position, positional fields included as the empty label, arity implied. It is precisely the non-subterm half of the type's identity — `compare_tuple_type` refuses two tuple types whose labels differ before it enqueues a single field, and equates them iff the labels agree and the fields convert — so a key that reads the labels and leaves the fields to unification splits the type along the same seam conversion already does. `HeadKey` gains one variant, `TupleType(Vec<String>)`, and `HeadKey::of_whnf` gains one arm reading `Telescope::labels()` off the `Subterm::TupleType` node (`curios-core/src/scope.rs`), which is the accessor the type's own `Eq` uses.

Consequences that follow rather than being decided:

- `{Nat, Bool}`, `{a: Nat, b: Bool}` and `{x: Nat, y: Bool}` are three keys, as they are three types. A witness for `{A, B}` does not serve `{x: Nat, y: Bool}`; the lookup misses and the report is the ordinary missing-witness report.
- `{}` keys as the empty shape. `Show({})` is a witness like any other, and the natural one shows `()`.
- A one-field tuple `{A}` keys as a one-position shape, distinct from `A`.
- The field types are not reduced to compute the key. The weak-head form of a tuple type is the node itself, and the labels are structural, so keying costs a walk down the spine and no evaluation.

**Rejected: arity alone.** `{A, B}` and `{x: A, y: B}` would then be duplicates in the table though they are distinct types, and the one-witness-per-key rule would refuse a witness for the second because one exists for the first. The key would be coarser than the type identity it stands for.

**Rejected: a single arity-blind `Tuple` head.** One witness per concept would then cover every tuple type — but its body cannot be written, because no term in the language today ranges over a tuple of unknown arity. Such a witness could only be filled by a compiler that writes bodies, which is a different objective ([Auto-derive](auto-derive-spec.md)), and a key nobody can write a witness for buys nothing here.

**Rejected: the component heads in the key** — `{Nat, Bool}` keyed as the pair of `Nat` and `Bool`. Keys would then overlap: `Show({Nat, B})` and `Show({A, Bool})` are both registrable and `Show({Nat, Bool})` is ambiguous, which is the instance-overlap problem the one-head-per-parameter discipline exists to make unrepresentable. A telescope variable at a position has no head to contribute in any case.

**Rejected: a nominal product in `/std`** — `Pair(A, B)`, `Triple(A, B, C)` — carrying the witnesses instead. `(1, true)` has type `{Nat, Bool}` and never `Pair(Nat, Bool)`; every call site would pay a conversion, and the language's own product would be the one type its standard library has no opinion about.

**Rejected: componentwise resolution wired into the compiler** for concepts known to distribute over products. Which concepts those are is library knowledge; the compiler has no business knowing that `Show` distributes and `Monad` does not.

### The higher-kinded position, for symmetry

A type constructor keys on its body: `Option` reduces to a lambda whose body is `Option`'s inductive node, and `Monad(Option)` keys on that name; `List` reaches the same arm through the intrinsic former. A lambda whose body is a tuple type — `let Pair(A: Type): Type = {Nat, A};` reduces to `(A: Type) => {Nat, A}` — keys on the body's shape by the same arm, so `satisfy Functor(Pair) { … }` registers where `satisfy Monad(Option) { … }` does. This costs one case in the existing body match and avoids an asymmetry that would otherwise need a sentence of its own in the diagnostic. It does not extend imitation: unification never guesses a tuple-typed constructor from `?M(?A) ≡ {Nat, Nat}`, since the guess is not unique (`(A) => {Nat, A}`, `(A) => {A, Nat}`, `(A) => {A, A}`); a goal reaches such a witness only where it spells the constructor. See Non-goals.

### Dependent and propositional tuple types need no rule

A dependent tuple type — `{n: Nat, v: Vec(n)}` — needs a label to refer to an earlier field, so a positional shape is non-dependent by construction, and a labeled dependent witness keys on its labels and is found exactly by them, with the dependency checked where all field structure is checked, by unification against the goal. Refusing dependent telescopes at registration would add a rule and an error for a well-typed, resolvable shape nobody is steered toward, and is not done. Likewise a `Prop`-sorted tuple — a record of proofs — keys on its shape like any other; what a witness over it can do with a proof is erasure's concern, not the key's.

### What changes in resolution beside the objective

- **A tuple goal defers instead of failing at once.** Today a tuple-headed parameter is non-keyable and step 3 answers `NoMatch`, an immediate error at the call. Keyed, a tuple goal with no entry answers `Missing` and defers to the end-of-module sweep exactly as a nominal goal does, so a tuple witness declared later in the module serves an earlier use, and a still-missing one is reported by the sweep with the same message as today, possibly after an unrelated error that once stood behind it.
- **A partially known tuple resolves by shape.** `Show({?A, Bool})` has a rigid head, keys as `{_, _}`, finds the witness, unifies `A := ?A`, and parks the premise `Show(?A)` on the hole — the shape is enough to choose the witness; the components wait as they do under any nominal head.
- **The `!` auto-lift oracle** (`curios-elab/src/elaborate/binding.rs`) reads a region's head key to decide whether to wrap an action in `lift`. A tuple-typed region was unkeyable and left the action bare; keyed, the oracle wraps and the `Lift` goal is what fails. A `!` in a tuple-typed position was an error before and is an error after; only the message moves, in the direction that oracle documents as acceptable.

### Coherence and the orphan rule

A tuple type has no declaring module, so the tuple former is owned by no mount — the answer `mount_of_head` already gives for intrinsic formers, and the answer it keeps. The orphan rule then reads, for a tuple key: **a tuple-keyed witness is declared where its concept is declared, or by a privileged root.** `/std` writes `Show({A, B})`; a program writes tuple witnesses for its own concepts; a program cannot add `Show` at an arity `/std` did not write, and is told so by the orphan report.

This is the same standing `List` has today, and it is the honest consequence of the coherence design (`documentation/design/language/concepts-resolve-with-global-coherence.md`): two independent packages each declaring `Show({Nat, Nat})` would collide at link with neither in the wrong, which is the collision the rule exists to make unrepresentable. It makes the standard library's arity ceiling a promise rather than a default, which is why the ceiling below is chosen generously.

**Rejected: the tuple former owned by every root.** Exactly the link-time collision above.

**Rejected: ownership through a component the declaring root owns.** Components are not in the key, and two roots each owning one component of `{Mine, Theirs}` could both declare.

**Out of scope: exempting an executable's root from the orphan rule** on the ground that it has no downstream. That is a statement about the orphan rule, true of nominal keys as much as tuple keys, and belongs to a decision about the rule rather than to this one.

### Surface

No grammar changes. A tuple type is already a type expression in a concept application; the change is that it is admitted where the concept's parameter is read for a head. The keying sentence in `documentation/syntax.md` ("Each head must reduce to an inductive, structure, intrinsic type, or supported higher-kinded type constructor") gains "or a tuple type, keyed by its labels", and the orphan-rule paragraph gains the consequence above.

### Diagnostics

- The registration refusal's second line lists what a head may be; it adds "a tuple type". After this lands the refusal is reachable only by a variable, a metavariable, a function type, a universe, or a computed type.
- A key displays by shape where a nominal key displays by name: `{}`, `{_, _}`, `{x: _, y: _}`. The duplicate and orphan reports already print the key and take this spelling for free.
- Optional, and worth shipping if it is cheap: a missing witness for a *labeled* tuple goal whose positional shape of the same arity *has* an entry gains a hint — "labels are part of the type: the witness for `{Nat, Bool}` does not cover `{x: Nat, y: Bool}`; name a struct, or declare the witness" — because that is the one surprise this design has, and the reader should meet the rule, not a bare miss.

### Standard library

The witnesses live in one new module, `/std/Tuple.crs`, registered in `std.crs`, importing the concept facades it needs — the one place a reader looks for what a tuple can do, as `List.crs` is for lists. The facades (`Show.crs`, `Eql.crs`, `Ord.crs`) stay facades.

For the empty shape and each positional arity from 1 to the ceiling:

- `Show`: the value as its literal is written — `()`, `(1,)`, `(1, true)`, `(1, (2, 3))` — so the rendering is source text, as the carriers' renderings are.
- `Eql`: `eql` is the componentwise conjunction, `neq` its negation.
- `Ord`: lexicographic, the first component that is not `eq` decides. Its `Eql` superclass slot is left to resolution and lands on the tuple `Eql` witness, whose premises land on the projections of the `Ord` premises — the ordinary machinery, composing.

Each is the regular shape `satisfy (@A: Type, @B: Type, use C(A), use C(B)) => C({A, B}) { … }`, keyed `{_, _}` and orphan-exempt as `/std`. Labeled shapes get no standard witnesses: labels are open-ended, and the labeled product with an owner is a `struct`, which is where a program wanting `Show` on `{x: Nat, y: Bool}` is pointed.

`Key({A, B})` for `Map` is not in the first landing: an injective pairing encoding needs a length prefix and a proof that concatenation under it is injective, which is a small piece of verified work of its own. It is listed under Adjacent work.

### Soundness discipline

The kernel is untouched. A tuple-keyed witness is an ordinary top-level definition with an ordinary elaborated type, zonked, erased and re-checked by `curios-cert` from the finished terms; the key is elaborator bookkeeping that decides *which* definition a goal names, never *whether* the result is well typed. The one perimeter rule this touches is coherence (`documentation/soundness/per-term-rules/witness-coherence-and-the-orphan-rule.md`), whose fixture table gains the tuple rows: a duplicate refused, a tuple-keyed witness for a `/std` concept refused at the entry root, and one for an entry-declared concept admitted. The archived `HeadKey` gains a variant; the prelude image is compiler-build-scoped and the store files a unit under its compiler's digest, so no reader ever meets the old layout.

### Non-goals

- Arity-generic or label-generic tuple witnesses: one witness serving every shape. That needs a description of the fields the language does not have, and is the auto-derive specification's territory.
- Standard witnesses for labeled tuple types.
- Imitation of a tuple-typed type constructor from a stuck application.
- Any change to the orphan rule.
- Derivation: no body is written by the compiler here.

## Relation to the auto-derive specification

[Auto-derive](auto-derive-spec.md) has a Tuples section resting on the sentence "a tuple type is not a witness key", and builds a `Fields`/`Tupled` bridge because of it. When this lands, that sentence is false and the bridge is no longer what tuple witnesses need: per-shape witnesses are written in the language, and a derived `Spell` for a tuple shape, should the derivation ever want one, can register under the same key like any other derived witness. The section is rewritten to depend on this key; the `SpellAll` question in its Decisions still open loses its tuple motivation and stands, if at all, on its own.

## What is left to build

Each item is one authorization and one commit, with its tests; the full gate runs once after the last.

1. `HeadKey::TupleType(Vec<String>)`: the variant, the first-order arm and the constructor-body arm of `of_whnf`, the display, and the comment and diagnostic lines that stop being true (`HeadKey`'s and `of_whnf`'s documentation, `mount_of_head`'s, the refusal's second line). Tests: shape keys in `curios-elab/src/concept/tests.rs`; a user concept resolving on `(1, true)`, a labeled goal missing an unlabeled witness, a later-declared tuple witness serving an earlier use, `{}` keyed, in `curios/src/tests/concepts/resolution_tests.rs`; a duplicate shape and an entry-root `Show({Nat, Bool})` refused, in `coherence_tests.rs`.
2. `/std/Tuple.crs` with `Show`, `Eql` and `Ord` for `{}` and arities 1 through the ceiling, registered in `std.crs`. Tests: `Show/show((1, true))`, `Show(List({Nat, Bool}))`, `%` over a tuple, `Eql` and `Ord` on pairs, in the cross-stage corpus; the prelude build exercises every witness on each workspace check.
3. The labeled-goal hint, if it costs a few lines at the report site.
4. Documentation: `syntax.md`'s keying and orphan paragraphs; the auto-derive specification's Tuples section; a decision file, `documentation/design/language/a-tuple-type-is-keyed-by-its-shape.md`, holding the decision and the rejected alternatives above; this file deleted and its roadmap line checked.

## Verification

- The two probes under Status: the first registers, the second prints `(1, true)`.
- `Show/show(())` prints `()`; `Show/show((1,))` prints `(1,)`; `Show/show([(1, true), (2, false)])` prints `[(1, true), (2, false)]`.
- `Show/show((x = 1, y = true))` reports no witness of `Show({x: Nat, y: Bool})`, with the hint if item 3 landed.
- `satisfy Show({Nat, Bool}) { … }` in an entry program reports an orphan; the same for an entry-declared concept registers; two witnesses of one shape report a duplicate.
- `satisfy Functor(Pair) { … }` registers for a `let Pair(A: Type): Type = {Nat, A};`.
- The coherence fixture table has its tuple rows and the soundness entry cites them.
- The gate in `CLAUDE.md`.

## Completion criteria

A witness keyed on a tuple type is declared, resolved and refused by the same rules as one keyed on a name, with the shape as its head; the standard library's carriers' concepts hold for the tuple shapes programs write; no document in the repository says a tuple type is not a key; and this specification is gone.

### Decisions taken here, still reversible

- **The ceiling is 8.** `{}` and arities 1 through 8, positional. Each arity is a few lines per concept and is elaborated on every prelude build; past eight a labeled `struct` is the readable type. Raising it is adding lines and nothing else — but because of the orphan rule only `/std` can, so the number is a promise and was chosen with room.
- **One module, `/std/Tuple.crs`,** rather than a witness in each concept's module.
- **The constructor-body arm is included** for symmetry rather than for a consumer. Dropping it is one line and one sentence in the refusal.

## Adjacent work

- `Key({A, B})` for `/std/Map`: a length-prefixed pairing encoding and its injectivity proof.
- Tuple witnesses for whatever operator concepts `/std` decides distribute over products (`Add` pointwise, and the like) — a `/std` decision with no compiler content.
