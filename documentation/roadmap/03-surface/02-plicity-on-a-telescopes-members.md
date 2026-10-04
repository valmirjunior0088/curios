# Plicity on a telescope's members

Working specification for making a member's plicity — plain, implicit (`@`) or witness (`use`) — one rule over every telescope the surface writes and every application that fills one, rather than a property of function parameters with a different carve-out at each site. One stage lands what remains of the rule; two more are decided and wait on [typed patterns](01-typed-patterns.md); one waits for a consumer.

## What this builds on

[Plicity is part of function identity](../../design/theory/plicity-is-part-of-function-identity.md) settles plicity for function types, [a call fills one parameter group](../../design/theory/a-call-fills-one-parameter-group.md) settles which list a call fills, and [concepts resolve with global coherence](../../design/surface/concepts-resolve-with-global-coherence.md) settles what a `use` member is filled with. The first of them says what a mark is followed by at each kind of site. None says how written members are matched to slots, and the code answers that differently at each site.

- **Written members meet their slots by three rules.** A call sorts its arguments into three queues, one per mark, each matched independently (`elaborate_apply`), so `join(3, use dict)` and `join(use dict, @Nat, 3)` are one call. A lambda's binder claims the next slot of its own mark and skips slots of another (`elaborate_func_check`). A concept literal pairs its `use` entries with the `use` positions wherever they are written (`elaborate_struct`). A constructor pattern writes every member.
- **No written form leaves a slot to the elaborator**, so a later hidden slot cannot be supplied alone: against two `use` slots, `use value` fills the first, and `_` is an unbound variable.
- **Reading a concept value ignores the marks.** A struct pattern over one counts every slot and binds a superclass edge to a plain name, and `.0` reaches an edge by position, where a literal leaves the edges out of its positional sequence.

## The rule

After a mark comes what the site's plain member would be: a type where the site declares, a binder where it binds, a value where it supplies. A `use` member has no binder anywhere.

| | Signature | Lambda, field sugar | Call, literal head, `satisfy` head, concept-literal entry |
| --- | --- | --- | --- |
| Plain | `n: Nat`, or `Nat` | `n`, `n: Nat`, `_` | `value` |
| `@`, named | `@n: Nat` | `@n`, `@n: Nat` | `@value` |
| `@`, unnamed | `@Holds(0 < n)`, or `@_: Holds(0 < n)` | `@_`, `@_: Holds(0 < n)` | `@_` |
| `use` | `use Show(A)` | `use _` | `use value`, or `use _` |

A signature is a function type, a `let` or `satisfy` telescope, a constructor's payload list and a declaration's type parameters.

- **`_` says nothing and holds a place.** The slot is filled as an omitted one is at that site: an `@` slot is inferred or proved, a `use` slot is resolved, and after a spread either is copied from the base.
- **A lambda never states a `use` member's type**, so a lambda with no expected function type has no `use` slot; a local `let` with a telescope declares one.
- **One alignment rule** serves binders, arguments and concept-literal entries. Plain members are always written, in order. Between two plain members, the hidden members are written in order from the first of the run, and the rest of the run may be left out. A written member's slot is its position in its run, never the next slot of its mark: against `(@A: Type, use Show(A), @B: Type, use Show(B), a: A, b: B)`, each of `(a, b)`, `(@A, a, b)` and `(@A, use _, @B, a, b)` is accepted, `(@B, a, b)` binds `A`, and `(use _, a, b)` is refused. `join(@_, use dict, xs)` supplies a dictionary; `join(use dict, xs)` and `join(xs, use dict)` are refused.
- **The witness scope is exactly the `use` members in scope.** A dictionary a program names is an ordinary value — a `let`, or a plain or `@` parameter of concept type — and reaches a `use` slot through a written `use value`.
- **Refused, each by the rule it breaks:** `use name`; `use _` in a signature; `use C(args)` or `@T` in a lambda; a plain member in a `satisfy` telescope; a plain member written as its type alone in a `let` telescope, which names what its body uses; a `use` entry in a `satisfy` body, `use _` included; a `use` parameter on a concept, whose superclass is a field and whose witness keys read every parameter.

## The alignment rule, and the placeholders

One walk over the written members replaces the three queues of a call, the per-mark claiming of a lambda and the pairing of a concept literal's `use` entries, and `@_` and `use _` are read where a value is supplied.

- A written hidden member carries the mark of the next hidden slot of its run or is refused, naming the slot.
- In a concept literal `use _` leaves an edge to resolution; after a spread it leaves the edge as the spread leaves it, copied from the base.
- The applications the compiler builds — a method's wrapper, a constructor, `!`'s bind, an operator's dispatch, a derived body, a bound's proof — write their hidden arguments in slot order.

**Acceptance:** the examples under *The rule* behave as stated; `two(@_, use _, @Bool, use other, 1, true)` supplies the second witness alone and `Both { use _, use b2, both(a, b) = … }` the second edge alone; `/std`, `programs/` and the tests compile after reordering and `@_` insertion alone.

## Decided, waiting on typed patterns

Lowering lays a pattern's columns out before it knows the constructor's signature, and an irrefutable struct pattern is projection sugar lowered before any type is known, so a pattern can neither leave a member out nor be held to a mark until [typed patterns](01-typed-patterns.md) hands elaboration the matrix.

- **A pattern follows the alignment rule**, so `cons(head, tail)` and `refl()` leave their hidden payloads out and `cons(@length, head, tail)` binds one.
- **A constructor's payload takes `use C(args)`.** Construction resolves it, and an arm that matches the constructor has it in its witness scope, written or not: `| pack(value) => Show/show(value)` over `pack(@A: Type, use Show(A), value: A)`.
- **A pattern over a concept value writes `use _` for an edge**, and a plain binder there is refused; `.0` counts plain members alone.

## Waiting for a consumer — `@` fields on a structure

A structure's field may carry the implicit mark, filled at construction by the routes an omitted `@` argument already uses: the proposition reduces to `True` and is filled with its constructor; it follows from the facts in scope by linear arithmetic; or it is determined by unification from the other fields or the expected type. Written explicitly as `S { x = k, @ok = p }`, and refused with the proposition it could not discharge.

```crs
pub struct At(s: Str): pub Type {
    offset: Nat,
    @within: Holds(offset <= Bytes/len(s.bytes)),
    @boundary: Holds(starts(Bytes/drop(s.bytes, offset, @within))),
}
```

`Str/At` is the case: two proof fields written at every construction site, where the same proposition as a function argument fills itself.

**A spread re-infers every `@` field and never copies one.** An `@` field is a *consequence* of the other fields, so copying preserves a stale proof about changed values. A spread that cannot re-infer a field is refused, naming the proposition. That is the one site where `_` after `@` and `_` after `use` part: a witness is a choice to preserve, a proof a consequence to recompute.

Still to refine:

- **Eta.** Records have eta, so a value must convert with its field-by-field expansion. A `Prop` field is free under irrelevance; an `@T: Type` field needs the field determined, and that case wants a worked example before anything is implemented.
- **Cost.** Filling a field runs the evaluation a bound does, so a record built in a loop pays per construction, priced by [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md).
- **Derived witnesses.** `Spell`, `Eql`, `Hash` and `Ord` are written over a structure's fields; `Spell` must skip an implicit field, since a spelled value reads back and an implicit field is not written.
- **The smallest honest slice**: `@` fields filled by reduction alone, which covers `Str/At` entirely, needs no prover work, and defers the cost and eta questions with the other two routes.

## One marked telescope

Core pairs a telescope with its marks by hand at three doors — `FuncType::new`, `Func::new`, `InductParam::new` — keeps a declaration's parameter marks in a vector beside its arity that no door seals (`InductDecl::plicities`, `StructDecl::plicities`), and states a concept's superclass edges as positions beside its field list, with a label minted for each. One type pairing a telescope with its marks serves all of them once a field can carry a mark: a concept's edges are then the `use` entries of its field telescope, read from the elaborated field type, so an alias of a concept application is an edge where a `use` parameter already accepts one, and no label is minted.

## Verification

- Every acceptance criterion above is a fixture, the refusals by their wording.
- `/std`, `programs/` and the test corpus compile at every stage; a program whose verdict changes other than by a refusal this spec lists is a finding, never a fixture update.
- `curios format` is the identity on every form of the table, and the grammar's corpus holds each.

## Design decisions this overturns or corrects

- [Plicity is part of function identity](../../design/theory/plicity-is-part-of-function-identity.md): alignment "positionally by plicity" becomes position in the run; its rejection of inserting omitted hidden constructor-pattern arguments falls with typed patterns, whose matrix reaches elaboration with the signature known.
- [`documentation/syntax.md`](../../syntax.md): the three independent queues of a call, and a concept literal's `use` entries written anywhere.

## Rejected

- **A `use` field on a structure.** With no label it cannot be projected, a structure's value is not in the witness scope, and resolving through the fields of a local is a second resolution rule. [`Map`](../08-standard-library/04-a-map-keyed-by-its-key-type.md) names its dictionary in its type instead.
- **Three queues at a call**, and **claiming the next slot of a mark**: which slot a written member fills then depends on the marks written before it, and two rules decide one question.
- **Resolving a `use` slot only after the later arguments are checked**, so that a type naming a dictionary decides it whatever the order. A numeral resolves eagerly, so a numeral at an associated type would meet a type still stuck on the witness; and a signature whose key type is an implicit is undecided at the slot already.
- **A block form bringing a value into the witness scope**, which a local `let` with a `use` premise already is, and **a generic `summon`**, which needs `use` at a type variable.

## Completion and retirement

One walk matches written members to slots. Record the alignment in `documentation/syntax.md` and in the plicity decision; what waits on typed patterns moves to that spec's stages, and `@` fields and the marked telescope stay here until they land. Replace the roadmap entry with a checked summary once nothing but those remains, verify that nothing references this filename, and delete it.
