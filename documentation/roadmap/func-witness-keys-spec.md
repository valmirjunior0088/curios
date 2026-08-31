# A function type is a witness key

## Status

Nothing is built. The ground needs no preparation: the plicity vector is already the non-subterm half of a function type's identity — `compare_func_type` (`curios-elab/src/convert.rs`) compares the whole vector, arity included, before it enqueues a single domain, and its binder names feed freshness, never equality — `HeadKey::of_whnf` (`curios-elab/src/concept.rs`) is the single producer every consumer of a key routes through, and `mount_of_head` (`curios-elab/src/context/program.rs`) already answers "claimed by no authored mount" for every head that is not a name. Rigidity was probed on 2026-08-31: `(Nat, Nat) -> Nat` does not convert with `(Nat) -> (Nat) -> Nat`, and `(a: Nat) -> Nat` does not convert with `(@a: Nat) -> Nat`. What is missing is one variant and two arms.

Today, against the tree's compiler:

```text
satisfy Show((Nat) -> Nat) { … }
→ witness '/witness@0' cannot be keyed: its concept's parameter 1 reduces to (Nat) -> Nat
    every parameter's head must be an inductive, a struct, an intrinsic type, or a tuple type

let Reader(A: Type) -> Type = (Nat) -> A;
satisfy Fun(Reader) { … }
→ witness '/witness@0' cannot be keyed: its concept's parameter 1 reduces to A =>
      (Nat) -> A
```

## Mission

A witness may be keyed on a function type. `satisfy (@A: Type, @R: Type, use Draw(A), use Spell(A), use Property(R)) => Property((A) -> R) { … }` registers, and a goal `Property((Nat) -> Test)` resolves to it with the domain and result checked by unification after lookup.

The objective is the ability to *declare* such witnesses, and nothing beside it: no `/std` witness ships here, deliberately. The consumer is the [property-testing specification](property-testing-spec.md), whose per-arity `Property` witnesses are these keys' reason to exist. A function type becomes one more thing a parameter can be rigidly headed by, and every other rule — regularity, coherence, the orphan rule, deferral — applies to it unchanged.

## Design

### The one decision: a function type's rigid head is its plicity vector

Every witness is keyed on the rigid head of each concept parameter, and what sits under the head is unification's business at resolution time. A function type has no name to be headed by, so the question is what part of it plays the role a name plays: rigid, cheap to read off a weak-head normal form, and exactly the part conversion does not delegate to the subterms.

That part is the **plicity vector**: the mark at each parameter position, arity implied by the length, domains and result excluded. It is precisely the non-subterm half of the type's identity — `compare_func_type` refuses two function types whose vectors differ before it enqueues a single domain, and equates them iff the vectors agree and the domains and result convert — so a key that reads the marks and leaves the subterms to unification splits the type along the same seam conversion already does, exactly as the tuple shape does ([A tuple type is keyed by its shape](../design/language/a-tuple-type-is-keyed-by-its-shape.md)). `HeadKey` gains one variant, `FuncType(Vec<Plicity>)`, and `HeadKey::of_whnf` gains one arm reading `FuncType::plicities()` off the node — the weak-head form of a function type is the node itself, so keying costs a copy of the marks and no evaluation.

Consequences that follow rather than being decided:

- `(A) -> B`, `(A, B) -> C` and `(A) -> (B) -> C` are three keys, as they are three types: Curios does not curry at the type level, so arity is identity.
- `(a: Nat) -> Nat` and `(b: Nat) -> Nat` are one key: binder names are alpha-convertible in function types — the exact opposite of tuple labels, and the same rule seen from the other side, because the key is whatever half of identity conversion keeps for itself.
- `(A) -> B` and `(@A: T) -> B` are two keys, as they are two types.
- `() -> A` keys as the empty vector, a distinct type from `A`.
- The result type is not in the key, so under the one-witness-per-key rule a concept commits, per shape, to one result discipline; the property specification leans on exactly that.

**Rejected: arity alone.** `(A) -> B` and `(@A: T) -> B` would then be duplicates in the table though they are distinct types, and the one-witness-per-key rule would refuse a witness for the second because one exists for the first. The key would be coarser than the identity it stands for.

**Rejected: binder names in the key.** Names are not identity in a function type — `compare_func_type` uses `first_hint` only to mint fresh openings — so a name-bearing key would split one type into as many keys as it has spellings, and a well-typed goal would miss a registered witness over a renaming.

**Rejected: the domain or result heads in the key.** Keys would then overlap — `Property((Nat) -> R)` and `Property((A) -> Test)` both registrable, `Property((Nat) -> Test)` ambiguous — which is the instance-overlap problem the one-head-per-parameter discipline exists to make unrepresentable. A binder-headed domain has no head to contribute in any case.

**Rejected: a single arity-blind `Func` head.** One witness per concept would then cover every function type — but its body cannot be written, because no term in the language ranges over a function of unknown arity. A key nobody can write a witness for buys nothing.

**Rejected: curry-normalizing the key** — keying `(A, B) -> C` as if it were `(A) -> (B) -> C`. The two do not convert, so one key would stand for two types no single witness body can serve, and resolution would depend on a normalization conversion itself refuses to perform.

### The higher-kinded position, for symmetry

A type constructor keys on its body, so a lambda whose body is a function type — `let Reader(A: Type) -> Type = (Nat) -> A;` reduces to `(A: Type) => (Nat) -> A` — keys on the body's plicity vector by the same arm that gives `Monad(Option)` its key and a lambda-bodied tuple its shape. This costs one case in the existing body match. It does not extend imitation: unification never guesses a function-typed constructor from `?M(?A) ≡ (Nat) -> Nat`, since the guess is not unique; a goal reaches such a witness only where it spells the constructor.

### Dependent and propositional function types need no rule

A dependent telescope — `(n: Nat, v: Vec(n)) -> Bool` — keys as `(_, _) -> _` like any other, with the dependency checked where all subterm structure is checked, by unification against the goal. A Π into `Prop` keys by its marks like any other; what a witness over it can do with a proof is erasure's concern, not the key's.

### What changes in resolution beside the objective

- **A function-headed goal defers instead of failing at once.** Today it is non-keyable and step 3 answers `NoMatch`, an immediate error at the call. Keyed, a goal with no entry answers `Missing` and defers to the end-of-module sweep exactly as a nominal goal does, so a witness declared later in the module serves an earlier use, and a still-missing one is reported by the sweep with the same message as today.
- **The `!` auto-lift oracle, at both of its call sites** — the region read in `elaborate_bang` and the action's declared result in `action_result_key` (`curios-elab/src/elaborate/binding.rs`); the tuple landing taught that naming only the region undercounts. A function-typed region or action result was unkeyable and left the action bare; keyed, the oracle may wrap and the `Lift` goal is what fails. Every such position is an error before and after, because no function-shaped `Monad` witness exists or ships (see Non-goals); only the message moves, in the direction that oracle documents as acceptable.

### Coherence and the orphan rule

A function type is owned by no mount — the catch-all answer `mount_of_head` already gives, needing no edit — so a function-keyed witness is declared where its concept is declared, or by a privileged root.

Stated honestly, this bites harder than the tuple case: the useful key space is nearly one point — `(_) -> _` above all — so a concept's owner claiming a shape claims it program-wide, forever. That is why the intended consumers are concepts whose function instance has one canonical meaning, `Property` first, and why `Monad` at a function shape is a non-goal rather than a roster entry: `/std/State.crs` already documents the nominal wrapper as the idiom — the result parameter sits last precisely so the family partially applies as a monad — and keeping bare function types monad-free keeps `syntax.md`'s "a `(Str, Bool) -> Bool` has nowhere to sequence one" a fact rather than a default.

### Surface

No grammar changes. The keying sentence in `documentation/syntax.md` gains "or a function type, keyed by its plicity vector", and the ownership paragraph gains the consequence above beside the tuple's.

### Diagnostics

- The registration refusal's second line adds "or a function type". After this lands the refusal is reachable only by a variable, a metavariable, a universe, or a computed type.
- A key displays with its marks and everything else elided, as a shape displays its labels: `() -> _`, `(_) -> _`, `(@_, use _, _) -> _`. The duplicate and orphan reports take the spelling for free.
- Optional, and worth shipping if it is cheap: a missing witness for a goal whose key differs from a registered one only in its marks gains a hint — the plicity twin of the labeled-tuple hint, built by generalizing `diagnose_shape`'s twin construction (`curios-elab/src/resolve.rs`).

### Standard library

Nothing, deliberately. No meaningful `Show` exists at a function type, `Eql` at one is undecidable, and `Monad` at one is declined above. The first `/std` witnesses keyed on a function type arrive with the property-testing specification, which is also where their arity ceiling is chosen.

### Soundness discipline

The kernel is untouched. A function-keyed witness is an ordinary top-level definition with an ordinary elaborated type, zonked, erased and re-checked by `curios-cert` from the finished terms; the key is elaborator bookkeeping that decides *which* definition a goal names, never *whether* the result is well typed. The coherence perimeter entry (`documentation/soundness/per-term-rules/witness-coherence-and-the-orphan-rule.md`) gains the function rows beside its tuple rows: a duplicate shape refused, a function-keyed witness for a `/std` concept refused at the entry root, and one for an entry-declared concept admitted. The archived `HeadKey` gains a variant; the prelude image is compiler-build-scoped, so no reader meets the old layout.

`Plicity` (`curios-utilities/src/plicity.rs`) gains `PartialOrd, Ord`, which `HeadKey`'s orderings require, and loses the documentation sentence `compare_func_type` contradicts: conversion does not ignore plicity — the marks are part of a function type's identity, which is precisely what makes this key sound.

### Non-goals

- Arity-generic function witnesses: one witness serving every shape.
- Imitation of a function-typed type constructor from a stuck application.
- Any `Monad` or `Lift` witness at a function shape, by anyone: `/std` declines the slot deliberately, and the orphan rule refuses everyone else.
- The `Property`/`Draw` roster and everything testing-shaped: the property-testing specification's.
- Any change to the orphan rule.

## Relation to the property-testing specification

That specification's parameterized tests close through goals of the form `Property((A, …) -> Test)`, keyed here; it depends on this one, and this one exists for it. Nothing else in the tree wants a function key today: every other candidate surveyed — a reader monad, memoization, spread/tupled adapters — either has the nominal idiom already or works as a plain polymorphic function that never needs to be found.

## What is left to build

Each item is one authorization and one commit, with its tests; the full gate runs once after the last.

1. `HeadKey::FuncType(Vec<Plicity>)`: `Plicity`'s `PartialOrd, Ord` and its corrected doc line; the variant, the first-order and constructor-body arms of `of_whnf`, the display, the refusal's second line, and the comment lines that stop being true. Tests: plicity-vector keys and name-blindness in `curios-elab/src/concept/tests.rs`; a user concept resolving on a lambda, a later-declared witness serving an earlier use, `() -> _` keyed, plicity-distinct shapes as distinct entries, and the constructor-body case, in `curios/src/tests/concepts/shape_tests.rs` — where `a_function_type_is_still_not_a_key` flips into the positive test and a variable-headed witness becomes the never-keyable fixture; a duplicate shape and an entry-root `Show((Nat) -> Nat)` refused, in `coherence_tests.rs`.
2. The plicity hint, if it costs a few lines at the report site.
3. Documentation: `syntax.md`'s keying and ownership paragraphs; the coherence perimeter entry's function rows; a decision file, `documentation/design/language/a-function-type-is-keyed-by-its-plicities.md`, holding the decision and the rejected alternatives above; this file deleted and its roadmap line checked.

## Verification

- The two probes under Status: both register.
- `concept Tag`, `satisfy Tag((Nat) -> Nat)`, `Tag/tag((n) => n + 1)` resolves; a later-declared function witness serves an earlier use; `Tag(())`-style nullary — `satisfy Tag(() -> Nat)` — registers as its own key.
- `satisfy Tag((Nat) -> Nat)` and `satisfy Tag((@n: Nat) -> Nat)` coexist as two entries; two witnesses of one shape report a duplicate; an entry-root `Show((Nat) -> Nat)` reports an orphan; an entry-declared concept's function witness registers.
- The coherence perimeter entry has its function rows and cites the tests.
- The gate in `CLAUDE.md`.

## Completion criteria

A witness keyed on a function type is declared, resolved and refused by the same rules as one keyed on a name or a shape, with the plicity vector as its head; no document in the repository says a function type is not a key; and this specification is gone.

### Decisions taken here, still reversible

- **The constructor-body arm is included** for symmetry with the nominal and tuple cases. Dropping it is one line.
- **The display spelling** `(@_, use _, _) -> _`.
- **The hint is optional** and may be dropped without a trace.

## Adjacent work

- The [property-testing specification](property-testing-spec.md), the consumer.
- `Dec` by enumeration — `Dec((a: A) -> P(a))` from an `Enum(A)` completeness witness, deciding a Π over a finite domain by exhaustion — the proof-side sibling of `Property`. Speculative: extracting `P` from the goal leans on higher-order unification in a way nothing here does, and it is not this specification's burden.
