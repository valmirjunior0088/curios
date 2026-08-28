# Auto-derive: a witness whose body the compiler writes

## Status

The ground is prepared; the mechanism is not built. Eight elaborator defects stood between the design and a compiler that could carry it, and all eight have landed with tests and a full gate:

- [x] 1. The positivity walk terminates on a type-level `rec`
- [x] 2. A struct's parameters are checked against elaborated telescopes
- [x] 3. A tuple literal synthesizes when nothing pins it
- [x] 4. An introduction form parks on a type stuck on a metavariable
- [x] 5. A witness may recurse through its own entry
- [x] 6. `wonder stage` answers for any rung it reached
- [x] 7. Unlabeled tuple fields print as written
- [x] 8. A missing-witness report names the premise and the head

What remains is the derivation itself, listed under [What is left to build](#what-is-left-to-build), and four items the work surfaced that belong to no part of it, listed under [Adjacent work](#adjacent-work).

The design was reached after a survey of deriving mechanisms in Lean 4, Rocq (coq-elpi, Equations, `Scheme Equality`), GHC, Idris 2, Agda, Isabelle/HOL, Scala 3, Swift, Rust, OCaml, PureScript, Roc and Elixir, and after probing the tree's compiler.

## Mission

A witness declaration may omit its body. The compiler then writes the body from the declaration of the type in the witness's key, or refuses compilation naming what it could not write. The first and, for now, only concept this applies to is `Spell`, whose witness spells a value as the Curios source text denoting it. Hand-written witnesses remain the norm: a carrier whose spelling is computed in the language — `Flt`, `Str`, `Bytes` — states its witness with a body, exactly as today, and derivation is the form for structural types whose spelling is nothing but their constructors and fields. A tuple has no declaration and no owner, so it is neither written nor derived on request: one witness per concept, written in the language over a description of the tuple's fields, serves every arity and every labeling, and the compiler supplies only the adapter for the shape a program actually asks for.

What is borrowed, and from where:

- From GHC's standalone deriving: **the programmer writes the signature, the compiler writes only the body**. Premises are never inferred; `satisfy (@A: Type, use Spell(A)) => Spell(Tree(A));` states them, and the existing regularity rule (`curios-elab/src/resolve.rs`, `register_witness`) checks them.
- From Swift SE-0185: **an empty body is the request**, opt-in and visible in source; synthesis needs the type's representation where it is declared, which here is the subtree rule for representations rather than a same-file rule.
- From Lean 4's deriving handlers: **the derived artifact is ordinary code**, elaborated and kernel-checked like anything written by hand; recursion is threaded through the witness table, and handlers are **keyed by the class** so that a body-less request for a class no handler serves is an error.
- From Scala 3's `Mirror`: **the compiler supplies structure, the library supplies logic** — for tuples, a description of the fields and a view onto them, with the generic printer written once in Curios.
- From Roc: a **structural** type has no owner and is served automatically, while a **nominal** type opts in.
- From Roc and Swift: a field the concept has no witness for — a function, a `Cell`, a `Handle` — **refuses the derivation** rather than being skipped.
- From coq-elpi and Isabelle's derive manager: the shape a *second* derivable concept will need — derivations that depend on other derivations and produce proofs — is noted under the extension seam and built later.

## What the ground now supports

Each of the design's prerequisites is a fact about the compiler in the tree, and each is pinned by a test rather than by an observation someone once made.

| The design needs | Established by |
| --- | --- |
| A description-indexed family declares, and a `struct` field or constructor payload may have its type computed by a self-calling type-level `rec` | `curios/src/tests/positivity/computed_type_tests.rs`; `curios-analysis/tests/driven.rs` |
| A `struct` or `concept` takes a parameter whose type is universe-polymorphic — `Fields`, `List(A)`, any family carrying a `Type` payload | `curios/src/tests/structs/construction_tests.rs` |
| A tuple literal reaches a type the description computes, both when the index is written and when it is left implicit | `curios/src/tests/inference/tuple_tests.rs` |
| A witness recurses through its own table entry, including through another former's witness | `curios/src/tests/concepts/recursion_tests.rs` |
| A missing witness names the type it needed and the premise that needed it | `curios/src/tests/concepts/resolution_tests.rs` |
| A pipeline rung can be read off a program that does not compile | `curios/src/wonder/tests.rs` |

Two further facts, held by the compiler rather than by a test:

- **Keying on a nominal wrapper works.** `satisfy (@F: Fields) => Show(Tupled(F))` registers, which is the route the tuple design depends on. Keying on a *computed* type — `Show(TupleOf(F))` — remains refused, as intended: the wrapper exists precisely because a `rec` is not a key.
- **The walk over a closed description specializes away completely.** `wonder stage ersd-optm` of the generic tuple printer reduces 37,636 lines of erased output to 424, with the walk absent entirely. This is stronger than "unrolled into one function per level", which is what the design assumed when it argued the description-passing style costs nothing.

## Decisions now settled

- **`Tupled` is a `struct`.** A value-parameterized struct is now accepted, so the wrapper takes the form the design preferred rather than the `induct` it would have had to settle for.
- **A derived body recurses through the table, not through a walker.** A witness may name its own entry, so the generated body for a self-recursive family is a plain `match` with `Spell/spell(sub)` calls and no local `rec`.
- **A nested occurrence needs no supplied dictionary.** A witness whose recursive occurrence sits under another former — `List(Rose)` — resolves through that former's witness and back into its own entry unaided.
- **Specialization is not a cost to argue about.** See the figure above.

## Decisions still open

**Conjunction goals, or explicit dictionary supply, for `SpellAll(F)`.** A premise that reduces to a tuple of concept applications is still not a regular premise, and a tuple-typed local `use` binder is still not searched componentwise. Nothing in the eight steps bore on this, so the design's stated preference for conjunction goals — general, independently useful, independently testable — stands on its original reasoning and no more. What would decide it: whether the compiler's step at a tuple goal can supply the dictionary tuple itself, since it holds the description concretely, in which case regularity need only admit the form.

## Design

### Permanent design decisions

1. **The signature is the programmer's.** A body-less witness has exactly the telescope and concept application a written one has, and derivation never adds a binder. A parameterized head must be applied under a telescope that binds its parameters and states the premises the body will need.
2. **Derivability is a property of the concept, not of `satisfy`.** A concept either has a derivation, registered in the compiler against its `SyntaxRegistry` slot, or it does not; a body-less witness for a concept without one is refused. The first landing registers one derivation, for `Spell`.
3. **Nominal types opt in; tuples are served by one library witness and a compiler adapter.** An `induct` or `struct` is derived only where a body-less `satisfy` asks, because it has an owner and the orphan rule applies. A tuple type has neither a declaration site nor an owner, and its labels are part of its identity, so no written or body-less witness can be keyed on it: a `Spell` goal whose parameter is a tuple type is answered through the concept's witness over `/syn/Tupled(F)`, with the description, the view and the dictionaries built by the compiler at that goal.
4. **The expansion is a term, not a rewrite.** Lowering carries a body-less witness into Core as a `Transient::Derive` in the body position of the same anonymous definition a written witness produces; elaboration replaces it with the derived body when checking the definition against its declared type. Like `Infix`, `NumLit` and `Bang`, a `Derive` that survives elaboration is refused by zonk and by the kernel (`curios-cert/src/kernel/infer.rs`, `KernelError::NotCore`).
5. **Derivation reads the registry, not the source.** The derived body is computed from the `InductDecl`/`StructDecl` of the key's head as elaboration holds it — constructor telescopes with their plicities, field telescopes, result sorts — so a type from the unit being compiled and a type from an established unit derive identically.
6. **A derived body is untrusted.** It is elaborated, zonked, erased and re-checked by the independent kernel like every other term; the soundness perimeter (`documentation/design/language/the-soundness-perimeter.md`) gains no rule.
7. **Derivation never widens what a hand-written witness could do.** Every rule a `satisfy` body obeys — the orphan rule, one witness per key, sealed concepts, representation privacy, regular premises — applies unchanged; a derived body that would violate one is refused by the check that already exists, wrapped in a derive frame that names the witness.
8. **Compiler-emitted names live in `/syn` and are reached through `/std`.** `Spell`, `Fields` and `Tupled` are declared in `/syn` because the elaborator emits their names, and every `.crs` consumer imports them from their `/std` facades, as every other emitted concept is imported today.

### The `Spell` concept

```crs
pub concept Spell(A: Type): pub Type {
    spell(A) -> Str,
}
```

`spell(v)` is the Curios source text that denotes `v` at its type: a term that re-parses under the surface grammar and, checked against `A` wherever every name in it is visible, elaborates to a value equal to `v`. `Show` is unchanged and remains the human-facing display concept; `Spell` exists for tooling — counterexamples, traces, `wonder` — where a reader must be able to paste the text back.

Spelled forms, by the type's kind:

| Type | Spelled as | Witness |
| --- | --- | --- |
| `Nat` | decimal | written, `/std` |
| `Int` | decimal with an explicit sign, so the text is `Int`-typed without context | written |
| `Byte`, `Bool` | decimal `0`–`255`; `true`/`false` | written |
| `Flt` | a float literal — decimal point mandatory, `e` exponent where `Flt/to_str` would use one; `-0.0` for negative zero; `/std/Flt/nan`, `/std/Flt/pos_inf`, `/std/Flt/neg_inf` for the non-finite values, which no literal denotes | written; **not** `Flt/to_str`, whose `+0`/`+inf`/`NaN` are not literals |
| `Char`, `Str` | `'…'`, `"…"` with the five escapes each admits; every other scalar value verbatim | written |
| `Bits`, `Bytes` | `b[0, 1, …]`, `x[0x48, …]` | written |
| `List(A)` | `[a₁, a₂]` under `use Spell(A)` | written |
| an `induct` value | the constructor's absolute path applied to its explicit payloads: `/Tree/node(/Tree/leaf(1), /Tree/leaf(2))`, `/std/Option/none()` | **derived, on request** |
| a `struct` value | the absolute head and its fields, labeled where the field is, positional otherwise: `/Point { x = 1, y = 2 }`, `/Meters { 7 }` | **derived, on request** |
| a tuple value | `(1, true)`, `(fst = 1, snd = true)`, `(x,)`, `(only = 1)`, `()` | **one library witness over `Tupled(F)`, adapted at the goal** |
| `Handle`, `Cell(A)`, `Io(A)`, functions, sorts, `Prop`-sorted types | no witness | — |

Absolute paths are chosen over shortest suffixes because the text is scope-independent: it re-parses from any module that can see the names. `Show(Option)` prints `some(3)`; `Spell` prints `/std/Option/some(3)`.

### Surface: the body-less witness

```crs
satisfy Spell(Point);
satisfy (@A: Type, use Spell(A)) => Spell(Tree(A));
satisfy (@T: Type, @n: Nat, use Spell(T)) => Spell(Vec(T, n));
```

Grammar: after the concept application, `;` in place of `{ … }`, which the parser refuses today. Both forms remain: a body-ful `satisfy` is untouched. `TopWitness.entries` becomes a body that is either written entries or absent; the parser (`curios-text/src/parse/top_level.rs`, `parse_top_witness`) accepts either terminator after the application, and the printer (`curios-text/src/print.rs`) prints the absent body as `;`. `curios format` round-trips the body-less form and never expands it; the expansion is observable at `wonder stage core-elab`, and `wonder stage core` shows the `Derive` transient in place.

`satisfy Spell(Tree);` for a parameterized `Tree(A: Type)` is refused as it is today — `Tree` is a type constructor, not a `Type` — and the report says to write the telescope, quoting the form above.

### Lowering and the `Derive` transient

`into_core` lowers a body-less witness exactly as a written one (`curios-text/src/into_core.rs`, the `TopItem::Witness` arm): an anonymous `FlatLet { kind: DefinitionKind::Witness, name: Global::Witness(id) }` whose declared type is the concept application under the telescope. Its body is `Subterm::Transient(Transient::Derive(Derive { span }))` — a hole that says "fill me from the expected type" and carries nothing else, since the expected type is the whole of what derivation needs.

`elaborate_module_let` (`curios-elab/src/elaborate/module.rs`) registers the witness's signature before its body as today, then checks the body against the elaborated type. Checking a `Derive` transient against an expected type is the one new `elaborate` arm: reduce the expected type to a concept application `C(p₁, …)`; look `C` up among the registered derivations; call the derivation, which returns a lowered-Core body; check that body against the expected type. A `Derive` met in inference mode, or against a type that is not a concept application, is refused — both are unreachable from the surface, since the transient is only ever born in a witness's body position.

The lowered-Core body is built with the builders `curios-elab/src/builders.rs` already exists for, in the shapes the lowerer emits: explicit-only applications (implicit insertion mints the metavariables), a `Match` whose motive is a fresh metavariable (`curios-text/src/into_core/match_compile.rs`, `motive_scope`) and whose cases are one `InductArm` per constructor in declaration order, `Proj` for struct fields, and a local `Rec` group where a mutual group's walkers are needed. The lowerer itself is unreachable from here — `curios-text` depends on `curios-elab`, not the reverse — which is why the derivation is *specified* by the surface program it is equivalent to and *implemented* as the Core the lowerer would have produced for it.

### Derivation of `Spell`

#### Eligibility

Each parameter of the concept application must reduce to a rigid head that is:

- E1. a registered `induct` or `struct` — not an intrinsic carrier (those have written witnesses), not the backing struct of a `concept` (`DefinitionKind::ConceptType`), and not `Prop`-sorted (a proof has no runtime value to spell);
- E2. representation-transparent at the declaring island — `rep_public`, or the island inside the declaration's `module` subtree — because the body matches on constructors and projects fields, which is what the privacy rule governs;
- E3. fully applied, every parameter and index bound by the witness's telescope or given concretely.

A tuple-type parameter is not derived on request at all; it takes the path under Tuples below. `Spell` is unary, so the first landing asserts one parameter; the seam is written for a tuple of heads because `WitnessKey` already is.

#### The generated body

The specification of the expansion is the surface program it is equivalent to. For

```crs
pub induct Tree(A: Type): pub Type
| leaf(A)
| node(Tree(A), Tree(A))
end

satisfy (@A: Type, use Spell(A)) => Spell(Tree(A));
```

the derived witness is

```crs
satisfy (@A: Type, use Spell(A)) => Spell(Tree(A)) {
    spell(v) =
        match v
        | leaf(x) => Str/concat("/Tree/leaf(", Str/concat(Spell/spell(x), ")"))
        | node(l, r) =>
            Str/concat("/Tree/node(", Str/concat(Spell/spell(l), Str/concat(", ", Str/concat(Spell/spell(r), ")"))))
        end,
}
```

— the recursive occurrences resolving through the witness's own table entry. For

```crs
pub struct Point: pub Type { x: Nat, y: Nat }
satisfy Spell(Point);
```

it is

```crs
satisfy Spell(Point) {
    spell(v) = Str/concat("/Point { x = ", Str/concat(Spell/spell(v.x), Str/concat(", y = ", Str/concat(Spell/spell(v.y), " }")))),
}
```

Rules:

- A field is spelled by `Spell/spell(field)`, resolved by ordinary witness resolution in the witness's own scope: a telescope variable's premise from the `use` binder, a concrete type from the table, a tuple through the adapter. Nothing is special-cased; a field type with no witness fails as any call would, inside a derive frame (see Diagnostics).
- A direct recursive occurrence of **the family itself** resolves through the table: a witness may name its own entry, and one that does is lowered as a group of one.
- A recursive occurrence in **another member of a mutual `induct` group** may not. Two witnesses that resolve each other have no order to declare them in, and the compiler refuses that cycle by name (`curios-elab/src/elaborate/module.rs`, `check_witness_cycles`). **A derivation reaching for the table form here would emit a program the kernel refuses**, which is why this rule is stated apart from the one above rather than folded into it. A mutual group's derived body carries one local `rec` group with one member per family member, so each member's witness is self-contained. Lifting this needs the first item under [Adjacent work](#adjacent-work).
- An occurrence *under another type former* — `List(Tree(A))`, `Option(Tree(A))`, a user `Pair(Tree(A), Nat)`, a tuple `{Tree(A), Nat}` — is spelled through that former's own witness, which returns to this witness's own entry one level down with nothing supplied by hand.
- Indexed families: the body matches with an omitted motive, and index inversion prunes the arms it prunes for hand-written matches.
- Constructor arguments are spelled in payload order, separated by `, `; a nullary constructor spells as `/Path/ctor()`; struct fields in declaration order, `label = ` before a labeled field and nothing before an unlabeled one, inside `{ ` and ` }`.

#### Tuples

A tuple type is not a witness key: `HeadKey::of_whnf` (`curios-elab/src/concept.rs`) has no key for a Σ-type, and resolution's third step (`curios-elab/src/resolve.rs`) turns a non-keyable head into `NoMatch`. Labels are part of a tuple type's identity, so a written witness per arity could neither cover labeled tuples nor print their labels; and a tuple has no declaration site to carry a `satisfy` and no owner for the orphan rule to consult. Yet the arity-generic, label-aware printer is writable in today's language: a description of the fields, a type computed from it, a tuple of dictionaries computed from it, and a walk whose result type refines per arm — the idiom `Fmt` already uses. What is missing is the bridge, the keying, and the automatic dictionaries.

The design puts the logic in the language once and lets the compiler build only an adapter per shape the program asks for:

- **`/syn/Fields`**, the description: `nil() | cons(label: Option(Str), A: Type, rest: Fields)`. Non-dependent; a dependent tuple carries a proof, which spells `?` anyway, so the generic witness refuses dependent tuples at the goal with its own message.
- **`TupleOf(F)`**, a plain library `rec`: `nil() => {}`, `cons(l, A, rest) => {A, TupleOf(rest)}` — the nested encoding P5 used. Not an intrinsic: no kernel rows, no `tail`, no decomposition.
- **`/syn/Tupled(F: Fields)`**, a nominal wrapper over `TupleOf(F)` — the keyable head for "all tuples", `HeadKey::Nominal(Tupled)` with `F` solved by unification. A `struct`: a value-parameterized struct over a universe-polymorphic description is accepted, and a witness keyed on the wrapper registers.
- **One witness per concept, in Curios**: `satisfy (@F: Fields, use SpellAll(F)) => Spell(Tupled(F))` around the `show_fields`-shaped walk, in `/std/Spell.crs`; `/std/Show.crs` writes its own; so can any user concept. Opt-in is that witness's existence; there is no registration table.
- **The compiler's step at the goal**, in resolution's non-keyable arm when the head is a `TupleType`: build `F` from the telescope — labels as `/syn/Str` values, the same act as a string literal becoming a library value; resolve `C(Tupled(F))` from the table like any goal; build the flat↔nested view for the known arity (`(t.0, (t.1, ()))` and back); transport the resolved dictionary along each method's type — eta-expand, `nest` at every argument occurrence of the tuple type, `unnest` at every result occurrence — and hand back the witness term. Concept-agnostic for first-order methods (`Spell`, `Show`, `Eql`, `Ord`, a hash); an occurrence under a type former is refused with a message. Kernel unchanged: the result is an ordinary closed term inline at the goal; hoisting it into a per-unit definition cached by shape is an optimization if a census shows duplication, and such a definition must never enter the program-wide table.
- **`SpellAll(F)`**, the premise: refused today as non-regular. Either conjunction goals — a goal or premise that reduces to a tuple type is solved componentwise, `{}` by `()`, a tuple-typed local `use` binder searched componentwise, regularity admitting a stuck application of a *total* type-level definition over telescope variables, with a depth guard as the termination story — or the compiler's step supplying the dictionary tuple explicitly, since it holds `F` concretely, with regularity merely admitting the form. Conjunction goals are preferred: general, independently useful, independently testable. Still open — see [Decisions still open](#decisions-still-open).
- **Specialization**: a walk over a closed description specializes away entirely — see the figure under [What the ground now supports](#what-the-ground-now-supports). The view's pairs and the dictionary projections cost nothing the optimizer leaves behind.

Coherence holds by construction: one witness per shape, and nobody can declare a competing one, which makes literal the sentence `documentation/design/language/concepts-resolve-with-global-coherence.md` already states — anonymous witnesses fill structure the goal already determines. Tuples thereby leave derivation entirely; derivation is nominal-only, resolution is structural. `Fields` is not a detour: a struct body is the same telescope, so the description is the representation a later user-facing generic derive would be written over.

#### Field populations

For each constructor payload, struct field, and tuple component, by its plicity and the sort of its type:

| Payload | Spelled |
| --- | --- |
| explicit, type of sort `Type` | `Spell/spell(field)` |
| implicit (`@`), any sort | omitted — the re-parsed call infers it, exactly as the source omitted it |
| explicit, type is a sort (`Type`, `Prop`) | **refused**: a type payload cannot be spelled as a value |
| explicit, type of sort `Prop` (a proof) | `?` — a written goal; the text denotes the value and re-parses, and `wonder diagnostics` on it reports the obligation with its candidate fits. Compiling the text requires discharging the goal, which is the truthful contract for a value that carried a proof |
| any, type has no `Spell` witness (functions, `Cell`, `Handle`, `Io`) | **refused** at that field |

Erasure already classifies payloads by sort (`curios-core/src/inductive.rs`), so the classification here is the same judgment read at the same telescope.

### `Fmt`: the `#` slot

`/std/Fmt` parses its format string at the type level (`parse_at`/`parse_lit_at`/`parse_esc_at` over the string's bytes), where byte 37 (`%`) becomes `Fmt/show(rest)` and `\%` a literal percent; `format_type_with` maps `show(rest)` to `(@A: Type, use Show(A), a: A) -> …` and `go_with` renders the argument through `Show/show`. The `#` slot is the same shape beside it, entirely in library code:

- a constructor `spell(Fmt)` on the `Fmt` inductive;
- byte 35 (`#`) recognized in all three parse functions, and `\#` as its escape, mirroring `\%`;
- an arm in `format_type_with`: `spell(rest) => (@A: Type, use Spell(A), a: A) -> format_type_with(T, rest)`;
- an arm in `go_with`: `spell(rest) => (use w, a) => go_with(T, finish, rest, Str/concat(acc, Spell/spell(a)))`.

The fuel bound (`3 * len + 3`, one step per byte) is unaffected. `%` is the show-slot and `#` the spell-slot: `Fmt/print("# vs %")("a\n")("a\n")` prints `"a\n"` quoted and escaped beside the raw string, and `Fmt/print("point = #")((1, true))` reaches the tuple witness through the adapter. A missing `Spell(Tree)` fails at the call exactly as a missing `Show` does today. The one cost is recorded: `#` is commoner in prose than `%`, so an existing format string with a bare `#` — `issue #42` — becomes a spell-slot and needs `\#`. Two-byte directives (`%#`, `%?`) were considered and rejected: they reserve no new byte but change the meaning of `%` before a literal `#`/`?`, so they break too and read worse.

### Diagnostics

Every refusal is reported at the `satisfy` declaration's span — or, for a tuple, at the goal's site as a missing witness is today — named by concept, key and declaring module as witness diagnostics are, in one frame:

- `Spell(Tree(A)) cannot be derived: constructor node, payload 2 has type Foo, and no witness for Spell(Foo) is declared` — the ordinary `NoWitness`, wrapped; when the failing type is a telescope variable, the frame adds `add use Spell(A) to the telescope`.
- `Spell({Nat, Foo}) cannot be served: component 2 has type Foo, and no witness for Spell(Foo) is declared` — the adapter's frame at a goal.
- `Spell(Nat) cannot be derived: Nat is an intrinsic type; write the witness` (E1); likewise for a concept's backing struct and a `Prop`-sorted head.
- `Spell(Secret) cannot be derived here: Secret's representation is private to Vault` (E2, wrapping the privacy refusal).
- `Spell(Tree) cannot be derived: Tree takes a parameter; write satisfy (@A: Type, use Spell(A)) => Spell(Tree(A));` (E3).
- `Foo has no derivation; write the body` for a body-less witness of any other concept.
- Sealed concept outside its subtree, orphan violation, duplicate key: the existing reports, unchanged — they fire on the signature before the body is derived.

Exit codes are unchanged: a refused derivation is a hard error, `1`. Steps 7 and 8 fix what these reports are built on.

### Coherence, privacy and the orphan rule

Nothing new. The derived witness is registered from its signature exactly as a written one is, before its body exists; the orphan rule and the one-witness-per-key insert run on the signature; representation privacy and sealing are checked on the body the derivation produces, in the declaring island. A tuple's adapted witness has no signature to register and no owner to check, and is canonical by shape. `documentation/design/language/concepts-resolve-with-global-coherence.md` and `concept-representations-may-be-sealed.md` are unaffected.

### Tooling

- Parser and printer as above; `curios format` round-trips `satisfy C(T);`.
- `editors/grammar/grammar.js` learns the `;` terminator; its committed `src/` is regenerated alongside, and `npm test` refuses drift.
- `documentation/syntax.md`: the witness-declarations section states the body-less form, the derivable-concept rule, the eligibility rules, and how a tuple is served; the quick reference gains the row.
- `wonder`: nothing new. `stage core` shows the transient, `stage core-elab` the expansion. The planned `witnesses` query, when it lands, flags a derived row.

### Standard library placement

- `/syn/Spell.crs` declares the concept, `/syn/Fields.crs` and `/syn/Tupled.crs` the description and the wrapper, all registered in `curios-prelude-archive/syn.crs`; `SyntaxRegistry` (`curios-utilities/src/syntax.rs`) gains the slots — `spell`'s `ConceptField`, and the `Fields` constructors and `Tupled` the adapter builds — spelled in `curios-prelude-archive/src/syntax.rs`, and the prelude build's presence check covers them as it covers every slot. They live in `/syn` because the compiler emits their names.
- `/std/Spell.crs` is the facade, registered in `std.crs` beside `Show`, in the two-line form every emitted concept has (`/std/Monad.crs` is `pub use /syn/{Monad}; pub use /syn/Monad/{pure, bind};`): `pub use /syn/{Spell}; pub use /syn/Spell/{spell};`. `/std/Fields.crs` and `/std/Tupled.crs` likewise.
- Every `.crs` consumer imports through the facades. Written witnesses for the carriers sit in their own `/std` modules under `use /std/{…, Spell}`, beside their `Show` witnesses (`Nat`, `Int`, `Byte`, `Bool`, `Flt`, `Char`, `Str`, `Bits`, `Bytes`, `List`), orphan-exempt as privileged roots; the tuple witnesses for `Spell` and `Show` in `/std/Spell.crs` and `/std/Show.crs`; `/std/Fmt` adds `Spell` to its existing `use /std/{Bytes, Byte, Str, Nat, Show, Io};` line. The only direct mentions of `/syn/Spell/spell`, `/syn/Fields/cons` and `/syn/Tupled` are the elaborator's, through the registry slots.
- Derived witnesses in `/std` for its own structural types — `Option`, `Result`, `Order` — written as body-less `satisfy`. The prelude then exercises the derivation on every `cargo clippy --workspace`, which is the corpus the mechanism is held to.

### Soundness discipline

The derived body is a term elaborated in checking mode against the witness's declared type — or built closed at a goal — zonked to a metavariable-free module, erased, and re-checked by `curios-cert` from the finished terms alone. `Transient::Derive` joins the three transients zonk treats as unreachable and the kernel refuses as `NotCore`, so an expansion that failed to happen cannot reach a judgment. `Spell` carries no law, and its witnesses are value-level, so no totality obligation is introduced; a walk over a description is structurally recursive in any case.

### Extension seam

The derivation lookup is a `match` on the concept's registry slot with one arm; the tuple adapter is concept-agnostic and needs no arm. A second compiler-important concept — `Eql`, `Ord`, later a generator and a shrinker for a test story — adds an arm and a `SyntaxRegistry` slot and nothing else at the seam. What such concepts will need that `Spell` does not is recorded so the seam is not mistaken for complete: derivations that depend on other derivations (an `Ord` body that reuses a derived `Eql`; coq-elpi's `dep1` graph, Isabelle's `compare → compare_order → linorder`), and derivations that produce proofs for a concept's law fields (Lean's `DecidableEq`, coq-elpi's `eqb_OK`, Isabelle's comparator locale). A user-facing derivation mechanism — a Curios-level `Generic` universe with generic defaults — is a different design: a blanket witness `satisfy (@A, use Generic(A)) => Spell(A)` is unkeyable by design, so it would need concept-level defaults, and is deliberately out of scope; `Fields` is the carrier it would be written over when its day comes.

### Non-goals

- Premise inference, deriving clauses on declarations, and any implicit derivation for a nominal type: an `induct` or `struct` is derived only where a body-less `satisfy` asks.
- Deriving for intrinsic carriers, `Prop`-sorted types, concept dictionaries, or a type whose representation is private at the declaration site.
- Tuple witnesses for any concept but `Spell` and `Show` in the first landing; the mechanism admits any concept, the standard library writes two.
- Dependent tuples through the generic witness.
- A precedence-aware spelling: Curios call syntax is fully parenthesized, so `spell` takes no precedence.
- A general derive table, user-registered derivations, or a `Generic` universe.
- Changing `Show`, or `%`.
- Row or telescope variables in the surface grammar (`{..R}`): the same power at far higher cost, against the grain of a language that already computes types from data. Core tuples as binary Σ-chains: makes `TupleOf` a plain type but pushes flattening onto erasure for every tuple in the program.

## What is left to build

The mechanism, in the order it can be built:

1. The body-less `satisfy` form — parser, printer, `curios format` round trip, and the tree-sitter grammar.
2. `Transient::Derive`, lowered into the body position of the same anonymous definition a written witness produces, and refused by zonk and by the kernel if it survives elaboration.
3. `/syn/Spell` with its `/std/Spell` facade, and the written witnesses for every carrier.
4. The `Spell` derivation itself: eligibility, the generated body, and the diagnostics below.
5. `/syn/Fields`, `TupleOf`, `/syn/Tupled`, the tuple witnesses for `Spell` and `Show`, and the compiler's adapter at a tuple goal — which needs [the open decision](#decisions-still-open) taken first.
6. `Fmt`'s `#` slot.
7. Derived witnesses in `/std` for `Option`, `Result` and `Order`, which is what holds the mechanism to the prelude on every workspace check.

## Verification

A `curios/src/tests/derive.rs` in the `run(source)`/`error(source)` style of `concepts.rs`, and rows in `fmt.rs`:

- Derived `Spell` for: an enumeration, a constructor with payloads, a parameterized family under a premise, a recursive family, a mutual group with one body-less witness per member, a family nested through `List`, through a user struct and through a tuple, a labeled struct, a positional single-field struct, a struct with a dependent `Prop` field (spells `?`), an indexed family (`Vec`).
- Tuples through the adapter: unlabeled, labeled, one-field, unit, nested, a tuple holding a derived nominal type, a tuple whose component has no witness (the frame names the component), a dependent tuple (refused with its message), and a user concept with its own `Tupled` witness.
- Round-trip: for each, the spelled text is spliced into a program checked against the type and compared with `==` through an `Eql` witness, or by re-spelling and comparing strings where no `Eql` exists.
- Refusals, each asserting the frame's text: intrinsic head; `Prop`-sorted head; concept-backed head; private representation outside the subtree; sealed concept outside the subtree; missing field witness naming constructor and payload, with the telescope hint when the type is a variable; function-typed field; explicit type payload; unapplied parameterized head; a body-less witness for a non-derivable concept; orphan and duplicate on a body-less signature.
- `Fmt`: `#` renders through `Spell` beside `%` through `Show`; `\#` is literal; a `Str` argument is quoted under `#` and verbatim under `%`; a tuple argument under `#`; a missing witness reports at the call.
- Formatter: `satisfy C(T);` round-trips unexpanded; `wonder stage core-elab` shows the expansion and `stage core` the transient.
- Prelude: the derived `Option`/`Result`/`Order` witnesses are held by the archive build; carrier witnesses round-trip through parse for every literal form, including `Flt`'s non-finite values and negative zero.

## Completion criteria

- The body-less form parses, prints, formats and is documented; the tree-sitter grammar agrees.
- `/syn/Spell`, `/syn/Fields`, `/syn/Tupled` exist with their registry slots and `/std` facades; every carrier has a written witness; `/std`'s structural types have derived ones; `Spell` and `Show` have their tuple witnesses; `/std/Fmt` has the `#` slot.
- Every row under Verification is a test, and the gate passes.
- The roadmap gains the item under Type System beside instance arguments, and `documentation/design/language/` gains one decision file — the transient-hosted, signature-is-the-programmer's, tuples-by-description design, with the Text-stage expansion, the intrinsic `TupleOf`, row variables and the `Generic`-universe route recorded as rejected for the stated reasons.
- This file is deleted, its contracts moved to `syntax.md`, the elaborator's `derive` module documentation, `Fmt.crs`, the `/syn` sources, and the decision file. The items under [Adjacent work](#adjacent-work) outlive it and move to the roadmap in their own right.

### Decisions taken here, still reversible

- Absolute paths in spelled text (over shortest suffixes).
- `Int` spelled with an explicit sign.
- `?` for an explicit proof payload (over refusing, or `_`).
- Tuples served by one library witness over `Tupled(F)` and a compiler adapter at the goal, inline, rather than by a compiler-written witness per concept or an intrinsic `TupleOf`.
- Conjunction goals over explicit dictionary supply, still to be taken.
- `#` as the spell-slot, with `\#` as its escape.
- `/syn` as the home of `Spell`, `Fields` and `Tupled`, reached through their `/std` facades.

## Adjacent work

Four items the preparatory work surfaced that belong to no part of the derivation, listed so they are not lost with this file.

1. **Mutually recursive witnesses.** Two witnesses that resolve each other are refused by name; the capability is unbuilt, and until it exists a derived body for a mutual `induct` group must use local walkers. Discovery has to be post-elaboration: the lowerer's witness edges (`curios-text/src/into_core/order.rs`) are deliberately over-approximate, and grouping on them would fuse unrelated witnesses — members of one group share a universe context and take a single `group_totality` verdict, so one partial member would poison every witness fused with it. Re-elaboration needs a fresh `Context`, since the module fold registers each declaration once and refuses a duplicate.
2. **A settled tuple's type is available only at the item's drain.** `let z = id((1, true)); z.0` refuses with "projected from a non-tuple": the literal's product is synthesized when the drain establishes that nothing further can pin it, which is after every expression in the item has elaborated.
3. **The two universe-generalization paths differ.** `finalize_definition` subtracts `result_sort_only_metas` before generalizing and `elaborate_module_rec` does not, so a witness that recurses through its own entry generalizes differently from one that does not. Nothing in `/std` is affected — no witness there is recursive — but the asymmetry is real and should be settled deliberately rather than discovered.
4. **A tuple type's labels are part of its identity, and nothing yet states that in `syntax.md`.** The rule is what makes a per-arity written witness impossible and the description-passing design necessary; it is currently recorded only here.
