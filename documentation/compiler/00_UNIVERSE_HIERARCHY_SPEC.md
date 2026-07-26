# Implicit cumulative universe hierarchy

Working implementation specification for replacing Curios's `Type : Type` rule with an implicit cumulative hierarchy while preserving the surface spelling `Type`, unrestricted general recursion, the existing `Prop`, and the current programming model.

This effort removes the inconsistency caused specifically by `Type : Type`. It does not make the current language logically sound by itself: unrestricted `rec` can still inhabit arbitrary types, and inductive declarations remain unchecked for strict positivity. Termination, guarded recursion, positivity, and a final sound-kernel mode are separate projects.

When this work lands, fold the permanent calculus and compiler invariants into the owning Core and Text module documentation, update `SYNTAX.md` without introducing level syntax, supersede the one-universe decision in `DESIGN.md`, update `ROADMAP.md`, and delete this working specification after no remaining document refers to it.

## Objective

Introduce an infinite predicative hierarchy:

```text
Type 0 : Type 1 : Type 2 : …
```

while users continue to write only:

```crs
Type
Prop
```

Every written `Type` receives an inferred universe level. Reusable declarations are universe-polymorphic, every use instantiates their universe parameters freshly, and cumulativity permits a type from a lower universe wherever a higher universe is expected.

The completed implementation must compile the existing `/sys`, `/syn`, `/std`, examples, and tests without requiring universe annotations or changes to Curios source merely to accommodate the hierarchy.

## Permanent design decisions

**No surface level syntax.** `Type`, declaration syntax, application syntax, and the parser remain unchanged. Universe levels exist only in lowered Core, elaboration state, diagnostics, and internal printing.

**Implicit cumulative universes.** A written `Type` mints a fresh universe metavariable. `Type u` is accepted where `Type v` is expected when `u ≤ v`.

**Algebraic levels.** Internal levels support zero, successor offsets, and finite least upper bounds. Canonical `max` expressions are required so a nominal type has one natural inferred universe rather than many constrained but needlessly distinct instances.

**Declaration-local universe polymorphism.** Top-level definitions, local lets, recursive groups, inductives, structs, concepts, constructors, witnesses, generated `/sys` definitions, and polymorphic primitive signatures carry inferred universe schemes.

**Fresh use-site instantiation.** Every occurrence of a generalized binding receives fresh universe metavariables and records the resulting universe instance in Core.

**Local polymorphism is real polymorphism.** Local lets generalize universe variables not owned by their ambient context. There is no top-level-only exception.

**Recursion is monomorphic inside a group.** A recursive group is universe-polymorphic when used externally, but every self-call and mutual call within its body uses the group's current universe instance. Universe-polymorphic recursion is not inferred.

**Mutual inductives infer per-family result levels.** Curios does not adopt Lean's additional restriction that all members of a mutual inductive block have identical parameters and one result universe. Each family receives its own result level, connected to the others by its constructors' ordinary universe constraints.

**Canonical least solutions.** Flexible result and classifier levels are solved to the least algebraic expression permitted by their lower bounds. Unconstrained flexible levels default to zero.

**Residual constrained polymorphism.** When constraints have no unique least solution, the involved levels remain implicit generalized parameters with a residual universe context. The compiler does not reject an otherwise valid program merely because there is no user syntax with which to select an instance.

**`Prop` remains separate and impredicative.** `Prop` is not encoded as another `Type` level. Its proof irrelevance, erasure, subsumption, and large-elimination restrictions remain intact.

**No backend universe representation.** Levels are erased before Ersd. They do not enter the ABI, runtime, continuation IR, WebAssembly model, or emitted modules.

## Non-goals

- Termination checking or removal of unrestricted `rec`.
- Guarded recursion, clocks, sized types, or productivity checking.
- Strict-positivity checking.
- A complete sound-kernel mode.
- User-written universe variables, level expressions, annotations, or commands.
- Lean-compatible or Rocq-compatible surface syntax.
- Cumulative inductive subtyping beyond ordinary Type cumulativity.
- Template polymorphism as a separate user-visible feature.
- A stable serialized interchange format for universe schemes.
- Specialization or duplication of erased code by universe instance.

## Sort calculus

Core uses:

```text
Sort ::= Prop | Type Level
```

The axioms are:

```text
Prop   : Type 0
Type u : Type (succ u)
```

Subsumption is:

```text
Prop   ⊑ Type 0
Type u ⊑ Type v    when u ≤ v
```

Transitivity therefore admits `Prop` in every sufficiently high `Type`. No `Type u` is a subtype of `Prop`.

Definitional equality and subsumption remain distinct:

- `Type u` and `Type v` are definitionally equal only when the universe solver proves `u = v`.
- Checking an inferred `Type u` against an expected `Type v` requires only `u ≤ v`.
- Cumulativity raises the universe in which a type is accepted; it does not construct a new nominal type or insert a runtime lift.

### Dependent functions

For a dependent function type:

```text
(x : A) -> B
```

if `B : Prop`, the whole function type inhabits `Prop` regardless of `A`. This preserves the current impredicative proposition rule.

Otherwise, if:

```text
A : Prop or Type a
B : Type b
```

the function type inhabits:

```text
Type (max(contribution(A), b))
```

where:

```text
contribution(A) = 0    when A : Prop
contribution(A) = a    when A : Type a
```

The rule applies to the type of the domain term, not to a guessed level from its spelling. In particular, a binder `(A : Type u)` contributes `u + 1`, because the domain term `Type u` itself has type `Type (u + 1)`.

### Dependent tuples

The empty tuple type inhabits `Type 0`.

A nonempty tuple whose fields are all propositions inhabits `Prop`, preserving Curios's current proof-record behavior.

Any other tuple inhabits the least `Type` universe containing its computational fields. Proposition-valued fields contribute zero. Field types are checked sequentially under the preceding fields exactly as they are now.

### Primitive types and type formers

Ground primitive types inhabit `Type 0`, including:

- `Nat`
- `Byte`
- `Int`
- `Flt`
- `Bool`
- `Bits`
- `Bytes`
- `Handle`

Primitive type formers compute their result universe from their type arguments:

- `Lst(A)` inhabits the universe required by `A`, or `Type 0` when `A : Prop`.
- `Cell(A)` follows the same formation rule.
- Primitive operations that quantify over types receive ordinary universe-polymorphic schemes.

No primitive may continue returning one undifferentiated `Type`.

### Motives and elimination

An eliminator motive may return either `Prop` or `Type u` for an inferred `u`.

The motive checker must no longer construct one hard-coded function type ending in nullary `Type`. It instead checks that the motive body is a sort and retains the inferred sort. An elided motive uses a fresh flexible result universe.

The current large-elimination guard remains sort-based:

- elimination from a proof into `Prop` remains permitted;
- elimination from a proof into `Type u` remains subject to the existing empty-or-singleton rule;
- the numeric value of `u` does not change that decision.

## Algebraic levels

The semantic level language is:

```text
Level ::= 0
        | parameter
        | metavariable
        | Level + natural
        | max(Level, …, Level)
```

The concrete Rust representation may use a tree while constructing expressions, but equality, hashing, archive contents, constraint solving, and printing operate over a canonical normal form equivalent to:

```text
max(n, p₁ + k₁, p₂ + k₂, …, pₙ + kₙ)
```

where `n` and each `k` are natural numbers and each `p` is a parameter or metavariable head.

Normalization must implement:

- associativity of `max`;
- commutativity of `max`;
- idempotence of `max`;
- neutrality of zero;
- distribution of successor offsets over `max`;
- coalescing repeated heads at their greatest offset;
- absorption such as `max(u, u + n) = u + n`;
- deterministic ordering of atoms.

Offsets must use a checked integer representation. Source programs cannot reasonably exhaust a machine-sized offset without first exhausting compiler resources, but overflow remains a compiler error rather than wrapping.

Core defines distinct identities for:

```rust
UniverseMetaId
UniverseParam
```

Generalized parameters use a capture-safe level scope. Their representation must support alpha-equivalence, stable hashing, nested local schemes, and deterministic archival without relying on globally meaningful printed names.

## Universe constraints

The solver accepts:

```text
u ≤ v
u = v
```

Equality is represented as both inequalities unless the solver has a more direct equivalent operation.

Every constraint carries an origin:

- the source span that caused it, when available;
- a short category such as cumulativity, Type successor, function formation, field sizing, constructor sizing, conversion, or scheme instantiation;
- optional declaration and binder names for diagnostics.

The solver must:

- normalize both sides before insertion;
- detect impossible positive cycles such as `u + 1 ≤ u`;
- decide consistency of algebraic `max` constraints;
- compute least solutions for flexible outputs where a principal solution exists;
- preserve residual constraints among generalized parameters;
- instantiate a stored context with fresh metavariables;
- project internal metavariables out of a finalized declaration while retaining every implied relation among exported parameters;
- produce a deterministic explanation path for inconsistencies;
- support transactional rollback.

The implementation should follow the algebraic-universe and typical-ambiguity model used by Rocq rather than inventing a numeric-level search. The relevant reference is Hugo Herbelin's [Type Inference with Algebraic Universes in the Calculus of Inductive Constructions](https://rocq-prover.org/papers/type-inference-with-algebraic-universes-in-the-calculus-of-inductive-constructions). Agda's documented level algebra provides the expected normalization laws: [Universe Levels](https://agda.readthedocs.io/en/v2.7.0.1/language/universe-levels.html).

### Metavariable roles

Universe metavariables carry an inference role:

```text
Generalizable
Flexible
```

`Generalizable` means the level represents an input over which a reusable binding should remain polymorphic unless constraints determine it.

`Flexible` means the level is an output or classifier to be minimized.

A generalizable metavariable may still be solved or merged when its constraints determine it. The role grants eligibility for generalization; it does not force an unnecessary scheme parameter.

### Lowering context

Text lowering tracks whether a term occurs beneath an input-domain context.

- A `Type` in a function binder's domain is generalizable.
- Once a complete type occurs as the domain of an outer binder, every `Type` within that domain is generalizable, including codomains of nested higher-kinded function types.
- A `Type` that is the result sort of a definition, inductive, struct, or concept is flexible.
- A `Type` in a constructor payload or structure field type is generalizable.
- A `Type` in an index binder's type is generalizable.
- A `Type` in a lambda parameter annotation is generalizable.
- A bare type value in an ordinary body is flexible unless it occurs beneath such an input domain.
- Fresh levels used only to classify inferred Π-, Σ-, motive-, or primitive types are flexible.
- Fresh levels obtained by instantiating generalized parameters retain generalization eligibility if they survive into an enclosing reusable declaration.

The lowering context is lexical, not based on names or later reduction.

## Core representation

### Type nodes

Replace:

```rust
Subterm::Type
```

with:

```rust
Subterm::Type(Level)
```

Constructors must distinguish:

- a written Type carrying its lowering-minted metavariable;
- `Type 0` required by a primitive or `Prop`;
- `Type (succ u)` inferred as the type of `Type u`;
- an internal Type carrying a fresh flexible classifier.

A nullary `Term::type_()` helper must not silently mint a level. Callers must state whether they require a ground `Type 0`, a Type at a known level, or a fresh classifier.

### Universe contexts and schemes

Reusable values carry:

```rust
struct UniverseConstraint {
    lower: Level,
    upper: Level,
}

struct UniverseContext {
    parameter_count: usize,
    constraints: Vec<UniverseConstraint>,
}

struct UniverseScheme<T> {
    context: UniverseContext,
    value: T,
}
```

The exact field layout may differ, but these semantics are mandatory:

- parameters are bound, ordered, and alpha-equivalent;
- constraints may refer to those parameters;
- instantiation substitutes fresh metas into both the value and constraints;
- finalized global schemes contain no universe metavariables;
- a nested local scheme may also refer to levels bound by enclosing schemes;
- equality and hashing include the normalized universe context.

Definitions need one shared universe context for their type and body rather than unrelated schemes that could be instantiated inconsistently.

### Occurrence-specific instances

Add an internal term node equivalent to:

```rust
UniverseInst {
    head: Term,
    levels: Vec<Level>,
}
```

It is introduced only by elaboration when a generalized binding is referenced.

The node:

- is never parsed from source;
- participates in traversal, equality, hashing, reachability, free-variable analysis, spans, printing, zonking, reduction, and conversion;
- preserves the ordinary term scope of `head`;
- carries one argument for every generalized parameter in the referenced scheme;
- is removed by erasure;
- may reduce only through a binding whose scheme arity matches the level argument count.

Wrapping existing variables is preferred to extending `Var`, because term-variable capture and release can then keep using the existing locally-nameless machinery.

### Bindings

The following Core representations acquire universe contexts:

- `Definition`;
- each local `Let` binding;
- `RecGroup`;
- `InductDecl`;
- `StructDecl`;
- `Concept`;
- registered `Witness` signatures.

A `RecGroup` has one shared universe context covering every member type and body. Individual members may not finalize incompatible copies of the group's universe metavariables.

### Nominal normal forms

The concrete universe instance is part of nominal Core normal forms:

```rust
InductType {
    name,
    universes,
    params,
    indices,
}

Variant {
    name,
    universes,
    params,
    tag,
    payload,
}

StructType {
    name,
    universes,
    params,
}

Struct {
    name,
    universes,
    params,
    fields,
}
```

The level vector identifies the declaration instance and is checked against the declaration's universe context before its registry telescope is used.

Conversion compares universe instances as well as names and term parameters. Flexible use-site level metas may be constrained equal during that comparison.

Witness and nominal lookup keys continue to ignore levels and key on their existing rigid nominal or primitive heads. A lookup candidate is accepted only after full instantiated-type conversion succeeds.

## Text lowering and ID floors

`curios-text` owns the first universe-metavariable allocation, just as it owns metavariables for written holes.

The lowering context adds a dedicated entropy source and returns its count. The public lowering result becomes semantically equivalent to:

```rust
(
    curios_core::Module,
    term_metavariable_floor,
    universe_metavariable_floor,
    ForeignStore,
)
```

`PreparedPrelude` stores:

- the universe floor;
- every lowered `Type` level metavariable in its prepared Core prefix;
- the existing term-metavariable and binder floors.

Entrypoint lowering seeds its universe allocator from the prepared prelude floor, exactly as it currently seeds term metavariables and binders. Elaboration then seeds its own fresh universe IDs above the complete lowered module's floor.

Minting during elaboration is forbidden for written `Type`: re-elaboration, sharing, speculative checks, and cache hits must not give one lowered node a different universe identity.

The surface AST and parser retain nullary `Type`.

## Elaboration

### Sort checking

Introduce one authoritative operation:

```text
check_is_sort(term) -> (rebuilt term, Sort)
```

It elaborates a term as a type or proposition without comparing it to an arbitrary `Type ?u`.

Existing sites that mean “this term must be a type” use `check_is_sort`, including:

- function domains and outputs;
- tuple fields;
- definition signatures;
- recursive signatures;
- inductive parameters, indices, payloads, and terminals;
- struct parameters and fields;
- concept parameters and fields;
- motives;
- primitive type arguments;
- term-metavariable result types.

This prevents fresh internal upper-bound levels from being introduced merely because the old implementation used `Term::type_()` as a universal expected type.

### Type elaboration

Inference returns:

```text
Type u  ↦ Type u : Type (u + 1)
Prop    ↦ Prop   : Type 0
```

Function and tuple elaboration synthesize their result sort from the calculus above. When a finite maximum is required, construct or solve a flexible level to the normalized algebraic maximum.

Primitive type elaboration returns explicit levels rather than nullary `Type`.

### Checking and cumulativity

`expect` handles universe subsumption directly:

- inferred `Prop`, expected `Prop`: ordinary conversion;
- inferred `Prop`, expected `Type v`: accepted through `Prop ⊑ Type 0` and `0 ≤ v`;
- inferred `Type u`, expected `Type v`: add `u ≤ v`;
- inferred `Type`, expected `Prop`: reject;
- all other cases: ordinary conversion at the appropriate type.

The former special case `Prop ⊑ Type` in `typing.rs` becomes the first case of this general sort rule.

### Type conversion

Conversion of:

```text
Type u ≡ Type v
```

adds `u = v`.

Conversion must no longer compare arbitrary type components under one hard-coded `Term::type_()`. Add dedicated helpers for:

```text
compare_types(left, right)
compare_sorts(left, right)
compare_at_known_type(type, left, right)
```

Type components of primitives, applications, functions, tuples, matches, inductives, structs, and projections route through these operations.

Proof irrelevance still fires only when the actual comparison type is proposition-valued.

### Binding lookup

The context distinguishes monomorphic assumptions from generalized schemes.

Looking up a monomorphic local assumption returns its current term and type unchanged.

Looking up a generalized binding:

1. Mints fresh universe metas for the scheme parameters.
2. Adds the instantiated residual constraints transactionally.
3. Rebuilds the occurrence as `UniverseInst(head, levels)`.
4. Returns the correspondingly instantiated type.

Reduction of such an occurrence substitutes the same level arguments into the stored body before unfolding it.

### Local lets

Elaborate local lets sequentially.

For each binding:

1. Elaborate its declared type under the preceding bindings.
2. Check its value.
3. Add all constraints generated by its value.
4. Finalize universe metas not owned by the ambient context.
5. Store a local universe scheme.
6. Make subsequent bindings and the tail instantiate that scheme freshly.

Universe generalization has no value restriction because universe parameters are static and erased.

The rebuilt `Let` retains the scheme so later reduction can instantiate its value correctly.

### Recursive groups

Elaborate a recursive group in this order:

1. Register every member signature provisionally under one monomorphic universe environment.
2. Elaborate and rebuild all signatures.
3. Reassume the rebuilt signatures.
4. Register recursive slots and check all bodies at the same group universe instance.
5. Retry parked term constraints as today.
6. Rebuild any declaration registries owned by the group.
7. Add declaration-size constraints.
8. Finalize the entire group once.
9. Replace provisional assumptions, definitions, slots, and registry entries with the finalized group scheme.
10. Publish every external member occurrence through fresh scheme instantiation.

An internal recursive call never receives fresh group-level universe parameters. If its types force a strictly larger self-instance, finalization reports a universe inconsistency.

This policy does not change termination or reduction of `rec`.

### Witnesses

A witness declaration is registered provisionally after its rebuilt signature is known, preserving the current ability for its body to resolve recursively through its own entry.

After the body and any associated concept literal have elaborated, replace the provisional witness signature with its finalized universe scheme.

Witness probing and candidate commitment instantiate schemes freshly. Failed probes roll back every term and universe mutation.

Output universe parameters do not join the rigid witness lookup key. Complete conversion after lookup validates them.

## Declaration finalization

Finalization is one compiler operation used by top-level lets, local lets, recursive groups, and declaration registries.

Its input is:

- the checked type or member types;
- checked body or member bodies;
- associated registry entries;
- the universe solver mark taken before elaborating the binding;
- the set of ambient universe metas that may not be generalized;
- metavariable roles and origins.

It performs:

1. Normalize every live level and constraint.
2. Check consistency and report any positive cycle.
3. Merge forced-equal metas.
4. Solve flexible metas to their least algebraic expressions over ambient and eligible generalizable metas.
5. Default unconstrained flexible metas to zero.
6. Project internal classifier metas out of the context while retaining their implied exported constraints.
7. Generalize eligible metas not owned by the ambient context.
8. Promote otherwise-unsolved metas involved in a non-principal valid constraint to generalized parameters.
9. Canonically order generalized parameters by stable origin and dependency, never by hash-map iteration.
10. Rewrite the type, body, registry metadata, and constraints with bound universe parameters.
11. Verify that no declaration-local universe metavariable remains.
12. Verify that every bound universe reference is in scope.
13. Verify that the finalized residual context is itself satisfiable.

For an entrypoint body, step 7 is replaced by choosing the canonical least ground solution. If no pointwise least ground solution exists, choose a deterministic solution by the solver's documented stable parameter ordering.

## Inductive declarations

### Result universes

Each Type-valued inductive result sort lowers to a flexible level:

```text
induct I(...) : Type i
```

Constructor sizing determines the least `i`. A closed ground datatype therefore lands in `Type 0`, while a container tracks the levels of the data it stores.

A proposition-valued inductive retains result sort `Prop`.

### Constructor size constraints

For a Type-valued family with result `Type i`, inspect each constructor's full params-first telescope.

For every leading uniform declaration parameter domain `P`:

- if `P : Prop`, add no Type constraint;
- if `P : Type p`, add `p ≤ i + 1`.

For every non-uniform constructor payload domain `B`:

- if `B : Prop`, add no Type constraint;
- if `B : Type b`, add `b ≤ i`.

These rules distinguish:

```text
A : Type u
```

as a uniform declaration parameter from a constructor that stores a type:

```text
code(A : Type u)
```

The former permits a result at `Type u`; the latter requires at least `Type (u + 1)`.

Ordinary stored data behaves as expected. If `x : A` and `A : Type u`, then the payload domain `A` has sort `Type u`, requiring only `u ≤ i`.

Apply these constraints once per actual constructor. A constructorless family receives no artificial lower bound from parameters alone.

For a `Prop`-valued inductive, impose no constructor universe-size constraints. Impredicative proposition formation and the existing large-elimination restriction provide the current behavior.

Agda identifies the corresponding constructor-argument check as the exact check disabled by its unsafe `NO_UNIVERSE_CHECK` pragma: [Universe Levels](https://agda.readthedocs.io/en/v2.7.0.1/language/universe-levels.html). Lean documents the same uniform-parameter versus other-constructor-parameter distinction under [Inductive Type Universe Levels](https://lean-lang.org/doc/reference/latest/The-Type-System/Inductive-Types/).

### Indices

Index binder types are checked as sorts and may contribute generalizable universe parameters to the type former.

There is no blanket rule requiring the result universe to dominate every index type. Any index expression needed to construct a value appears through the constructor telescope or target and is constrained by that constructor's actual binders.

Constructor terminals must be saturated applications of the family's current universe instance. Registry rebuilding verifies that terminal indices and parameters use the same declaration instance as the constructor being checked.

### Mutual inductives

One mutual group has one shared universe context but may contain distinct result expressions:

```text
I : Type i
J : Type j
```

Each constructor contributes constraints to the family it constructs. A payload containing another group member contributes that member's instantiated result universe in the ordinary way.

The group finalizes once so recursive references share a coherent instance. Members may retain parameters unused by that member when those parameters are required by the shared recursive scheme; finalization should remove parameters that are provably irrelevant to the entire group but must not split the group into inconsistent schemes.

Mixed `Prop`- and Type-valued mutual groups require the same existing elimination and erasure guarantees as independent declarations. Universe inference alone does not add a blanket same-sort restriction.

### Registry phases

The current lowering duplicates declaration information between generated definitions and registry entries. Those copies must share one finalized universe context.

For a mutual inductive group:

1. Seed provisional lowered registry entries.
2. Elaborate type-former signatures.
3. Rebuild parameter and index telescopes.
4. Check type-former bodies and fill recursive slots.
5. Rebuild constructor telescopes and terminals.
6. Add constructor-size constraints.
7. Finalize the group universe context.
8. Rewrite both the generated definitions and every `InductDecl` with that context.

No lowered or provisional level may survive into later reduction.

## Structs and concepts

A Type-valued struct or concept has flexible result universe `i`.

Its declaration parameters use the uniform inductive rule:

```text
P : Type p  ⇒  p ≤ i + 1
```

Its fields use the non-uniform payload rule:

```text
F : Type f  ⇒  f ≤ i
```

Proposition-valued parameters or fields add no Type-level lower bound.

The existing rule for a `Prop`-valued struct remains stronger and unchanged: every field must itself be a proposition because projection is an unrestricted eliminator.

A concept's `Concept` and `StructDecl` entries share the same universe context and parameter telescope. Generated method wrappers independently generalize their own definitions while instantiating the concept scheme consistently.

Struct construction, projection, concept superclass traversal, and witness literals instantiate registry metadata from the concrete `StructType` universe vector before inspecting fields.

## Primitive and generated declarations

Generated `/sys` definitions are not privileged with respect to universes.

Their current source or generated-Type occurrences lower through the same allocator and are generalized like authored declarations. Ground carriers remain in `Type 0`; polymorphic list, cell, foreign, and helper definitions receive schemes.

Primitive inference must return explicit result levels for every primitive node. Any primitive that accepts a type argument checks it with `check_is_sort` and computes its carrier universe from the resulting sort.

The canonical syntax registry remains unchanged because no new `/syn` name or surface spelling is introduced.

## Reduction

Reduction remains deadline-bounded and retains Curios's coinductive treatment of recursive terms.

Universe-related reduction consists only of:

- substituting level arguments when unfolding `UniverseInst`;
- normalizing algebraic level expressions;
- consulting an instantiated registry entry;
- preserving universe vectors on nominal normal forms.

Level normalization must terminate independently of term reduction and must not consume the type-level reduction deadline through an unbounded term loop.

A universe instance never causes runtime specialization. Two uses of one definition at different levels unfold the same body under different erased static substitutions.

Local `let` reduction must use the binding's stored universe scheme rather than substituting a raw polymorphic value with no level environment.

Recursive members retain their group's current level instance during unfolding.

## Transactions, parking, and caches

### Composite solver marks

Replace the term-only solution mark with a composite elaboration mark covering:

- term-metavariable solution-log length;
- universe-metavariable solution-log length;
- universe-constraint watermark;
- solver union or normalization mutations that require restoration;
- any universe-origin diagnostics added speculatively.

Every current caller of `solution_mark` and `rollback_solutions` migrates to the composite mark.

Rollback:

- removes all constraints and assignments introduced after the mark;
- restores solver equivalence classes or substitutions;
- clears every cache whose result may have depended on rolled-back state;
- advances the mutation stamp as appropriate;
- does not reuse a rolled-back ID for a different semantic metavariable.

### Parked work

Universe inequalities involving unsolved universe metas are recorded immediately and need not create parked goals merely because their values are unknown.

Term work still parks on unsolved term metas as today. When a retried term comparison introduces universe constraints, those constraints use the retry's normal transaction.

A universe inconsistency discovered after a term meta solves reports at the originating comparison or declaration finalization, not as a generic unsolved term goal.

### Reduction cache

Do not cache a reduct whose input or result contains an unresolved universe metavariable.

Finalized bound universe parameters are immutable and may be cached when every other existing cache condition holds.

Rollback of universe state clears reduction entries that could have observed the reverted state.

### Elaboration cache

Do not cache elaboration of a term or expected type containing unresolved universe metas. A cached elaboration result must never stand in for the universe constraints that elaborating the node would have inserted.

The mutation-stamp eligibility test includes universe-solver mutation.

Term nodes add cached `has_universe_meta` or equivalent O(1) derived metadata alongside their existing term-metavariable and local-free-name facts.

## Zonking and final validation

Universe zonking:

- substitutes solved universe metas;
- normalizes all level expressions;
- rewrites finalized parameters into their bound representation;
- zonks universe arguments in every term and registry node;
- zonks residual constraints.

`zonk_module` fails if:

- any universe metavariable remains outside a declaration finalization boundary;
- a declaration contains a universe meta after finalization;
- a level parameter is out of scope;
- a universe-instance arity differs from its binding or registry scheme;
- a residual universe context is inconsistent;
- generated definition and registry copies disagree about their universe context.

The elaborated and zonked Core module is the sole input to erasure. Lowered modules containing universe metas never reach erasure.

Add a final universe-validation pass after zonking. It is an invariant checker, not a second inference engine. It validates closure, arities, normalized contexts, and registry synchronization so archive construction fails loudly on malformed generalized Core.

## Erasure

Universe information is static and fully erased:

- `Type(level)` erases exactly as `Type` does now.
- `UniverseInst` erases to the erased meaning of its head.
- universe contexts and constraints emit no Ersd items or operands;
- nominal universe vectors do not enter runtime schemas;
- one source definition produces one erased definition regardless of the number of universe instances.

Universe levels cannot change whether a term is a proof. `Prop` remains a separate sort, so existing proof/type relevance masks and struct/variant layouts remain valid across every universe instance.

No change is permitted in:

- `curios-abi`;
- `curios-ersd` representations;
- `curios-cont`;
- `curios-wasm`;
- `curios-binaryen`;
- `curios-runtime`;
- native or browser host bindings.

Core erasure code still requires mechanical support for traversing or dropping the new nodes and schemes.

## Prelude archive and replay

The build-scoped prelude archive changes incompatibly and therefore increments `curios-prelude`'s archive schema.

The prepared Text portion archives:

- lowered prelude Core containing universe metas;
- the universe allocator floor;
- existing resolver, interface, term-metavariable, and binder state.

The elaborated Core portion archives:

- universe-closed definitions and recursive groups;
- bound universe contexts;
- normalized registry contexts and universe vectors;
- no universe metas.

The Ersd portion remains universe-free.

Replay:

1. Restores and validates the archived Core universe contexts.
2. Registers inductive, struct, and concept schemes.
3. Replays definitions as schemes rather than monomorphic type/body pairs.
4. Re-registers witnesses from finalized schemes.
5. Seeds entry-owned lowering and elaboration above the archived universe floors.
6. Instantiates cached prelude bindings freshly at user use sites.

From-scratch prelude elaboration and cached-prefix replay must produce structurally equal zonked user Core and equal Ersd.

The archive remains compiler-build-scoped and is not a stable universe interchange format.

## Printing and diagnostics

### Surface printing

The Text parser and printer continue to round-trip:

```text
Type
```

They never emit universe levels.

### Core printing

Core debug printing may use:

```text
Type.{0}
Type.{u}
Type.{u+1}
Type.{max(u,v)}
name.{u,v}
```

This syntax is diagnostic-only and need not parse as Curios source.

Stable parameter names are assigned from declaration-local order. Raw allocation IDs must not leak into golden output unless a test explicitly prints pre-finalization state.

### Errors

Add an error equivalent to:

```rust
UniverseInconsistency {
    lower: Level,
    upper: Level,
    path: Vec<UniverseConstraintOrigin>,
}
```

User-facing messages lead with the semantic conflict:

```text
this Type would need to be strictly below itself
```

or:

```text
this recursive use requires a larger universe than the recursive definition's current universe
```

When useful, follow with an internal explanation:

```text
required constraints: u + 1 ≤ v and v ≤ u
```

Diagnostics anchor first at the source construct that closed the inconsistent cycle and may cite the earlier Type, binder, field, constructor, or use that supplied the other edge.

A type mismatch caused only by universe levels should report a universe inconsistency rather than printing two visually identical surface `Type` terms as an ordinary mismatch.

No diagnostic instructs the user to write a level annotation.

## Code ownership and expected changes

### `curios-core`

Owns:

- level algebra and normalization;
- universe constraints and inference;
- bound universe scopes and schemes;
- explicit Type levels and universe-instance terms;
- sort synthesis and cumulativity;
- conversion and transaction integration;
- declaration finalization;
- inductive and struct sizing;
- registry instantiation;
- zonking and universe validation;
- erasure of universe structure.

A focused `universe.rs` plus adjacent tests should own the algebra, solver, contexts, and diagnostic explanation. It must not be hidden inside term conversion.

The principal existing consumers are:

- `term.rs` and `scope.rs`;
- `context.rs`;
- `typing.rs`;
- `convert.rs` and `convert/prim.rs`;
- `elaborate.rs` and its `apply`, `aggregate`, `binding`, `match_`, `metavar`, `module`, `prim`, and `struct_` submodules;
- `module.rs`, `inductive.rs`, `structure.rs`, and `concept.rs`;
- `reduce.rs`;
- `zonk.rs`;
- `print.rs`;
- `erase_ir`.

### `curios-text`

Owns:

- minting one level meta per written `Type`;
- generalizable-versus-flexible lowering context;
- universe floors;
- preserving the nullary surface syntax;
- carrying universe state through prepared-prelude lowering.

The parser and surface AST do not acquire level fields.

### `curios-prelude`

Owns:

- the incompatible archive schema increment;
- build-time storage of universe floors and finalized schemes;
- restoration validation.

No authored `/syn` or `/std` change is expected.

### `curios-pipeline`

Threads the universe floor from Text lowering into Core elaboration and keeps full and cached-prelude compilation behavior identical.

### Downstream crates

No semantic change. Any compile error outside Core, Text, Prelude, or Pipeline should be caused only by an API signature or exhaustive-match update and must not introduce a downstream universe representation.

## Standard-library compatibility audit

At the time this specification was written, `/syn` and `/std` contain 214 textual `Type` occurrences across 34 source files. Eleven recursive or mutual declarations have Type-bearing signatures.

The implementation has a zero-source-churn acceptance target for those files.

Representative inferred schemes are:

```text
Option      : (A : Type u) -> Type u
Result      : (A : Type u, E : Type v) -> Type (max(u, v))
Vec         : (T : Type u, n : Nat) -> Type u
Eq          : (A : Type u, x : A, y : A) -> Prop
Show        : (A : Type u) -> Type u
Parse       : (A : Type u) -> Type u
```

The exact concept universe for higher-kinded concepts such as `Monad` is inferred from the kind of the constructor parameter and its method-field types. It may be one level above the element universes because the dictionary stores methods that themselves quantify over types.

`Eq/subst` uses independent universes for `A` and the motive result:

```text
A : Type u
P : A -> Type v
```

No equality between `u` and `v` is required.

`Fmt/format_type_with` is the strongest prelude stress case. Its scheme must admit constraints equivalent to:

```text
T : Type u
generated show argument : Type a
result : Type w
u ≤ w
a + 1 ≤ w
```

Its recursive call uses the same `u`, `a`, and `w`. The recursion does not ascend to a fresh larger universe.

The `Pause` and `Async(A)` mutual inductive block must continue to compile. Per-family result inference and one shared group context may cause a private member to inherit an otherwise unused group universe parameter; this is acceptable and erased.

Every audited generic recursive function calls itself at the same universe instance. Universe-polymorphic recursion is not required by the existing prelude.

If any authored prelude source must change, stop and classify the cause:

- a genuine former `Type : Type` cycle;
- unintended lack of local or declaration polymorphism;
- an over-conservative inductive sizing rule;
- a missing scheme instantiation;
- a solver defect;
- an unrelated pre-existing error exposed by the change.

Do not rewrite the prelude to accommodate an incomplete inference implementation.

## Verification

### Level algebra

- Normalize zero, successors, nested maxima, duplicate atoms, and absorbed offsets.
- Verify commutativity, associativity, idempotence, and deterministic hashing.
- Reject offset overflow.
- Confirm alpha-equivalent bound level contexts compare and hash equally.

### Constraint solver

- Accept acyclic inequalities.
- Reject `u + 1 ≤ u`.
- Reject longer positive cycles and report their complete origin path.
- Solve flexible upper levels to canonical maxima.
- Default unconstrained flexible outputs to zero.
- Merge equality classes.
- Project internal variables while preserving implied exported constraints.
- Retain valid non-principal constraints as residual contexts.
- Instantiate one residual context twice with disjoint fresh metas.
- Roll back every solver mutation to an earlier composite mark.

### Sort rules

- Infer `Prop : Type 0`.
- Infer `Type u : Type (u + 1)`.
- Accept `Prop` where `Type 0` is expected.
- Accept `Type u` where `Type v` is expected under `u ≤ v`.
- Reject `Type` where `Prop` is expected.
- Infer proposition-valued Π- and all-proof tuple types as `Prop`.
- Infer computational Π- and tuple types at the normalized maximum of their components.

### Polymorphism

- Use one top-level identity definition at `Nat` and at `Type 0` in the same program.
- Do the same with a local let.
- Pass a higher-kinded nominal type former such as `Option` to `Monad`.
- Confirm two occurrences of one scheme have distinct fresh level metas before conversion.
- Confirm conversion can constrain two compatible occurrences equal.
- Confirm a body-only unused classifier does not become an unnecessary universe parameter.

### Recursion

- Use a recursive function polymorphically from two external call sites.
- Confirm its internal self-calls retain the current group instance.
- Reject a constructed Core recursive call that requires a strictly larger self-instance.
- Compile every existing generic `rec` declaration.
- Compile `Fmt/format_type_with` and assert its normalized scheme constraints.

### Inductives and structs

- Infer a closed enumeration in `Type 0`.
- Infer `Option(A)` in `Type u`.
- Infer `Result(A, E)` in `Type (max(u, v))`.
- Infer a constructor storing `A : Type u` in at least `Type (u + 1)`.
- Give a constructorless inductive no payload-derived lower bound.
- Keep arbitrary Type parameters on `Prop`-valued `Eq`.
- Preserve Eq's singleton large elimination.
- Preserve rejection of informative fields in a `Prop` struct.
- Compile `Pause`/`Async` with distinct per-family result expressions.
- Confirm constructor terminals and registry instances agree.

### Witnesses

- Register and resolve a universe-polymorphic witness at multiple instances.
- Keep nominal witness keys independent of universe levels.
- Confirm full conversion rejects an incompatible instantiated candidate.
- Confirm a failed candidate probe leaks no universe constraints or solutions.
- Confirm a recursive witness sees its provisional same-instance entry and publishes a finalized external scheme.

### Reduction and conversion

- Unfold one definition at two universe instances to level-substituted copies of the same body.
- Convert Type levels by equality and check them by inequality.
- Compare nominal instances including their universe vectors.
- Confirm proof irrelevance is unchanged.
- Confirm reduction and elaboration caches refuse unresolved universe metas.
- Confirm rollback invalidates affected cache entries.

### Lowering and printing

- Lower distinct written `Type` occurrences to distinct metas.
- Preserve one meta when a lowered node is cloned.
- Seed entry IDs strictly above prepared-prelude IDs.
- Round-trip surface `Type` without levels.
- Print deterministic Core universe parameter names and normalized maxima.

### Archive and erasure

- Build and restore a prelude archive containing lowered universe metas and finalized bound schemes.
- Reject an archive with an invalid schema, out-of-scope level, inconsistent context, or mismatched registry scheme.
- Compare from-scratch and cached-prefix zonked Core.
- Compare from-scratch and cached-prefix Ersd.
- Confirm two universe instances erase to one runtime definition and one unchanged layout.

### Integration

- Compile all `/sys`, `/syn`, and `/std` sources without edits.
- Compile every program and cross-stage integration fixture.
- Run stage-local Core, Text, Prelude, and Pipeline tests while iterating.
- Before handoff, run the full repository validation gate from `AGENTS.md`.

## Expected churn

This is a low source-compatibility risk but a medium-to-large compiler implementation.

Expected production impact:

- approximately 30–40 Rust files;
- approximately 3,500–6,500 production lines;
- concentrated in Core with smaller Text, Prelude, and Pipeline changes;
- no runtime or backend representation changes.

Expected test impact:

- approximately 2,000–4,000 lines;
- substantial mechanical updates to exact Core-term fixtures;
- new solver, transaction, scheme, inductive-sizing, archive, and compatibility tests.

Expected authored Curios source impact:

- zero `/sys`, `/syn`, or `/std` changes;
- zero example changes;
- no level annotations.

These estimates include full local-let polymorphism, residual constraints, archive replay, and transactional integration. A smaller patch that merely attaches integers to `Type` does not satisfy this specification.

## Completion criteria

- Surface Curios still has only `Type` and `Prop`, with no level syntax.
- Core has no `Type : Type` rule.
- Every written `Type` receives a fresh inferred level.
- `Prop : Type 0`, Type cumulativity, Π formation, tuple formation, primitive formation, and motive formation implement this specification.
- Every reusable binding kind carries a finalized universe scheme and every use instantiates it freshly.
- Recursive groups are polymorphic externally and monomorphic internally.
- Inductive and struct result levels satisfy the constructor and field sizing rules.
- Nominal terms and registries preserve concrete universe instances coherently.
- Failed speculative elaboration rolls back universe state with term-meta state.
- No cache reuses a result that omitted live universe constraints.
- Zonked modules and archived Core contain no universe metas.
- Ersd and all downstream representations remain universe-free.
- `/sys`, `/syn`, `/std`, examples, and existing Curios programs compile without source changes attributable to the hierarchy.
- Direct inconsistency tests such as `u + 1 ≤ u` are rejected with source-oriented diagnostics.
- The full repository validation gate passes.
- Durable rules are transferred to their owning documentation and tests, the old one-universe design decision is superseded, the roadmap records the landed capability, and this working specification is deleted.
