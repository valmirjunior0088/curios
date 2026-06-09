# Inductive Types

Working design sketch. Not yet implemented.

## Framing: structural vs. nominal

This migration isn't primarily about fixing the brittleness in the current
union desugar — that's a side-effect. The change is about making explicit a
seam Curios has been implicit about: there are two type-formation principles
in the language, and they should be primitives that serve different roles.

- **Structural types**: tuples, function types. Anonymous, compared by
  shape. Two `{x : Nat, y : Bin}`s are the same type because they look the
  same; there's no further identity. The structural side is for ad-hoc data
  bundling, multi-return, function signatures — anywhere "the shape *is* the
  type."

- **Nominal types**: inductives (the subject of this doc). Declared,
  compared by identity. `Result(A, E)` and `Option(A)` are different types
  even if they encode similarly. The nominal side is for types whose
  identity is part of the design — exhaustiveness checking, named
  constructors, abstract interfaces — anywhere "the name *is* the type."

These coexist on purpose. The seam answers design questions that have been
murky: "tuple or union?" becomes "do you want structural or nominal
identity?" The atom-plus-tuple combination (`{'foo, payload}`) plays a clean
intermediate role — ad-hoc nominal-flavored structural, useful when you
want discrimination without a formal declaration.

The migration is the moment this seam stops being implicit. Tuples stay
structural. `union` declarations become primitive nominal types. The
brittleness in the current encoding came from trying to express nominal
identity through structural machinery; separating them removes the
mismatch.

Future design slots in cleanly on whichever side a feature belongs:
structural extensions (anonymous records, row polymorphism, structural
subtyping) on one side; nominal extensions (indexed inductives, abstract
types, exhaustiveness/totality work) on the other.

## Goals

- Make union types a primitive in `core` rather than a tagged-tuple encoding.
- Type arm binders directly from the constructor's stored telescope; no projections from a stuck payload.
- Static arity checking on match arms (today: arity mismatches against the constructor signature are silent).
- Eliminate the surface-vs-canonical refinement-key dance that the tagged-tuple encoding forces on dependent pattern matching.
- Open the door to flat sum-of-products at runtime (Phase 5; not required for the elaboration win).

## Non-goals

- **Indices.** Parameter-only inductives. Indexed inductives, dependent pattern matching with index unification, heterogeneous equality, JMeq — all out of scope for v1.
- **Mutual / nested inductives.** Out for v1.
- **Local inductives.** Module-level only, same as today's `union`.
- **Recursors / induction principles.** Match node is the only eliminator. No auto-generated `Result_rec`.
- **Removing atoms.** Atom and atom-match stay primitives. Their removal is a possible later step once inductives are stable; not required.
- **Removing `Bln` and `Nat` as kernel primitives.** They could be re-expressed as well-known inductives later; for v1 they stay primitive for ergonomics and performance.

## Surface

No surface changes. Existing `union` declarations, `match | ctor(x) => …` expressions, and `Result/success(value)` call sites work identically. Users do not see the kernel switch.

```
pub union Result(A : Type, E : Type)
    | success(A)
    | failure(E)
    end

let r : Result(Nat, Bin) = Result/success(42);

let n = match r
    | success(value) => value
    | failure(_)     => 0
    end;
```

The above is what users write today and what they continue to write. Implementation underneath swaps.

## Core IR additions

Three new `Subterm` variants:

```rust
// The inductive type as a primitive normal form. Built inside the
// automatically-generated type-constructor function's body. Users never write
// one directly — they write `Result(A, E)` and the type-constructor function
// reduces to this.
UnionType {
    name: Qualifier,            // Result
    params: Vec<Term>,          // [A, E]
}

// A constructor application as a primitive normal form. Built inside the
// automatically-generated value-constructor function's body. Users never write
// one directly — they write `Result/success(value)` and the constructor
// function reduces to this.
UnionCtor {
    name: Qualifier,            // Result (the inductive's qualified name)
    params: Vec<Term>,          // [A, E]  — instantiated inductive-type parameters
    tag: Atom,                  // 'success
    payload: Vec<Term>,         // [value] — constructor's payload values
}

// The primitive eliminator. Replaces the tagged-tuple Match::Union path.
UnionMatch {
    head: Term,
    motive: Scope<One>,                       // (x : Result(A, E)) -> Type
    cases: BTreeMap<Atom, Scope<Many>>,       // arity per case = constructor's payload arity
}
```

`Match` (atom-match), `BlnMatch`, `NatMatch` stay unchanged. They handle their own primitive domains; `UnionMatch` handles inductive types.

`UnionCtor` carries `name` and `params` redundantly with its type: they're recoverable from the term's inferred type. The redundancy is taken deliberately — storing them on the term keeps `convert` purely structural (no context lookups mid-comparison) at the cost of a few extra fields per constructor value.

### Inductive declaration registry

Inductive declarations live in a module-scoped side-table. When the elaborator processes a `union` declaration:

```
union Result(A : Type, E : Type) | success(A) | failure(E) end
```

it produces three artifacts:

1. **Type-constructor function binding** `Result : (A : Type, E : Type) -> Type` whose body is `(A, E) => UnionType { Result, [A, E] }`. Calling `Result(Nat, Bin)` beta-reduces to `UnionType { Result, [Nat, Bin] }` — a primitive normal form. Two `UnionType`s are convertible iff same name and pointwise-convertible params.

2. **Registry entry** under `Result`. The entry records:
   - The parameter telescope `(A : Type, E : Type)`.
   - The per-constructor signatures: `success : (value : A) -> Result(A, E)`, `failure : (err : E) -> Result(A, E)`. Each signature is a `Telescope<Term>` (the existing telescope machinery) terminating in `UnionType { Result, [A, E] }`.

3. **Constructor function bindings** `Result/success`, `Result/failure`. Each is a regular `let` whose body is a function returning a `UnionCtor`:

   ```
   let Result/success(@A : Type, @E : Type, value : A) -> Result(A, E) =
       <UnionCtor { name: Result, params: [A, E], tag: 'success, payload: [value] }>
   ```

The registry is consulted by the elaborator during `UnionMatch` checking — it provides the per-constructor telescopes used to type arm binders.

## Walking through `Result`

### Declaration

User writes:

```
pub union Result(A : Type, E : Type) | success(A) | failure(E) end
```

After lowering:

- `Result : (A : Type, E : Type) -> Type = (A, E) => UnionType { Result, [A, E] }` registered as a public let.
- `Result/success : (@A : Type, @E : Type, value : A) -> Result(A, E) = (@A, @E, value) => UnionCtor { Result, [A, E], 'success, [value] }`.
- `Result/failure : (@A : Type, @E : Type, err : E) -> Result(A, E) = (@A, @E, err) => UnionCtor { Result, [A, E], 'failure, [err] }`.
- Registry records the parameter telescope and per-constructor signatures.

### Construction

User writes `Result/success(42)`:

1. Parser produces `Apply(Result/success, [42])`.
2. Elaborator infers `Result/success`'s type, inserts implicit args (`@A := Nat`, `@E := ?` to be solved by use), checks `42 : Nat`. The elaborated term is the `Apply`; its normal form (after unfolding the constructor function's let definition and beta-reducing) is `UnionCtor { Result, [Nat, ?E], 'success, [42] }`.

### Match

User writes:

```
match r
    | success(value) => value
    | failure(_)     => 0
    end
```

Elaboration:

1. Elaborate `r`; infer its type to `UnionType { Result, [Nat, Bin] }` (or whatever).
2. Look up `Result` in the registry. Get parameter telescope and per-constructor signatures.
3. For each arm:
   - Find the matching constructor's telescope, instantiated at the inferred params.
   - Open the telescope with the arm's binder names. `success(value)` → `value : Nat` in scope.
   - **Arity check**: the arm's binder count must equal the constructor telescope's binder count. Static error otherwise.
   - Build the constructor value `UnionCtor { Result, [Nat, Bin], 'success, [Var(value)] }`.
   - Open the motive with that constructor value to get the arm's expected type.
   - Check the arm body against the expected type.
4. Emit `UnionMatch { head: r_elaborated, motive, cases: <arm scopes> }`.

The arm-body type-checking *does not* depend on refinement to make binder types valid — they're typed directly from the telescope. Refinement still propagates to other occurrences of `r` in the arm body (it gets refined to the constructor value per arm), but that's a clean refinement on a Var, not the projection-through-stuck-payload story.

## Reduction

```
reduce(UnionMatch { head, motive, cases }):
    let head' = reduce(head);
    match head' {
        UnionCtor { tag, payload, .. } if cases.contains(tag) =>
            cases[tag].open(&payload)         // beta-reduce the arm
        _ =>
            UnionMatch { head: head', motive, cases }   // stuck
    }

reduce(UnionCtor { name, params, tag, payload }):
    // Reduce sub-terms; otherwise inert (no further reduction).
    UnionCtor { name, params: params.map(reduce), tag, payload: payload.map(reduce) }

reduce(UnionType { name, params }):
    UnionType { name, params: params.map(reduce) }
```

`UnionCtor` and `UnionType` are normal forms. `UnionMatch` reduces if its head reduces to a known constructor; otherwise stays stuck.

## Conversion

```
convert(UnionType { n1, ps1 }, UnionType { n2, ps2 }):
    n1 == n2 && ps1.pairwise_convertible(ps2)

convert(UnionCtor { n1, ps1, t1, p1 }, UnionCtor { n2, ps2, t2, p2 }):
    n1 == n2 && t1 == t2 && ps1.pairwise(ps2) && p1.pairwise(p2)

convert(UnionMatch { h1, m1, c1 }, UnionMatch { h2, m2, c2 }):
    convert(h1, h2) && convert_scope(m1, m2)
        && c1.keys() == c2.keys()
        && c1.zip(c2).all(|(s1, s2)| convert_scope(s1, s2))
```

Standard structural rules. No projection or tag-dispatch gymnastics. Stuck `UnionMatch`es (head not yet a `UnionCtor`) compare by reducing both heads first and then applying these rules — the same way stuck atom-matches are compared today.

## Elaboration: match algorithm

In `src/core/elaborate.rs`, a new `elaborate_union_match`:

```
1. Elaborate head; infer its type; reduce.
2. The type must be UnionType { name, params }; otherwise type error.
3. Look up name in the inductive registry; get param telescope and ctor signatures.
4. Substitute params into each ctor signature to get instantiated telescopes.
5. check_motive(context, head_type, motive)              // single motive, returns checked scope
6. For each arm (atom, binders, body) ∈ user's cases:
       a. The atom must exist in ctor signatures; else error.
       b. binders.len() must equal the ctor telescope's arity; else error.
       c. Open the ctor telescope with fresh names paralleling the user's binder names.
          Each binder gets its declared type from the telescope.
       d. Build the constructor value:
              ctor_val = UnionCtor { name, params, tag: atom, payload: <fresh binder vars> }
       e. Open the motive with ctor_val to get expected_type.
       f. With refinement (head := ctor_val), check body against expected_type.
       g. Close the arm body as Scope<binder_count>(binders, body_elaborated).
7. Emit UnionMatch { head, motive, cases } and the full match's return type
   = motive.open(&[&head_elaborated]).
```

Arity-check at step 6b is the new static safety net.

Refinement at step 6f propagates `head := ctor_val` to *other* uses of `head` in the arm body, but the binder types themselves come from the telescope (step 6c) — no projection-through-payload, no refinement-keying brittleness.

## Lowering (erase / ersd)

For Phases 1–4, preserve the existing runtime representation: a `UnionCtor` lowers to a tagged tuple, exactly what today's desugar emits. The emitted `ersd::Match` is atom-shaped — `ersd` has no nominal-union concept, only atom-match — so erasure produces the same shape it produces today, just from a different upstream source.

```
erase(UnionCtor { tag, payload, .. })
    = ersd::Tuple { fields: [ersd::Atom(tag), ersd::Tuple { fields: payload.map(erase) }] }

erase(UnionMatch { head, motive, cases })
    = ersd::Match { head: ersd::Proj(erase(head), 0),                // atom-match on the tag
                    cases: cases.map(|s| s.open(&projections_of(head, payload_arity))) }
```

`cont`/`optm`/`wasm` need no changes through Phase 4 — they continue to see only the atom-match-plus-projections shape they already handle.

Phase 5 swaps this lowering to flat sum-of-products. `UnionCtor` emits a single allocation per constructor, with per-variant arities; `UnionMatch` dispatches and projects directly. The `projectable_at` workaround in `src/core/erase.rs` is no longer needed because erase always sees a primitive `UnionMatch`, not an atom-match-plus-stuck-payload-projection.

## Migration phases

Each phase ends with `cargo test --workspace` passing and the codebase compiling.

### Phase 1: Kernel scaffolding (no behavior change)

- Add `Subterm::UnionCtor`, `Subterm::UnionMatch`, `Subterm::UnionType` to `src/core/term.rs`.
- Add the inductive registry to `src/core/context.rs` (or a new module).
- Stub `reduce`, `convert`, `zonk`, `erase`, `print` cases for the new variants. They handle them structurally but are never exercised because no surface code produces them yet.
- All existing tests pass (the new variants are dead code).

### Phase 2: Migrate `Result` end-to-end

- Change `src/text/to_core.rs`'s `TopItem::Union` desugar to emit `UnionType` + registry entries + ctor functions for `Result` specifically (gated by name during the spike).
- Implement `elaborate_union_match` for `UnionMatch`.
- Implement the reduction and convert rules for `UnionCtor` / `UnionMatch` for real (not stubs).
- Wire erase to the tagged-tuple lowering for `UnionCtor` / `UnionMatch`.
- Verify: `std/Result.crs` and its dependents (Parse, Json) work end-to-end. Other unions (Fmt, Json's own union, etc.) stay on the legacy tagged-tuple path.

### Phase 3: Migrate all unions

- Remove the `Result`-only gate; all `TopItem::Union` declarations emit the new IR.
- Migrate `std/Fmt.crs`, `std/Json.crs`, etc. — surface stays the same, lowering changes.
- Restore implicit args for parameters at constructor functions (the previously-reverted implicit-args work, but now built on a clean foundation).
- All 427 tests should pass. The `printf_partial_evaluation_reduces_residual` and `projection_through_a_stuck_union_payload_lowers` failures from the prior session dissolve because the brittleness they exhibited belonged to the tagged-tuple desugar.

### Phase 4: Remove old union path

- Delete the tagged-tuple union desugar in `src/text/to_core.rs` (the ~130 lines that emit a tagged-tuple `Result : Type` and its constructor functions).
- Delete the `Match::Union` lowering in `src/text/to_core/elaborate.rs` that turns surface union matches into atom-match-plus-projections. The surface `Match::Union` variant *stays* — users still write `match | success(x) => …` — only the lowering path changes to emit `UnionMatch` instead.
- Audit `refine_head` and the canonical-form fallback in `src/core/typing.rs`; the union-projection cases may no longer need the dual registration. Simplify if so.

### Phase 5: Flat sum-of-products representation

- Change `erase` for `UnionCtor` / `UnionMatch` to emit per-variant flat tuples and per-variant payload projections.
- Update `ersd`/`cont`/`wasm` if any layer requires layout info per variant.
- Remove the `projectable_at` workaround in `src/core/erase.rs:392`.
- Verify benchmarks: alloc count drops roughly by half across union-heavy programs.

## Open questions

These don't block the sketch but should be settled before Phase 2:

1. **Where does the inductive registry live?** Likely on the `core::Module`'s flat output so it travels with the module across compilation boundaries; `core::Context` borrows it during elaboration. Putting it on `core::Context` directly would make it per-elaboration-frame, which is wrong — the metadata outlives any single elaboration.
2. **How are constructor functions named in the registry?** `Result/success` is the function; the constructor *itself* lives inside the function body. Probably both the registry entry and the function name resolve to the same qualified name; the distinction is only that one is "the function I call" and the other is "the primitive term the function builds."
3. **Printing.** `UnionCtor { Result, [Nat, Bin], 'success, [42] }` could print as `Result/success(@Nat, @Bin, 42)` (showing implicit args), or as `Result/success(42)` (hiding them), or as the raw `'success(42)` form. Match it to whatever style implicit args use.
4. **Universe levels.** Curios is currently `Type : Type`. The inductive design doesn't change that; if/when universes are added, inductive declarations get a universe level too.
