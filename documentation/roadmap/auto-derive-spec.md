# Auto-derive: a witness whose body the compiler writes

## Status

Not started. Ten steps in reading order: eight fixes to defects found while probing the design — none derive-specific, each an ordinary program a user can write today — then a re-probe once the ground is stable, then a rewrite of this file's design section into what remains. A landed step is replaced here by one checked line; its contracts move to their owners (`syntax.md`, the owning module's rustdoc, `Fmt.crs`, a decision file under `documentation/design/language/`), and the design section is rewritten by step 10 to state what is left against what landed, restating nothing its owner now holds.

The design was reached after a survey of deriving mechanisms in Lean 4, Rocq (coq-elpi, Equations, `Scheme Equality`), GHC, Idris 2, Agda, Isabelle/HOL, Scala 3, Swift, Rust, OCaml, PureScript, Roc and Elixir, and after probing the tree's compiler; the probes are the ledger below, and every claim in the design section that a probe bears on names its row.

## Mission

A witness declaration may omit its body. The compiler then writes the body from the declaration of the type in the witness's key, or refuses compilation naming what it could not write. The first and, for now, only concept this applies to is `Spell`, whose witness spells a value as the Curios source text denoting it. Hand-written witnesses remain the norm: a carrier whose spelling is computed in the language — `Flt`, `Str`, `Bytes` — states its witness with a body, exactly as today, and derivation is the form for structural types whose spelling is nothing but their constructors and fields. A tuple has no declaration and no owner, so it is neither written nor derived on request: one witness per concept, written in the language over a description of the tuple's fields, serves every arity and every labeling, and the compiler supplies only the adapter for the shape a program actually asks for.

What is borrowed, and from where:

- From GHC's standalone deriving: **the programmer writes the signature, the compiler writes only the body**. Premises are never inferred; `satisfy (@A: Type, use Spell(A)) => Spell(Tree(A));` states them, and the existing regularity rule (`curios-elab/src/resolve.rs`, `register_witness`) checks them.
- From Swift SE-0185: **an empty body is the request**, opt-in and visible in source; synthesis needs the type's representation where it is declared, which here is the subtree rule for representations rather than a same-file rule.
- From Lean 4's deriving handlers: **the derived artifact is ordinary code**, elaborated and kernel-checked like anything written by hand; recursion is threaded through **local instances**; handlers are **keyed by the class** and a body-less request for a class no handler serves is an error.
- From Scala 3's `Mirror`: **the compiler supplies structure, the library supplies logic** — for tuples, a description of the fields and a view onto them, with the generic printer written once in Curios.
- From Roc: a **structural** type has no owner and is served automatically, while a **nominal** type opts in.
- From Roc and Swift: a field the concept has no witness for — a function, a `Cell`, a `Handle` — **refuses the derivation** rather than being skipped.
- From coq-elpi and Isabelle's derive manager: the shape a *second* derivable concept will need — derivations that depend on other derivations and produce proofs — is noted under the extension seam and built later.

## Steps

- [ ] 1. The positivity walk terminates on a type-level `rec`
- [ ] 2. A struct's parameters are checked against elaborated telescopes
- [ ] 3. A tuple literal synthesizes when nothing pins it
- [ ] 4. An introduction form parks on a type stuck on a metavariable
- [ ] 5. A witness may recurse through its own entry
- [ ] 6. `wonder stage` answers for any rung it reached
- [ ] 7. Unlabeled tuple fields print as written
- [ ] 8. A missing-witness report names the premise and the head
- [ ] 9. Re-probe on stable ground
- [ ] 10. Rewrite the design into what remains

Each step is one authorization and one commit, lands its repro as a test before the fix, and runs the full gate at its end. Steps 1–8 are independent of one another except that 4 follows 3 (both touch `curios-elab/src/elaborate/aggregate.rs`); 9 needs all of 1–8; 10 needs 9. A defect found in 9 becomes a new step inserted before 10; numbers past it shift, since one file keeps no append-only order.

### 1. The positivity walk terminates on a type-level `rec`

**Defect.** `curios-analysis/src/positivity.rs`, `Walk`: `forced` hands a closed inline `Rec` head to `force_rec`, which unfolds it to its member lambda (a head constructor counts as progress); `opaque` descends into the lambda; its body holds the recursive call with the group substituted inline, `Apply(Rec, [Bound])`; the walk forces that head again. Sampled at two depths, the node is the same one. The only guard, `unfolded: BTreeSet<Free>`, is keyed by definition name, and an inline group has none. Unbounded: a 1 GB stack overflows. Shared by both checkers, so the kernel carries the same loop. Forcing a `rec` head is there for a reason — a mutual `induct` group lowers its type constructors into a `rec`, and forcing one yields an `InductType` the walk records without descending — and the case never considered is a `rec` whose member is a function.

**Repro.** `struct S4(L: Labels): pub Type { value: Count(L) }` with `rec Count(L: Labels) -> Type = match L | nil() => {} | cons(l, rest) => {Nat, Count(rest)} end`; the same through `Option(Count(rest))`, through a bare `Count(rest)`, and over `Nat`; `induct Tupled(F: Fields): pub Type | mk(TupleOf(F)) end` used at a witness site; the `Wrapped`/`Certified(bytes)` program already kept as `a_struct_refinement_field_overflows_the_test_thread_stack` in `curios/src/tests/reduction.rs`. Rows that must stay green: the same type through a plain `let` alias, and BigNat's own `is_trimmed`, which recurses through the fold hypothesis `; ih` and is why the prelude never trips this — not the stack size the existing note cites, since no `stack_size` exists anywhere in the tree.

**Fix direction.** Give `Walk` for groups the memory `blocked` has for names — a set keyed by the inline `RecGroup`'s identity — or treat a lambda obtained by forcing a `Rec` as opaque exactly once. Either keeps the mutual-`induct` reason intact.

**Verification.** The ignored test un-ignored, relocated beside the walk, and its note corrected to name the location and the fold-form contrast; the repros above as tests on the default test-thread stack; the prelude certifies, since the workspace check already walks every `/std` declaration.

**Unlocks.** Every description-indexed declaration — the `Fields`/`TupleOf`/`Tupled` family, and any user type whose payload is computed by a self-calling type-level `rec`.

### 2. A struct's parameters are checked against elaborated telescopes

**Defect.** `elaborate_module_let` checks the struct former's body — `elaborate_struct_type → check_args_against → check(F, …) → expect` — against the registry's raw lowered arity (`curios-text/src/into_core.rs`, the `StructDecl` built with `universe_context: empty()`), and only afterwards does `elaborate_struct` (`curios-elab/src/elaborate/module.rs`) rebuild the telescopes with elaborated types. When a parameter's type mentions a universe-polymorphic former, the binder in Γ is the normal form at a fresh instance (`InductType { Fields, universes: [?319] }`) and the registry side is the raw reference (`Var(Global(Fields))`) at the declaration's bound level; `identify_universe_levels` declines, as it documents it must, and the report prints two identical spellings. Concepts inherit it, being struct-backed. Inductives rebuild inside `elaborate_module_rec` and pass.

**Repro.** `struct P1(xs: List(Nat))`, `struct P2(o: Option(Nat))`, `struct P6(A: Type, xs: List(A))`, `struct S1(F: Fields)` with `Fields` carrying an `A: Type` payload, `concept C5(F: List(Type))`. Green rows: `A: Type` (cumulativity), `x: A`, `n: Nat`, `s: Str`, `p: {Nat, Nat}`, `f: (Nat) -> Nat`, and `induct I6(xs: List(Nat))`.

**Fix direction.** Rebuild, or share, the parameter telescope's elaboration with the former's type before the body is checked — the order the induct path takes. Beside it: a universe-instance disagreement must render its levels rather than `X ≠ X`.

**Verification.** The repros as tests; the prelude unchanged (it declares no such struct, which is why this survived).

**Unlocks.** `struct Tupled(F: Fields)`; every value-parameterized struct and concept.

### 3. A tuple literal synthesizes when nothing pins it

**Defect.** `elaborate_tuple` (`aggregate.rs`) synthesizes a non-dependent product in `Infer` mode and, in `Check` mode against a bare metavariable, parks every non-empty literal — rightly while a solution can still arrive, since a dependent telescope can only come from the expectation. When nothing else ever mentions the metavariable — a polymorphic function whose type variable only that argument determines, or a written `?` — the apply force tier (`apply.rs`) re-checks against the same bare metavariable and parks again, and `drain_parked` (`typing.rs`) reports the survivor as "its expected type never gained structure", or, when a witness goal parked on it first, as `no witness of Show(?)`. The fallback granted to `()` and taken by list literals — synthesize, then `expect` — is never taken for a non-empty tuple.

**Repro.** `let y: ? = (1, true)`, `let y: ? = (1,)`, `let z = id((1, true))`, `let n: Nat = g((1, true))`, `Show/show((true, false))`, `f((1, true))` under `use Show(A)`. Green rows: `let y: ? = ()`, `let z: ? = [1, 2]`, `let n: Nat = g([1, 2])`, `let z: {Nat, Bool} = id((1, true))`. A lambda literal, `g((x) => x)`, stays refused: it cannot be synthesized, and that is not this defect.

**Fix direction.** At the force tier after the authoritative turnaround, and at `drain_parked`'s no-progress sweep, synthesize the non-dependent product and `expect` it. Whether a dependent expectation could still have arrived is decidable at both points: nothing newly solved, nothing left to retry.

**Verification.** The repros as tests; `syntax.md`'s `?` promise holds on a tuple literal.

**Unlocks.** `spell((1, true))`, `Fmt/print("#")((1, true))`, the `?` loop on tuples, any generic call with a tuple-literal argument.

### 4. An introduction form parks on a type stuck on a metavariable

**Defect.** `blocked_on_metavar` (`typing.rs`) recognizes only a bare unsolved `Metavar` as blocking (plus the lambda domain and codomain cases), and `elaborate_tuple`'s park arm likewise parks only on `Metavar`. A `match` stuck on a metavariable scrutinee — `Count(?L)`, the payload type of `Boxed/mk` before `?L` is solved — is neither, so the argument is checked eagerly and refused one step before `expect(output, expected)` would have solved `?L := L`. Conversion already treats such terms as `Outcome::Blocked`.

**Repro.** `let t: Boxed(L) = Boxed/mk((1, ()));` with `induct Boxed(L: Labels) | mk(Count(L))`. Green row: the same with `@L` supplied.

**Fix direction.** One "blocked weak-head form" predicate — flex head, flex scrutinee, flex projection head — shared by `blocked_on_metavar` and the intro-form park arms, reusing what conversion computes for `Blocked`. After step 3.

**Verification.** The repro as a test; the roadmap's postponement rows unchanged.

**Unlocks.** Description-indexed constructors without written implicits — `Tupled/mk(…)` as a user would write it.

### 5. A witness may recurse through its own entry

**Defect.** `elaborate_module_let` registers a witness's signature before its body "so a recursive witness (a `Show(Tree)` whose fields show subtrees) can resolve through its own entry", and elaboration accepts such a witness — but the kernel refuses the result, `unbound name /witness@0`, because the definition is an `Item::Let` whose body names itself and cross-definition value recursion is unexpressible outside a `rec` item by construction. No test exercises the comment's claim. The decision is taken: the comment states the intended semantics, and the fix is to honor it.

**Repro.** `satisfy Show(Tree)` whose `node` arm calls `Show/show` on both subtrees; `Rose` with `List(Rose)` children spelled through the `List` witness with no local dictionary. Green rows that must stay green: the same witnesses written with a local `rec`, and the local-dictionary form for the nested case.

**Fix direction.** Lower a witness whose elaborated body mentions its own name as a single-member `rec` item, and teach `Established::replay_definitions` to register witnesses that are `Item::Rec` members (today only the `Item::Let` arm consults `module.witnesses`). The erased verifier already admits an initializer whose lambdas name the group; a witness that *evaluates* itself is refused there as any `rec` member is.

**Verification.** The repros as tests through `run`; the witness-table replay test in `curios-prelude-archive` still holds one witness per key; the coherence probe file under `documentation/soundness/per-term-rules/` gains the rec-member row.

**Unlocks.** A derived body that is a plain `match` with `Spell/spell(sub)` calls — no walkers, no local dictionaries — and mutual groups whose witnesses reference each other through the table.

### 6. `wonder stage` answers for any rung it reached

**Defect.** `curios/src/wonder/stage.rs`: the observer stores the rendering into `text` when the rung matches, then `compile_with_units(...)?` propagates a later failure and discards it. `stage text` and `stage core` on a program that fails elaboration print only the diagnostic, though both rungs were observed (`compile.rs` observes `Text` before lowering and `Core` before elaboration). The transport's own contract says a program that stops *before* the rung has not answered — implying one that stops after it has.

**Repro.** `let x: Nat = true;` with `wonder stage text` and `wonder stage core`.

**Fix direction.** On `Err`, answer when `text` is `Some` — rendering to stdout, diagnostics to stderr, exit 0 as for any answered question — and refuse only when the rung was never observed. One branch in `stage()`.

**Verification.** A `wonder` test per rung on a failing program; `usage.md`'s stage table gains the sentence.

### 7. Unlabeled tuple fields print as written

**Defect.** The lowerer mints an unlabeled tuple field's binder with the empty string as its hint (`curios-text/src/into_core/lowerer.rs`, `unwrap_or_default()`), where the pipeline's convention for "no label" is a hintless binder; the printer's tuple arm (`curios-core/src/print.rs`) prints `label: type` whenever a hint is present, and the rename map disambiguates the shared `""` into `2`, `3`. `{Nat, Bool, Str}` renders `{: Nat, 2: Bool, 3: Str}` in every diagnostic that names it.

**Repro.** `let q: {Nat, Bool, Str} = p;` against a labeled `p`; `Show/show(p)` with `p: {Nat, Bool}`.

**Fix direction.** Mint unlabeled fields hintless and let `Telescope::labels()` keep rendering them as `""` (the convention `TupleType::eq` and the positional label check rely on), or have the printer treat an empty hint as unnamed.

**Verification.** A printer test per shape; the mismatch report reads `{Nat, Bool, Str}`.

### 8. A missing-witness report names the premise and the head

**Defect.** Two placeholders meet in one line. `func_label` (`curios-elab/src/elaborate/apply.rs`) names the head only when it is a `Var`, so a curried call reports `'<function>'`; the binder half prints the `use` binder's hint, and a `use` parameter in `let`/`rec`/`satisfy` sugar is anonymous by design, so every premise of user-written code reports `'_'`. Only the generated method wrappers, whose lambda writes `use w`, ever show a name.

**Repro.** `Fmt/print("… %")((1, true))` → `needed by '<function>' for its 'use' binder '_'`; `f((1, true))` under `let f(@A: Type, use Show(A), a: A)` → `needed by '/f' for its 'use' binder '_'`.

**Fix direction.** Name the premise by its position among the head's `use` slots and its declared type — `for its 2nd premise, use Show(A)` — and a non-reference head by its innermost reference or its source text. Data the report already holds.

**Verification.** The two repros as report tests.

### 9. Re-probe on stable ground

**Inputs.** The ledger below, re-run in full against the compiler that steps 1–8 produced.

**Outputs.** The diff of answers, row by row; what each fix unlocked beyond its target; and each design question re-decided with the probe that decided it: whether `Tupled` is a `struct` (step 2) or stays an `induct`; walkers versus recursion through the table (step 5); conjunction goals versus explicit dictionary supply; whether a `Tupled`-keyed witness resolves end to end from a flat tuple goal; whether a closed-description walk specializes away, as a figure beside its probe under `programs/README.md`'s rule; whether `#` on tuples needs anything beyond two library witnesses; and whether any of the machinery listed in the design below has become unnecessary.

**Rule.** No implementation in this step. A defect found here is a new numbered step before 10.

### 10. Rewrite the design into what remains

The design section below is provisional in every sentence. This step rewrites it to state only the machinery still to build — the body-less `satisfy` form, `Transient::Derive`, the `Spell` derivation, `/syn/Spell` and its `/std/Spell` facade, `Fmt`'s `#`, `/syn/Fields`, `Tupled`, the tuple witnesses and the adapter — or further numbered fixes, with nothing a landed step already owns. The ledger is retired with it: rows that became tests are named by their test, rows that decided a question are cited by the decision.

## Probe ledger

Every probe is a program on standard input; the answer column is the tree's compiler on 2026-08-27, before any step.

| # | Probe | Answer today | Bears on |
| --- | --- | --- | --- |
| P1 | `satisfy Show(Tree)` whose arms call `Show/show(subtree)` | elaborates; kernel refuses `unbound name /witness@0` | step 5; derived-body shape |
| P2 | the same recursing through a local `rec go` | `(1 (2 3))` | walkers |
| P3 | `Rose` with `List(Rose)` children, spelled through a helper with `use Show(Rose)` fed `Show(Rose) { show = go }` | `1[2[], 3[]]` | nested occurrences |
| P4 | `satisfy Show(Tree);` | `Expected '{', obtained ';'` | the surface form |
| P5 | `Fields`, `TupleOf`, `ShowAll`, `show_fields` by dependent match, dictionaries by hand | `a = 1; b = true;` | the in-language generic tuple witness |
| P6 | `satisfy (@F: Fields) => Show(TupleOf(F))` with `TupleOf` a `rec` | `cannot be keyed: … every parameter's head must be an inductive, a struct, or an intrinsic type` | keying |
| P7 | `let f(@F: Fields, use ShowAll(F))`, then `f(@F)` | declaration accepted; call: `no witness of ShowAll(F)` | conjunction goals |
| P8 | `use {Show(A), Show(B)}` binder, `Show/show(a)` under it | `no witness of Show(A)` | conjunction goals, local side |
| P9 | `struct Tupled(F: Fields): pub Type { value: TupleOf(F) }` | `type mismatch — inferred: Fields, expected: Fields` | step 2 |
| P10 | `induct Tupled(F: Fields) \| mk(TupleOf(F))`, keyed witness, `Tupled/mk(@F, …)` | stack overflow | step 1 |
| P11 | `Tupled/mk((1, (true, ())))` against `Tupled(F)` without `@F` | `introduced a tuple where the expected type is not a tuple type — expected: TupleOf(?)` | step 4 |
| P12 | registration of `satisfy (@F: Fields) => Show(Tupled(F))` | accepted | keying on a nominal wrapper |
| P13 | `wonder stage ersd-optm` of P5 | the walk unrolled into one function per level, labels folded to constants, dictionary calls indirect | the specialization figure |
| P14 | `Show/show((1, true))` | `no witness of Show(?)` | step 3 |
| P15 | `Fmt/print("issue #% -> %")(42)((1, true))` | `needed by '<function>' for its 'use' binder '_'` | step 8 |
| P16 | `let q: {Nat, Bool} = p` with `p: {fst: Nat, snd: Bool}` | mismatch — labels are part of identity; renders `{: Nat, 2: Bool}` | tuple keying; step 7 |
| P17 | `/Tree/node(/Tree/leaf(1), …)`, `/std/Option/some(3)`, `/Point { x = 1, y = 2 }`, `Meters { 7 }` | all re-parse and run | spelled forms |
| P18 | `wonder stage text` / `core` on `let x: Nat = true;` | diagnostic only, nothing on stdout | step 6 |
| P19 | `let y: ? = (1, true)` / `(1,)` / `[1, 2]` / `()` | never gained structure / same / `? = List(Nat)` / `? = {}` | step 3 |
| P20 | `struct P1(xs: List(Nat))`, `P2(o: Option(Nat))`, `P6(A: Type, xs: List(A))`, `concept C5(F: List(Type))` | all `type mismatch X ≠ X`; `induct I6(xs: List(Nat))` fine | step 2 |
| P21 | `Count` over `Labels` through `{Nat, …}`, `Option(…)`, bare; `Rep` over `Nat`; `Wrapped` over `Bytes` and `Bits`; BigNat's fold-form `is_trimmed` | all overflow; the fold form compiles | step 1 |

## Design (provisional — owned by step 10)

### Permanent design decisions

1. **The signature is the programmer's.** A body-less witness has exactly the telescope and concept application a written one has, and derivation never adds a binder. A parameterized head must be applied under a telescope that binds its parameters and states the premises the body will need.
2. **Derivability is a property of the concept, not of `satisfy`.** A concept either has a derivation, registered in the compiler against its `SyntaxRegistry` slot, or it does not; a body-less witness for a concept without one is refused. The first landing registers one derivation, for `Spell`.
3. **Nominal types opt in; tuples are served by one library witness and a compiler adapter.** An `induct` or `struct` is derived only where a body-less `satisfy` asks, because it has an owner and the orphan rule applies. A tuple type has neither a declaration site nor an owner, and its labels are part of its identity (P16), so no written or body-less witness can be keyed on it: a `Spell` goal whose parameter is a tuple type is answered through the concept's witness over `/syn/Tupled(F)`, with the description, the view and the dictionaries built by the compiler at that goal.
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

Absolute paths are chosen over shortest suffixes because the text is scope-independent: it re-parses from any module that can see the names (P17). `Show(Option)` prints `some(3)`; `Spell` prints `/std/Option/some(3)`.

### Surface: the body-less witness

```crs
satisfy Spell(Point);
satisfy (@A: Type, use Spell(A)) => Spell(Tree(A));
satisfy (@T: Type, @n: Nat, use Spell(T)) => Spell(Vec(T, n));
```

Grammar: after the concept application, `;` in place of `{ … }` (P4 is the refusal today). Both forms remain: a body-ful `satisfy` is untouched. `TopWitness.entries` becomes a body that is either written entries or absent; the parser (`curios-text/src/parse/top_level.rs`, `parse_top_witness`) accepts either terminator after the application, and the printer (`curios-text/src/print.rs`) prints the absent body as `;`. `curios format` round-trips the body-less form and never expands it; the expansion is observable at `wonder stage core-elab`, and `wonder stage core` shows the `Derive` transient in place.

`satisfy Spell(Tree);` for a parameterized `Tree(A: Type)` is refused as it is today — `Tree` is a type constructor, not a `Type` — and the report says to write the telescope, quoting the form above.

### Lowering and the `Derive` transient

`into_core` lowers a body-less witness exactly as a written one (`curios-text/src/into_core.rs`, the `TopItem::Witness` arm): an anonymous `FlatLet { kind: DefinitionKind::Witness, name: Global::Witness(id) }` whose declared type is the concept application under the telescope. Its body is `Subterm::Transient(Transient::Derive(Derive { span }))` — a hole that says "fill me from the expected type" and carries nothing else, since the expected type is the whole of what derivation needs.

`elaborate_module_let` (`curios-elab/src/elaborate/module.rs`) registers the witness's signature before its body as today, then checks the body against the elaborated type. Checking a `Derive` transient against an expected type is the one new `elaborate` arm: reduce the expected type to a concept application `C(p₁, …)`; look `C` up among the registered derivations; call the derivation, which returns a lowered-Core body; check that body against the expected type. A `Derive` met in inference mode, or against a type that is not a concept application, is refused — both are unreachable from the surface, since the transient is only ever born in a witness's body position.

The lowered-Core body is built with the builders `curios-elab/src/builders.rs` already exists for, in the shapes the lowerer emits: explicit-only applications (implicit insertion mints the metavariables), a `Match` whose motive is a fresh metavariable (`curios-text/src/into_core/match_compile.rs`, `motive_scope`) and whose cases are one `InductArm` per constructor in declaration order, `Proj` for struct fields, and — until step 5 lands — a local `Rec` group for the walkers and `Struct`/`Var` for local dictionaries. The lowerer itself is unreachable from here — `curios-text` depends on `curios-elab`, not the reverse — which is why the derivation is *specified* by the surface program it is equivalent to and *implemented* as the Core the lowerer would have produced for it.

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

the derived witness is, once step 5 has landed,

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

— the recursive occurrences resolving through the witness's own table entry — and, until then, the same with a local `rec go` in place of the recursive calls (P2). For

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
- A direct recursive occurrence — the family itself, or any member of its mutual group — resolves through the table after step 5; before it, walkers: one local `rec` group with one member per family member, so a witness for one member of a mutual group is self-contained.
- An occurrence *under another type former* — `List(Tree(A))`, `Option(Tree(A))`, a user `Pair(Tree(A), Nat)`, a tuple `{Tree(A), Nat}` — is spelled through that former's own witness; before step 5, with the walkers supplied as local dictionaries through a helper with `use` binders (P3). `Spell` is transparent, so the dictionary literal is admissible wherever the witness may be declared.
- Indexed families: the body matches with an omitted motive, and index inversion prunes the arms it prunes for hand-written matches.
- Constructor arguments are spelled in payload order, separated by `, `; a nullary constructor spells as `/Path/ctor()`; struct fields in declaration order, `label = ` before a labeled field and nothing before an unlabeled one, inside `{ ` and ` }`.

#### Tuples

A tuple type is not a witness key: `HeadKey::of_whnf` (`curios-elab/src/concept.rs`) has no key for a Σ-type, and resolution's third step (`curios-elab/src/resolve.rs`) turns a non-keyable head into `NoMatch`. Labels are part of a tuple type's identity (P16), so a written witness per arity could neither cover labeled tuples nor print their labels; and a tuple has no declaration site to carry a `satisfy` and no owner for the orphan rule to consult. Yet the arity-generic, label-aware printer is writable in today's language (P5): a description of the fields, a type computed from it, a tuple of dictionaries computed from it, and a walk whose result type refines per arm — the idiom `Fmt` already uses. What is missing is the bridge, the keying, and the automatic dictionaries (P6, P7, P8).

The design puts the logic in the language once and lets the compiler build only an adapter per shape the program asks for:

- **`/syn/Fields`**, the description: `nil() | cons(label: Option(Str), A: Type, rest: Fields)`. Non-dependent; a dependent tuple carries a proof, which spells `?` anyway, so the generic witness refuses dependent tuples at the goal with its own message.
- **`TupleOf(F)`**, a plain library `rec`: `nil() => {}`, `cons(l, A, rest) => {A, TupleOf(rest)}` — the nested encoding P5 used. Not an intrinsic: no kernel rows, no `tail`, no decomposition.
- **`/syn/Tupled(F: Fields)`**, a nominal wrapper over `TupleOf(F)` — the keyable head for "all tuples", `HeadKey::Nominal(Tupled)` with `F` solved by unification (P12 shows registration accepted). A `struct` after step 2, an `induct` until then (P9, P10).
- **One witness per concept, in Curios**: `satisfy (@F: Fields, use SpellAll(F)) => Spell(Tupled(F))` around the `show_fields`-shaped walk, in `/std/Spell.crs`; `/std/Show.crs` writes its own; so can any user concept. Opt-in is that witness's existence; there is no registration table.
- **The compiler's step at the goal**, in resolution's non-keyable arm when the head is a `TupleType`: build `F` from the telescope — labels as `/syn/Str` values, the same act as a string literal becoming a library value; resolve `C(Tupled(F))` from the table like any goal; build the flat↔nested view for the known arity (`(t.0, (t.1, ()))` and back); transport the resolved dictionary along each method's type — eta-expand, `nest` at every argument occurrence of the tuple type, `unnest` at every result occurrence — and hand back the witness term. Concept-agnostic for first-order methods (`Spell`, `Show`, `Eql`, `Ord`, a hash); an occurrence under a type former is refused with a message. Kernel unchanged: the result is an ordinary closed term inline at the goal; hoisting it into a per-unit definition cached by shape is an optimization if a census shows duplication, and such a definition must never enter the program-wide table.
- **`SpellAll(F)`**, the premise: refused today as non-regular (P7). Either conjunction goals — a goal or premise that reduces to a tuple type is solved componentwise, `{}` by `()`, a tuple-typed local `use` binder searched componentwise (P8), regularity admitting a stuck application of a *total* type-level definition over telescope variables, with a depth guard as the termination story — or the compiler's step supplying the dictionary tuple explicitly, since it holds `F` concretely, with regularity merely admitting the form. Conjunction goals are preferred: general, independently useful, independently testable. Step 9 decides.
- **Specialization**: the ersd optimizer already unrolls a walk over a closed description into one function per level with the labels folded (P13); the view's pairs and the dictionary projections are the figure step 9 takes.

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

The fuel bound (`3 * len + 3`, one step per byte) is unaffected. `%` is the show-slot and `#` the spell-slot: `Fmt/print("# vs %")("a\n")("a\n")` prints `"a\n"` quoted and escaped beside the raw string, and `Fmt/print("point = #")((1, true))` reaches the tuple witness through the adapter (after step 3). A missing `Spell(Tree)` fails at the call exactly as a missing `Show` does today. The one cost is recorded: `#` is commoner in prose than `%`, so an existing format string with a bare `#` — `issue #42` — becomes a spell-slot and needs `\#`. Two-byte directives (`%#`, `%?`) were considered and rejected: they reserve no new byte but change the meaning of `%` before a literal `#`/`?`, so they break too and read worse.

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
- `wonder`: nothing new beyond step 6. `stage core` shows the transient, `stage core-elab` the expansion. The planned `witnesses` query, when it lands, flags a derived row.

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

### Verification

A `curios/src/tests/derive.rs` in the `run(source)`/`error(source)` style of `concepts.rs`, and rows in `fmt.rs`:

- Derived `Spell` for: an enumeration, a constructor with payloads, a parameterized family under a premise, a recursive family, a mutual group with one body-less witness per member, a family nested through `List`, through a user struct and through a tuple, a labeled struct, a positional single-field struct, a struct with a dependent `Prop` field (spells `?`), an indexed family (`Vec`).
- Tuples through the adapter: unlabeled, labeled, one-field, unit, nested, a tuple holding a derived nominal type, a tuple whose component has no witness (the frame names the component), a dependent tuple (refused with its message), and a user concept with its own `Tupled` witness.
- Round-trip: for each, the spelled text is spliced into a program checked against the type and compared with `==` through an `Eql` witness, or by re-spelling and comparing strings where no `Eql` exists.
- Refusals, each asserting the frame's text: intrinsic head; `Prop`-sorted head; concept-backed head; private representation outside the subtree; sealed concept outside the subtree; missing field witness naming constructor and payload, with the telescope hint when the type is a variable; function-typed field; explicit type payload; unapplied parameterized head; a body-less witness for a non-derivable concept; orphan and duplicate on a body-less signature.
- `Fmt`: `#` renders through `Spell` beside `%` through `Show`; `\#` is literal; a `Str` argument is quoted under `#` and verbatim under `%`; a tuple argument under `#`; a missing witness reports at the call.
- Formatter: `satisfy C(T);` round-trips unexpanded; `wonder stage core-elab` shows the expansion and `stage core` the transient.
- Prelude: the derived `Option`/`Result`/`Order` witnesses are held by the archive build; carrier witnesses round-trip through parse for every literal form, including `Flt`'s non-finite values and negative zero.
- The specialization figure: an ignored test carrying the command, the date, and what `ersd-optm` printed for a closed description, in the pattern of `stored_prelude_measurements`.

### Completion criteria

- The body-less form parses, prints, formats and is documented; the tree-sitter grammar agrees.
- `/syn/Spell`, `/syn/Fields`, `/syn/Tupled` exist with their registry slots and `/std` facades; every carrier has a written witness; `/std`'s structural types have derived ones; `Spell` and `Show` have their tuple witnesses; `/std/Fmt` has the `#` slot.
- Every row under Verification is a test, and the gate passes.
- The roadmap gains the item under Type System beside instance arguments, and `documentation/design/language/` gains one decision file — the transient-hosted, signature-is-the-programmer's, tuples-by-description design, with the Text-stage expansion, the intrinsic `TupleOf`, row variables and the `Generic`-universe route recorded as rejected for the stated reasons.
- This file is deleted, its contracts moved to `syntax.md`, the elaborator's `derive` module documentation, `Fmt.crs`, the `/syn` sources, and the decision file.

### Decisions taken here, still reversible

- Absolute paths in spelled text (over shortest suffixes).
- `Int` spelled with an explicit sign.
- `?` for an explicit proof payload (over refusing, or `_`).
- Tuples served by one library witness over `Tupled(F)` and a compiler adapter at the goal, inline, rather than by a compiler-written witness per concept or an intrinsic `TupleOf`.
- Conjunction goals over explicit dictionary supply, pending step 9.
- `#` as the spell-slot, with `\#` as its escape.
- `/syn` as the home of `Spell`, `Fields` and `Tupled`, reached through their `/std` facades.
