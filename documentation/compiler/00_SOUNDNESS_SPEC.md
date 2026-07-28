# Totality of the erased program

Working implementation specification for making Curios sound as a logic by requiring that everything erasure deletes is total, while leaving unrestricted general recursion available to programs.

This effort closes the three remaining routes to a closed inhabitant of `False`. It does not make Curios strongly normalizing, does not remove `rec` or `exit`, and does not give canonicity: a closed `Str` may still be bottom. What it establishes is that no closed proof of `False` exists, and therefore that a `Str` which *is* a value satisfies its certificate.

When this work lands, fold the permanent calculus and compiler invariants into the owning `curios-core` module documentation, record the theorem and its trusted base in `DESIGN.md`, update `ROADMAP.md`, and delete this working specification after no remaining document refers to it.

Steps 1 through 7 are implemented, both gates reject, and the fork step 6 raised is closed — see "Appendix: the fork on (T), and how it closed". The amendments implementation forced are marked **Amended** in place, and the figures under "Measurements" are computed rather than estimated.

**Step 8 replaced (V)'s seeding entirely, and that is the largest change this specification has recorded.** Every defect found in (V) after step 7 — six of them, each located by counting rather than by reading — was an incompleteness in one mechanism: a walk that re-derived, from the finished term, a judgment elaboration had already computed for every term in the program. The walk is gone. (V) is now seeded where the answer is known, and its coverage is a consequence of elaboration being a typechecker rather than a property anyone has to maintain. See "Obligation (V) is not a walk". The consequence for this document is that the amendments describing the walk's rules, its gaps, and its instrumentation now describe deleted code; they are kept only where the reasoning still bears on something live, and marked **Historical** where it does not.

## Objective

Establish the following, and record it as the claim the compiler makes:

```text
There is no closed term of type /syn/False.
```

The mechanism is one property, applied to the two things erasure deletes:

```text
Everything erasure deletes must be total.
What it retains may diverge.
```

Erasure deletes types and it deletes `Prop`-sorted proofs. A divergent type breaks type formation; a divergent proof proves anything. Terms that survive erasure are programs, and a program that loops is a program, not an unsoundness.

The completed implementation must compile the existing `/sys`, `/syn`, `/std`, examples, benchmarks, and tests with the two Curios source changes specified in step 4.

## The unsoundness being closed

Three routes, each verified by an executable reproduction, each independent of the other two. Closing any one closes neither other.

**A partial `rec` at a `Type`-sorted carrier releases a `Prop`.** The certificate leaves through a projection or a `match` arm binder, so the escaping term need not mention the partial definition syntactically.

```crs
induct Box : pub Type | wrap(p : Valid(bad)) end
rec sneak : Box = sneak;
let escaped : Valid(bad) = match sneak | wrap(p) => p end;
let forged : Str = Str { bytes = bad, valid = escaped };
```

**`Prim::Exit` behind a nullary `Prop`-valued function is dropped by erasure.** The exit never fires, so the program continues with a forged invariant rather than terminating.

```crs
let forge() -> False = /std/proc/exit(0);
let one_bad : BigNat = BigNat { rep = untrimmed, canonical = forge() };
```

**Amended.** `/std/proc/exit` was itself an instance of this route: its declared result was `False`, so the prelude shipped a closed inhabitant of `False` as public API, and the exit it names was erased at every honest use. Step 4 retypes it at `/std/Never`, a `Type`-sorted empty carrier. That retyping is not the fix — `/sys/exit : (@A : Type) -> Nat -> A` is polymorphic and `Prop` sits under `Type` by cumulativity, so `/sys/exit(@False, 0)` stays writable whatever `proc/exit`'s type is, and `Never/absurd(@SomeProp, exit(1))` launders the effect straight back into an erased position. **Gate (V) is the fix**, and it rejects the laundering at the point of laundering. The retyping is what keeps the honest use legal, and it independently repairs the semantic defect that at `False` the exit did not fire.

**A type-level `rec` ties the negative knot strict positivity exists to forbid.** `check_positivity` walks `InductDecl` and `StructDecl` registry entries; a `rec` is neither, so it is never analyzed. The exploit contains no `rec` and no `exit` — a lambda, a constructor application, and one application — so no analysis of value-level partiality can see it.

```crs
induct Sink(A : Type) : pub Type | sink(f : (A) -> False) end   -- accepted: never reaches itself
rec Bad : Type = Sink(Bad);                                      -- positivity never looks here
let delta(x : Bad) -> False = match x | sink(g) => g(x) end;
let boom : False = delta(Sink/sink(delta));
```

The third route also has two variants that a narrower rule would miss. Hiding the type behind a projection, `rec P : {Type, Nat} = (Sink(P.0), 0)`, defeats a rule keyed on the member being sort-valued. Applying a *total* type-level function to a *partial value*, `rec Shape(f : F) -> Type = match f | stop() => False | more(rest) => Sink(Shape(rest)) end` with `rec inf : F = F/more(inf)`, reties the knot without any partial type-former at all. This last variant is why the type obligation must be the aggressive one below.

Separately, `rec Bad : Type = (Bad) -> False` is accepted at its declaration and overflows the compiler's stack at first use. That is fail-closed but it is an abort rather than a diagnostic, and step 6's local form is what turns it into one.

## Permanent design decisions

**Two obligations, one analysis.** Partiality is a single transitive closure over zonked Core, seeded twice. **(T)** every definition reachable from a term in a type position must be total. **(V)** every definition reachable from a term checked against a `Prop`-sorted type must be total. Neither subsumes the other: the type-level `rec` routes violate only (T), and the carrier and `exit` routes violate only (V).

**Amended — the two seedings are not the same kind of thing, and treating them alike is what cost this effort six defects.** (T) asks a *syntactic* question: which written positions are types. Its aggressive reading answers deliberately more generously than any typing judgment would — it seeds the body of every definition whose declared type ends in a sort, which is how it reaches through `/std/BigNat/Canonical` into `is_trimmed`, a dependency no annotation records. A walk can answer that completely, and `type_positions` does. (V) asks a *typing* question: which terms are propositions. A walk can only re-derive that from the finished term, and every re-derivation is incomplete somewhere. The closure they share is genuinely one analysis; the seedings are not, and only (T) belongs in a walk. See "Obligation (V) is not a walk".

**Amended — the aggressive reading's stated justification is contradicted by the corpus, and this is unresolved.** The argument below is that the precise reading "would buy nothing, because (V) independently rejects every nontrivial way to inhabit a proposition about a partial program, leaving only reflexivity-only statements." Implementing the gate showed that reflexivity-only statements do *not* survive: the rejection lands on the **statement**, not on the proof. `Eq(Show/show(x), Show/show(x))` is rejected because `Show/show` at `Nat` is `/std/Nat/to_str`, which is partial. So is `Eq(BigNat/add(a, b), BigNat/add(b, a))` for `a = BigNat/of_nat(6)`, because `of_nat` recurses on `n / 2`. Three existing tests fail on exactly this — `tests::concepts::prop_laws_concept_resolves`, `tests::erasure::proof_bound_as_a_statement_does_not_run_its_certificate`, and `tests::strings::utf8_decimal_is_ascii_carries_its_proof` — and the pattern they share is the verified-interface idiom over any partial function, which `/std` makes ubiquitous. The prelude itself passes, because its proofs quantify over abstract variables rather than concretely computed values; the cost falls entirely on user code. The distinguishing feature between the harmless `Eq(to_str(x), to_str(x))` and the dangerous `Shape(inf)` is whether a type-level eliminator *scrutinizes* the partial value, which is exactly the interprocedural summary this section defers. Resolution is open.

**(T) is the aggressive reading.** Everything reachable from a type must be total, not merely the values that reach a type-level eliminator. The precise reading is unsound under any cheap approximation — exempting the index positions of nominal declarations leaks through `induct Trap(f : F) | mk(x : Shape(f)) end`, where the dangerous type arises only by instantiation and is never written anywhere a syntactic walk could see it. Closing that requires a per-parameter, interprocedural "scrutinized at the type level" summary computed as a fixpoint over the call graph, which is a second positivity analysis. It would buy nothing: (V) independently rejects `Eq/cong(partial_f, p)` and every other nontrivial way to inhabit a proposition about a partial program, leaving only reflexivity-only statements. Aggressive rejects strictly more, so refining later breaks nothing already accepted.

**Partiality is a classification, not an error.** A `rec` the checker cannot accept is recorded as partial and remains usable in programs. `/std/Json/decode` is nullary and productive, `/std/Async` is corecursive, and `/std/BigNat/convert/to_str_go` recurses on a computed quotient; none can pass a size-change checker and none needs to.

**Size-change termination, not structural recursion.** `add/raw` descends on either of two `Bits` arguments depending on the arm, `raw_assoc` does the same over three, and `raw_comm`/`raw_swap_step` need the mutual closure. A single-argument structural rule rejects all of them, and a fold cannot express them because a fold cannot short-circuit.

**Descent is measured through match refinement.** Each parameter is expanded through the accumulated arm refinements into a constructor tree over binder atoms, and call arguments are compared by the proper-subterm order on those trees. This is not an optimization. Without it, the `b\` argument in `add/raw`'s nil arm grades as unknown rather than unchanged, the composite of two call matrices is idempotent with no descending diagonal entry, and foetus rejects `add/raw` on a call path that is infeasible but that a syntactic checker still considers.

**Amended — the descent analysis reduces, under a fuel bound.** The original decision was that the analysis is purely syntactic and never calls `reduce`. That is wrong as written, and step 2 found it: `/std/Bits/cons` is an ordinary `let`, not a constructor, so `raw_trimmed`'s descent onto a rebuilt cons cell is invisible to a walk that cannot perform one delta-plus-beta step. The implementation reads a shape syntactically first and falls back to at most `UNFOLD_FUEL` weak-head steps, only on subterms with no loose de Bruijn index, discarding a `ReduceError` as an unreadable shape. Unlike `check_positivity` this does contact the deadline, so the fallback is isolated in one function (`unfolded_shape`) and can be replaced by a precomputed registry of cons-formers if it ever registers in a profile.

**Totality is a property of `Definition`, not of `RecItem`.** Local `rec` groups inside `let` bodies — `/std/Str/at`'s `go`, the indexed folds in `Bytes`, `Bits`, and `Lst`, and fourteen in `Async` — are `Subterm::Rec`, not `Item::Rec`. One flag per definition, computed as the transitive closure, is also exactly the right cross-module summary: "this prelude definition is partial" means something partial is in its closure, so a user proof that mentions it has that same thing in its own closure.

**No new surface syntax.** Adding an induction hypothesis to `Cases::Induct` would convert about five of the forty-eight must-pass proofs and helps none of the ones that matter, because `Nat/Lte/trans` scrutinizes two proofs, `drop_valid` and its siblings vary their other arguments across the recursion, and the `BigNat` bulk already has `; ih` available on `Bits` and deliberately does not use it.

**The theorem is consistency, not canonicity.** Partial `rec` survives, so a closed `s : Str` may be bottom and projecting `s.bytes` may diverge. The certificate idiom is preserved because the `valid` field is `Prop`-sorted, so (V) forces every constructed `Str` to carry a total proof, and divergence is observed as divergence rather than as a valid-looking wrong answer.

## Non-goals

- Strong normalization of the whole language, or canonicity.
- Removing `rec`, removing `exit`, or making general recursion opt-in.
- Guarded corecursion, productivity, clocks, or sized types. `/std/Async` stays partial by design.
- Making `/std/Json/decode`, `/std/Toml`, `/std/Flt`, `/std/Map`, `/std/Vec`, or `/std/BigNat/convert` total.
- The precise reading of (T), and the interprocedural summary it requires.
- An independent proof checker, a small kernel, or a verified implementation.
- ~~Removing the reduction deadline, or making acceptance machine-independent.~~ **Superseded.** The wall-clock deadline is gone: reduction is bounded by a deterministic per-declaration step budget (`DEFAULT_STEP_BUDGET`), so acceptance is a fact about the program rather than about the machine. This mattered here rather than incidentally — the gates reduce, so totality's own verdicts were machine-dependent while the deadline stood.
- Reconciling Core's unbounded `Nat` with the runtime's wrapping `u32` carrier.
- Repairing effects dropped from erased arguments, which is a semantics defect rather than a forgery route.

## The totality analysis

The analysis runs per `rec` group over elaborated, zonked Core and classifies the group `Total` or `Partial`.

### Descent atoms

A parameter is *refined* by the arms it is scrutinized under. Both eliminator forms in `Cases` contribute:

```text
Cases::Induct        arm for constructor c binds its payload; the scrutinee refines to c(p₁ … pₙ)
Carrier::Nat         Scope<Two>   binds (pred, ih); the scrutinee refines to pred + 1
Carrier::Bin, Lst    Scope<Three> binds (head, tail, ih); the scrutinee refines to cons(head, tail)
Cases::Bool, Switch  the scrutinee refines to the arm's literal; no binders, no descent
```

Refinement applies only when the scrutinee is a variable the analysis tracks. `Cases::Induct`'s `default` arm binds nothing and contributes no refinement. `Cases::Induct` stores arms in the owning inductive's declaration order, so the walk is canonical without further normalization.

### Size comparison

Expand each parameter through its accumulated refinements into a constructor tree whose leaves are binder atoms. For a call argument `a` against parameter `p`, grade:

```text
<   a is a proper subterm of p's expanded tree
=   a is p's expanded tree
?   otherwise
```

This grades the three shapes the prelude actually uses. `raw(xt, …)` where `x` refined to `cons(xh, xt)` is `<`. `raw(b\, …)` in the arm where `x` refined to `b\` is `=`. `raw_trimmed(b\, cons(a2, b2), …)`, where `cons(a2, b2)` is a rebuilt constructor equal to the nested binder `yt`, is `<` because expansion reaches it structurally without needing to fold the constructor back to its binder.

### Acceptance

Build one call matrix per call site, over the callee's parameter vector as it appears in Core, including inserted implicit arguments. Close the matrices transitively over the group, composing entrywise with `<` absorbing, `=` neutral, and `?` annihilating. **Accept the group iff every idempotent matrix in the closure has a `<` on its diagonal.** A call whose arguments are not analyzable grades `?`, which withholds information rather than failing.

A definition is `Partial` if any `rec` group it contains is rejected, if it mentions `Prim::Exit`, or if it mentions a `Partial` definition. That relation is a transitive closure over the module's definitions and is computed once.

## The gates

Both gates run post-zonk, as siblings of `validate_universes` and `check_positivity`, at the two sites in `curios-core/src/elaborate/module.rs` those already use. Post-zonk placement is what makes them metavariable-free by construction: `zonk_module` errors on an unsolved hole first, so no metavariable can later be solved to a partial term.

### Seeding

(T) is seeded from every type position in the module: each `Definition`'s type, each telescope binder type, each `induct` payload and `struct` field type, each `Match` motive, each argument at a parameter whose declared type ends in a sort, and each type-parameter argument of a nominal declaration. The closure then follows definitions into their bodies, which is essential and easy to overlook — `/std/BigNat/Canonical` reaches `is_trimmed` only through its own body, and no type annotation anywhere records that.

(V) is seeded from the elaborator, not from a walk. Every term elaboration settles carries the type it settled at; `Context::record_checked` collects the pair at the one `Step::Settle` in `elaborate`'s driver, and `checked_proof_positions` keeps the `Prop`-sorted ones. That is the whole rule. Sort-hood is decided at the gate rather than at the hook, because a type may still carry unsolved metavariables while elaborating; post-zonk every solution is materialized, so `is_prop` is asked once per *distinct* type, and hash-consing keeps that set far smaller than the term count.

### Obligation (V) is not a walk

**This replaced a walk, and the walk is why (V) had six defects.** What follows records the reasoning, because the same mistake is available anywhere else in this compiler.

The original design re-derived, from the finished term, a judgment elaboration had already computed and discarded. Reconstructing it needed a partial reimplementation of type synthesis — `synth_neutral` for an application's head, `scrutinee_type` and `constructor_telescope` for a match's arm binders, declaration lookups for a literal's fields, and a binder-opening traversal to keep all of it metavariable-free. Every defect found after step 3 was an incompleteness in that reconstruction: the `reach() == 0` guards, the unseeded argument positions, the entrypoint blind spot, `synth_neutral` declining at 6010 of 6041 sites, the prelude-declaration lookups, and `scrutinee_type` having no case for an `Apply` at a `RecMember` — which left the arm rule inert for *every parameterized family*, `Option`, `Lst`, `Vec`, and `Eq` among them. Six, none found by reading, each fixed by adding a case, with no reason to believe the seventh was not queued behind them.

The cure is not a better walk. It is not asking the question twice. Elaboration decides, for every term in the program, what type that term has; a later pass can only guess at it. Seeding from `Step::Settle` buys two properties that were previously maintained by hand and are now structural:

- **Coverage** follows from elaboration being a typechecker. A position it never settles is a position the program was never typed at, which is a far louder failure than a silently dropped obligation.
- **The prelude boundary** follows from replay. The archived prefix is *defined* into the context by `define_assuming_scheme`, never elaborated, so it settles nothing and seeds nothing, and its verdicts arrive through `recorded_totality` exactly as before. The defect amended below cannot recur, because there is no lookup left to miss.

Two design notes worth keeping. The obligation is recorded at `Settle` *after* span restamping, so the recorded term is the one that reaches the module. And it is independent of `Mode`, because the settled type is the term's type whether it was checked or inferred — which also covers proofs produced in inference position, a case the `Mode::Check` formulation misses.

**Historical.** The three amendments that follow describe the walk's rules and their gaps. They are retained because the *method* they record still holds — every one of these was found by counting and none by reading — but the code they describe is deleted.

**Historical — the obligations are over *terms*, not over the names a term reaches.** "A gate fails when a `Partial` definition is reachable from a seed" is not sufficient, because a `rec` written inline in an erased position mentions no definition at all. `rec Bad : Type = Sink(Bad)` as a local binding satisfies every closure over `Global`s while retying exactly the knot the closure exists to forbid, and there is no name to stamp `Partial`. Each gate therefore collects the erased *positions* — carrying the site for the diagnostic — and rejects a position on either ground: it reaches a definition classified `Partial`, or it is itself partial, containing a `rec` group that does not descend or a `Prim::Exit`. The second test is `term_is_locally_partial`, the same machinery `definition_is_locally_partial` uses, applied one level down.

**Historical — the entrypoint expression is not covered, and both gates must cover it.** `into_core` emits top-level `let`s as `Module::items` but leaves the trailing expression in `Module::body` and its annotation in `Module::type_`. Steps 2 and 3 walk `items` only, so an exploit written wholly inside the trailing expression — a local `rec` and a struct literal with a forged certificate, which is all three reproductions need — is seen by neither obligation. There is no `Definition` to stamp for it, so this is not something persistence can fix: each gate must additionally seed from `body` and `type_`, and classify the local `rec` groups they contain, treating the pair as one anonymous definition whose partiality is its own. Nothing today observes the gap, because the classifier only reports.

**Historical — step 3's (V) seeding has two gaps, and both must close before step 7 rejects anything.** The seeding walk descends with `any_child_term`, which hands back a scope body without opening it, so a `Struct` or `Variant` under a binder carries loose de Bruijn indices and `Sort::of` panics on the free occurrence rather than answering. It is currently guarded by a `reach() == 0` test, which *skips* those positions: a certificate constructed inside a function body is not seeded. The second gap is that argument positions at `Prop`-typed binders are not seeded at all; an inline proof inside a proof-valued definition is covered by that definition's whole-body seeding, but the same proof inside an ordinary function is not. Both are invisible while the pass only reports. Closing them means giving the seeding walk the scope-opening traversal the classifier already has, which is the first task of step 7 rather than a follow-up to it.

**Both are closed, and "two gaps" understated the second by two orders of magnitude.** The `reach() == 0` guard is gone, replaced by a `debug_assert` that holds across the prelude: the walk opens every binder it descends through and defers `rec` groups — whose member scopes bind the group's own members — to a per-module worklist. The argument rule exists, but *stating* it was not the same as its firing; see the measurement amendment under "Measurements".

**Historical — the gates do not see prelude declarations, and three seeding rules need them.** At the replay site both gates run on `user_module`, whose `induct_decls` and `struct_decls` are the *user suffix alone*; the prelude's are spliced in on the lines immediately after. Every seeding rule that resolves a declaration therefore finds nothing for `/std/Option`, `Result`, `Lst`, `Eq` and the rest, and silently skips — the constructor-payload rule, the struct-field rule, and the arm-binder typing that the argument rule depends on. The minimal pair differs only in where the inductive is declared:

```crs
induct Opt : pub Type | some(f : (False) -> Nat) | none() end     -- rejected
match o | some(f) => f(rec b : False = b; b) | none() => 0 end

                                                                  -- /std/Option: accepted
match o | some(f) => f(rec b : False = b; b) | none() => 0 end
```

The boundary is correct where it was argued. Prelude items cannot mention user code, so they are sinks of the *occurrence* relation, and inheriting their recorded verdicts rather than re-analyzing them is sound — that is what `check_positivity` and `recorded_totality` rely on. The argument does not transfer to these three rules, because they ask a different question: not "what does this prelude definition reach" but "which fields of this declaration are `Prop`-sorted", asked about a *user* term whose type happens to be declared in the prelude. It is a lookup, not a reachability claim, and it was carried across the boundary by proximity. `Context` carries the full registries — elaboration could not typecheck `/std/Option` otherwise — so consulting those rather than `Module` was the obvious fix. It is not the one taken: the seeding that needed the lookup no longer exists, and the reproduction above now compiles to a rejection because the inline `rec` settles at `False`.

### Rejection

A gate fails when a `Partial` definition is reachable from a seed. (T) additionally needs a **local form** at `Item::Rec` elaboration: a partial member may not have a sort in an extractable position of its type — its codomain after peeling arrows, or any component reachable by projection. Parameter positions are exempt, so a partial polymorphic program keeping `@A : Type` is unaffected. This local form exists because `rec Bad : Type = (Bad) -> False` overflows the stack during elaboration of its first *use*, long before any post-zonk pass runs; without it the compiler aborts instead of diagnosing.

**Amended — the local form belongs to both `rec` elaboration paths.** `Item::Rec` and `Subterm::Rec` are elaborated by different code, and the same shape written as a *local* `rec` still aborted after the top-level one was fixed. Both call the check on the rebuilt group, before its members are defined into the context and before the tail (or the next item) is elaborated, which is where the first use lives. Verified by probing each shape out of process: both now report `the recursive definition 'Bad' is a type position but does not terminate on every input`, the local one with a span.

## Persistence

`Definition` gains a `totality: Totality` field, mirroring how `InductDecl` and `StructDecl` gained `polarities`. Carry it through the `zonk_module` rebuilds, initialize it at every remaining struct-literal site, and bump `SCHEMA` in `curios-prelude/src/archive.rs`.

The archived prelude is restored and trusted rather than re-typechecked (`curios-core/src/into_ersd/lower.rs`), exactly as its polarity vectors are. The prelude is therefore gated at compiler-build time, on the from-scratch elaboration path, and the persisted flag is what lets a user program's gates see prelude partiality without re-analyzing it.

## Implementation steps

**1. Record the theorem.** `DESIGN.md` gains the claim, the trusted base, the deadline's consequence for decidability, and definitional K as a permanent commitment against univalence — currently noted only in `curios-core/src/invert.rs`. Nothing else is measured against anything until this exists.

**2. The totality classifier, non-rejecting.** Implement the analysis above and emit a report. Change no acceptance. This is the largest and riskiest piece, and shipping it inert is what converts every hand classification below into a measurement.

**3. The reachability report, non-rejecting.** Implement both closures and report which partial definitions are reachable from a type and from a proof.

**4. Fix the prelude** until that report is empty. Expected content is `/std/Fmt/parse_at`, `parse_lit_at`, and `parse_esc_at`, which recurse on `pos + 1` and `pos + 2` with `bytes` fixed. Add a descending `fuel : Nat` to all three, wrap each body in `match fuel | 0 => … | fp + 1 => …`, pass `fp` at the eight recursive call sites, and seed it from `Bytes/len` in `parse`.

**Amended.** The `Fmt` prediction was exact and nothing else appeared under (T). The seed is `3 * Bytes/len(bytes) + 3`: a step either advances `pos` or hands off at the same `pos` through `parse_at` → `parse_lit_at` → `parse_esc_at`, so three units per byte is a bound the parse cannot reach and the `0` arms terminate the format rather than truncating a directive. (V) reported one name the spec had not anticipated, `/sys/exit` through `/std/proc/exit`, which is the second source change: a new `/std/Never` and `proc/exit` retyped at it. `Never` lives in `/std` and not `/syn` because no Rust lowering emits it, which is what `/syn` membership means.

**5. Persist totality**, as specified above. Precedes the gates because they need cross-module partiality.

**Amended.** `Definition` and `RecDefinition` both carry the flag — `Definition` is the *opened view* of a recursive member, so the field has to round-trip through `RecItem::try_new` and `RecItem::definitions` or it is discarded on every zonk. It threads through `Module::shared` and `zonk_definition`, and is projected out at erasure alongside the universe context, since nothing past erasure reads it. `record_totality` is the single writer, running post-zonk beside `check_positivity` at both elaboration sites; `recorded_totality` is the reader, and is how the replay site inherits `/std`'s verdicts without re-analyzing it. `SCHEMA` went 16 → 17.

**6. Gate (T)**, both the local form and the closure form.

**Amended — implemented, and unblocked.** Both forms work and every reproduction in "The unsoundness being closed" is now rejected with a diagnostic, including the two that previously aborted the compiler. The gate initially rejected three existing tests; teaching the descent analysis one shape retired all three without weakening (T). See the appendix.

**7. Gate (V)**, with `Prim::Exit` folded into the partiality relation.

**Amended — the two seeding gaps close against `convert.rs`'s primitives, not the classifier's walk.** The instruction below to give the seeding walk "the scope-opening traversal the classifier already has" is wrong, and following it would have done damage. The classifier mints binders with `Context::fresh` and never assumes their types, which is enough to compare shapes and useless for deciding sorts; assuming them instead is what the hazard actually is, because `Context::assume` bumps `mutation_stamp` and would invalidate the memoization caches continuously. `Sort::of` already faces exactly this and already solves it: `Opened = [(Free, Term)]` (`curios-core/src/convert.rs:91`) threads locally-opened binders *beside* the context rather than into it, which is what keeps sort decisions observationally read-only. The seeding walk carries the same vector, and both `reach() == 0` guards come out with it.

The second gap has a ready answer in the same file. Resolving an application's parameter types by looking its head up among the module's definitions would cover only globals; `synth_neutral` (`curios-core/src/convert.rs:97`) synthesizes a neutral spine's type from the primitives `infer` itself uses, returns `None` conservatively, and covers locals, globals, curried spines, and projections alike — so the argument rule is stated against the existing judgment rather than a second one that could drift from it.

Measure before rejecting, as step 3 did. Closing the gaps makes (V) see strictly more, and the report is what says whether the corpus pays for it.

**Amended — implemented and gated; the corpus paid nothing, and the instruction to measure was the only thing that found the real defect.** Both gaps closed as directed above, the walk became iterative (a `Str` literal is one certified-UTF-8 link per byte, so a recursive walk overflows a default test stack), and `check_proof_totality` runs beside `check_type_totality` at both elaboration sites. No prelude source changed and no test changed.

The direction to state the argument rule against `synth_neutral` rather than a second judgment was right, and it also understated what it was buying. `synth_neutral` had no case for a universe instance, a recursive member, a partially applied spine, or a structure projection, so the rule it backs almost never fired — see "Measurements". Extending it was not only a totality repair: `compare_same_rec_apply` (`curios-core/src/convert.rs`) exists to compare applications of the same recursive member, its head is by construction a `RecMember`, and with no such case `apply_param_types` fell back to `Term::type_ground()` unconditionally, silently disabling η and proof irrelevance on the one path written for them. The prelude hash-consed to 25976 distinct structures before and after every kernel change, which is the evidence that elaboration did not shift.

Steps 6 and 7 are independent and may ship separately.

**8. Reseed (V) from the elaborator**, and delete the walk.

Not planned. It arrived from investigating why the defects kept coming, and it is the reason the "Left open" list is two entries shorter rather than one. Instrumenting the walk's rules at the replay site — the configuration no measurement had ever covered — found the prelude-declaration defect exactly where it was predicted *and* a seventh gap that no reading had suggested: `scrutinee_type` unfolds a bare `RecMember` but has no case for an `Apply` at one, so the arm rule was inert for every parameterized family. That is the same shape of omission as `synth_neutral`'s, one function over, in a rule whose blindness the previous step's fix had made invisible.

Two counting rounds settled the design. A first proposal seeded from erasure's own `is_erasable` — the classifier every erasure walk shares — on the argument that the obligation is stated over what erasure deletes. Measurement refuted it: erasure deletes 1, 4, and 6 terms on `hello_curios`, `binary_trees`, and a written probe, against (V)'s 15, 34, and 8 seeds. `classify.rs` classifies against the *declared* signature with binders opened opaquely, which is what fixes runtime arity and is deliberately blind to the instantiation where a proposition appears; seeding from it would have enforced a strictly weaker property than the one definitional proof irrelevance already leans on. The second proposal — seed from the check judgment — measured as a superset in the same run, and the deltas were `+1`, `+4`, `+1` against `v-struct-invisible` counts of `1`, `4`, `1`: the hook recovers exactly the seeds the boundary defect was dropping, three times out of three.

The differential also refuted half of its own design. `hook-T` came back a strict *subset* of (T)'s seeds — 26 against 30, 35 against 51 — because (T) is not a typing judgment and its aggressive reading seeds bodies no `Mode::Check` classifies as a type position. (T) therefore stays a walk. The unified single-hook design was wrong on the half nobody had measured, and only measuring it said so.

## Measurements

Originally estimated by a throwaway extractor over `curios --print=core`, with three known gaps: elided `Match` motives, bare nullary references, and descent reasoned about rather than computed. Steps 2 and 3 closed all three by construction, so the figures below are now **computed** by the classifier and the two closures over from-scratch prelude elaboration.

| | before step 4 | after step 4 |
| --- | --- | --- |
| definitions | 1061 | 1063 |
| classified `Partial` | 174 | 169 |
| **(T) partial and reachable from a type** | **4** | **0** |
| **(V) partial and reachable from a proof** | **1** | **0** |

**Amended.** Teaching the descent analysis arithmetic descent on `Nat` — `n / k` and `n - k` for a literal `k`, under a guard that excludes zero — moved 23 more definitions to `Total`, taking the partial count from 169 to 146 with nothing moving the other way. `/std/Nat/to_str` and `/std/BigNat/of_nat` are among them, which is what retired the three rejected tests without touching (T) or any `/std` source.

The (T) content was exactly the four the estimate predicted — `/std/Fmt/parse` and its three workers, reached because `format_type_with(Str, parse(s))` is the result type of `render` and `print`. The (V) content was `/sys/exit`, which the estimate did not anticipate.

Every name the design depends on classifies `Total`: `add/raw`, `raw_assoc`, `raw_trimmed`, `raw_comm`, `cmp/raw`, `mul/raw`, `succ/raw`, `is_trimmed`, `trim`, `count_scalars`, `drop_width`, and `format_type_with`. **`/syn` has no partial definition at all**, which is the sharpest single statement of the corpus cost: the module tree that carries the certificates is total throughout.

The 169 remaining partials are concentrated where the design expects them and none is reachable from a type or a proof: `/std/Toml` (54), `/std/Async` (16), `/std/tcp` (12), `/std/Json` (11), `/std/Flt` (10), `/std/BigInt` (9), `/std/http` (8), `/std/File` and `/std/BigNat` (6 each), `/std/Map` (3), and a scattering of one- and two-name modules. `/std/print` and `/std/Fmt/print` are partial only because `/std/Handle/write` is, which is the classifier being honest about a retry loop rather than a problem.

**Amended — (V)'s argument rule was near-inert, and the count that says so does not cover the path user programs take.** The rule recovers a head's parameter telescope through `synth_neutral` and seeds the arguments sitting at `Prop`-declared entries. Instrumenting every head `synth_neutral` declined, over a from-scratch prelude elaboration:

| application head | before | after |
| --- | --- | --- |
| `UniverseInst` | 3429 | 0 |
| `RecMember` | 1719 | 0 |
| `Proj` | 751 | 3 |
| `Apply`, curried | 83 | 0 |
| `Var` / `Func` / `Match` | 28 | 28 |
| **total** | **6010** | **31** |

`@A : Type` generalizes nearly every `/std` definition, so the rule did not fire for polymorphic code at all. What that admitted needs no `match`, no data type, and no recursion but the forgery's own:

```crs
let ignore(@A : Type, x : A, p : False) -> A = x;
let leak() -> Nat = ignore(0, rec b : False = b; b);
```

Two cautions on the figures, both larger than the figures. They count **one rule of one gate**: the definition-level, struct-field, and constructor-payload rules of (V), and every rule of (T), have never been instrumented. And they were computed during the *prelude build*, where `Module` is the prelude and every declaration lookup therefore succeeds — the one configuration in which the boundary defect amended under "The gates" cannot appear. **The replay path is unmeasured.**

**Amended — the replay path was then measured, and both cautions were justified.** Instrumenting every (V) rule at replay, on `hello_curios`, `binary_trees`, and a probe written to exercise each rule against a user declaration and a prelude one:

| | hello | trees | probe |
| --- | --- | --- | --- |
| struct rule, user declaration | 1 | 0 | 1 |
| struct rule, **invisible** (prelude) | **1** | **4** | **1** |
| arm rule, **invisible** (prelude) | 0 | 0 | **3** |
| scrutinee declined, parameterized family | 0 | **4** | **2** |
| (V) seeds pushed | 15 | 34 | 8 |

The boundary defect is live at one seed per string literal — every `Str` literal is a `/syn/Str` struct value whose declaration only `Context` holds. The constructor-payload rule, which this document lists as one of the three the defect breaks, turned out **not** to be broken but *redundant at replay*: a user never writes a prelude `Variant` node, because `Option/some(p)` elaborates to an application of the generated constructor wrapper, and the wrapper's body is a prelude definition and a sink. Three rules were named from reading; measurement found two live, one redundant, and one entirely different defect the reading had not suggested.

**The erasure premise gained evidence from the pre-flight, in the direction that matters.** The measurement made for the rejected erasure-seeded design counted what erasure actually deletes: 1, 4, and 6 terms, against (V)'s 15, 34, and 8 seeds, with **zero** deleted terms containing a `rec` group or an exit. On this corpus (V) is a strict superset of erasure's deletions, which is the containment the premise needs. It is three small programs and a proxy for local partiality only, so it is evidence, not a demonstration.

The method is worth recording with the numbers, and step 8 strengthened it. Every attempt in this effort to locate the gaps by reading the seeding walk mis-scoped them — first as "two gaps", then as three narrow ones, then as three rules of which one was redundant — and every attempt to count them found something the reading had missed, including one defect (`scrutinee_type` at a parameterized family) that no reading had suggested at all. Counting also refuted two designs that argument alone had endorsed: erasure-seeding, and the unified single-hook that would have taken (T) with it. Instrumentation should lead here, a count is only as good as the configuration it was taken in, and a design that has not been measured on the half nobody looked at is not yet a design.

## Verification

Regression tests in `curios/src/tests/soundness.rs`, using the `assert!(crate::run_text(…).is_err())` idiom already established in `curios/src/tests/positivity.rs`. Every reproduction in "The unsoundness being closed" becomes a rejection test, including the projection and total-function-on-partial-value variants and the `(Bad) -> False` shape whose current behavior is a stack overflow.

Acceptance tests must pin the classifications the design depends on: `add/raw` and `raw_assoc` accepted, which fails without refinement expansion; `raw_comm`/`raw_swap_step` accepted, which fails without the mutual closure; `format_type_with` accepted; `/std/Json/decode` and `/std/Async/bind` classified partial and still usable in a program; and a partial definition permitted in a runtime term while rejected in a proof.

Unit tests in `curios-core/src/totality/tests.rs` for the size lattice, matrix composition, idempotent-closure acceptance, and the refinement-expansion cases above.

**Amended — a (V) fixture must assert *which* gate fired, and the argument rule needs one fixture per head shape.** (T) runs first, so a fixture that accidentally put a partial definition in a type position would pass a bare `is_err` while proving nothing about proof positions; `rejected_as_a_proof` asserts the proof-position diagnostic instead. And because the argument rule can only fire where the head's type can be synthesized, one representative program does not cover it: the fixtures enumerate the head shapes — polymorphic, match-arm binder, primitive fold binder, structure projection, and concept method — because each fails independently of the others. Two of those five compiled and ran before this step.

**Amended — keep the five fixtures, and note that they no longer test what they were written to test.** They enumerate head shapes because the argument rule needed `synth_neutral` to succeed on the head. Step 8 removed head synthesis from (V) entirely, so the five shapes are now one case, and all five pass unchanged. That is worth more as a *regression* than the enumeration ever was as coverage: it is the direct evidence that the defect class went away rather than being re-covered case by case. `rejected_as_a_proof` keeps its job, and gains one — the proof-position diagnostic is now produced by the hook, so the assertion pins the new seeding as well as the gate.

The full gate applies, in order, with the suite run once into a file and inspected there:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features > /tmp/curios-tests.txt 2>&1
```

## Risks

**The classifier rejects something step 3 did not predict.** The measurements above are from a throwaway extractor with known gaps. Steps 2 and 3 exist to find that before any gate can break the build, and a surprise rejection is a finding to surface rather than a reason to weaken the check.

**`Sort::of` in (V)'s seeding is the one deadline contact.** Measure with `make curios/profile CURIOS_PROFILE_SOURCE=programs/hello_curios.crs` before and after; the fallback is recording sort-hood at elaboration time.

**Amended — the fallback became the design, and the contact moved rather than closing.** (V) no longer decides sort-hood while walking; it records `(term, type)` at `Step::Settle` and classifies once per *distinct* type at the gate, after zonking. The hot path is a `Vec` push, and hash-consing keeps the classified set far below the term count. Two costs replace the old one and neither is measured: the push happens for every settled node, and each recorded pair is zonked at the gate. The prelude hash-consed to 25976 distinct structures before and after, so elaboration did not shift, but that is not a timing.

**Archive churn.** The new `Definition` field forces a prelude image rebuild, so the first build after the `SCHEMA` bump is slow.

## Left open

Two kernel rules are load-bearing once these gates exist and have no written argument. Definitional proof irrelevance (`curios-core/src/convert.rs:2046`) accepts without inspecting either term, which is correct precisely because every `Prop` inhabitant will now be total, and its side condition is the large-elimination guard. The conversion recurrence rule (`curios-core/src/convert.rs:2056`) accepts on the absence of finite disagreement; its canonicalization is alpha-renaming over binders minted during the run, its history is per-run with explicit removal on park and retry, and aggressive (T) removes its exposure to bottom-typed terms. Both appear sound. Both should be argued in `DESIGN.md` rather than assumed. Neither has been probed.

Three more, ordered by how much each would change what the claim is worth. Three earlier entries closed with step 8: the prelude-declaration defect, which had no fix because the lookup that needed it no longer exists; "only one seeding rule has been measured", since every (V) rule was counted at replay before being deleted; and the residual 31, which counted heads `synth_neutral` declined for a rule that is gone.

**The erasure premise is stated, not demonstrated.** Both obligations rest on the objective's second claim — that erasure deletes types and `Prop`-sorted proofs, and nothing else. If `into_ersd` deletes anything further, (T) and (V) are aimed at the wrong set of positions. It is the one assumption whose failure invalidates the structure rather than leaving a hole in it, and it is an argument about a single lowering pass against a specification that already states what it should delete. The pre-flight under "Measurements" is the first evidence in its favour — (V) seeded a strict superset of erasure's deletions on three programs — and the deletion sites are now known to be five, four of which fire only during the prelude build: `masked_fields`'s mask skip, `kept_operand_at`, the non-direct proof-valued callee, `is_proof_constructor`, and the erasable scrutinee. That reduces the argument to a bounded reading rather than an open question.

**(T) is a walk, and walks are what this effort kept finding defects in.** Step 8 removed (V)'s, and the differential showed why (T)'s cannot go the same way: its aggressive reading deliberately seeds more than any typing judgment does, so `hook-T` measured a strict subset of it. That justifies the walk but does not audit it. Every rule of (T) has now been counted exactly once, in one configuration, and the entrypoint blind spot it once had was found by reading a lowering contract rather than by a count.

**What is enforced is narrower than what `DESIGN.md` claims.** `DESIGN.md` states "there is no closed term of type `/syn/False`". The mechanism enforces that no term *at a `Prop`-sorted type* and nothing in a type position is partial, which is the term-level property and stronger than the value-level one an erasure-seeded design would have given — the distinction is recorded under step 8 because it decided that design. The two are not obviously the same statement, and the difference should be settled in `DESIGN.md` rather than left to the reader.

## Appendix: the fork on (T), and how it closed

Gate (T) is implemented and works. It initially rejected three programs that compiled before and that the repository has tests for, and the shared reason contradicted the argument this specification gives for the aggressive reading. **It is closed: none of the three resolutions below was taken, because the premise they all shared turned out to be false.** All three rejections traced to one shape the descent analysis could not read, not to the aggressive reading being too strong. See "What actually happened" at the end.

### What is rejected

```text
tests::concepts::prop_laws_concept_resolves
  the type of '/take' is a type position but reaches 'witness7', which is not known to terminate
  from   let take(q : Eq(Show/show(7), Show/show(7)), n : Nat) -> Nat = n;

tests::erasure::proof_bound_as_a_statement_does_not_run_its_certificate
  the type of '/p' is a type position but reaches '/a', which is not known to terminate
  from   let p : Eq(BigNat/add(a, b), BigNat/add(b, a)) = BigNat/add/comm(a, b);

tests::strings::utf8_decimal_is_ascii_carries_its_proof
  the type of '/decimal_is_ascii' is a type position but reaches '/decimal', which is not known to terminate
```

`Show/show` at `Nat` is `/std/Nat/to_str`, and `/std/BigNat/of_nat` recurses on `n / 2`. Both are classified `Partial`, correctly. The statements above merely *mention* them.

### Why this invalidates the stated argument

"(T) is the aggressive reading" defends the imprecise rule on the ground that the precise one "would buy nothing, because (V) independently rejects every nontrivial way to inhabit a proposition about a partial program, leaving only reflexivity-only statements." The premise is that reflexivity-only statements survive. They do not: the rejection lands on the **statement**, not on the proof, so `Eq(Show/show(x), Show/show(x))` — reflexivity itself — is rejected. What aggressive (T) actually costs is the ability to state *any* proposition about the result of a partial function, which in a standard library where `to_str`, `of_nat`, `join`, and `Show/show` are all partial is the verified-interface idiom entire.

The prelude passes because its proofs quantify over abstract variables rather than concretely computed values. The whole cost falls on user code, which is why steps 2 through 4 did not find it and only the gate could.

### What distinguishes the two cases

```crs
Eq(to_str(x), to_str(x))    -- harmless: Eq never scrutinizes its indices
Shape(inf)                  -- dangerous: Shape matches on its argument, and unfolds forever
```

Whether a type-level eliminator *scrutinizes* the partial value — which is exactly the per-parameter, interprocedural summary this specification defers. Every cheaper approximation examined leaks through the `Trap` counterexample already recorded above: exempting index positions, or checking only arguments of sort-valued functions, both admit `induct Trap(f : F) | mk(x : Shape(f)) end` at `Trap(inf)`.

### The three resolutions

**Build the precise reading.** Implement the deferred summary. Restores all three tests, keeps every route closed. Costs a second analysis comparable in size to positivity, which must be right or `Trap` leaks.

**Accept the cost.** Keep (T) as specified, rewrite or delete the three tests, and record in `DESIGN.md` that a proposition may not mention a partial function. Simplest and strictly sound. Two of those tests guard real past bugs, and the idiom does not come back.

**Ship (V) first, hold (T) at report-only.** Land step 7 and decide (T) with more evidence. Note that (V) seeds a `Prop`-typed definition's whole body, so `let p : Eq(…) = BigNat/add/comm(a, b)` reaches `of_nat` from the other side; this likely defers the fork rather than avoiding it, and that should be measured before choosing it.

### What actually happened

None of the three. All three rejections were one shape the descent analysis could not read:

```crs
rec to_str(n : Nat) -> Str = match n < 10  | true => … | false => … to_str(n / 10) …
rec of_nat(n : Nat) -> BigNat = match n == 0 | true => … | false => … of_nat(n / 2) …
rec decimal(n : Nat) -> …  = match Nat/lt(n, 10) | true => … | false => … decimal(Nat/div(n, 10)) …
```

A guard comparing a `Nat` against a literal, and a recursive call on that `Nat` divided by a literal. Size-change graded the argument unknown because a quotient is not a constructor tree, so `to_str` and `of_nat` classified `Partial` — correctly, given what the checker could see, and uselessly, since both plainly terminate.

The analysis now reads it. A `Cases::Bool` or `Cases::Switch` arm that excludes zero records the binder as nonzero, and `n / k` for a literal `k >= 2` (or `n - k` for `k >= 1`) then grades a decrease against it. The rule is sound on one line — `v >= 1` and `k >= 2` give `v / k <= v / 2 < v` — and the guard is not conservatism: without it `rec loop(n : Nat) -> Nat = loop(n / 10)` would be accepted, and it diverges at zero.

**Nothing else moved.** (T) keeps the aggressive reading, no `/std` source changed, and no test source changed. The corpus paid nothing: 23 definitions moved to `Total` and none moved the other way.

What survives is the *limit*, not the fork. A proposition still may not mention a function outside the checker's boundary, so `/std/Json/decode` and the rest of the deliberately-partial corpus remain unmentionable in a statement. That is now a documented boundary with a known shape rather than a decision blocking the work, and the case for widening it should come from a program that hits it.

## State of the worktree

Everything above is committed and the full gate passes: `make curios/runtime`, `cargo fmt --check`, `RUSTFLAGS="-Dwarnings" cargo clippy`, and `cargo test --workspace --all-targets --all-features` — 1187 tests across 17 binaries, one ignored (the bundle end-to-end test, which AGENTS.md runs explicitly).

All eight steps are implemented. (T) is a syntactic walk over type positions; (V) is seeded by the elaborator and has no walk. The boundary defect that made the previous revision of this section hedge is closed — not fixed, removed, along with the seeding that could exhibit it.

Three things are worth stating plainly about what that did and did not buy.

The five `curios/src/tests/soundness.rs` fixtures written one-per-head-shape — polymorphic, match-arm binder, primitive fold binder, structure projection, concept method — all pass unchanged. They exist *only* because the argument rule could fire solely where `synth_neutral` could synthesize the head, and two of the five compiled and ran before step 7. Under the hook there is no head synthesis, so the distinction they guard has stopped existing rather than been re-covered. That is the sharpest available evidence that the defect class is closed and not merely patched.

The hook's cost is unmeasured. Recording is a `Vec` push per settled node and classification is memoized per distinct type, and the prelude hash-consed to 25976 distinct structures before and after — so elaboration did not shift — but no before/after timing was taken. `make curios/profile` is what would settle it.

And the objective is still not met, for reasons that no longer include this one. What remains is under "Left open": the erasure premise, (T)'s own walk, two unargued kernel rules, and the gap between what `DESIGN.md` claims and what the mechanism enforces.
