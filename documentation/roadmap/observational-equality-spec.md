# Equality is defined at the type, not assumed over all of them

## Status

Deliberately unrefined. What is settled is the *target theory* and the audit that settled it, both recorded below; the design space itself is untouched, and of the questions in the last section only two carry any answer — both of them inventories, neither of them a decision. Nothing is started.

This would touch the trusted base at its most consequential rule — conversion — and it would change what `/std/Eq` means for every proof already written against it. It wants review, and answers to the questions below, before it wants code.

## Why it exists

[Definitional proof irrelevance](../soundness/per-term-rules/definitional-proof-irrelevance.md) names the destination: *"What is still missing is the model, not the reasoning … That is the first properly metatheoretic work this project would undertake, and it is what an observational-equality layer would rest on directly."* This specification is the location that sentence was pointing at.

Three separate things point the same way.

- **Function extensionality is not derivable and is not present.** `/std` contains no `funext` and no axiom standing in for one. Postulating it would not help: transport along an axiom is a stuck term forever, which for a language that erases and emits WebAssembly is worse than not having it.
- **Two closed inhabitants of `False` have already been produced by the same interaction, and both are recorded.** A `Prop`-sorted `Box` carrying a `Type` payload, where irrelevance identifies `mk(A)` with `mk(B)`, congruence through `unbox` carries that to `Eq(Type 0, A, B)` for *any* two types, and transport turns `()` into a proof of `False` (`a_derivation_through_a_type_carrying_proposition_is_refused`, in `curios-cert/src/recheck/proposition_tests.rs`); and a constructor whose index target mentions its payload through a non-injective function, which mentions it without determining it ([Large-elimination guard](../soundness/per-term-rules/large-elimination-guard.md)). Both are closed. Both routed through *definitional irrelevance × an inductive `Eq` × large elimination into `Type`*, which is the combination an observational equality does not have.
- **A consumer has already been written against extensionality that does not exist.** [The map wall falls by classes, not by symptom](../design/toolchain/the-map-wall-falls-by-classes-not-by-symptom.md) commits a future certified `/std/Map` to binding its proofs to `entries`/get-extensionality rather than node anatomy. That is an extensional specification of a type whose equality is currently intensional.

## What the literature settles

The relevant line is Pujet–Tabareau, and its metatheory is finished and machine-checked. Cited by venue and year so a later reader can tell which result is being leaned on.

- **TT^obs** — *Observational Equality: Now For Good*, POPL 2022. Predicative `U_i` beside a proof-irrelevant `Ω_i`. Normalization, canonicity, decidable conversion and consistency, formalized in Agda.
- **CC^obs** — *Impredicative Observational Equality*, POPL 2023. TT^obs extended with an **impredicative** universe of definitionally proof-irrelevant types. Decidability of conversion is obtained by using proof-irrelevance to avoid computing with impredicative proof terms, and normalization is proved in plain MLTT. **This is the target**, and the audit below is why: it is the only result in the line whose sort structure is the one Curios already has.
- **CIC^obs** — *Observational Equality Meets CIC*, TOPLAS 47(2), April 2025. CC^obs plus CIC's indexed inductive scheme, via *Fording*; plus the **Cast-Refl** rule `cast A B e t ≡ t` when `A ≡ B`, added as a conversion rule with a convertibility precondition rather than as a reduction rule, and proved not to cost decidability; plus quotient types. Implemented as a Coq fork on rewrite rules.
- **The cliff** — Abel and Coquand, *Failure of Normalization in Impredicative Type Theory with Proof-Irrelevant Propositional Equality*, LMCS 16(2), 2020. Normalization fails for an impredicative `Prop` with a proof-irrelevant propositional equality **when transport reduces on endpoint convertibility without inspecting the proof** (Werner's rule). The divergent term is `Ω h = δ (ω h)` with `ω h A = cast ⊤ A (h ⊤ A) δ`, which reduces to itself precisely because `cast` ignores the neutral `h ⊤ A`. Consistency and canonicity are not at stake; normalization and hence decidable conversion are. CC^obs is the answer to the question this paper left open.
- **Accessibility** — *Definitional Proof Irrelevance Made Accessible*, LICS 2026. Accessibility predicates in the irrelevant universe, in two variants: decidable via propositional unfolding, or flexible via definitional unfolding at the cost of possibly-diverging typechecking. Relevant only if a future well-founded-recursion facility wants to live in `Prop`.

The neighbours were looked at and are not candidates. Cubical type theory rejects UIP, requires an interval and Kan composition, and has its own regularity problem; it buys univalence, which Curios does not want. Higher observational type theory has no complete published theory and no proof assistant implementing it.

## What Curios already has

The audit is why CC^obs rather than TT^obs, and why this is a layer rather than a rewrite of the sort structure.

- **The sort structure is already CC^obs's.** A cumulative predicative `Type` hierarchy with algebraic inferred levels, beside a *separate impredicative* `Prop` with definitional proof irrelevance — `curios-core/src/universe.rs:3`, `curios-cert/src/kernel/module.rs:403`, and [`Prop` is strict, proof-irrelevant, and definitionally K](../design/language/prop-is-strict-proof-irrelevant-and-definitionally-k.md). That is `(U_i, Ω)`, and the impredicativity is exactly what makes TT^obs the wrong target and CC^obs the right one.
- **The conversion apparatus OTT needs is in place.** Conversion is type-directed and carries the type at every goal; η fires at Π and at Σ; irrelevance is discharged at a `Prop`-sorted goal before either side is examined — `curios-cert/src/kernel/convert.rs:3` and `curios-elab/src/convert.rs:2064`. An inductive type-former's arguments are already compared at the declaration's own index telescope, which is what lets `Eq(@P, p, q)` at a `Prop`-sorted `P` convert with `Eq(@P, p, p)` (`curios-cert/src/kernel/convert.rs:19`).
- **The piece an observational equality replaces is `curios-prelude-archive/std/Eq.crs`**: a `Prop`-valued inductive family with `refl`, and `sym`/`trans`/`cong`/`subst` derived by matching on it. Its large elimination into `Type` is admitted by the non-informativeness excuse in the [Large-elimination guard](../soundness/per-term-rules/large-elimination-guard.md), and that guard is what both recorded defects were found in.
- **Erasure already deletes `Prop` wholesale**, and the argument is irrelevance itself — [`Prop` is strict, proof-irrelevant, and definitionally K](../design/language/prop-is-strict-proof-irrelevant-and-definitionally-k.md). An `Ω`-valued observational equality erases by the same argument; what does *not* obviously erase is `cast`, which is question 11.
- **Quotients do not exist**, and neither does any surface notion of a type carrying a chosen equality.

## What is read from source, and not probed

**The claim: Curios does not have Werner's rule, so the Abel–Coquand divergence cannot fire today.** Irrelevance in both checkers is a *conversion* rule at a `Prop`-sorted goal (`curios-elab/src/convert.rs:2064`, `curios-cert/src/kernel/convert.rs:117`); it never rewrites a scrutinee, and reduction of a match on `Eq` still demands a literal `refl` constructor. The neutral `h ⊤ A` in the counterexample therefore stays stuck rather than being identified with `refl` and reduced through.

This was read from the source and **not** probed. Under this file's own vocabulary it is *auditable only*, and it is recorded here rather than under "what Curios already has" because it has not earned that place. Question 18 is the standing debt it creates.

**What that safety costs, if the reading is right.** `subst(P, p, v)` does not reduce to `v` when `p` is neutral, even where the endpoints are convertible. That is the cast-on-refl gap verbatim — the thing CIC^obs's Cast-Refl rule exists to close — so the present position is: on the safe side of the cliff, paying the exact incompleteness the 2025 result was written to remove.

## The questions to answer

Each question records what constrains it, what turns on it, and how it would be known answered, so that picking this up is a matter of deciding rather than re-deriving. Only questions 2 and 3 carry any answer, and neither carries a decision. Four of them govern the rest: **15** and **16** decide the size of the break and therefore what may be promised about it, **9** decides whether the break reaches pattern matching at all, and **19** is the only question whose cost rises with time.

### Theory target

**1. CC^obs alone, or all the way to CIC^obs?** The sort structure is CC^obs's, but Fording, Cast-Refl and quotients are CIC^obs's, and indexed inductive families are a landed capability here. The honest reading is that CC^obs is the *foundation* and CIC^obs is the *coverage*, so the question is which increments in which order, not which paper.

**2. Does `Prop` stay impredicative?** CC^obs says it may, and that is the whole content of the POPL 2023 result. Making it predicative instead would target the smaller and older theory, but would break whatever impredicative encodings `/std` rests on.

Those encodings are enumerated, and there are four. Of the thirty `Prop`-sorted declarations across `/std` and `/syn`, only `Eq` (`curios-prelude-archive/std/Eq.crs:2`), `Accessible` and `WellFounded` (`curios-prelude-archive/std/WellFounded.crs:5` and `:11`) and `IsSome` (`curios-prelude-archive/std/Option.crs:63`) quantify over `Type` at all; the other twenty-six are first-order predicates over concrete carriers — `Nat/Lt`, `Str/Valid`, `Flt/Finite`, `BigNat/Canonical`, `Char/Scalar`, `Toml/Date/Valid`. `WellFounded(@A: Type, R) -> Prop = (x: A) -> Accessible(R, x)` is the one that forms a proposition by quantifying over an arbitrary `A : Type`, and it is therefore the one a predicative `Ω` would move.

What the enumeration does not settle is whether those four *require* impredicativity or merely sit inside it, and that is the half the question turns on. Answered when each of the four has been typed under a predicative `Ω` and the level it lands at recorded — four declarations, not a library-wide survey.

### The migration surface

**3. Can `/std/Eq` remain the surface while the underlying notion changes?** Whether `refl`/`sym`/`trans`/`cong`/`subst` can keep their spellings and types over an observational `~` is the single question that decides whether this is a campaign or a rewrite of every proof in the library.

The counts at the foot of this file measure uses of the function surface, and that is the wrong denominator for this question: a call to `subst` migrates if `subst` survives with its type, whatever it is implemented over. What has no function-surface equivalent is a *destructuring* of `refl`, and there are fifteen of those in `/std` and `/syn` — four inside `Eq.crs` itself, and eleven outside it across three files. Every one of the eleven reads as congruence: the six in `Ordering.crs` are `Eq/cong(is_eq, p)` and its siblings written out, the three in `BigNat.crs` are `cong` in one argument twice and `cong` in two arguments once (`curios-prelude-archive/std/BigNat.crs:180`, which wants either a `cong2` the module does not export or two `cong`s joined by `trans`), and `curios-prelude-archive/std/BigNat/cmp.crs:110` is congruence of a lambda. That reading is **audited and not probed**: no rewritten `/std` has been compiled, and until one has, the eleven are a classification rather than a result.

The larger half is that `Eq/refl` is not migration surface at all. It is the biggest count in the table, and its uses typecheck because *conversion* already identifies their endpoints rather than because anything is done with the eliminator — `let comm(a: Nat, b: Nat) -> Eq(Nat/add(a, b), Nat/add(b, a)) = Eq/refl();` is accepted today on the free-monoid normal form, and so is the associativity spelling. An observational layer adds to conversion and removes nothing from it, so those proofs are well-typed afterwards for the reason they are well-typed now. Both spellings are probed.

Answered when a `/std/Eq` exposing only the function surface compiles with the eleven sites ported and the kernel certifying the result.

**4. Does `~` get a surface spelling at all, or stay compiler-internal?** [Syntax forms are closed](../design/language/syntax-forms-are-closed-semantics-extend-by-witness.md): a user-visible `~` is not a new operator but an existing form opted into by a `/syn` concept, and `/syn` ownership is `curios-prelude-archive/README.md`'s to state. The alternative is that `~` is never written by a user and only `/std/Eq` is.

**5. What becomes of the existing `Eq` inductive?** Deleted, or kept as a nominal type related to the observational one. CIC^obs keeps CIC's `Id` and gives it observational content, which is what backward compatibility with existing Coq developments demanded; whether the same compatibility is worth anything here depends on question 3.

Sealing `Eq`'s representation *before* the change — dropping the inner `pub` at `curios-prelude-archive/std/Eq.crs:2`, so construction and matching are confined to the declaring subtree and only the function surface is exported — was considered and rejected. It would make the change additive for every consumer at a cost of eleven rewrites paid early, and it is the right move for a language that intends no major version. It is the wrong one here: it removes `match p | refl => …` from the teaching surface, which is the clearest demonstration of what an equality proof is, and it buys nothing once the change is taken as a major version with a migration behind it.

### The intrinsic bill

**6. Who owns `~` and `cast` for the intrinsic type formers?** The roster is closed and enumerated at `curios-core/src/intrinsic.rs:26` — `BoolType`, `NatType`, `IntType`, `FltType`, `BinType`, `ListType`, `HandleType`, `CellType`, `IoType`. Each needs an observational equality and cast computation rules. This is the largest concrete piece of work and it has **no analogue in any of the papers**, which face only Π, Ω, and declared inductives. Whether it is a table beside `Intrinsic::signature` — which is already the source of truth both checkers walk rather than restate — or per-type rules in each checker, is open, and the existing table is the obvious precedent to argue from. `NatType` and `ListType` are the two rows a teaching corpus leans on, so they are the two whose answers are wanted first even though the roster is decided as one.

**7. What is `~` at `Flt`?** [`Flt` is specified by a model, and the runtime conforms to it](../design/language/flt-is-specified-by-a-model-and-the-runtime-conforms.md) makes term identity value identity — bitwise, with exactly one NaN — and folds every operation, so `Flt` is a carrier whose values have no structure to decompose, like `Nat`. Whether `~` at such a carrier is the primitive identity with no structural clauses, or whether it should be stated through `to_le_bytes` so that it reduces to `~` at `Bytes`, is open.

**8. Does `Io` need a `~` at all?** [Effects are descriptions and the carrier has no eliminator](../design/language/effects-are-descriptions-and-the-carrier-has-no-eliminator.md): there is deliberately no eliminator from `Io(T)` to `T` and there never may be. An observational equality at `Io` would have to say what it means for two descriptions to be equal without observing either, and the answer may legitimately be that the carrier has no `~` — but that is a decision with a reason, not an omission.

### Indexed families

**9. Fording, or keep indices primitive?** CIC^obs elaborates an indexed family into a non-indexed one whose constructors carry equality constraints, and inserts casts in match branches. Curios has an entire apparatus built on indices being primitive — index inversion, coverage, the K-adjacent deletion rule ([Index inversion and K](../soundness/per-term-rules/index-inversion-and-k.md), `curios-analysis/tests/driven.rs`). These are two answers to one question and cannot both be the answer. Whether Cast-Refl can be taken *without* Fording is question 15, and the answer to that is what decides whether this question has to be asked at all.

**10. What breaks when indexed types gain non-canonical inhabitants?** Under Fording, a value cast along a non-convertible-but-equal index is a new normal form: `Vec A n` acquires inhabitants that are neither `vnil` nor `vcons`. The pattern-matrix compiler, `curios-ersd`'s optimizer, and `curios`' codegen ladder all currently assume canonical constructor forms. Nothing has been surveyed for this. It is also the only change in this file that could fail *quietly* — every other one is refused by a checker, while a non-canonical value reaching a match that assumed canonicity is a miscompilation, which is what makes the survey worth doing before the decision rather than after it.

### Erasure and runtime

**11. Does `cast` erase to the identity, and under what obligation?** In CIC^obs, `cast` on a list *rebuilds* the list. If two observationally equal types have identical erased representations then the cast should erase away, but that is an argument someone must make and that `curios-elab/src/into_ersd.rs`'s erasure obligations must carry. This is the one place where an observational equality could show up as *runtime* cost rather than checker cost, which for this project is not a footnote.

**12. Do the totality obligations change shape?** [Totality of the erased program](../design/language/totality-of-the-erased-program.md) and (T)/(V) rest on every `Prop`-typed term being total, and [Definitional proof irrelevance](../soundness/per-term-rules/definitional-proof-irrelevance.md) rests on that in turn as a fixpoint rather than a circle. Whether an `Ω`-valued `~` and a proof-relevant `cast` preserve that argument unchanged, or move where the seeds are taken, is unexamined.

### The two-checker seam

**13. Do both checkers get `~` and `cast`, written separately?** [An independent kernel re-checks what the elaborator accepts](../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) says the rules are written twice on purpose. Conversion is already the rule that decision singles out as *not* held to a differential, and the first goal-level differential written for it found a real disagreement. Adding a type-directed cast to both copies raises the stake on that missing differential rather than lowering it.

**14. What becomes of the kernel's irrelevance rule?** It is presently *inert* — no conversion goal in `curios-cert` ever arrives at a `Prop`-sorted type, because proofs reach conversion in untyped child positions compared at `Type`. Under an observational equality the population of `Ω`-typed goals changes, and whether the rule stays inert, starts firing, or becomes load-bearing in the checker where it currently does nothing is a question the [Definitional proof irrelevance](../soundness/per-term-rules/definitional-proof-irrelevance.md) entry would have to be re-graded against.

### Increments

**15. Is Cast-Refl without Fording a standalone first step?** Adding the conversion rule that makes transport compute on convertible endpoints would close the incompleteness named above without touching index inversion or minting new normal forms. It is the cheapest increment with visible value, and it is also the one that walks toward Werner's rule — so whether it can be taken in the *conversion* direction (checked after reduction, with a convertibility precondition, as CIC^obs does) rather than the *reduction* direction (which is what Abel–Coquand refutes) is the thing to establish before believing it is cheap. Answered when the rule is stated with its precondition and a fixture shows the Abel–Coquand term still stuck under it.

**16. What is the smallest change that makes function extensionality true in `/std`?** Not the largest — the smallest. If a single `~` clause at Π plus a cast rule buys `funext` for a library that presently has none, that is a self-contained deliverable independent of everything above, and it should be priced as one. Question 22 is the same question asked about usability rather than truth, and the two answers are not the same answer.

### Evidence and metatheory

**17. Does an observational layer need the missing model first, or supply it?** [Definitional proof irrelevance](../soundness/per-term-rules/definitional-proof-irrelevance.md) states that the rule is *argued* and not modelled, and that the model needs `Prop` to be a genuine subsingleton with only total terms interpreted. CC^obs proves normalization for its impredicative irrelevant universe in plain MLTT. Whether that result can be *inherited* here — and what the gap is between its system and this one — is the first question a metatheory effort would ask, and answering it may be worth more than the implementation.

**18. What fixture pins that Curios lacks Werner's rule today?** The reading above is unprobed. A fixture that constructs Abel–Coquand's `Ω` in surface Curios and observes it stick rather than diverge would convert an audit into evidence, and would fail loudly if a future reduction change quietly introduced the rule. This is worth writing whether or not anything else here is ever picked up.

### The downstream surface

The migration questions above are about source. These are about what the change costs a program that already exists somewhere else, and they are why that cost is not constant in time.

**19. The break is a flag day, and the resolver is what decides that.** `curios-package/src/graph.rs:5` states the law: a name resolves to one place or the resolution is refused. That is one version per canonical name, graph-wide — stricter than Cargo, which admits semver-incompatible duplicates into one graph, and than Go's minimal version selection, which picks a winner. Beside it, the orphan rule and the program-wide one-witness-per-concept-key rule forbid a compatibility layer: a package cannot satisfy a concept for a type it does not own, so nothing can bridge two spellings of `Eq`. There is therefore no equivalent of Python's `six`, no `__future__` import, and no edition — an edition would need two kernels disagreeing about definitional equality, which is two languages sharing a build system. A dependency graph holds one version of the language and ports whole.

This is **read from source and not probed**; no conflicting graph has been constructed and watched refuse. What turns on it is timing rather than mechanism: the cost of the change is one coordinated port per package in the ecosystem, so it rises monotonically with adoption and is least today. Answered when a fixture builds a graph whose two dependents pin one name differently and asserts the refusal names both.

**20. Does a package say which language it is written in?** `curios.toml` carries `name`, `description`, `default`, `dependencies`, `executables`, `members` and `catalog`, and nothing else (`curios-package/src/manifest.rs:175`). Absence of a marker can serve as the marker for the older language, but a bare `.crs` file has no manifest at all and so carries no signal, and `#[serde(deny_unknown_fields)]` on line 174 means a field added later is refused by every compiler predating it rather than ignored. A package self-identifies only if the field exists before the change and `curios new` writes it. This is answered by a decision rather than by evidence: either the field is added and scaffolded, or absence is documented as the marker and the bare-file case is accepted as unsignposted.

**21. What does a migration tool guarantee?** A rewriter over a language with a kernel can offer what a source-to-source translator cannot: rewrite, then certify, so its contract is that every file it touched still certifies and here are the ones it refused, by location. The eleven sites in question 3 are the shape it must handle, and its refusals are the definition of the manual surface. Answered when the contract is stated as a property the tool checks rather than as a description of what it attempts.

### What a user writes

None of the questions above is about the experience of writing a proof, and the objective the campaign is ultimately for — extensional reasoning that is pleasant rather than merely available — lives entirely here. Whether these belong in a document of their own is itself the first thing to decide; that they currently have no home is why they are recorded in this one.

**22. What is the smallest program that proves two functions equal, and does the compiler help write it?** Question 16 asks what makes function extensionality *true*. This asks what makes it *usable*, and they are different questions with different answers: an equality at Π that unfolds pointwise makes `funext` derivable while still leaving a user to write pointwise proofs with explicit motives by hand. The goal oracle already answers well at an equality goal — at `Eq(List/len(xs), List/len(ys))` under `p: Eq(xs, ys)` it offers `Eq/cong(?, p)` and `Eq/subst(x => Eq(List/len(xs), x), ?, ?)` with the motive synthesized, which is the argument that is worst to write by hand elsewhere. Whether it offers the pointwise decomposition at an `Eq(f, g)` goal once `~` unfolds there is unexamined, and it is the cheapest ergonomic win the change could carry.

**23. What does a refused equality say?** [A refusal is a panic the emitter renders](../design/toolchain/a-refusal-is-a-panic-the-emitter-renders.md) is the standard the runtime is held to, and the elaborator's own diagnostics meet it — an undischarged instance argument names the obligation and how to supply it. An `Eq(f, g)` that will not close under a pointwise `~` should name the argument the two sides differ at rather than report that two types failed to convert. No question above would produce that, because none of them is about diagnostics.

**24. Is rewriting sugar in scope, and is it worth more than the theory?** There is no `rewrite`, no tactic and no equational block in [syntax.md](../syntax.md), and an observational equality adds none: transport ergonomics — explicit motives, congruence at more than one argument, rewriting inside a hypothesis — is a surface problem that Agda and Coq both attack with syntax rather than with foundations. It is independent of everything above and cheaper than any of it, and for the stated objective it may buy more. Pricing it against question 16 is the comparison that decides whether this campaign is the shortest route to its own purpose.

## Deliberately not decided here

Nothing: [Definitional proof irrelevance](../soundness/per-term-rules/definitional-proof-irrelevance.md)'s forward reference now links here.

## How to retake the counts

The size of the migration surface for question 3, over `/std` and `/syn`, taken **2026-09-09**. The first command counts uses of the function surface, and the second counts what question 3 actually turns on — the destructurings of `refl`, which are what has no function-surface equivalent.

```sh
grep -rho "Eq/refl\|Eq/sym\|Eq/trans\|Eq/cong\|Eq/subst\|Eq(" curios-prelude-archive/std curios-prelude-archive/syn | sort | uniq -c | sort -rn
grep -rl "Eq(" curios-prelude-archive/std curios-prelude-archive/syn | wc -l
grep -rn --include='*.crs' -E '\| *refl\(' curios-prelude-archive/std curios-prelude-archive/syn
```

The first two last printed 324 `Eq(` type spellings — which includes the declaration in `curios-prelude-archive/std/Eq.crs` itself — beside 113 `Eq/refl`, 111 `Eq/trans`, 67 `Eq/sym`, 20 `Eq/subst` and 13 `Eq/cong`, across 21 files. The third last printed sixteen lines: the constructor's own declaration, four destructurings inside `Eq.crs`, and the eleven outside it that question 3 prices.
