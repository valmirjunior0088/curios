# Equality is defined at the type, not assumed over all of them

## Status

Deliberately unrefined. What is settled is the *target theory* and the audit that settled it, both recorded below; the design space itself is untouched, and none of the questions in the last section has been answered. Nothing is started.

This would touch the trusted base at its most consequential rule — conversion — and it would change what `/std/Eq` means for every proof already written against it. It wants review, and answers to the questions below, before it wants code.

## Why it exists

[Across the perimeter](../soundness/across-the-perimeter.md) already names the destination without linking anywhere: *"What is still missing is the model, not the reasoning … That is the first properly metatheoretic work this project would undertake, and it is what an observational-equality layer would rest on directly."* This specification is the location that sentence was pointing at.

Three separate things point the same way.

- **Function extensionality is not derivable and is not present.** `/std` contains no `funext` and no axiom standing in for one. Postulating it would not help: transport along an axiom is a stuck term forever, which for a language that erases and emits WebAssembly is worse than not having it.
- **Two closed inhabitants of `False` have already been produced by the same interaction, and both are recorded.** A `Prop`-sorted `Box` carrying a `Type` payload, where irrelevance identifies `mk(A)` with `mk(B)`, congruence through `unbox` carries that to `Eq(Type 0, A, B)` for *any* two types, and transport turns `()` into a proof of `False` (`curios-cert/src/recheck/tests.rs:129`); and a constructor whose index target mentions its payload through a non-injective function, which mentions it without determining it ([Large-elimination guard](../soundness/per-term-rules/large-elimination-guard.md)). Both are closed. Both routed through *definitional irrelevance × an inductive `Eq` × large elimination into `Type`*, which is the combination an observational equality does not have.
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

- **The sort structure is already CC^obs's.** A cumulative predicative `Type` hierarchy with algebraic inferred levels, beside a *separate impredicative* `Prop` with definitional proof irrelevance — `curios-core/src/universe.rs:3`, `curios-cert/src/kernel/module.rs:403`, and [Strict Prop under Type](../design/language/strict-prop-under-type.md). That is `(U_i, Ω)`, and the impredicativity is exactly what makes TT^obs the wrong target and CC^obs the right one.
- **The conversion apparatus OTT needs is in place.** Conversion is type-directed and carries the type at every goal; η fires at Π and at Σ; irrelevance is discharged at a `Prop`-sorted goal before either side is examined — `curios-cert/src/kernel/convert.rs:3` and `curios-elab/src/convert.rs:2064`. An inductive type-former's arguments are already compared at the declaration's own index telescope, which is what lets `Eq(@P, p, q)` at a `Prop`-sorted `P` convert with `Eq(@P, p, p)` (`curios-cert/src/kernel/convert.rs:19`).
- **The piece an observational equality replaces is `curios-prelude-archive/std/Eq.crs`**: a `Prop`-valued inductive family with `refl`, and `sym`/`trans`/`cong`/`subst` derived by matching on it. Its large elimination into `Type` is admitted by the non-informativeness excuse in the [Large-elimination guard](../soundness/per-term-rules/large-elimination-guard.md), and that guard is what both recorded defects were found in.
- **Erasure already deletes `Prop` wholesale**, and the argument is irrelevance itself — [Strict Prop under Type](../design/language/strict-prop-under-type.md). An `Ω`-valued observational equality erases by the same argument; what does *not* obviously erase is `cast`, which is question 11.
- **Quotients do not exist**, and neither does any surface notion of a type carrying a chosen equality.

## What is read from source, and not probed

**The claim: Curios does not have Werner's rule, so the Abel–Coquand divergence cannot fire today.** Irrelevance in both checkers is a *conversion* rule at a `Prop`-sorted goal (`curios-elab/src/convert.rs:2064`, `curios-cert/src/kernel/convert.rs:117`); it never rewrites a scrutinee, and reduction of a match on `Eq` still demands a literal `refl` constructor. The neutral `h ⊤ A` in the counterexample therefore stays stuck rather than being identified with `refl` and reduced through.

This was read from the source and **not** probed. Under this file's own vocabulary it is *auditable only*, and it is recorded here rather than under "what Curios already has" because it has not earned that place. Question 18 is the standing debt it creates.

**What that safety costs, if the reading is right.** `subst(P, p, v)` does not reduce to `v` when `p` is neutral, even where the endpoints are convertible. That is the cast-on-refl gap verbatim — the thing CIC^obs's Cast-Refl rule exists to close — so the present position is: on the safe side of the cliff, paying the exact incompleteness the 2025 result was written to remove.

## The questions to answer

None of these is answered. Each records what constrains it and what turns on it, so that picking this up is a matter of deciding rather than re-deriving.

### Theory target

**1. CC^obs alone, or all the way to CIC^obs?** The sort structure is CC^obs's, but Fording, Cast-Refl and quotients are CIC^obs's, and indexed inductive families are a landed capability here. The honest reading is that CC^obs is the *foundation* and CIC^obs is the *coverage*, so the question is which increments in which order, not which paper.

**2. Does `Prop` stay impredicative?** CC^obs says it may, and that is the whole content of the POPL 2023 result. Making it predicative instead would target the smaller and older theory, but would break whatever impredicative encodings `/std` rests on — which has not been inventoried. Record this as decided by evidence, not assumed.

### The migration surface

**3. Can `/std/Eq` remain the surface while the underlying notion changes?** Whether `refl`/`sym`/`trans`/`cong`/`subst` can keep their spellings and types over an observational `~` is the single question that decides whether this is a campaign or a rewrite of every proof in the library. The counts at the foot of this file are what is at stake.

**4. Does `~` get a surface spelling at all, or stay compiler-internal?** [Syntax forms are closed](../design/language/syntax-forms-are-closed-semantics-extend-by-witness.md): a user-visible `~` is not a new operator but an existing form opted into by a `/syn` concept, and `/syn` ownership is `curios-prelude-archive/README.md`'s to state. The alternative is that `~` is never written by a user and only `/std/Eq` is.

**5. What becomes of the existing `Eq` inductive?** Deleted, or kept as a nominal type related to the observational one. CIC^obs keeps CIC's `Id` and gives it observational content, which is what backward compatibility with existing Coq developments demanded; whether the same compatibility is worth anything here depends on question 3.

### The intrinsic bill

**6. Who owns `~` and `cast` for the intrinsic type formers?** The roster is closed and enumerated at `curios-core/src/intrinsic.rs:26` — `BoolType`, `NatType`, `IntType`, `FltType`, `BinType`, `ListType`, `HandleType`, `CellType`, `IoType`. Each needs an observational equality and cast computation rules. This is the largest concrete piece of work and it has **no analogue in any of the papers**, which face only Π, Ω, and declared inductives. Whether it is a table beside `Intrinsic::signature` — which is already the source of truth both checkers walk rather than restate — or per-type rules in each checker, is open, and the existing table is the obvious precedent to argue from.

**7. What is `~` at `Flt`?** [`Flt` is specified by a model, and the runtime conforms to it](../design/language/flt-is-specified-by-a-model-and-the-runtime-conforms.md) makes term identity value identity — bitwise, with exactly one NaN — and folds every operation, so `Flt` is a carrier whose values have no structure to decompose, like `Nat`. Whether `~` at such a carrier is the primitive identity with no structural clauses, or whether it should be stated through `to_le_bytes` so that it reduces to `~` at `Bytes`, is open.

**8. Does `Io` need a `~` at all?** [Effects are descriptions and the carrier has no eliminator](../design/language/effects-are-descriptions-and-the-carrier-has-no-eliminator.md): there is deliberately no eliminator from `Io(T)` to `T` and there never may be. An observational equality at `Io` would have to say what it means for two descriptions to be equal without observing either, and the answer may legitimately be that the carrier has no `~` — but that is a decision with a reason, not an omission.

### Indexed families

**9. Fording, or keep indices primitive?** CIC^obs elaborates an indexed family into a non-indexed one whose constructors carry equality constraints, and inserts casts in match branches. Curios has an entire apparatus built on indices being primitive — index inversion, coverage, the K-adjacent deletion rule ([Index inversion and K](../soundness/per-term-rules/index-inversion-and-k.md), `curios-analysis/tests/driven.rs`). These are two answers to one question and cannot both be the answer. Whether Cast-Refl can be taken *without* Fording is question 15.

**10. What breaks when indexed types gain non-canonical inhabitants?** Under Fording, a value cast along a non-convertible-but-equal index is a new normal form: `Vec A n` acquires inhabitants that are neither `vnil` nor `vcons`. The pattern-matrix compiler, `curios-ersd`'s optimizer, and `curios`' codegen ladder all currently assume canonical constructor forms. Nothing has been surveyed for this.

### Erasure and runtime

**11. Does `cast` erase to the identity, and under what obligation?** In CIC^obs, `cast` on a list *rebuilds* the list. If two observationally equal types have identical erased representations then the cast should erase away, but that is an argument someone must make and that `curios-elab/src/into_ersd.rs`'s erasure obligations must carry. This is the one place where an observational equality could show up as *runtime* cost rather than checker cost, which for this project is not a footnote.

**12. Do the totality obligations change shape?** [Totality of the erased program](../design/language/totality-of-the-erased-program.md) and (T)/(V) rest on every `Prop`-typed term being total, and [Definitional proof irrelevance](../soundness/per-term-rules/definitional-proof-irrelevance.md) rests on that in turn as a fixpoint rather than a circle. Whether an `Ω`-valued `~` and a proof-relevant `cast` preserve that argument unchanged, or move where the seeds are taken, is unexamined.

### The two-checker seam

**13. Do both checkers get `~` and `cast`, written separately?** [An independent kernel re-checks what the elaborator accepts](../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) says the rules are written twice on purpose. Conversion is already the rule [Across the perimeter](../soundness/across-the-perimeter.md) singles out as *not* held to a differential, and the first goal-level differential written for it found a real disagreement. Adding a type-directed cast to both copies raises the stake on that missing differential rather than lowering it.

**14. What becomes of the kernel's irrelevance rule?** It is presently *inert* — no conversion goal in `curios-cert` ever arrives at a `Prop`-sorted type, because proofs reach conversion in untyped child positions compared at `Type`. Under an observational equality the population of `Ω`-typed goals changes, and whether the rule stays inert, starts firing, or becomes load-bearing in the checker where it currently does nothing is a question the [Definitional proof irrelevance](../soundness/per-term-rules/definitional-proof-irrelevance.md) entry would have to be re-graded against.

### Increments

**15. Is Cast-Refl without Fording a standalone first step?** Adding the conversion rule that makes transport compute on convertible endpoints would close the incompleteness named above without touching index inversion or minting new normal forms. It is the cheapest increment with visible value, and it is also the one that walks toward Werner's rule — so whether it can be taken in the *conversion* direction (checked after reduction, with a convertibility precondition, as CIC^obs does) rather than the *reduction* direction (which is what Abel–Coquand refutes) is the thing to establish before believing it is cheap.

**16. What is the smallest change that makes function extensionality true in `/std`?** Not the largest — the smallest. If a single `~` clause at Π plus a cast rule buys `funext` for a library that presently has none, that is a self-contained deliverable independent of everything above, and it should be priced as one.

### Evidence and metatheory

**17. Does an observational layer need the missing model first, or supply it?** [Across the perimeter](../soundness/across-the-perimeter.md) states that definitional proof irrelevance is *argued* and not modelled, and that the model needs `Prop` to be a genuine subsingleton with only total terms interpreted. CC^obs proves normalization for its impredicative irrelevant universe in plain MLTT. Whether that result can be *inherited* here — and what the gap is between its system and this one — is the first question a metatheory effort would ask, and answering it may be worth more than the implementation.

**18. What fixture pins that Curios lacks Werner's rule today?** The reading above is unprobed. A fixture that constructs Abel–Coquand's `Ω` in surface Curios and observes it stick rather than diverge would convert an audit into evidence, and would fail loudly if a future reduction change quietly introduced the rule. This is worth writing whether or not anything else here is ever picked up.

## Deliberately not decided here

Whether [Across the perimeter](../soundness/across-the-perimeter.md)'s forward reference should be turned into a link to this file. It presently names an observational-equality layer and points nowhere, which is the decay the citation rule exists to prevent — but editing a soundness entry is a separate change from filing this one.

## How to retake the counts

The size of the migration surface for question 3, over `/std` and `/syn`, taken **2026-08-21**:

```sh
grep -rho "Eq/refl\|Eq/sym\|Eq/trans\|Eq/cong\|Eq/subst\|Eq(" curios-prelude-archive/std curios-prelude-archive/syn | sort | uniq -c | sort -rn
grep -rl "Eq(" curios-prelude-archive/std curios-prelude-archive/syn | wc -l
```

It last printed 321 `Eq(` type spellings — which includes the declaration in `curios-prelude-archive/std/Eq.crs` itself — beside 139 `Eq/trans`, 109 `Eq/refl`, 86 `Eq/sym`, 21 `Eq/subst` and 16 `Eq/cong`, across 21 files.
