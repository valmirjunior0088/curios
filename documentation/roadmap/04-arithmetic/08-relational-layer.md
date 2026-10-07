# Relational facts decided in conversion, justified by checked evidence

**Not refined yet.** This specification reserves the relational layer: conversion deciding a Boolean combination of linear comparisons, or an equation whose truth needs a case split, with no hypothesis in scope — each verdict found outside the certifier and justified by evidence the certifier checks. It opens when a consumer needs such a fact *by conversion*, and until then it records the direction below and the rejection that constrains it. It is not an implementation plan.

## The trigger

A consumer states a fact of this kind that must hold by conversion, with the reason a proof does not serve it: `a <= b || b <= a` reducing to `true` where no proposition reaches, or `min(a, b) + max(a, b)` meeting `a + b` inside a type index. A bound over such a fact is not a trigger, since [the elaborator proves it from the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) and [the declared operations](02-declared-operations.md) decide the semilattice equations, a negation or a literal factor under `abs` and `sign`, and a sum in an exponent. The first example's disjunction is a lemma in [the numeric laws](03-numeric-laws.md), and no consumer needs it by conversion.

## The direction to preserve

- **Hypothesis-free.** Validity of a quantifier-free linear formula over atoms, with the context's hypotheses out of conversion. This is Coq Modulo Theory's fragment, whose metatheory with strong elimination is Jouannaud and Strub's (2017).
- **Search outside, checking inside.** The search is [the elaborator's](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), `curios-elab`'s `entailment`. A verdict carries a certificate keyed by the canonical linear view of the formula, which `curios-core`'s `linear` module publishes ([the carriers' algebra](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md)) and is checked by a certifier-grade checker in `curios-algebra`. The evidence travels with the module, and missing evidence is a refusal, never an acceptance (*The evidence's transport*, below).
- **Integer completeness stated.** Farkas certificates decide rational validity; a consequence that holds only over the integers needs cuts or the omega test's shadows, and the certificate language states which it admits. A proposed certificate family is not a complete integer decision procedure until it says so.
- **The audit extended.** Symmetry, transitivity and substitution with the layer in conversion, and freeness as inversion reads it. A relational clash becomes an impossibility inversion may use only where a certificate proves one; the truth table's disagreement over opaque atoms remains no counterexample.

## The evidence's transport

Reserved with the layer it serves, and refined with it when the trigger is met:

- **Evidence is keyed by what it is about.** A certificate is found under the canonical form of the formula it justifies, so the certifier's relation is a function of the question and never of where it was asked.
- **It travels with the module.** A unit carries a table of the evidence its verdicts rest on, stored and validated as the unit is, and a later walk reads it there.
- **Missing or failing evidence is a refusal**, never an acceptance, and a certificate is checked each time it is read.
- **The checker is certifier-grade.** It meets the six clauses of [the kernel's design decision](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), lives inside the certifier's closure, and is held against certificates corrupted one step at a time, each corruption refused. The search that found a certificate is not trusted.
- **The certifier's time is measured** before and after the layer opens, and reported with it.

## Rejected

- **Hypotheses in conversion.** [A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)'s rejections give the Calculus of Congruent Inductive Constructions' own account of why its metatheory holds for the weak recursor only. This layer stays hypothesis-free.
- **A trusted decision procedure in the kernel.** The certificate is checked; the search that found it is not trusted.

## Retirement

Refined into a working specification when its trigger is met, or deleted with its roadmap entry if a later decision rules the layer out.
