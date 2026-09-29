# The carriers' algebra stays in conversion

**Decision.** Conversion decides the carriers' algebra. The laws `curios-algebra` states for `Nat`, `Int`, `Bool`, the words and the conversions between them are definitional equalities, and both checkers take them. The context's hypotheses stay out of them. The laws are not supplied as casts along propositional equalities over a syntactic conversion. Three places hold what the theory is:

- the law table in `curios-algebra` holds its laws, each as a generated row of `curios`'s `tests::laws`;
- `curios-core`'s `linear` module holds what conversion decides of a comparison;
- `tests::laws::audit` holds the conditions a theory in conversion must meet.

**Rationale.**

- **A decided bound discharges by reduction** because the comparison it reflects reduces ([A bound is stated in a decided proposition and discharged by reduction](a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)). With the laws as casts, a bound over `x + 0` or `len(a ++ b)` would no longer reduce to truth, and would need a cast written wherever it is used.
- **Casts would leave stuck terms.** A cast along a hypothetical proof stays stuck until the proof is matched on ([`Prop` is strict, proof-irrelevant, and definitionally K](prop-is-strict-proof-irrelevant-and-definitionally-k.md)). Casts would therefore leave stuck terms in every type a law reaches, and every reduction behind one would stop there.
- **Checked proofs stay sums of facts.** A proof the elaborator writes for a bound is checked by conversion's cancellation, as [algebra part 2](../../roadmap/algebra/02-bounds-from-facts-spec.md)'s will be. That keeps each proof a sum of facts rather than a chain of lemmas restating the laws.

This is Coq Modulo Theory's premise: a decidable theory in conversion, with the context's hypotheses kept out of it (Strub, 2010). Its metatheory with strong elimination is Jouannaud and Strub's (2017). The conditions it needs are these:

- conversion stays symmetric;
- conversion stays transitive;
- conversion stays closed under substitution with the theory in it;
- constructors stay free modulo the theory.

`tests::laws::audit` holds each condition at every declared law, through both checkers, and freeness at every intrinsic case inversion distinguishes. That is evidence about the implemented fragment, not a metatheorem.

**What it costs.**

- **The theory is trusted code shared by both checkers.** [An independent kernel re-checks what the elaborator accepts](an-independent-kernel-re-checks-what-the-elaborator-accepts.md) records why a shared rule costs the second opinion and why the algebra is shared anyway.
- **Every new law must be decided alike by both checkers.** So a law enters through a declaration and the rows generated from it, never through an arm one checker holds ([A law is decided where it neither respells nor invents](../toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md)).
- **A wrong law is unsound everywhere.** It admits by [the perimeter's shared route](the-soundness-perimeter.md), and a wrong disequality additionally excuses an arm at inversion.

**Rejected — the laws as casts.** Conversion would be kept syntactic: β, δ, ι, η, proof irrelevance and the closed folds. Each carrier law would become a `/std` lemma a program transports along. The trusted theory would shrink to what evaluation decides of closed values. The three consequences above are what that buys:

- every bound over an open term would be written by hand;
- every law a type reaches would leave a stuck term;
- every proof part 2 writes would restate the laws it relies on.

The theory is small, decidable and held by generated evidence, so the trust it costs is bounded. The cost of casts falls on every program that states a bound.

**Not this decision's.**

- **Hypotheses in conversion.** They are the Calculus of Congruent Inductive Constructions' design. [A bound is stated in a decided proposition and discharged by reduction](a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md) records why they stay out.
- **Relational facts decided in conversion from checked evidence.** These are [algebra part 4](../../roadmap/algebra/04-relational-layer-spec.md)'s, not refined yet.
