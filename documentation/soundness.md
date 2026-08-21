# The soundness perimeter

Every rule that can admit a term, what it assumes, and how far it has been checked. The *decision* to state the trusted base this way — and what it replaced — belongs to [The soundness perimeter](design/language/the-soundness-perimeter.md); this file is the evidence, and it is where a hunt records what it found.

Each entry names what it assumes and how far it has been checked, and the vocabulary is deliberate: **probed** means an adversarial program was written and the compiler rejected it; **argued** means a written justification exists; **auditable only** means no surface program can exercise the rule, so reading the implementation is the only available evidence. An entry may be probed and unargued, or argued and unprobed; they are independent.

An entry's evidence is the fixtures it names. A fixture is named rather than counted because a name is a contract — moving it is a change someone must make deliberately — while a count is true on the day it is taken and quietly false afterwards. Where this file used to carry population figures, retake them from the probes beside the code: `curios-prelude-archive`'s `kernel_disagreements` for the whole-prelude walk, and each crate's own test module for the rest. A defect's history is kept only where the defect is still expressible; where the mechanism that admitted it is gone, git holds the account.

The perimeter has four parts. Two are halves of the same thing and differ only in how coverage is obtained; the third is different in kind, and is a rule about *not* running the other two; the fourth is not a rule at all. The **whole-module passes** run once over a module: the elaborator's from `finalize_and_check` in `curios-elab`, the single site every entry point that produces an elaborated module comes through, which is what keeps a check from reaching one configuration and not the other. The **per-term rules** run during elaboration, so their coverage follows from the elaborator visiting every term rather than from a pass enumerating positions. **Admission without judgment** is the rule about *not* running the other two: a verdict already reached, under an address carrying what it was reached about, may be believed instead of re-decided. **What the kernel consults** holds the components a rule reads an answer *out of* rather than deriving — a remembered reduct, an assumed equation, a classification recorded earlier, a verdict carried from a walk already run. None of them decides whether a term is well typed; each of them supplies something a rule then believes, so a wrong answer there is admitted by whichever rule asked.

An entry earns its place by *assuming* something, not merely by admitting. The discriminator is whether the rule rests on anything beyond the shape of the term in front of it — a declaration, a carried number, a cached verdict, an equation stated elsewhere. A rule that only destructures inherits its correctness from the representation and is not listed; `Var`, `Let`, the bare `Func` rule and the `Type`/`Prop` head rules are the ones that fall the other side of that line, and they inherit [Telescope instantiation](soundness/per-term-rules/telescope-instantiation.md) and [Binder identity](soundness/per-term-rules/binder-identity.md) rather than earning rows.

Three of the four things that criterion names — a cached verdict, an equation stated elsewhere, a carried number — are *components* rather than facts, and a component earns its own entry instead of being re-argued at each rule that reads it. That is what the fourth part below is for, and adding it is what the criterion had always implied without anyone reading it that way: the table enumerated the judgments and left the things the judgments believe to be argued in the source beside them.

One entry per file, under [soundness/](soundness), in the four directories the parts above name. Listing a directory is how you find an entry, and each entry states its own **Assumes** and **Status** — what a rule rests on, and how far that has been checked, are the entry's to say and are written once. The index below carries the grades alone, so the perimeter can be read at a glance without a second copy of anything. [Across the perimeter](soundness/across-the-perimeter.md) is what the entries say together rather than one of them.

Cite an entry by its path, so a moved or renamed one fails loudly instead of leaving a quoted title that no longer exists.

## Index

Grades only. Each entry's evidence is the entry it links to.

| Whole-module pass | Grade |
| --- | --- |
| [`zonk_module`](soundness/whole-module-passes/zonk_module.md) | **probed** |
| [`validate_universes` (inside `zonk_module`)](soundness/whole-module-passes/validate_universes-inside-zonk_module.md) | **probed** |
| [`check_induct_decl` / `check_struct_decl`](soundness/whole-module-passes/check_induct_decl-check_struct_decl.md) | **probed** |
| [`check_positivity`](soundness/whole-module-passes/check_positivity.md) | **probed** |
| [`record_totality` + (T)](soundness/whole-module-passes/record_totality-t.md) | **probed** |
| [(V)](soundness/whole-module-passes/v.md) | **probed** |

| Per-term rule | Grade |
| --- | --- |
| [Coverage](soundness/per-term-rules/coverage.md) | **probed** |
| [Large-elimination guard](soundness/per-term-rules/large-elimination-guard.md) | **probed** |
| [Sort formation](soundness/per-term-rules/sort-formation.md) | **argued** and **probed** |
| [Subsumption and level entailment](soundness/per-term-rules/subsumption-and-level-entailment.md) | **probed** at the oracle, auditable only from source |
| [`Prop` non-informativeness](soundness/per-term-rules/prop-non-informativeness.md) | **probed** |
| [Witness coherence and the orphan rule](soundness/per-term-rules/witness-coherence-and-the-orphan-rule.md) | **probed** |
| [Foreign wire contract](soundness/per-term-rules/foreign-wire-contract.md) | **probed** |
| [Definitional proof irrelevance](soundness/per-term-rules/definitional-proof-irrelevance.md) | **argued** |
| [Index inversion and K](soundness/per-term-rules/index-inversion-and-k.md) | auditable only from the surface, **probed** by construction at the deletion rule and the clash rule's license |
| [Conversion recurrence](soundness/per-term-rules/conversion-recurrence.md) | **argued** |
| [Eta and untyped child positions](soundness/per-term-rules/eta-and-untyped-child-positions.md) | **argued** and **probed**, the grounded scope's stand-in included; **argued** for the caller-side fence beneath it |
| [Elimination carrier agreement](soundness/per-term-rules/elimination-carrier-agreement.md) | **probed** |
| [Nominal occurrence well-formedness](soundness/per-term-rules/nominal-occurrence-well-formedness.md) | **probed** |
| [Type former well-formedness](soundness/per-term-rules/type-former-well-formedness.md) | **probed** |
| [Scrutinee refinement](soundness/per-term-rules/scrutinee-refinement.md) | **probed** |
| [Intrinsic signatures](soundness/per-term-rules/intrinsic-signatures.md) | auditable only |
| [Intrinsic fold laws and the free-monoid peel](soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md) | **probed** at the `Nat` peel, the `Bin`/`List` peel verdicts and open fold laws over values, the division family and its bounds oracle; **argued** for the bit-grain twins of the byte-grain arms |
| [The closed machine](soundness/per-term-rules/the-closed-machine.md) | **probed** — behavioral battery, kernel differential at both demands, whole-prelude certification; one memo-keying divergence found and closed |
| [Telescope instantiation](soundness/per-term-rules/telescope-instantiation.md) | **probed** incidentally, never directly |
| [Checked rules at deferred child positions](soundness/per-term-rules/checked-rules-at-deferred-child-positions.md) | **argued** |
| [Binder identity](soundness/per-term-rules/binder-identity.md) | **argued** |
| [A type is a pure term](soundness/per-term-rules/a-type-is-a-pure-term.md) | **argued** |
| [Universe instance discharge](soundness/per-term-rules/universe-instance-discharge.md) | **probed** |
| [Driver-supplied convertibility](soundness/per-term-rules/driver-supplied-convertibility.md) | **argued** |

| Admission without judgment | Grade |
| --- | --- |
| [Cached verdicts](soundness/admission-without-judgment/cached-verdicts.md) | **argued** |
| [Reused payloads](soundness/admission-without-judgment/reused-payloads.md) | **argued**, inheriting the entry above; **probed** at every clause of the record |
| [Judging only what is not in scope](soundness/admission-without-judgment/judging-only-what-is-not-in-scope.md) | **argued** at the premise, discharged in another crate; **probed** at the consequence |

| What the kernel consults | Grade |
| --- | --- |
| [The evaluation memo](soundness/what-the-kernel-consults/the-evaluation-memo.md) | **argued**, and **probed** at the case-equation interlock |
| [Case equations inside an arm](soundness/what-the-kernel-consults/case-equations-inside-an-arm.md) | **argued**, and **probed** at both consultation points and at each half of the two-tier key |
| [The refinement key](soundness/what-the-kernel-consults/the-refinement-key.md) | **probed** — its premise was false and the kernel certified a coercion between distinct types; closed, the elaborator's conversion copy since fell to the goal-level differential and is closed, and its store copy is open |
| [Recorded positions and sort-hood](soundness/what-the-kernel-consults/recorded-positions-and-sort-hood.md) | **argued** |
| [The carried totality verdicts](soundness/what-the-kernel-consults/the-carried-totality-verdicts.md) | **probed** |

