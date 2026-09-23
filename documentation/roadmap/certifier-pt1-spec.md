# Certifier, part 1: independent certification records and call-site discovery

Working specification for replacing carried elaborator totality stamps with the certifier's own verdict record, recording call sites during its own typing walk, and profiling its judgments. These are independently verifiable changes to where certification gets its authority. The stronger restrictions on trusted reasoning and the evidence architecture belong to [part 2](certifier-pt2-spec.md), whose design still needs refinement.

## What this builds on

- **The crate boundary.** `curios-cert` reaches nothing of `curios-elab`, so it cannot consult a metavariable store, a refinement layer or a parked goal ([An independent kernel re-checks what the elaborator accepts](../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), and [the certifier README](../../curios-cert/README.md)).
- **The kernel's own walk.** Typing, conversion, level entailment and the erased positions recorded by `kernel/positions.rs` already belong to the kernel; those positions seed obligations (T) and (V).
- **Stored certification.** [Cached verdicts](../soundness/admission-without-judgment/cached-verdicts.md) provides the identity and reuse discipline the new verdict record must follow.
- **Evidence.** The [soundness perimeter](../design/language/the-soundness-perimeter.md), two-checker fixtures in `curios/src/tests/perimeter.rs`, and existing memo and evaluator differentials provide the validation framework.

## The gap

The totality stamps written by the elaborator's `record_totality` are read for items already in scope through the non-total set in `kernel/globals.rs` and the carried branch of `obligation.rs`. [The carried totality verdicts](../soundness/what-the-kernel-consults/the-carried-totality-verdicts.md) records the unchecked input this delivery removes.

Both checkers currently call `curios_analysis::group_totality`; the drivers differ, but the kernel does not discover call sites through its own typing walk. The shared discovery's fail-open cases are the reason to derive those sites where the kernel checks calls, while retaining shared graph closure.

The certifier's existing profile does not distinguish enough of its judgments to justify decisions about which shared implementation should be replaced. Profiling here supplies evidence for later decisions; it does not select a new evaluator.

## Contracts

**The certifier files its own verdicts.** Each certified unit carries a record of what the certifier certified and how it classified each definition's totality, stored with the unit and keyed by the certifier's identity. Later certification reads this record, never an elaborator stamp. An elaborator annotation cannot substitute for missing certification evidence.

**Call sites come from the certifier's typing walk.** Record the calls the kernel checks, as `kernel/positions.rs` records erased positions. The size-change graph closure remains shared; the source of its call sites is the kernel's walk. Define and test how groups and calls encountered by the walk reach that closure.

**Derivation and reuse retain their owners.** The kernel derives classifications; the stored-verdict machinery owns their identity and reuse. Preserve the kernel's independence from elaboration and its normal dependency boundary. Elaborator metadata such as polarity vectors and binder floors continues to be independently derived where it is already re-derived.

**Each changed claim is corrected when it becomes true.** The verdict record and call-site migration update their own perimeter entries and documentation in the same landing. Part 2's stronger trusted-code requirement is not claimed by these changes.

## Stages

Each lands alone, with focused verification and the repository's validation discipline.

1. **Profile the certifier.** Add spans for its judgments, report costs by stage, and establish a baseline for subsequent changes. This requires neither algebra part 2 nor the verdict-record migration.
2. **Replace carried verdicts.** Define the certifier-owned totality record, integrate its storage and reuse, migrate later walks, and remove the carried elaborator-stamp path. Hold record identity and reuse to the cached-verdict contract.
3. **Record call sites during typing.** Feed the existing graph closure from the kernel's own walk and remove its reliance on shared call-site discovery. Keep the elaborator's responsibilities separate.

The latter two changes need no new arithmetic search or certificate language. Each establishes its own acceptance evidence; part 2 does not block completion.

## Verification

- Keep the two-checker fixtures and `kernel_disagreements` clean through the migration.
- File a false totality stamp through the elaborator and require the kernel to refuse what rests on it. Exercise later-unit consumption of the certifier record as well as certification of the original unit.
- Check storage and reuse under the certifier identity, including missing or inapplicable records, without accepting a fallback elaborator verdict.
- Compare recorded call sites with shared discovery over the corpus and investigate differences. A mutation hiding a call must be caught, so agreement is not the only evidence.
- Measure the certifier before and after each stage, naming the judgment or stage rather than attributing elaboration timings to certification.

## Documentation and rejected alternatives

The verdict-record and call-site contracts belong in the certifier README and rustdoc. Update [An independent kernel re-checks what the elaborator accepts](../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md)'s totality account when discovery moves, and correct the module documentation's elaborator dependency to describe its dev-dependency. Retire [The carried totality verdicts](../soundness/what-the-kernel-consults/the-carried-totality-verdicts.md) with the carried path, replacing it with the new record's assumptions and evidence. Update the cached-verdict account where record storage changes its claims.

The broader architecture's [rejected alternatives](certifier-pt2-spec.md#rejected), including changing the Core calculus to avoid inversion or termination checks, remain in part 2. This delivery adds no kernel IR and selects no replacement evaluator. Carry those references to permanent owners as either specification retires.

## Completion and retirement

- No totality verdict consumed by the certifier comes from an unchecked elaborator stamp; subsequent units use the certifier-owned record under the correct identity.
- Kernel call sites are derived during its typing walk, with shared graph closure and adversarial evidence against omitted calls.
- Profiling, focused tests and the implementation validation record are complete, and documentation states these implemented properties without claiming part 2's trusted-code restriction.

Transfer durable contracts, rationale, rejected alternatives and evidence to their permanent owners, replace the roadmap entry with a checked summary, and update part 2 and all other dependents to link to those owners. Verify that nothing references this filename, then delete it. Part 2's unfinished work does not block retirement.
