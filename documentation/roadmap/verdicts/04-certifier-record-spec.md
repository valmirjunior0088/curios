# Verdicts, part 4: the certifier files its own verdicts

Working specification for replacing the elaborator's carried totality stamps with the certifier's own verdict record, and for recording call sites — and every read of another item — during the certifier's own typing walk. These are independently verifiable changes to where certification gets its authority. The record is specified per declaration, because it is the verdict cell [part 5](05-one-environment-spec.md)'s environment holds; until part 5 lands it is filed with its unit.

It needs [the certifier's profile baseline](../../../curios-cert/README.md#measuring-the-certifier) and nothing else: no arithmetic search, and no certificate language. The stronger restrictions on trusted reasoning, and evidence, are [part 7](07-checked-evidence-spec.md)'s.

## What this builds on

- **The crate boundary.** `curios-cert` reaches nothing of `curios-elab`, so it cannot consult a metavariable store, a refinement layer or a parked goal ([An independent kernel re-checks what the elaborator accepts](../../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), and [the certifier README](../../../curios-cert/README.md)).
- **The kernel's own walk.** Typing, conversion, level entailment and the erased positions recorded by `kernel/positions.rs` already belong to the kernel; those positions seed obligations (T) and (V).
- **Stored certification.** [Cached verdicts](../../soundness/admission-without-judgment/cached-verdicts.md) provides the identity and reuse discipline the new verdict record must follow.
- **Evidence.** The [soundness perimeter](../../design/language/the-soundness-perimeter.md), the two-checker fixtures in `curios/src/tests/perimeter.rs`, and the existing memo and evaluator differentials provide the validation framework.

## The gap

**The totality stamps written by the elaborator's `record_totality` are read** for items already in scope through the non-total set in `kernel/globals.rs` and the carried branch of `obligation.rs`. [The carried totality verdicts](../../soundness/what-the-kernel-consults/the-carried-totality-verdicts.md) records the unchecked input this part removes.

**Both checkers call `curios_analysis::group_totality`**; the drivers differ, but the kernel does not discover call sites through its own typing walk. The shared discovery's fail-open cases are the reason to derive those sites where the kernel checks calls, while retaining the shared graph closure.

## Contracts

**The certifier files its own verdicts.** Each certified declaration has a record of what the certifier certified and how it classified the declaration's totality, keyed by the certifier's identity and filed with its unit. Later certification reads this record, never an elaborator stamp. An elaborator annotation cannot substitute for missing certification evidence. Part 5 reads the same record as the declaration's verdict cell.

**Call sites and reads come from the certifier's typing walk.** Record the calls the kernel checks, as `kernel/positions.rs` records erased positions, and with them each read of another item — its signature, or its body unfolded. The size-change graph closure remains shared; the source of its call sites is the kernel's walk. The reads are the kernel's half of the item graph part 5 records. Define and test how groups and calls encountered by the walk reach the closure.

**Derivation and reuse retain their owners.** The kernel derives classifications; the stored-verdict machinery owns their identity and reuse. Preserve the kernel's independence from elaboration and its normal dependency boundary. Elaborator metadata such as polarity vectors — and binder floors, until [part 3](03-no-minted-identity-spec.md) deletes them — continues to be independently derived where it is already re-derived.

**Each changed claim is corrected when it becomes true.** The verdict record and the call-site migration update their own perimeter entries and documentation in the same landing. Part 7's stronger trusted-code requirement is not claimed by these changes.

## Stages

Each lands alone, with focused verification and the repository's validation discipline.

1. **Replace carried verdicts.** Define the certifier-owned per-declaration record, integrate its storage and reuse, migrate later walks, and remove the carried elaborator-stamp path. Hold record identity and reuse to the cached-verdict contract.
2. **Record call sites and reads during typing.** Feed the existing graph closure from the kernel's own walk and remove its reliance on shared call-site discovery; record each read beside the calls. Keep the elaborator's responsibilities separate.

## Verification

- Keep the two-checker fixtures and `kernel_disagreements` clean through the migration.
- File a false totality stamp through the elaborator and require the kernel to refuse what rests on it. Exercise later-unit consumption of the certifier record as well as certification of the original unit.
- Check storage and reuse under the certifier identity, including missing or inapplicable records, without accepting a fallback elaborator verdict.
- Compare recorded call sites with shared discovery over the corpus and investigate differences. A mutation hiding a call must be caught, so agreement is not the only evidence.
- Measure the certifier before and after each stage against its [recorded baseline](../../../curios-cert/README.md#measuring-the-certifier), naming the judgment or stage.

## Documentation and rejected alternatives

The verdict-record and call-site contracts belong in the certifier README and rustdoc. Update [An independent kernel re-checks what the elaborator accepts](../../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md)'s totality account when discovery moves, and correct the module documentation's elaborator dependency to describe its dev-dependency. Retire [The carried totality verdicts](../../soundness/what-the-kernel-consults/the-carried-totality-verdicts.md) with the carried path, replacing it with the new record's assumptions and evidence. Update [cached verdicts](../../soundness/admission-without-judgment/cached-verdicts.md) for the record's identity and reuse; part 5 restates its per-item argument over recorded reads, and the two revisions are designed together here so that the second extends the first rather than replacing it.

The broader architecture's [rejected alternatives](07-checked-evidence-spec.md#rejected), including changing the Core calculus to avoid inversion or termination checks, remain in part 7. This part adds no kernel IR and selects no replacement evaluator.

## Completion and retirement

- No totality verdict consumed by the certifier comes from an unchecked elaborator stamp; later units use the certifier-owned record under the correct identity.
- Kernel call sites are derived during its typing walk, with shared graph closure and adversarial evidence against omitted calls, and its reads are recorded beside them.
- Documentation states these implemented properties without claiming part 7's trusted-code restriction.

Transfer durable contracts, rationale, rejected alternatives and evidence to their permanent owners, replace the roadmap entry with a checked summary, and update part 7 and all other dependents to link to those owners. Verify that nothing references this filename, then delete it. Part 7's unfinished work does not block retirement.
