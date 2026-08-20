# The map's remaining distance is decomposed before it is spent

## The issue

`spines` — map inserts under a plateaued live set — is the harness's largest remaining distance: 2.91× off Lean at run 08 ([benchmarks/08_RESULTS.md](../../benchmarks/08_RESULTS.md)), against the 0.85×–2.80× the other four columns span. The insert slope stands at 744 ns (minimum of seven; the medians, the method, and the dated captures live beside the probe, `map_wall_spines_slope` in `curios/src/tests/codegen/map_wall.rs`), and the campaign that produced that figure spent every class it had accounted; what remains is being decomposed before anything else is spent.

A whole-process ablation cannot produce the decomposition. Deleting one phase of the insert changes the live set the collector marks, so the residual charges collection cost to whichever phase happened to grow the heap — a split taken that way reverses sign between base-map sizes. The instrument must separate mutator from collector explicitly, or the ranking it produces is noise.

## The approach

Decompose with instrumentation; spend only in ranked order. The first cut is taken: **the collector's share is nil** — zero collections per thousand inserts at stock, at both Ns, on the current code (`spines_collection_decomposition`, `curios/src/tests/codegen/churn.rs`) — so the remainder is mutator work, the rebuild-and-collection candidate is measured out for this workload, and [the generational nursery](generational-nursery-spec.md) is demoted with it. What is left to rank:

- **The uniform-representation tax on the descent** — every crit, child, and key read through `ref.cast` + `i31.get` or a rope box helper. Owned by [a monomorphic field carries its own type](typed-heap-fields-spec.md), whose census now prices the class: one always-boxed scalar field reads at 17% of a dispatch-heavy loop's per-element budget, and the box/unbox class is the largest static population in the optimized module.
- **Per-insert key construction**, `Bytes/of_nat`, which [programs/README.md](../../programs/README.md) flags as a workload confound rather than a map cost.
- **The dispatch and compare residue of the descent** — the two-case matches branch, but three or more cases still take a `br_table`, and whether a compare chain beats a small table at map arity is an open probe question.

## The implementation

1. **The key's share.** A like-for-like slope pair — keys prebuilt into a resting structure against `Bytes/of_nat` per insert — same N range, same sitting, identical live sets by construction.
2. **The compare-chain probe.** One in-program-copy variant replacing the descent's remaining `br_table` dispatch with a compare chain, priced on the slope. The two-case form is already a branch; this decides the three-or-more form.
3. **The spend.** Each ranked class goes to its owner: the representation class to the typed-fields campaign, the local residues to probes here. This specification closes when every remaining class is spent, handed to the spec that owns it, or measured not worth its cost.
4. **The cross-language record.** A run 09 harness capture after the next landing puts the effect on the Lean column on record — the campaign's landings since run 08, the −61% `chain` cast step included, are not yet on the cross-language record. In-process probes cannot make that claim; only the harness can.

Figures land in the probes' dated sections beside the code that retakes them; this file records only what each figure decided.
