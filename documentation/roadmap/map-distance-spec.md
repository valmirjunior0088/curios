# The map's remaining distance is decomposed before it is spent

## The issue

`spines` — map inserts under a plateaued live set — is the harness's largest remaining distance: 2.91× off Lean at run 08 ([benchmarks/08_RESULTS.md](../../benchmarks/08_RESULTS.md)), against the 0.85×–2.80× the other four columns span. The insert slope stands at 744 ns between N = 25 000 and 75 000 (minimum of seven; the medians, the method, and the dated captures live beside the probe, `map_wall_spines_slope` in `curios/src/tests/codegen/map_wall.rs`), and that figure has never been soundly decomposed: every class that was accounted has been spent, and what remains is known only as a total.

A whole-process ablation cannot produce the decomposition. Deleting one phase of the insert changes the live set the collector marks, so the residual charges collection cost to whichever phase happened to grow the heap — a split taken that way reverses sign between base-map sizes. The instrument must separate mutator from collector explicitly, or the ranking it produces is noise.

## The approach

Decompose first; spend only in ranked order. Four candidate classes, each named with the owner that would spend it:

- **The uniform-representation tax on the descent** — every crit, child, and key read through `ref.cast` + `i31.get` or a rope box helper. Owned by [a monomorphic field carries its own type](typed-heap-fields-spec.md): its census prices this class, and its mechanisms are the only ones that delete it.
- **Per-insert key construction**, `Bytes/of_nat`, which [programs/README.md](../../programs/README.md) flags as a workload confound rather than a map cost.
- **The rebuild's allocation and the collection it drives.** Owned by [the generational nursery](generational-nursery-spec.md) if its share turns out large.
- **The dispatch and compare residue of the descent** — the two-case matches branch, but three or more cases still take a `br_table`, and whether a compare chain beats a small table at map arity is an open probe question.

## The implementation

1. **The collector's share.** Run `spines` under the GC log bridge `curios/src/tests/codegen/churn.rs` already uses, at two N points, and split the slope into mutator and collector components. This is the one number that partitions everything else, and it runs first.
2. **The key's share.** A like-for-like slope pair — keys prebuilt into a resting structure against `Bytes/of_nat` per insert — same N range, same sitting, identical live sets by construction.
3. **The compare-chain probe.** One in-program-copy variant replacing the descent's remaining `br_table` dispatch with a compare chain, priced on the slope. The two-case form is already a branch; this decides the three-or-more form.
4. **The spend.** Each ranked class goes to its owner: the representation classes to the typed-fields campaign, the collection share to the nursery, the local residues to probes here. This specification closes when every remaining class is spent, handed to the spec that owns it, or measured not worth its cost.
5. **The cross-language record.** A run 09 harness capture after the next landing puts the effect on the Lean column on record. In-process probes cannot make that claim; only the harness can.

Figures land in `map_wall_spines_slope`'s dated sections beside the probe that reproduces them; this file records only what each figure decided.
