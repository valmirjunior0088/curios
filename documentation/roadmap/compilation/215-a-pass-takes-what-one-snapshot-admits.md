# A pass takes what one snapshot admits

The five one-candidate passes fall short of [A pass applies the independent plans its snapshot admits](../../../curios-cont/README.md#a-pass-applies-the-independent-plans-its-snapshot-admits). The implementation is staged below; the current baseline is `eb40b82e9`. The public optimizer API, host ABI, pass order, per-copy ceilings, parameter ceiling and round backstop remain unchanged.

## Decisions

Discover and group candidates once per invocation, revalidate their assumptions against the live graph, then prepare and consume executable plans without intervening mutation. Keep distinct pass-specific plans. Index incoming sites and batch compatible edits so a sweep does not rebuild or scan the whole module per candidate. Changed source bodies, argument origins, caller rosters, signatures and return relationships defer dependent work; compatible appends to lexical anchors do not. New candidates wait for a fresh snapshot.

Specialization uses non-refundable module-wide copied-node allowances: jump and call specialization each receive `64 × BRANCH_SPECIALIZATION_GROWTH_LIMIT`, SCC specialization `64 × SCC_CLONE_NODE_LIMIT`. Charge the complete copy and reconstruction nodes only after successful admission. Prioritize matching sites per copied node with exact integer comparisons and stable identity-based ties. Jump and call specializers select one context per generic root per sweep. SCC contexts can proceed together while component-local revalidation establishes independence; a changed invariant-known verdict enables cleanup and defers the remainder. Loop-entry copying keeps its separate policy.

## Remaining stages

5. Batch independent SCC specializations, revalidating contexts locally without rebuilding whole-module analyses per context.
6. Verify source consumers and performance, reconcile the stale measurement finding, audit loop-entry scheduling, update references and retire this specification.

Each implementation stage carries workspace formatting and Clippy, owning tests, relevant raw/optimized execution tests and the standard-library corpus. Discovery and rejection leave arenas and allowances untouched. Existing size ceilings, stacks, lint settings and backstops are not raised.

## Verification and retirement

Independent candidates advance in one invocation. Dependent candidates wait for a stated changed assumption, and a successful earlier rewrite ensures another round will reconsider them. Equivalent sites share copies; nominal rows remain distinct; recursive copies and peeled entries preserve their existing backedge rules. Whole-module analyses and site indices are built per invocation, not per candidate. Existing suspension, cancellation, opaque-callback, mixed-layout and growth controls remain valid.

Capture source-level consumers for every converted pass and compare serialized raw and Binaryen-optimized Wasm against the baseline, including the TOML driver and affected parser, monadic, rope-walk and coordination programs. Explain every size increase. Size changes within unchanged regression ceilings are accepted only with no demonstrated runtime regression. Run final workspace formatting, Clippy, default-feature workspace tests and browser harness tests; the full gate is not required.

Performance comparisons use isolated baseline and candidate builds, matching configurations and separate artifacts. Take seven warm, interleaved release compilation samples per revision, recording commit IDs, medians and spans; use matching profiling builds for attribution through the existing instrument. Time runtime consumers with interleaved samples under the repository's span-based noise interpretation. Require demonstrated compilation improvement on the TOML driver and no demonstrated compilation or runtime regression on affected consumers. Missing evidence or unexplained regressions keep verification pending.

Refresh the fixpoint measurement at its owning test, correcting the interpretation of fired rounds, and reconcile the tools finding that holds its stale reading. Other measurements are updated only when retaken. Audit `separate_loop_entries` without extending implementation absent demonstrated demand. General cross-pass analysis caching, speculative rollback, callback precision and representation changes remain separate work. Once every stage and check passes, update references to the owning contracts and delete this specification without a replacement inventory.
