# A copy is planned within its allowance

## What is missing

Known-call inlining and loop-entry separation (`curios-cont/src/optimize/inline.rs` and `contify.rs`) construct a complete `CopyPlan` before checking its copied-node allowance. An oversized callee is walked in full at every attempted inline. The shared planner already offers bounded function and continuation requests, used during specialization discovery. Call and jump specialization still request an unlimited plan during final extent revalidation.

The current pass timings belong to `curios`'s `tests::instrumented::fixpoint_pass_measurements`. They include analysis and execution, so they do not establish the fraction bounded planning removes. The next comparison separates those costs and counts rejected planning work.

## Goal and settled decisions

- Select an allowance before planning. Preserve inclusive inline limits of `MULTI_SITE_INLINE_LIMIT` for multiple known sites or an escaping callee, and `SCC_CLONE_NODE_LIMIT` otherwise. Derive a loop copy's allowance by checked subtraction of `CONTIFICATION_NODE_OVERHEAD` from `SCC_CLONE_NODE_LIMIT`.
- Admission and execution read one complete lexical extent, including transitively nested definitions. A successful bounded request produces the same plan as an unlimited request. Refusal mints no arena identities and spends no expansion allowance.
- Prepare an executable plan against the current graph and consume it without an intervening rewrite. A call inside a callee can be inlined earlier in the same sweep, changing that callee's extent. No callee-keyed cache or preliminary counting walk is introduced.
- Bound call and jump specialization's final planning by the admitted extent length, retaining exact-set equality, dependency guards and successful-only charges. Preserve scheduling, priorities, mappings, allocation order, analyses, pruning and all existing allowances.
- Retain the shared walk. It encounters at most one node beyond its allowance, but eagerly enumerates binding-group members; count that metadata work separately rather than claiming a bound on all planning work.

## Stages and checks

The inliner tests pin inclusive boundaries for single-site, multiple-site and escaping callees, including nested extents, refusal without minting, and a callee enlarged earlier in a sweep. They pass against unchanged production behavior, whose revision is the measurement baseline. Existing planner and loop-separation tests hold their boundaries and atomicity.

1. Bound known-call planning before argument cloning; retain the existing inlining transformation and correct the sweep comment. Check Cont tests and the copying, sweeps and dead-parameter codegen consumers.
2. Bound loop planning and specialization revalidation, preserving the nested-entry guard and atomic copy-and-contify. Put contracts in rustdoc and the owning README. Check the affected pass and codegen tests.
3. Verify counted work, consumer identity and default-release cost; complete workspace format, Clippy and tests, including the standard-library corpus. Retire this specification.

## Verification

Use the comparison protocol in `curios/src/tests/instrumented/fixpoint.rs`: isolated source revisions, separate default-feature and profiling targets, frozen executables, identical lockfile and toolchain, one warmup and seven interleaved readings. Compare the last pre-production-change revision with the final production revision. The eleven-source cohort, batch sizes, clock-resolution treatment, artifact capture and profiling procedure remain that protocol's.

Add temporary `curios-profile` spans in the comparison copies for each affected pass's call analysis, planning and execution, plus samples for attempts, refusals, distinct nodes encountered and binding-group members enumerated. Nothing only the campaign reads goes into production. For counted synthetic work, temporary Cont tests drive the production inliner over body spines of 512, 1,024 and 2,048 nodes with 1, 16 and 64 sites; accepted bodies and wide binding groups are controls. At allowance `L`, a bounded attempt encounters at most `L + 1` distinct nodes; group enumeration is reported separately. Keep all test-only instrumentation and scripts outside the tracked tree.

Require identical optimized Cont, raw Wasm, Binaryen-optimized Wasm and archived foreign declarations for all eleven consumers, unchanged optimizer rounds and fired counts, and successful raw/optimized execution of the existing suspension and capture consumers. Artifact or scheduling differences are failures to investigate. Overlapping timing spans are acceptable if counted work demonstrates the intended bound and no reproducible regression appears; report them without claiming a speedup. A remaining material binding-group cost is a finding, not an invitation to add a new fuel limit.

## Completion and retirement

Record the actual comparison revisions, protocol and latest counted and timed results beside `fixpoint_pass_measurements`, replacing superseded figures there. Keep the landed contract in `CopyPlan`'s rustdoc and the Cont README. Delete this file once the four bounded call sites and their checks have landed, and verify that nothing references it. No replacement inventory is filed.
