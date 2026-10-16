# A dead-parameter sweep repairs the graph once

## What is missing

`eliminate_dead_parameters` in `curios-cont/src/optimize/dead_parameters.rs` decides removals from one demand snapshot, but repairs the node arena once per affected continuation or function and again per removed binding. The recorded TOML reading of `fixpoint_pass_measurements`, at `ce963cf3f`, attributes 397 ms across eleven invocations to the entire pass, including demand analysis; it does not establish the removable fraction.

## Goal and decisions

Each of the pass's two phases repairs the entire live node arena at most once, irrespective of how many entities or bindings it removes. The demand lattice, optimizer order, public interfaces and growth budgets stay as they are. A removal mask uses the original parameter positions, and every surviving operand retains its order.

Continuation cleanup precedes the escape snapshot. Removing a continuation argument can remove a function's last value occurrence and admit that function's cleanup immediately. Function cleanup reads this fixed escape snapshot; further escape opportunities it creates wait for the next invocation. Both phases read the original demand snapshot: removing uses cannot falsify `Unused`.

Operation and call result receivers keep their signatures, as do escaping functions. Each phase updates definitions, repairs every incoming transfer and fills remaining inert occurrences before tombstoning the removed bindings. The arena-wide repair includes nested bodies and nodes awaiting end-of-round pruning. A continuation's complete removal mask also updates its field records once. Indirect callees, effects and traps retain their existing treatment.

## Stages and checks

1. Batch continuation removals and their graph repair. Check repeated switch targets, default edges, nested bodies, transient live nodes and exact field-record shifts, shrinking and deletion.
2. Batch known-function removals and their graph repair. Check distinct masks, self and mutual recursion, all-parameter removal, retained escaping signatures and unchanged invocations.
3. Check source consumers through raw and Binaryen-optimized Wasm, including argument order, effects and suspension. Record the contract in rustdoc and the owning crate's decision; finish formatting, Clippy and workspace tests.

## Verification

Use temporary profiling instrumentation, removed before landing, to separate demand-solving cost from rewrite cost and count repair visits on generated workloads with 64, 128 and 256 affected entities. Each invocation must repair at most twice the node arena's slot count. These counts concern repair walks, not the solver's own iterations or the read-only scans that establish interface boundaries.

Compare the implementation against `62cf6687d` using the eleven-source cohort and the interleaved default-release compilation and isolated-profile protocols beside `fixpoint_pass_measurements`. Require identical optimized Cont, raw and Binaryen-optimized Wasm and harvested foreign declarations. Investigate any difference rather than accepting it as a new optimization. Replace the affected latest-reading figures with paired results naming their revisions; record artifact identity and execution coverage instead of retiming identical runtime artifacts. The scripts and temporary instrumentation remain outside the repository.

## Retirement

Rewrite this specification as stages land so it describes only remaining work. Once the checks, comparison and owning documentation are complete, remove its references and delete it. No replacement inventory records the completed work.
