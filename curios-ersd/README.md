# curios-ersd

The Curios erased IR (post type-erasure): ersd→ersd optimization (`optm/` — prune, the `evaluate`/`specialize` compile-time staging pair, and the `worker_wrapper` engine over a shared `call_graph`/suffix-view cursor) and lowering to continuation-passing style (`to_cont/`).

Sits between [`curios-core`](../curios-core) (which erases into ersd via `core/erase`) and [`curios-cont`](../curios-cont) (which it lowers into). Code dependencies run opposite to data flow: this crate depends on `curios-cont`, not the other way around.

See [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios-ersd --open`.
