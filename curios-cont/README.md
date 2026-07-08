# curios-cont

The Curios continuation-passing IR: cont→cont optimization (`optimize/` — inlining, DCE, copy/tag/jump threading, tail recursion, …) and wasm emission (`into_wasm/`).

Sits between [`curios-ersd`](../curios-ersd) (which lowers into cont via `ersd/into_cont`) and [`curios-wasm`](../curios-wasm) (which it emits into). Code dependencies run opposite to data flow: this crate depends on `curios-wasm`, not the other way around.

See [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios-cont --open`.
