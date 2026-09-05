# xtask

The workspace's build recipes as a cargo subcommand: `cargo x runtime`, `build`, `js`, `rust-docs`, `std-docs`, `profile`, `benchmarks`, `clean`, and the editor bridges `grammar`, `vscode` and `zed`, which run npm or cargo in their tree under `editors/` with the arguments given. The two npm bridges need `npm`; every other recipe needs cargo alone. The alias lives in `.cargo/config.toml`; the recipes and their reasons are the crate's own documentation.

## Decisions

### Recipes are a cargo subcommand, not a Makefile

**Decision.** Every build recipe is a subcommand of this crate, reached through the `x` alias, and the workspace has no Makefile.

**Rationale.** A fresh clone then needs exactly one tool, cargo, on Linux and macOS alike. Each recipe is cargo with flags followed by one step cargo does not do — copy the launcher, generate the browser bindings, run a container — and Rust states those steps with the same error handling on Linux and macOS, without a shell between the recipe and cargo.

**Rejected.** A build script for the browser bundle: it would run before `curios-js` compiles, so there is nothing to generate bindings from, and a nested cargo inside a build script contends for the target-directory lock. Keeping the Makefile beside this crate: two entry points is the cost this crate removes.

### The bindings generator is a library dependency

**Decision.** `js` calls `wasm-bindgen-cli-support`, the crate the `wasm-bindgen` command line wraps, pinned in the workspace manifest at the version of `wasm-bindgen` itself.

**Rationale.** The generator must match the `wasm-bindgen` crate version exactly. As a dependency that match is the lockfile's — the two rows sit together in the workspace manifest — and the generator refuses a module built against another version, naming both, so a drift fails loudly.
