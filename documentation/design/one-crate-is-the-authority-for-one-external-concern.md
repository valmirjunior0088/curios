# One crate is the authority for one external concern

**Decision.** Among the product crates' normal dependencies, every external dependency with a surface worth governing is named in exactly one manifest, and that crate is its authority: `curios-profile` for `tracing`, `curios-archive` for rkyv, `curios-num` for `num-bigint` and `num-traits`, `curios-package` for TOML, `curios-runtime` for `wasmtime`, `curios-utilities` for `stacker` and `sha2`. Each names its own pin rather than taking a `[workspace.dependencies]` row, and nothing above it spells the dependency's types. `xtask` and build scripts are tools and take what they need. The one shared row is `wasm-bindgen` beside `wasm-bindgen-cli-support`, taken by `curios-js` and `xtask`, because the CLI support crate generates the glue for the runtime crate and their versions must move together, which two pins cannot say and one row can. Rust owns the native host — Binaryen optimization, Wasmtime precompilation and execution, bundling, the CLI and operating-system services — each behind its authority crate.

**Rationale.**

- **A shared row concentrates configuration, not authority.** Any crate may add `foo = { workspace = true }` without anyone deciding it should, while a dependency in one manifest cannot be taken elsewhere without writing its version again, which is a question a reviewer asks. The pin and the feature set have one home, so `curios` and `curios-runtime` cannot drift on Wasmtime — and they must not, since `curios` precompiles a `.cwasm` the launcher deserializes, and Wasmtime refuses an artifact of another version at load time.
- **What makes a facade possible is sealing.** A dependency whose surface is a mechanism — rkyv's derives and `With` adapters, `tracing`'s macros — is absorbed by one crate trivially. One whose surface is a type is wrapped in a type of the owning crate's and kept private: `curios-num`'s `Natural` holds a private `BigUint` behind a `pub(crate)` constructor, so no one above it says `BigUint`.
- **A feature that must not reach every consumer is a feature flag.** `curios` needs Cranelift and `curios-runtime`'s default build must not have it; Cargo unifies features across a workspace build, so `curios-runtime` declares a `cranelift` feature `curios` opts into, and the pin stays single.
- **Reimplementing the host in Curios would add risk without teaching the language anything**; the language is defined by the stages from parsing through Wasm generation.

**Rejected.**

- **A `[workspace.dependencies]` table as the governing mechanism**, which makes versions consistent — the easy half — and says nothing about who may take a dependency.
- **Re-exporting the foreign type from its owner**, which concentrates the pin and not the vocabulary.
- **Exempting a dependency whose type spans many crates, or whose features differ by consumer**: `num-bigint` and Wasmtime are such cases, and sealing and a feature flag cover them.
