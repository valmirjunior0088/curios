# CLAUDE.md

Operational guide for working on Curios. Read it before investigating or changing the repository.

## Working with the user

- **Mutation requires explicit authorization, narrowly scoped.** Investigation, explanation and proposals are read-only. Do not edit, format, generate, delete, stage, commit or otherwise mutate the repository unless the user has authorized that specific change. Authorization for one change covers neither adjacent refactors, cleanup, dependency upgrades or unrelated fixes, nor a broader scope that would make the change easier. When the boundary is ambiguous, stop and ask. Once a change is authorized, keeping formatters and linters passing on the touched code is in scope.
- **Report, don't fix, problems you were not asked to solve.** A discovered bug, inefficiency or cleanup opportunity is a finding to surface; the user decides whether it becomes work.
- **Run every decision through the user.** Where more than one design is reasonable, present the alternatives and their trade-offs, recommend one plainly, and wait for the choice.
- **Preserve existing work.** Every uncommitted change belongs to the user. Do not overwrite, revert, reformat, stage or incorporate it unless the user includes it in the task.
- **Commit only when asked, as a single line.** One imperative, capitalized, descriptive subject — no body, no bullets, no trailer, and never a co-author. Include only the authorized changes; commit to `main` directly.
- **State findings plainly, once.** Name an uncertainty with its evidence and let the user resolve it. Do not reopen settled decisions, hedge in loops, or delegate to subagents unless asked.

## Interacting with a Curios codebase

The compiler is the interface. A claim about what a Curios program means is a hypothesis until the compiler has answered it, and the tools below are how it is asked.

**Test a theory on standard input.** `run` and every `wonder` query take `-` for a program on standard input, so a probe is a heredoc, never a file left in the tree:

```sh
cargo run --package curios -- run - <<'CRS'
/std/print("hello\n")
CRS
```

**Derive facts with `wonder`.** `curios wonder <query> [target]` answers a question from the compilation that would build the target and executes nothing. The answer goes to stdout; exit 0 means it was answered, including when the answer is a list of errors.

- `wonder diagnostics -` is the loop: every error and goal, rendered as `run` reports it. A snapshot stops at the first failure, so iterate one error at a time.
- `?` is the type oracle, addressed by text rather than by coordinate. `let y: ? = e` reports `? = ` the type of `e`; a bare `?` reports the local scope, the expected type, the obligations holding it up and candidate fits. Several `?` in one program report in one compile. This is how the type of a binder, an expression or a hole is learned — never by guessing from the surrounding code.
- `wonder stage <stage> -` reprints the program at one pipeline rung (`text`, `core`, `core-elab`, `ersd`, `ersd-optm`, `cont`, `cont-optm`, `wasm`, `wasm-optm`): the way to see what a lowering or an optimization actually produced.
- A file target is placed in the unit that declares it: a module under a package's directory is analysed as that package's library, an executable's entry as that executable, anything else standalone.

**Read the sources for the rest.** Every fact `wonder` does not answer is on disk: `documentation/syntax.md` is the normative surface reference, `curios-prelude-archive/std/` is the idiom reference and where a standard-library signature is read, and a dependency's sources are materialized under `.curios/sources/`. Search with `rg`, read the narrowest authoritative source, and widen only when the evidence requires it. Do not reconstruct from memory what a file states.

**The binary is the tree's.** `cargo run --package curios --` runs the compiler this checkout builds, which is the one a change is being made to; it needs `cargo x runtime` once per checkout for the launcher it embeds. An installed `curios` on `PATH` is a different build and answers for it, not for the tree.

## Before changing anything

- Read [roadmap.md](documentation/roadmap.md) before proposing or implementing a capability, and confirm whether the work is new, pending, or already represented differently.
- Identify the subsystem that owns the behavior and read its crate-level and module-level `//!` documentation before changing Rust.
- Read [syntax.md](documentation/syntax.md) in full immediately before writing or modifying Curios source. This holds even when no `.crs` file has been opened yet.
- Trace a public contract to its consumers before changing it. Pipeline stages, the host ABI, the runtime, the JavaScript harness and the embedded standard library impose downstream obligations.

## System at a glance

Curios is a functional, dependently typed language implemented in Rust 2024. It compiles `.crs` source through several intermediate representations to WebAssembly and executes precompiled modules with Wasmtime: `curios-text` parses and lowers to core, `curios-elab` elaborates and erases, `curios-ersd` optimizes and lowers to continuations, `curios-cont` optimizes and emits WebAssembly, `curios-wasm` encodes it. Beside that chain, `curios-core` owns the term representation, `curios-analysis` the rules both checkers run over it, and `curios-cert` the kernel that only one of them does.

Data flows downward; Rust dependencies between stages point **upward**, because a lowering depends on the representation it constructs. `curios-elab` takes `curios-cert` as a *dev*-dependency only, so nothing whose build script reaches elaboration reaches the kernel through it.

What each crate owns is its `description` in its `Cargo.toml` and its `README.md`. Read those rather than a copy kept here.

### Change routing

The obligations below are the ones a search does not reveal.

| Changing… | Also inspect… |
| --- | --- |
| Surface grammar, syntax tree, or printing | `into_core/`, parser tests, `documentation/syntax.md` |
| Elaboration, typing, or conversion | Text lowering, erasure, diagnostics, integration tests |
| Kernel judgments (`curios-cert`) | `curios-core`'s representation, `recheck.rs`, `documentation/design/language/the-soundness-perimeter.md` |
| A shared analysis (`curios-analysis`) | Both drivers — `curios-cert`'s `Kernel` and `curios-elab`'s `Context` — and `curios-analysis/tests/driven.rs` |
| A numeric carrier or its arithmetic | Every constant folder sharing `scalar` (`curios-core`, `curios-ersd`, `curios-cont`), and `documentation/design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md` |
| Concepts or witness resolution | Surface declarations, standard-library witnesses, syntax documentation |
| A derivation (`curios-elab/src/derive.rs`) | The `SpellSyntax` slots, `/syn/Spell`'s renderers, the derived-vocabulary edges in `curios-text/src/into_core/order.rs`, `curios/src/tests/derive.rs` |
| Host operations or foreign calls | `curios-abi`'s row, compiler use, native runtime implementation, JavaScript implementation |
| What a unit hands its successors | Every stage whose artifact `Unit` holds, `curios-pipeline`'s fold, the store's stored-unit format |
| A `wonder` query, a record, or what a diagnostic carries | `curios-utilities`'s `Report`, every stage's `report`/`reports_with_hints`, `CompileError` and `check_with_units`, both transports (`ask.rs`, `server.rs`), `curios-package`'s `Membership` |
| Manifests, dependency resolution, or the store | The CLI subcommands wrapping it, `Qualifier`/`Mount`, and `documentation/soundness/admission-without-judgment/cached-verdicts.md` when the store's keys are involved |
| Runtime or bundle format | Slim-launcher dependency boundary, bundle integration tests |
| A build recipe (`xtask`) | `curios/build.rs`, the CI workflows calling the recipe, `README.md`'s build steps |
| Binaryen version, build, or FFI | Shared cache behavior, native compiler linkage, optimize round-trip tests |

## Architectural invariants

- Compiler stages own their representations. A lowering belongs to the crate holding the source representation and depends on the crate holding the destination representation.
- `curios-pipeline` is the compiler boundary: no dependency on Binaryen, Wasmtime, the runtime or the CLI. It may name the fixed prelude in `standard.rs` alone; `compile_entrypoint` takes a scope and cannot tell which unit is `/std`.
- `curios-package` sits beside that boundary, never under it. `curios-pipeline` must not depend on it, and `curios-js` must not touch it.
- `curios-unit` sits below the kernel: `cargo tree -p curios-unit --edges normal` must not contain `curios-cert`, because a build script that reached the certifier would re-elaborate the whole standard library on every kernel edit.
- `curios-runtime` is runtime-only in its default feature set: no `curios`, no Binaryen, and no Cranelift. Its `cranelift` feature exists for `curios` and never enters `default`; `curios/src/bundle.rs` enforces this on the shipped launcher image.
- `curios` is the only crate combining Binaryen with Cranelift-enabled Wasmtime. It names no wasmtime type, reaching the runtime through `curios_runtime::validate` and `curios_runtime::precompile`. The Wasmtime pin lives in `curios-runtime/Cargo.toml` and nowhere else.
- Crate boundaries, not Cargo features, separate the compiler, runtime and browser products.
- `curios-abi` is the source of truth for the host/guest wire contract. A host operation is complete only when its ABI row, compiler use, native runtime implementation and JavaScript implementation agree.
- `Intrinsic::signature` is the source of truth for what an intrinsic demands and produces; both checkers walk it rather than restate it. `/sys`'s declarations state the same types a second time, deliberately, and the prelude build checks them against the table. A new operation is typed by adding a row.
- `/std` and `/syn` are owned by `curios-prelude-archive` and compiled into an rkyv image in that crate's `OUT_DIR`. Every source module is registered in its Curios index.
- Production compilation has no fixed-prelude source fallback or cache-miss branch. Archive construction or restoration failure is a compiler invariant and fails loudly.
- No crate below `curios-prelude-archive` may spell a `/syn` name. The one exception is `curios-analysis`'s `fixture`, gated on `test-support`, which is what keeps those names out of every build that ships.
- Binaryen is built from a verified pinned source release, shared through the locked cache under `curios-binaryen/.artifacts/<triple>`, never a fingerprint-specific `OUT_DIR`.
- Recursive lowering and packed-value interpretation must work on the default test-thread stack. Never use `RUST_MIN_STACK` to hide a regression.
- Generated `.wasm` files and other build products are not source and are not committed. `Cargo.lock` is source. `editors/grammar/src/` is the one committed generated artifact, because git is Zed's distribution channel for it: it is committed with the `grammar.js` it was generated from, and `editors/grammar`'s `npm test` refuses any drift between them.

## Build and validation

The build recipes are `cargo x <recipe>`, reached through the alias in `.cargo/config.toml`. The native compiler embeds the slim `curios-runtime` launcher with `include_bytes!`, and that launcher must be built in its own Cargo invocation so workspace feature unification keeps Cranelift and Binaryen out: `cargo x runtime` does that stage alone and files the launcher at `curios/.artifacts/<triple>`; `cargo x build` does both stages in order. Building `curios` without that stage fails naming the recipe to run. A `curios-runtime` binary from a workspace build is not evidence the isolated launcher is slim.

### While iterating

- Run the smallest check or test that exercises the changed behavior, and prefer stage-local crate checks. **Never run two Cargo builds concurrently.**
- **In a multi-step task, run the full gate once, after the last step.** Between steps, `cargo clippy --workspace --all-targets --all-features -- -Dwarnings` plus `cargo fmt --all` is the check, even when each step is its own commit. Do not add `cargo check` beside it.
- Keep the feature set constant within a work session: `--all-features` enables `profile` and a plain `cargo build` does not, and alternating maintains two prelude archives that evict each other.
- The full suite can take more than five minutes. Run it in the background with output redirected to a file, and read the file after completion.

`cargo clippy --workspace` already elaborates every `/std` and `/syn` module, erases them through `erase_unit`, and certifies the whole module with the kernel. A change on the Text, Core, Ersd or certification path is therefore exercised over the entire standard library by a step already in the gate, and needs only its own crate's tests beside it. Nothing below Ersd is reached, so `curios-cont` and `curios-wasm` are detected only by the cross-stage corpus in `curios`.

### Before handing off code changes

Run this gate, in order. All commands must pass; Clippy warnings are errors in CI.

```sh
cargo x runtime
cargo fmt --all -- --check
cargo clippy --workspace --all-targets --all-features -- -Dwarnings
cargo test --workspace --all-targets --all-features
cargo test --workspace --doc --all-features
cargo doc --workspace --no-deps --document-private-items
```

`cargo check` is deliberately absent: `clippy` is the same compilation with more lints. The doctest step is separate because `--all-targets` excludes `--doc`, so nothing above it compiles a documentation example. The documentation build is CI's Documentation job verbatim, and has been red on its own while every step above it was green: rustdoc's lints — a broken intra-doc link above all — are checked by no other step. It carries `--document-private-items` because these crates state their invariants on `pub(crate)` items: without it the step lints the public surface alone, which in this tree is a small fraction of the prose it exists to check. Its denial is `[workspace.lints.rustdoc] all = "deny"` in the root manifest, inherited by every crate through `[lints] workspace = true`, so neither this gate nor CI carries an environment variable that can be forgotten in one of them. Measure a step and name the step; never quote a whole-gate total.

### Additional gates

- Changes to `curios-js` or its dependencies must also pass `cargo x js`.
- Changes to `curios-binaryen/build.rs` must verify an empty-cache build and a cache hit from a different Cargo mode or build-script fingerprint.
- Changes to runtime dependencies must rebuild through `cargo x runtime` and confirm that neither `cranelift-codegen` nor `curios-binaryen` entered its graph — name those crates, since Wasmtime's runtime legitimately pulls the `cranelift-bitset`, `cranelift-bforest` and `cranelift-entity` utility crates.
- Changes to the bundle format must run the ignored end-to-end test in `curios/tests/bundle.rs` explicitly.
- Profile through the built-in `tracing` mechanism, not an external sampler: `cargo x profile programs/hello_world.crs` is the only build in which the `profile` subcommand exists.

## Repository conventions

- Do not mix vendored changes, generated files or unrelated formatting into a feature commit.
- Use non-interactive Git, and never discard work with a destructive reset or checkout unless the user requests that exact action. Never rewrite history.
- Keep source files focused. Prefer extending an existing ownership boundary over creating a parallel abstraction for the same responsibility.
- `rust-toolchain.toml` pins the toolchain; the floor is Wasmtime's, so bump it when the pin in `curios-runtime/Cargo.toml` moves past it.
- `curios-binaryen` builds a pinned Binaryen release with CMake, which needs a C++ toolchain.
- A build product that outlives the build that made it lives in `.artifacts/` beside its owner, never under `target/`. `cargo clean` never removes them; delete one by hand to force a rebuild.
- `target/debug/incremental` is pure rustc cache and safe to delete when no build is running; `CARGO_INCREMENTAL=0` suppresses it per invocation.
- `curios-js` is built by `cargo x js`; do not introduce `wasm-pack` or `wasm-opt` without a design decision.

## Conventions that load on demand

Three rule files in `.claude/rules/` hold the only copy of their subject and load when a matching file is read: [rust.md](.claude/rules/rust.md) for Rust layout, tests and naming, [curios.md](.claude/rules/curios.md) for `.crs` source, and [documentation.md](.claude/rules/documentation.md) for which document owns which fact.
