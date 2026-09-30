# CLAUDE.md

Curios is a dependently typed functional language implemented in Rust 2024, compiled from `.crs` through Core, an erased IR and a continuation IR to WebAssembly-GC and run under Wasmtime. What a crate owns is its `Cargo.toml` description and `README.md`; why the language and toolchain are the way they are is `documentation/design/`; what exists and what is pending is `documentation/roadmap.md`. Read the owner, never a copy.

## Working here

- Investigate, explain and propose freely; change the repository only as the user authorized, narrowly. Keeping formatters and linters passing on touched code is in scope.
- A problem you were not asked to solve is a finding: state it once, with its evidence, and the user decides whether it becomes work. Don't reopen settled decisions.
- Where more than one design is reasonable, present the alternatives with their trade-offs, recommend one, and wait.
- Uncommitted changes are the user's, and other sessions commit to `main` while you work: never revert, reformat or stage what you did not write, and stage explicit paths. Never discard work with a reset or a checkout, and never rewrite history.
- Commit only when asked: one imperative, capitalized line — no body, trailer or co-author — on `main`.
- Don't delegate to subagents unless asked. Stop only your own processes, by exact PID.

## The compiler is the interface

A claim about what a Curios program means is a hypothesis until this tree's compiler answers it: `cargo run --package curios --`, after `cargo x runtime` once per checkout. An installed `curios` is another build.

- Probe on standard input, never with a file left in the tree: `cargo run --package curios -- run - <<'CRS' … CRS`.
- `wonder diagnostics -` reports every error and goal as `run` would. It stops at the first failure, so iterate.
- `?` is the type oracle: `let y: ? = e` reports `e`'s type; a bare `?` reports the scope, the expected type, what blocks it and candidate fits.
- `wonder stage <rung> -` reprints the program at one pipeline rung, `text` through `wasm-optm`.
- A file is analysed in the unit whose `mod` lines reach it. A file under `curios-prelude-archive/std/` compiles as the package `std` over the archived prelude, so a question costs the closure of the edit.

Every other fact is on disk: `documentation/syntax.md` for the surface language, `curios-prelude-archive/std/` for idiom and signatures, `.curios/sources/` for dependencies.

## Build and check

- `cargo x <recipe>` runs every build step; `cargo x runtime` builds the launcher the compiler embeds, once per checkout.
- Between the steps of a larger effort, and as the whole check for a focused change: `cargo x clippy` and `cargo x fmt`, plus the change's own tests by name. Clippy already elaborates, erases and certifies all of `/std`, so a Text, Core, Ersd or certifier change needs nothing more; a change to `curios-cont`, `curios-emit` or `curios-wasm` adds the `curios` corpus tests that reach it. No `cargo check`, and no retaking of measurements — name any figure the change may have moved. While a check runs, draft the next step in a scratchpad mirror, not in the tree.
- Run a long command as a tracked background task writing `cmd > log 2>&1; echo "EXIT=$?" >> log`, and read the exit code from the log. Never detach one with `&` or add a shell that waits on it.
- Keep the feature set constant within a session: `--all-features` builds a second prelude archive, and the two evict each other.
- Before handing off code: `/full-gate`, once, after the last step, on the user's go-ahead.

## Where the rest is

- `.claude/rules/` loads by itself when a matching file is read: the Rust, Curios and documentation conventions, and one file per area of the workspace with what a change there must also inspect.
- Before proposing a capability, read `documentation/roadmap.md`. Before changing a public contract — a pipeline stage, the host ABI, the runtime, the JavaScript harness, the embedded standard library — trace it to its consumers.
