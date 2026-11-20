# CLAUDE.md

Curios is a dependently typed functional language implemented in Rust 2024, compiled from `.crs` through Core, an erased IR and a continuation IR to WebAssembly-GC and run under Wasmtime. What a crate owns is its `Cargo.toml` description and `README.md`; why the language and toolchain are the way they are is `documentation/design/`; pending work lives under `documentation/roadmap/`, filed as [Pending work is owned by its specification](documentation/design/pending-work-is-owned-by-its-specification.md) states. Read the owner, never a copy.

## Working here

- Investigate, explain and propose freely; change the repository only as the user authorized, narrowly. Keeping formatters and linters passing on touched code is in scope.
- A problem you were not asked to solve is a finding: state it once, with its evidence, and the user decides whether it becomes work. Don't reopen what the user settled. A decision document's reasons are weighed again where a question is asked of them, and said plainly where one no longer holds.
- Where more than one design is reasonable, present the alternatives with their trade-offs, recommend one, and wait.
- Prefer the fix that bears load long-term to the narrow patch: Curios promises no stability yet, so a breaking change is welcome where it is the right shape.
- Uncommitted changes are the user's, and other sessions commit to `main` while you work: never revert, reformat or stage what you did not write. Stage each file by its name, never a directory, `-A`, `-u` or `.`. Never discard work with a reset or a checkout: a mutation is put back from a copy kept beside it.
- A scripted edit names the tracked files it rewrites: a sweep of the directory also reaches the worktrees under `.claude/worktrees/` and the grammar clone under `editors/zed/grammars/`.
- Every change a tool's automatic fix makes is read before it is kept.
- Commit only when asked: one imperative, capitalized line, a few words naming the change — no body, trailer or co-author — on `main`. Push nothing without the user's word for that push; a passing handoff is not that word.
- A fix to a commit of yours not yet pushed is folded into it, tip or not, never a commit after it.
- Don't delegate to subagents unless asked. Stop only your own processes, by exact PID.

## The compiler is the interface

A claim about what a Curios program means is a hypothesis until this tree's compiler answers it: `cargo run --package curios --`, after `cargo xtask launcher build` once per checkout. An installed `curios` is another build.

- Probe on standard input, never with a file left in the tree: `cargo run --package curios -- run - <<'CRS' … CRS`.
- `wonder -` reports every error and goal as `run` would. It stops at the first failure, so iterate.
- `?` is the type oracle: `let y: ? = e` reports `e`'s type; a bare `?` reports the scope, the expected type, what blocks it and candidate fits.
- `wonder --stage <rung> -` reprints the program at one pipeline rung, `text` through `wasm`.
- A file is analysed in the unit whose `mod` lines reach it. A file under `curios-prelude/std/` compiles as the package `std` over the archived prelude, so a question costs the closure of the edit; its tests are each subject's private `tests` module, run by `curios test` in `curios-prelude/std`, and `--filter /std/<Subject>/tests` narrows a run to one.

Every other fact is on disk: `documentation/language.md` for the surface language, `curios-prelude/std/` for idiom and signatures, `.curios/cabinet/sources/` for dependencies.

## Build and check

- `cargo xtask <task>` runs a task of the files `xtask/tasks.toml` and `xtask/tasks/*.toml`, where every step is stated once: a path such as `cargo xtask workspace lint` names a group or a step, a group runs every step it holds, and `--help` at any depth lists what is there. `cargo xtask launcher build` builds the launcher the compiler embeds, once per checkout.
- Between the steps of a larger effort, and as the whole check for a focused change: `cargo xtask workspace lint`, which holds the formatting too, after `cargo xtask format`, plus the change's own tests by name, `cargo xtask workspace test -E 'test(~<name>)'`. The lint already elaborates, erases and certifies all of `/std`, so a Text, Core, ANF or certifier change needs nothing more; a change to `curios-cps`, `curios-emit` or `curios-wasm` adds the corpus tests that reach it, `curios-prelude`'s; a change of Markdown alone takes nothing and is committed at once; doc comments alone take `cargo xtask format` and `cargo xtask docs build`, and no lint. No `cargo check`, and no retaking of measurements — name any figure the change may have moved. While a check runs, draft the next step in a scratchpad mirror, not in the tree.
- Run a long command as a tracked background task with nothing after it and its output unredirected: the harness keeps the whole of it, both streams, in the file it names at the launch, where the user can watch it, and the task's notification reports the command's own exit code. Read that file for what a run printed, never the run again; report a finished check by the runner's own lines. Never detach one with `&` or add a shell that waits on it.
- Keep the feature set constant within a session: every feature builds a second prelude archive, and the two evict each other. `cargo xtask instrumented lint` and `docs build` are the steps that build with every feature.
- Past two packages' tests, run the suite once: each package selects another feature set, and a chain of by-package runs builds the prelude's archive before each.

## Where the rest is

- `.claude/rules/` loads by itself when a matching file is read: the Rust, Curios and documentation conventions, and one file per area of the workspace with what a change there must also inspect.
- `.claude/commands/` holds what a session is asked to run by name; what a session does unprompted is stated here or in a rule.
- Before proposing a capability, read the applicable findings and specifications under `documentation/roadmap/`. Before changing a public contract — a pipeline stage, the host ABI, the runtime, the JavaScript harness, the embedded standard library — trace it to its consumers.
