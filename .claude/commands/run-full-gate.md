---
description: Run the hand-off gate — every step in the background as soon as what it reads is ready, plus the checks the change's files call for — and report each step's result
allowed-tools: Read, Edit, Write, Grep, Glob, Monitor, Bash(cargo:*), Bash(git:*), Bash(npm:*), Bash(rg:*), Bash(cp:*)
---

The hand-off gate for code. Why the gate holds these steps and no others is `documentation/design/every-gate-step-catches-what-no-other-step-does.md`. The base it runs against is the one the user names, and local `main` otherwise — where the effort is committed on `main` itself, the commit it started from. `origin/main` is never the base: it stands where the last push left it.

## What it needs

The browser and editor steps need the `wasm32-unknown-unknown` and `wasm32-wasip2` targets and Node 22 or later with `npm`, and `test` needs `cargo-nextest`. Every step but the three checks below is `cargo xtask <step>`.

Add the checks the change calls for, read off `git diff --name-only` against the base:

- **The dependency check**, where `curios-runtime`'s dependencies changed: `cargo tree -p curios-runtime -e normal` names neither `cranelift-codegen` nor `curios-binaryen` (Wasmtime's `cranelift-bitset`, `cranelift-bforest` and `cranelift-entity` are expected).
- **The bundle test**, where the bundle format changed: `cargo test -p curios --all-features --test bundle -- --ignored`, the end-to-end tests the ordinary suite ignores.
- **The Binaryen cache check**, where `curios-binaryen/build.rs` changed: remove `curios-binaryen/.artifacts/<host triple>`, with the host triple `rustc -vV` reports; then `cargo build -p curios-binaryen` builds Binaryen from its source, and `cargo clippy -p curios-binaryen`, another Cargo mode with a build-script fingerprint of its own, reuses that build: no CMake runs, and the entry's `done` marker keeps its modification time.

## How it runs

Every step starts as soon as what it reads is ready, and there is no job cap. A chain, written with `→`, is one tracked background task running its steps in order and stopping at the first that fails; every other step is a task of its own:

| Starts | Steps |
| :--- | :--- |
| At once | `runtime`; `fmt-check`; `js-test`; `grammar-install` → `grammar-test`; `vscode-install` → `vscode-test` → `vscode-build`; `zed-fmt-check` → `zed-clippy` → `zed-build`; the dependency check, where the change calls for it |
| Once `runtime` has passed | `clippy`; `doctest`; `docs`; the bundle test, where the change calls for it |
| Once `grammar-test` has passed | `zed-test` |
| Once `clippy`, `doctest`, `docs` and the bundle test have finished | the Binaryen cache check, where the change calls for it |
| Once every other step has finished | `test`, by itself |

What orders them, and nothing else does:

- **A step that builds `curios` waits for `runtime`**, because `curios` embeds the launcher it files and a build before it embeds the old one. `js-test` builds the browser bundle, which depends on neither `curios` nor the launcher, so it does not wait.
- **`zed-test` waits for `grammar-test`**, since it hashes `editors/grammar` as it is on disk and `grammar-test` regenerates its `src/`. The extension's other steps read nothing of the grammar, and `zed-build`, like `vscode-build`, is the one that produces what the editor loads.
- **An npm chain keeps its order**, because `npm clean-install` deletes `node_modules` under a test running beside it.
- **The Binaryen cache check waits for every step that builds `curios`**, since it deletes the Binaryen build `curios` links.
- **`test` takes hours**, so it starts once every other step has finished and runs by itself.

Cargo locks a build directory per target and profile. `clippy`, `doctest`, `docs` and `test` all build in the root workspace's `target/debug`, so they compile in turn, in whatever order they reach the lock, while `runtime` and `js-test` build beside them in directories of their own and the Zed steps in `editors/zed`. Every `cargo xtask` launch takes the `target/debug` lock too, being a `cargo run` in the root workspace: a step launched during one of those builds waits for it before its own tool starts, which is why a step that can start at once does.

- Each step writes `<its command> > <scratchpad>/gate/<step>.log 2>&1; echo "EXIT=$?" >> <scratchpad>/gate/<step>.log`, a chain's steps one log each. The exit code is the log's last line; a task notification's code is the wrapper's.
- A step that waits starts when the notification of the last one it waits on arrives. Never detach with `&`, and never add a shell that waits on another. Do other work until the notifications arrive, and read the logs then.
- `test` gets a Monitor on its log, started with it: one event when the build ends and the suite starts, one for every five hundredth test to finish and one for every test that does not pass, since the suite runs with `--no-fail-fast` and its exit code comes only at the end. Nextest numbers each result `(N/TOTAL)`, which is what the filter below reads. Re-arm it at each expiry, with `tail -n 0` so nothing is reported twice, until the step's notification arrives.

```sh
tail -n +1 -F <scratchpad>/gate/test.log | awk '/^ +PASS /{ if ($0 ~ /\( *[0-9]*(000|500)\//) { print; fflush() } next } /^ +SLOW /{ next } /^ +[A-Z][A-Z0-9 -]* \[/ || /^error/ || /^ +Starting / || /^EXIT=/ { print; fflush() }'
```

## When a step fails

The commits since the base may be other sessions' as well as the effort's, so a failure may be theirs, and a change that makes a step pass without restoring what its test holds is a bypass, not a fix. Before changing anything, get up to speed:

1. **Read the failure whole.** Every failing test's name, its assertion and its error message, and every compiler, Clippy or rustdoc diagnostic with its notes and help, in full rather than by their first line. Then reproduce it alone: a test by name, with `--all-features`; a diagnostic by rerunning its step on its package, as `cargo xtask clippy <package>` does.
2. **Read what the test holds.** Its doc comment, the code it exercises, and the document that states the rule it guards — the crate's `README.md`, a design decision, a soundness entry, an area rule in `.claude/rules/`. Weigh any reasoning they state three times over before judging against it.
3. **Find the change that broke it.** `git log` from the base, or from the last run that passed, over the test, the code it exercises and what that code reads; `git log -S` for a name that moved; `git blame` on the assertion and on the lines under test. Where that does not settle it, check out the last commit that passed in a worktree of its own under `target/`, never by moving the checkout: copy `curios-binaryen/.artifacts` into it so Binaryen is not built again, run `cargo xtask runtime` there first, and narrow between the two commits with `git bisect` inside it.
4. **Read that change whole.** Its message, its whole diff, the documents it changed and the tests it added: it was made for a reason, and the fix keeps that reason.
5. **Decide on evidence which side is wrong.** The code, where it no longer does what the rule says. The test, only where the change deliberately moved the rule and the test still states the old one, and then the test follows the document that owns the rule. An expected output, only with the reason the new one is right: a difference is a finding before it is an update.
6. **Fix the cause.** Never by loosening an assertion, ignoring or deleting a test, raising a budget or a limit, regenerating an expected output without its reason, special-casing the failing input, swallowing an error or allowing a lint.
7. **Stop where the evidence does not settle it.** A cause still in doubt, or a fix that would undo or reshape what another session's commit decided, is reported with its evidence — an older build passing, a probe, blame, the commit — and left as it is.

Then rerun the failed tests alone, by name, with `--all-features`, and once they pass, every step whose inputs the fix changed — `fmt-check` and `clippy` after any Rust edit — and no other.

## Report

Each step with its exit code; each failure with its cause, the commit that introduced it, and its fix or, where it was left, its evidence; and, where it matters, each step's time by name. Never quote a whole-gate total.
