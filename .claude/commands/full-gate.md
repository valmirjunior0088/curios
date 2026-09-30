---
description: Run the hand-off gate — every step at once in the background, plus the checks the change's files call for — and report each step's result
allowed-tools: Bash(cargo:*), Bash(git:*), Bash(npm:*), Read
---

The hand-off gate for code. It runs once, after the last step of an effort, and only on the user's go-ahead; a plan covering several specs gets one gate, at its end. Why the gate holds these steps and no others is `documentation/design/architecture/every-gate-step-catches-what-no-other-step-does.md`.

## The steps

`runtime`, `fmt-check`, `clippy`, `test`, `doctest`, `docs`, `js-test`, `zed-fmt-check`, `zed-clippy`, `zed-build` and `zed-test`, each `cargo x <step>`; and two npm chains, each in order: `grammar-install` → `grammar-test`, and `vscode-install` → `vscode-test` → `vscode-package`. The browser and editor steps need the `wasm32-unknown-unknown` and `wasm32-wasip2` targets and Node 22 or later with `npm`.

Add the checks the change calls for, read off `git diff --name-only` against the effort's base:

- `curios-binaryen/build.rs` changed: an empty-cache build, then a cache hit from another Cargo mode or build-script fingerprint.
- `curios-runtime`'s dependencies changed: `cargo x runtime`, then `cargo tree -p curios-runtime -e normal` names neither `cranelift-codegen` nor `curios-binaryen` (Wasmtime's `cranelift-bitset`, `cranelift-bforest` and `cranelift-entity` are expected).
- The bundle format changed: the ignored end-to-end test in `curios/tests/bundle.rs`, run explicitly.

## How it runs

- Launch every step at once, each as its own tracked background task, with no job cap: Cargo's lock serializes what conflicts. Each npm chain is one task, because `npm clean-install` deletes `node_modules` under a test running beside it.
- Each task writes `cargo x <step> > <scratchpad>/gate/<step>.log 2>&1; echo "EXIT=$?" >> <scratchpad>/gate/<step>.log`. The exit code is the log's last line; a task notification's code is the wrapper's.
- Never detach with `&`, and never add a shell that waits on another. Do other work until the notifications arrive, and read the logs then.

## When a step fails

- A failure the effort caused is fixed; then rerun only the failed tests, by name, with `--all-features`, and resume from the next step. Never rerun a step whose inputs have not changed since it passed.
- A failure another session's commit caused is reported with its evidence — an older binary passing, a probe, blame — not fixed.

## Report

Each step with its exit code, and where it matters, its time by name. Never quote a whole-gate total.
