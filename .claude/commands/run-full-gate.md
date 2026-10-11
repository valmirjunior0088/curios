---
description: Run the hand-off gate — every subject's command, together as the machine allows — and report each step's result
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(cargo:*), Bash(git:*), Bash(npm:*), Bash(rg:*), Bash(cp:*)
---

The hand-off gate for code. Why the gate holds these steps and no others is `documentation/design/every-gate-step-catches-what-no-other-step-does.md`; what each subject's steps are is the table's, `xtask/src/steps.rs`, and `cargo xtask <subject> --help` prints them.

## What it runs

Every subject, each by its bare command. A bare subject runs its steps one after another, formatting as a check, goes on past a step that fails, and names the ones that failed where it ends.

| Command | Needs beyond cargo |
| :--- | :--- |
| `cargo xtask launcher` | |
| `cargo xtask workspace` | `cargo-nextest`; `npm`, which fetches Biome |
| `cargo xtask instrumented` | |
| `cargo xtask docs` | |
| `cargo xtask compiler` | |
| `cargo xtask bundle` | the `wasm32-unknown-unknown` target; Node |
| `cargo xtask grammar` | `npm` |
| `cargo xtask vscode` | `npm` |
| `cargo xtask zed` | the `wasm32-wasip2` target |

A step that lacks a tool refuses before it builds, naming what installs it.

## How it runs

What runs together is yours to decide, from what the machine is doing and what the change touched. What constrains it, and nothing else does:

- **`launcher` first, by itself.** Every subject that builds the compiler builds the launcher it embeds before anything else, so none has to wait for it; run first, it is built once rather than by each subject started beside it.
- **`zed` after `grammar`**, since `zed test` hashes `editors/grammar` as it is on disk and `grammar test` regenerates its `src/`.
- **`workspace` beside nothing that builds the compiler, where `curios-binaryen/build.rs` differs from local `main`.** Its last step then deletes the Binaryen build `instrumented`, `docs` and `compiler` link, and makes it again from source. Otherwise the step says `skipped`. It compares the working tree with local `main`, so an effort committed on `main` itself is one it cannot see: say so in the report where the script changed there.
- **Cargo serializes builds that share a directory.** `workspace`, `instrumented` and `docs` build in the root workspace's `target/debug`, so started together they compile in turn, in whatever order they reach the lock. `launcher`, `compiler` and `bundle` build in directories of their own, the Zed steps in `editors/zed`, and the npm subjects build nothing with cargo. Every `cargo xtask` launch takes the `target/debug` lock first, being a `cargo run` in the root workspace.

Each command is a tracked background task writing `cargo xtask <subject> > <scratchpad>/gate/<subject>.log 2>&1; echo "EXIT=$?" >> <scratchpad>/gate/<subject>.log`. The exit code is the log's last line, and the line before it names the steps that failed; a task notification's code is the wrapper's. Never detach with `&`, and never add a shell that waits on another. Do other work until the notifications arrive, and read the logs then.

## When a step fails

The commits since the base may be other sessions' as well as the effort's, so a failure may be theirs, and a change that makes a step pass without restoring what its test holds is a bypass, not a fix. Before changing anything, get up to speed:

1. **Read the failure whole.** Every failing test's name, its assertion and its error message, and every compiler, Clippy or rustdoc diagnostic with its notes and help, in full rather than by their first line. Then reproduce it alone, by the action that ran it: a test by name, as `cargo xtask workspace test <package> --filter <name>` does; a diagnostic by rerunning its action on its package, as `cargo xtask workspace lint <package>` does.
2. **Read what the test holds.** Its doc comment, the code it exercises, and the document that states the rule it guards — the crate's `README.md`, a design decision, a soundness entry, an area rule in `.claude/rules/`. Weigh any reasoning they state three times over before judging against it.
3. **Find the change that broke it.** `git log` from the base, or from the last run that passed, over the test, the code it exercises and what that code reads; `git log -S` for a name that moved; `git blame` on the assertion and on the lines under test. Where that does not settle it, check out the last commit that passed in a worktree of its own under `target/`, never by moving the checkout: copy `curios-binaryen/.artifacts` into it so Binaryen is not built again, run `cargo xtask launcher build` there first, and narrow between the two commits with `git bisect` inside it.
4. **Read that change whole.** Its message, its whole diff, the documents it changed and the tests it added: it was made for a reason, and the fix keeps that reason.
5. **Decide on evidence which side is wrong.** The code, where it no longer does what the rule says. The test, only where the change deliberately moved the rule and the test still states the old one, and then the test follows the document that owns the rule. An expected output, only with the reason the new one is right: a difference is a finding before it is an update.
6. **Fix the cause.** Never by loosening an assertion, ignoring or deleting a test, raising a budget or a limit, regenerating an expected output without its reason, special-casing the failing input, swallowing an error or allowing a lint.
7. **Stop where the evidence does not settle it.** A cause still in doubt, or a fix that would undo or reshape what another session's commit decided, is reported with its evidence — an older build passing, a probe, blame, the commit — and left as it is.

Then rerun the failed tests alone, by name, as `cargo xtask workspace test <package> --filter <name>` does, and once they pass, every action whose inputs the fix changed — `cargo xtask workspace format --check` and `cargo xtask workspace lint` after any Rust edit — and no other.

## Report

Each subject with its exit code and the steps it named as failed; each failure with its cause, the commit that introduced it, and its fix or, where it was left, its evidence; and, where it matters, each step's time by name. Never quote a whole-gate total.
