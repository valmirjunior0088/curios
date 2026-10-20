---
description: Run the hand-off gate — `cargo xtask gate`, every step of the table one after another — and report each step's result
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(cargo:*), Bash(git:*), Bash(npm:*), Bash(rg:*), Bash(cp:*)
---

The hand-off gate for code. Why the gate holds these steps and no others is `documentation/design/every-gate-step-catches-what-no-other-step-does.md`; what the steps are, and what each is run after, is the table's, `xtask/src/steps.rs`, and `cargo xtask <subject> --help` prints a subject's.

## What it runs

One command, `cargo xtask gate`: every step of the table, one after another in the table's order, each after the steps it reads, formatting as a check. Each step ends on a line saying whether it passed or failed and in how long; a step whose prerequisite did not pass is not run, and says which; the steps that read nothing of a failure go on, and the last line names what failed and what was not run.

It needs, beyond cargo, `cargo-nextest`, Node and `npm` (which also fetches Biome), and the `wasm32-unknown-unknown` and `wasm32-wasip2` targets `rust-toolchain.toml` installs. A step that lacks a tool refuses before it builds, naming what installs it.

## How it runs

The command is a tracked background task writing `cargo xtask gate > <scratchpad>/gate.log 2>&1; echo "EXIT=$?" >> <scratchpad>/gate.log`. The exit code is the log's last line, and the line before it names what failed and what was not run; a task notification's code is the wrapper's. Never detach with `&`, and never add a shell that waits on another. Do other work until the notification arrives, and read the log then.

## When a step fails

The commits since the base may be other sessions' as well as the effort's, so a failure may be theirs, and a change that makes a step pass without restoring what its test holds is a bypass, not a fix. Before changing anything, get up to speed:

1. **Read the failure whole.** Every failing test's name, its assertion and its error message, and every compiler, Clippy or rustdoc diagnostic with its notes and help, in full rather than by their first line. Then reproduce it alone, by the action that ran it: a test by name, as `cargo xtask workspace test --filter <name>` does, which picks it out of the workspace's one build; a diagnostic by rerunning its action on its package, as `cargo xtask workspace lint <package>` does.
2. **Read what the test holds.** Its doc comment, the code it exercises, and the document that states the rule it guards — the crate's `README.md`, a design decision, a soundness entry, an area rule in `.claude/rules/`. Weigh any reasoning they state three times over before judging against it.
3. **Find the change that broke it.** `git log` from the base, or from the last run that passed, over the test, the code it exercises and what that code reads; `git log -S` for a name that moved; `git blame` on the assertion and on the lines under test. Where that does not settle it, check out the last commit that passed in a worktree of its own under `target/`, never by moving the checkout: copy `curios-binaryen/.artifacts` into it so Binaryen is not built again, run `cargo xtask launcher build` there first, and narrow between the two commits with `git bisect` inside it.
4. **Read that change whole.** Its message, its whole diff, the documents it changed and the tests it added: it was made for a reason, and the fix keeps that reason.
5. **Decide on evidence which side is wrong.** The code, where it no longer does what the rule says. The test, only where the change deliberately moved the rule and the test still states the old one, and then the test follows the document that owns the rule. An expected output, only with the reason the new one is right: a difference is a finding before it is an update.
6. **Fix the cause.** Never by loosening an assertion, ignoring or deleting a test, raising a budget or a limit, regenerating an expected output without its reason, special-casing the failing input, swallowing an error or allowing a lint.
7. **Stop where the evidence does not settle it.** A cause still in doubt, or a fix that would undo or reshape what another session's commit decided, is reported with its evidence — an older build passing, a probe, blame, the commit — and left as it is.

Then rerun the failed tests alone, by name, as `cargo xtask workspace test --filter <name>` does, one `--filter` a test, and once they pass, every action whose inputs the fix changed — `cargo xtask workspace format --check` and `cargo xtask workspace lint` after any Rust edit — and no other.

## Report

The gate's exit code, and each step that failed or was not run; each failure with its cause, the commit that introduced it, and its fix or, where it was left, its evidence; and, where it matters, each step's time by name. Never quote a whole-gate total.
