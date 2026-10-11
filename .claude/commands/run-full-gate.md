---
description: Run the hand-off gate — every step of the table, in the order it derives — and report each step's result
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(cargo:*), Bash(git:*), Bash(npm:*), Bash(rg:*), Bash(cp:*)
---

The hand-off gate for code. Why the gate holds these steps and no others is `documentation/design/every-gate-step-catches-what-no-other-step-does.md`; what the steps are, what each runs after and what each needs is the table's, `xtask/src/steps.rs`, and `cargo xtask --help` prints it.

## How it runs

`cargo xtask gate`, as one tracked background task writing `cargo xtask gate > <scratchpad>/gate.log 2>&1; echo "EXIT=$?" >> <scratchpad>/gate.log`. It starts each step as a process of its own once the steps it runs after have passed, writes each step's output to `target/gate/<step>.log`, and says in its own log how each ended: `passed`, `FAILED`, `REFUSED` for a tool that is missing, `HELD` for a step whose prerequisite did not pass, `skipped` for the one step a path condition holds back. Every step that can run does, so one run reports every failure.

The base a path condition reads is the one the user names, passed as `--base <revision>`, and local `main` otherwise — where the effort is committed on `main` itself, the commit it started from. `origin/main` is never the base: it stands where the last push left it.

A `REFUSED` step names what installs the tool it lacks. Do other work until the task's notification arrives, and read the logs then.

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

Each step with how it ended, as the gate's log states it; each failure with its cause, the commit that introduced it, and its fix or, where it was left, its evidence; and, where it matters, each step's time by name. Never quote a whole-gate total.
