---
description: Run the hand-off gate — `cargo xtask gate`, every subject's steps one after another — and report each step's result
allowed-tools: Read, Edit, Write, Grep, Glob, Monitor, Bash(cargo:*), Bash(git:*), Bash(npm:*), Bash(rg:*), Bash(cp:*), Bash(systemd-run:*)
---

The hand-off gate for code. Why the gate holds these steps and no others is `documentation/design/every-gate-step-catches-what-no-other-step-does.md`; what the steps are, and what each runs after, is the task files', `xtask/tasks.toml` and `xtask/tasks/`, and `cargo xtask <subject> --help` prints a subject's.

## What it runs

One command, `cargo xtask gate`: every step of the subjects the gate requires, one after another in the order it names them, each after what it requires and consumes; each subject's `lint` holds its formatting, and nothing writes. Each step ends on a line saying whether it passed or failed and in how long; a step whose prerequisite did not pass is not run, and says which; the steps that read nothing of a failure go on, and the last line names what failed and what was not run.

It needs, beyond cargo, nextest, Biome, Node, `npm` and the `wasm-bindgen` program, and the `wasm32-unknown-unknown` and `wasm32-wasip2` targets `rust-toolchain.toml` installs. A step that lacks a tool, or has one the `programs` of `xtask/tasks.toml` do not admit, refuses before it builds, naming what installs it.

## Before it runs

The checkout is the gate's own while it runs: no other session builds in it or shares it, so whatever it waits on is its own. The gate holds the history it runs over, so that history is final before it starts: the tree clean, `git status --short` printing nothing, and every fixup folded into the commit it belongs to, `GIT_SEQUENCE_EDITOR=true git rebase -i --autosquash <base>`, with any commit a document cites by its hash restated where the fold moved it. A branch of a worktree is rebased onto local `main` once, here, and gated as rebased; where it does not apply cleanly, say so and gate what the rebase made.

## How it runs

The command is a tracked background task, `systemd-run --user --scope -q -p MemoryMax=24G -p MemorySwapMax=0 cargo xtask gate > <scratchpad>/gate.log 2>&1`: the scope caps what the gate's changed code may hold, so a run that outgrows it ends rather than taking the machine, and standard error, where the gate writes each step's verdict, joins its output in the log. Nothing follows the gate in the command, so its exit code is the gate's, and the task's notification reports it; where the gate fails, the log's last line names what failed and what was not run. Never detach with `&`, and never add a shell that waits on another.

Beside it, one monitor reports each step as it ends: an event for each line of the log that ends a step or the gate, in order, and an exit once the gate's process is gone. It is armed at the longest deadline a monitor takes, and armed again at its expiry while the gate runs on, starting past the lines it already reported:

```sh
lines=' (passed|failed) in |not run|^failed:'
seen=$(grep -cE "$lines" <scratchpad>/gate.log 2>/dev/null || true)
seen=${seen:-0}
while true; do
  running=$(pgrep -f 'xtask gate' > /dev/null && echo yes || echo no)
  n=$(grep -cE "$lines" <scratchpad>/gate.log 2>/dev/null || true)
  n=${n:-0}
  if [ "$n" -gt "$seen" ]; then
    grep -E "$lines" <scratchpad>/gate.log | sed -n "$((seen+1)),${n}p"
    seen=$n
  fi
  [ "$running" = no ] && break
  sleep 2
done
```

The process is looked for before the log is read, so the lines a gate writes as it exits are reported before the monitor stops.

Do other work between events. A step that fails is read as its event arrives and fixed then, as the next section says, while the steps that read nothing of it run on: the fix and its reruns wait only for the failed step to end, since a rebuild under a step still running overwrites what it launches, and the gate is never stopped for one. The whole log is read once the gate has exited.

## When a step fails

The commits since the base may be other sessions' as well as the effort's, so a failure may be theirs, and a change that makes a step pass without restoring what its test holds is a bypass, not a fix. Before changing anything, get up to speed:

1. **Read the failure whole.** Every failing test's name, its assertion and its error message, and every compiler, Clippy or rustdoc diagnostic with its notes and help, in full rather than by their first line. Then reproduce it alone, by the action that ran it: a test by name, as `cargo xtask workspace test -E 'test(~<name>)'` does, which picks it out of the workspace's one build; a diagnostic by rerunning the step that reported it, by its path, as `cargo xtask workspace lint clippy` does.
2. **Read what the test holds.** Its doc comment, the code it exercises, and the document that states the rule it guards — the crate's `README.md`, a design decision, a soundness entry, an area rule in `.claude/rules/`. Weigh any reasoning they state three times over before judging against it.
3. **Find the change that broke it.** `git log` from the base, or from the last run that passed, over the test, the code it exercises and what that code reads; `git log -S` for a name that moved; `git blame` on the assertion and on the lines under test. Where that does not settle it, check out the last commit that passed in a worktree of its own under `target/`, never by moving the checkout: copy `curios-binaryen/.artifacts` into it so Binaryen is not built again, run `cargo xtask launcher build` there first, and narrow between the two commits with `git bisect` inside it.
4. **Read that change whole.** Its message, its whole diff, the documents it changed and the tests it added: it was made for a reason, and the fix keeps that reason.
5. **Decide on evidence which side is wrong.** The code, where it no longer does what the rule says. The test, only where the change deliberately moved the rule and the test still states the old one, and then the test follows the document that owns the rule. An expected output, only with the reason the new one is right: a difference is a finding before it is an update.
6. **Fix the cause.** Never by loosening an assertion, ignoring or deleting a test, raising a budget or a limit, regenerating an expected output without its reason, special-casing the failing input, swallowing an error or allowing a lint.
7. **Stop where the evidence does not settle it.** A cause still in doubt, or a fix that would reverse a choice another commit made on purpose, is reported with its evidence — an older build passing, a probe, blame, the commit — and left as it is. A latent bug another commit introduced, which this history only exposed, is no reason to stop: it is fixed, in a commit of its own placed before the one that exposed it, so no commit of the history is broken.

A fix is committed as a fixup of the commit the fault belongs to, `git commit --fixup=<commit>`, and folded in before the history is handed on; the bug of the step above is the one commit of its own. Then rerun the failed tests alone, by name, as `cargo xtask workspace test -E 'test(~<name>)'` does, one `test(~<name>)` a test, joined by `|`; every step the failure left not run, by its path; and, once they pass, every action whose inputs the fix changed — `cargo xtask workspace lint`, which holds the formatting too, after any Rust edit — and no other.

## Report

The gate's exit code, as the task's notification gives it, and each step that failed or was not run; each failure with its cause, the commit that introduced it, and its fix or, where it was left, its evidence; and, where it matters, each step's time by name. Never quote a whole-gate total.
