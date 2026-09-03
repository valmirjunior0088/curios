---
description: Hunt for an unsoundness in the trusted base, record it as a regression test, and land the fix only when it is unambiguous
argument-hint: "[perimeter entry, by file name — check_positivity, coverage, large-elimination-guard, index-inversion-and-k, …]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*)
---

Find a closed term of `/std/False` the compiler accepts.

`$ARGUMENTS` names an entry under `documentation/soundness/`, by file name. With none given, choose the entry whose status is weakest — auditable only, then argued and unprobed, then probed at its easy rung with the side condition untouched — and say why. One invocation is one entry; iteration lives outside, under `/loop`, and the entry's committed status is what one run hands the next.

## Authority

Invoking this command is standing authorization for exactly two kinds of commit: a regression test with the fix it guards and the entry's updated status, or a probe with its null recorded in the entry. That overrides CLAUDE.md's prohibition on unattended edits and commits for those two and for nothing else. Commit on the branch checked out; never create, switch or rebase one. Run `git status` first: a file already modified belongs to the user, and an entry implemented in one, or an uncommitted regression test from an earlier run, is spent for this run. Prefer stopping to guessing.

## What a find is

A program that inhabits `/std/False`, or whose erased output diverges where totality was required — a `.crs` file on the scratchpad, or, where no surface program reaches the rule, a module built by hand beside the fixtures in `curios-cert/src/recheck/` and put to `recheck_module_verdicts`. A story about a rule being wrong is not a find: run the program or hand the term to the kernel. The one find without a fixture is the kernel believing an elaborator conclusion it did not establish, where the *what the kernel consults* entries do not already record it; report that, with what re-deriving it would take.

Hunt agreement on a wrong rule. Disagreement already fails the build, and a program the elaborator refuses that the kernel would certify is a wrong refusal, which is `/hunt-warts`'s.

## Read first

`documentation/design/language/the-soundness-perimeter.md` states the claim, the grades, the four parts and the boundaries; each entry states what it assumes, its status, what has been tried and which fixtures hold it; `curios-analysis`'s crate documentation is the roster of what both checkers share. Read those there. This file restates none of them, because its last two revisions drifted from them.

## Recording

Write the test before the fix; its failure is the demonstration. It goes beside the entry's existing fixtures, in that file's own conventions, and three hold everywhere: assert the diagnostic that names the refusing rule, never bare failure; pair it with a control that still passes; carry in the comment, in the past tense, how the hole was verified while open. The entry's status updates in the same commit. A test is never committed ignored and never committed alone.

A null is a result. Commit the probe and the status it moves. Its gate is `cargo x runtime`, `cargo fmt --all -- --check`, workspace clippy with `-Dwarnings`, and one targeted run of the probe as committed; a probe leaves every rule as it found it, so the suite has nothing to catch.

A fix changes what the compiler decides, so it clears CLAUDE.md's full hand-off gate, suite output redirected to a file, plus two judgments the gate cannot make: the control passes, and the diff is additive or corrective only — no deleted assertion, loosened bound, `#[ignore]` or edited expectation. Do not run the ignored `kernel_disagreements`; it tallies and never fails.

If the fix is not unambiguous, correct and targeted, yield: leave the failing test on the tree uncommitted, say which file and what it asserts, and do not try a second approach or weaken the test.

## Commits and report

One line, imperative, capitalized, naming the rule now enforced; no body, no trailers. Close with five fields: entry attacked, technique, verdict, what was committed, what is on the tree uncommitted.
