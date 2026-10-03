---
description: Fix findings entries — autonomously, those whose fix is beyond doubt, or supervised, one at a time with recommendations and checkpoints
argument-hint: "[autonomous | supervised] [area, findings file or entry — compilation, documentation/roadmap/00-findings.md, …]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*)
---

Fix entries of the findings files `$ARGUMENTS` scopes, in the mode it names first, `autonomous` or `supervised`. With no mode given, work supervised; with no scope, every findings file: each area's `documentation/roadmap/<area>/00-findings.md` and the workspace's `documentation/roadmap/00-findings.md`.

## What every fix is

One entry, one commit. The commit makes the change the entry names and deletes the entry, as the opening paragraphs of `documentation/roadmap.md` require, and nothing beside it: anything noticed on the way is a find, never part of this fix. Its check is `cargo xtask fmt`, `cargo xtask clippy` and `cargo xtask test <crate>` for every crate it touches, narrowed with `--filter` to the tests that reach the change when the crate is `curios`; it is committed as CLAUDE.md says.

An entry is a claim someone filed, and the code may have moved since. Before changing what it names, weigh what the tree argues for that code three times over — the decision the entry cites, the crate's `README.md`, the `//!` and the comments saying why — as `/hunt-warts` weighs stated reasoning. An entry the code no longer bears out is dropped, in a commit of its own that deletes it and nothing else. An entry marked uncertain is investigated first, and what the investigation shows decides it: it is fixed like any other entry, or dropped the same way.

## Autonomous

Invoking this mode is standing authorization for three kinds of commit — the fix of a trivial entry, the drop of an entry the code no longer bears out or an investigation shows unfounded, and the finds it files at its end — and for rewriting this run's own commits where the full gate calls for it. An entry is trivial when anyone reading it beside the code would write the same diff — its fix is stated, there is one way to make it, and its check is named and proves it. Nothing else is trivial: a fix that chooses a name or a scope, a change to what a checker admits or a stage emits, a change to a public contract CLAUDE.md names, a fix whose check fails. An entry marked uncertain is investigated here too: dropped where the investigation shows it unfounded, fixed where it leaves a trivial fix. Every entry, uncertain or not, is left rather than guessed at.

Work through the entries in scope in file order. Fix each trivial one, drop each the code no longer bears out or an investigation shows unfounded, and leave every other untouched. A fix whose check fails was not trivial: undo your own edit and leave the entry.

Once no trivial entry is left in scope, and if this run committed any fix, run `/run-full-gate` against the commit the run started from, and deal with what it surfaces as that command's failure guide says. Where a failure traces to one of this run's fixes, the outcome goes into that fix's own commit: a correction that is itself trivial is folded in; otherwise the commit is rewritten to leave the code as it was and keep its entry, now carrying **Uncertain, and warrants investigation:** with what the gate showed. A later fix of this run that no longer replays over the rewrite is rewritten the same way. A failure this run did not cause is gathered with the evidence the guide asks for and reported, never fixed: it is no entry's trivial fix.

Once the gate passes, or there was nothing for it to check, file what the run noticed on the way, as many entries as it holds, each in the findings file of its area or the workspace's, in the form `documentation/roadmap.md` states, all of them in one commit of prose. Then close with the commits made, by subject; each entry filed; each entry dropped, with why; each entry left, with why it is not trivial; and the gate's report, with each fix it corrected or turned uncertain, and why.

## Supervised

Work through the entries with the user, one at a time, in the order they name. Failing that, take first the entries an autonomous run would leave, those that are not trivial, by consequence; a trivial entry is an autonomous run's, and comes last.

1. Present the entry: what the code shows now and whether it still bears the entry out, the fix you recommend and any real alternative with its trade-off, and the steps it takes, each ending on its check. Then wait.
2. On approval, make the steps one at a time. At the end of each, a checkpoint: what changed, what its check said, and what comes next. Where a step meets a decision the entry did not settle, stop and ask.
3. Commit when the user approves the result.
4. Go on to the next entry.

Close with the entries fixed, by commit subject; those left, with where each stands; and what was noticed on the way, for `/hunt-warts`.
