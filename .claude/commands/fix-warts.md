---
description: Fix findings entries — autonomously, those whose fix is beyond doubt, or supervised, a case at a time, each fixed on the spot or lifted into a spec
argument-hint: "[autonomous | supervised]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*)
---

Fix entries of the findings files, each area's `documentation/roadmap/<area>/00-findings.md` and the workspace's `documentation/roadmap/00-findings.md`, in the mode `$ARGUMENTS` names, `autonomous` or `supervised`. With no mode given, work supervised.

## What every fix is

An entry is a claim someone filed, and the code may have moved since. Before changing anything, show the defect as the entry states it, by the probe it gives or by the code it names read against the rule it cites, and weigh what the tree argues for that code as `/hunt-warts` weighs stated reasoning: the decision the entry cites, the crate's `README.md`, the `//!` and the comments saying why. An entry marked uncertain is investigated until what it says is not known is known.

- **A drop**, where the code no longer bears the entry out, is one commit that deletes it and nothing else.
- **A fix** is one commit that makes the fix the entry states, adds the test its check names where it names one, and deletes the entry, as the opening paragraphs of `documentation/roadmap.md` require. A supervised case is one such commit over its entries. Its check is the entry's own and CLAUDE.md's for a focused change, and where it changes code, the whole test suite, `cargo xtask test`; a comment or a doc comment is not code. It is committed as CLAUDE.md says. Anything noticed on the way is a find, never part of it.

## Autonomous

Invoking this mode is standing authorization for four kinds of commit: the fix of a trivial entry, the drop of an entry, the restatement of an uncertain entry its investigation settled, and the finds the run files at its end.

An entry is trivial when anyone reading it beside the code would write the same diff: its fix is stated, there is one way to make it, and its check is named and proves it. A fix that chooses a name or a scope, changes what a checker admits or a stage emits, or changes a public contract CLAUDE.md names is not trivial, and neither is one whose check fails.

1. Work through every entry, file by file. Fix each trivial one and drop each the code no longer bears out. Investigate each uncertain one: drop it where it is unfounded, fix it where what is left is trivial, and otherwise restate it with what the investigation showed. Leave every other entry untouched, and never guess: a fix whose check fails was not trivial, so undo your own edit and leave the entry.
2. File what the run noticed on the way, each a find as `/hunt-warts` states one, in the findings file of its area or the workspace's and in the form `documentation/roadmap.md` states, all in one commit of prose.

Close with the commits made, by subject; each entry filed; each entry dropped or restated, with why; and each fix undone, with what its check showed.

## Supervised

Work through the entries that are not trivial, a case at a time. A trivial entry is an autonomous run's; when only those are left, close.

1. Take the entry of most consequence and investigate around it: the code it names, what the tree argues for that code, and every entry and open spec that names it or follows from the same cause. The case is that entry and every other the same change settles.
2. Present the case: the entries it holds, what the code shows now and whether it still bears each out, and the outcome you recommend. It is fixed on the spot where that is one pass, and lifted into a spec where the fix takes stages that land apart or rests on a question this session cannot settle. For a fix, give the design, any real alternative with its trade-off, and the steps, each ending on its check; for a lift, the spec's open line and what the spec will state. Then wait.
3. On approval of a fix, make the steps one at a time. At the end of each, a checkpoint: what changed, what its check said, and what comes next. Where a step meets a decision the case did not settle, stop and ask.
4. On approval of a lift, write the spec and its open line as the opening paragraphs of `documentation/roadmap.md` state, or rewrite the open spec that already covers the case, delete the case's entries, and present the draft.
5. Commit when the user approves the result, one commit a case.
6. Go on to the next case.

Close with the cases fixed, by commit subject; those lifted, by spec; the entries left, with where each stands; and what was noticed on the way, for `/hunt-warts`.
