---
description: Take the next most urgent specification that can run beside the other sessions into a worktree of its own — investigate it in depth, rewrite it from its headline where that serves, plan it, implement it to its retirement — and hold just before the handoff, landing it on the user's word
argument-hint: "[optional — a specification to take, as `area/nnn`, or directions in prose]"
allowed-tools: Read, Edit, Write, Grep, Glob, Monitor, Bash(rg:*), Bash(git:*), Bash(cargo:*), Bash(cp:*), Bash(mkdir:*), Bash(python3:*), Bash(systemd-run:*), Bash(df:*), Bash(du:*), WebSearch, WebFetch, AskUserQuestion, EnterWorktree, ExitWorktree, EnterPlanMode, ExitPlanMode, ListAgents
disable-model-invocation: true
---

Take one specification of the roadmap from investigation to retirement, in a worktree of its own, with the user present in the thread. `$ARGUMENTS` names the specification or gives directions in prose; without any, pick it as the first section says. Every decision is the user's, asked with `AskUserQuestion` the moment it arises — the recommendation first, a draft in an option's preview, in prose. The command is the user's ask to commit on its branch, a stage at a time; the work stops just before the handoff and lands only on the user's word.

Keep these steps in view, and say at each which one the work is at; none is skipped, whatever the specification's size:

1. The specification, picked or given.
2. The worktree.
3. The investigation, closing on whether to rewrite from the headline.
4. The plan, in plan mode.
5. The effort, stage by stage.
6. The retirement, the specification's checks shown first.
7. A stop just before the handoff.
8. The landing, on the user's word.

## Read first

[Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) states what a specification holds, how the roadmap is numbered, and how one is refined and retired; `.claude/rules/findings.md` states what a finding is and how one is filed; CLAUDE.md says how to probe, build, check and commit. Read those there. This file restates none of them.

## 1. The specification

Without one named, take the most urgent specification that can run beside the sessions already at work: the first in the roadmap's order, `1nn` before `2nn` before `3nn`, whose files no other session is changing. Those sessions are what `ListAgents` shows and the worktrees `git worktree list` holds, each branch's work read off its own commits against `main`. Where every specification would collide with one of them, say so and yield.

## 2. The worktree

Branch from local `main`, never `origin/main`: `git worktree add -b <branch> .claude/worktrees/<branch> main`, the branch named for the specification's subject, then `EnterWorktree` by its `path` — by its name it would branch from `origin/main`. Copy what the main checkout has built and the worktree would otherwise build again, with `cp -a --reflink=auto`: `curios/.artifacts` (the launcher), `curios-binaryen/.artifacts`, `curios-js/.artifacts`, `curios-prelude/.artifacts` (the prelude's images), `curios-profile/.artifacts` (Perfetto's schema), `curios-unicode/.artifacts` and `xtask/.artifacts` (the tools the steps run).

Inside it, the harness refuses a command whose paths it cannot read off the command's text: write every path literally, run `git` from the worktree's root and never with `-C`, keep one kind of action to a command, and write a probe's source to the scratchpad with `Write` and feed it on standard input. The worktree's `target` grows without bound; clearing `target/debug/incremental` while no build of the session's runs is routine, mentioned in a line.

## 3. The investigation

Investigate the specification in thorough depth before proposing anything.

- **Show it again.** Every claim it makes of the tree — a defect, a cost, a figure, a missing capability — reproduced with this tree's compiler before it is trusted; a claim the tree no longer bears out is said so.
- **Question its frame.** Read the decisions it cites, their rationale and their rejections, and question each three times over: whether its reasoning still holds of the tree as it stands, and whether a revised rule would serve the language better and let the code be written more plainly.
- **Survey the peers**, cited: what each does and why, as a ledger of what Curios should adopt and what it should avoid, and where its own checkers, rules and what it already holds open a way no peer has.
- **Weigh a rewrite from the headline.** A specification's body freezes an early reading of the problem; its headline is the commitment. Answer, in prose and with the draft, whether it should be rewritten from its headline alone, taking at every turn the approach, solution, refactor or rework that bears load long-term on the codebase — the robust, principled, consistent, coherent, cohesive one: the Curios one.
- **Fold in the findings it meets.** A finding easy to tackle or relevant to the effort is done as part of it, as a stage of its own, sooner rather than later; any other is filed, at any point, as `.claude/rules/findings.md` files one.

## 4. The plan

On the user's agreement, bring the specification up to speed — rewritten or refined as agreed, and committed on the branch — then enter plan mode and plan the effort: its stages, each a commit or a few, findings first. Every stage names the claim of the specification it serves, and every claim has a stage or is named as left out and filed. Ask every fork the plan leaves open at once, each with a recommendation. While the plan is presented, a remark of the user's is an edit to it: fold it in and present the plan again. Only the plan's approval starts the work.

## 5. The effort

Run the stages back to back, committing each as it lands, and stop only where something turns out ambiguous or unpredictable — a fork nobody settled, a behaviour outside what was agreed, two oracles disagreeing. A decision that arises is asked then.

- **Each stage's checks** are what CLAUDE.md names for what it touched, with its own tests, each test's claim mutation-checked. While a check runs, draft the next stage in a scratchpad mirror, never in the tree.
- **Land what is consumed.** What the branch's code uses, and the members of the rule set the change already states, land; the unconsumed rest is written into the roadmap as a finding or a specification of its own.
- **A retired mechanism may be the only thing accepting a program no test holds.** Before retiring a lookup, a fallback or a rule, run the programs the documents and comments name through `run -`, and keep the one that breaks as a test.
- **A landed stage is dropped from the specification**, the stages left keeping their numbers and restated only where the landing moved them, as the pending-work decision keeps one up to date.
- **A fixup folds into the commit it belongs to**, `GIT_SEQUENCE_EDITOR=true git rebase -i --autosquash <base>`, the base being the hash the branch sits on, `git merge-base HEAD main`, and never `main` itself, which rebases onto wherever another session has moved it.
- **Rebase onto `main`** on the user's word or a peer session's request, at the next convenient checkpoint — between stages, never mid-stage — and restate any commit a document cites by its hash where the rebase moved it.

## 6. The retirement

When the effort is in place, show each check the specification states holding — its tests passing, its figures retaken where it names them as its own — and only then retire it, as the pending-work decision retires one: delete it and leave its number as a gap, renaming nothing else; fix what linked to it; restate the documents whose rules the effort changed; and file what stays pending as findings or specifications of their own. A retirement touching documents and doc comments alone takes the checks CLAUDE.md names for those.

## 7. Before the handoff

Stop just before the handoff and report: the commits, by subject; what the specification became; what was filed; and what the handoff has yet to run. Nothing past this point happens without the user's word.

## 8. Landing, on the user's word

Branches land one at a time: where several wait, the user orders them, and each lands on the tip the one before left. The user says how the branch lands: through the handoff, as `/run-handoff` runs it, or past it, on the workspace's format, lint, tests and docs build. Then, from the worktree: rebase onto local `main`; run the checks the user named once, after the rebase and never before it, chained as one tracked run, fixing what they surface in the commits it belongs to; `ExitWorktree`; fast-forward `main` from the main checkout with `git merge --ff-only <branch>` — no merge commit, nothing pushed — leaving `main`'s uncommitted changes as they are, and stopping to ask where one touches a file the branch moves; then remove the worktree and delete the branch. A check failing because another session's latent bug surfaced is fixed on the branch, in a commit of its own ordered before the one that exposed it.

The tree is the record of the work: the commits, the specification's retirement and the roadmap say what was done, and nothing else is kept.
