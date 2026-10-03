---
description: Hunt for a proof of False the checkers admit, file it on the soundness board as an open ticket, and land the fix as the next commit only when it is unambiguous
argument-hint: "[part — formation, introduction, elimination, conversion, totality, admission — and optionally a rule in it]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*)
---

Find a closed term the checkers admit at `/std/Bool/False`.

`$ARGUMENTS` names a part of the board, a file under `xboard/src/board/`, and may name a rule in it by its entry under `documentation/design/soundness/<part>/`. With none given, choose a rule and say why. One invocation is one rule; iteration lives outside, under `/loop`, and the board as committed is what one run hands the next.

## Authority

Invoking this command is standing authorization for exactly two kinds of commit: an open ticket with its witnesses, alone; and the fix of an open ticket, with the ticket turned to fixed. Commit on the branch checked out; never create, switch or rebase one. A ticket already open on the board is spent: it is a find waiting for its fix, not a target. Prefer stopping to guessing.

## What a find is

A proof of `False`, and nothing else. It is shown by a witness, never by a story about a rule being wrong: a source program whose tail is the proof, or, where no program reaches the rule, a module built by hand with the term it closes with, put to the kernel. The witness supplies the term and the board states the type, so a false equation or a declaration wrongly admitted is driven all the way to `False` before it is a find. Both are written in the part's board file and put by `cargo xboard`.

What stops short of `False` is not a ticket: a term a checker admits that its rule should refuse, a program compiled to do what its source does not say, a refusal of something sound, an effect outside `Io` that proves nothing. Each goes where `/hunt-warts` files a find, and is proposed in the closing report with its text exactly as it would be written and why it belongs there; nobody here commits it.

A program the elaborator admits and the kernel refuses is refused: a compilation fails closed. The kernel is what is trusted, so a module it admits at `False` is a find whatever the elaborator makes of its source, and a sound program the elaborator refuses is a wrong refusal, which is `/hunt-warts`'s.

## Read first

`xboard/README.md` states what a part, a ticket and a witness are, and `documentation/design/soundness/the-soundness-board.md` what a rule is, the trusted base, and the boundaries no rule covers. The part's board file holds every flaw already found in it, each with its witnesses, and the rule's entry under `documentation/design/soundness/<part>/` states what the rule assumes. `curios-cert`'s README and `curios-analysis`'s crate documentation are the roster of what the kernel decides and what both checkers share. Read those there. This file restates none of them.

## Recording

A find is two commits, and the first is the find alone.

**The open ticket.** File the ticket under its part with `Status::Open`, the day, and its witnesses: each expects a refusal the checkers do not yet make, by the checker that should make it, with the error left unnamed where it does not exist yet. `cargo xboard` must then show the ticket `OPEN` and its witness admitted, which is the demonstration. Its check is `cargo xtask fmt-check`, `cargo xtask clippy` and `cargo xboard`. Commit it by itself, changing no checker.

**The fix.** The next commit changes the checker and turns the ticket to `Status::Fixed`, naming in each witness the error that now refuses it. Before changing the checker, weigh what the rule's entry and the code's own documentation state it relies on three times over: a fix that closes this hole by breaking what the rule's argument rests on opens another. The board forces the two together: with the flaw closed and the ticket still open, the run fails. So the commit that closes a flaw is the one git blames for its status line, and nothing else records it. Its check is the open ticket's — `cargo xtask fmt-check`, `cargo xtask clippy` and `cargo xboard` — plus `cargo xtask test <crate>` for every crate the fix changes, narrowed with `--filter` to the tests that reach the change when the crate is `curios`, and one judgment none of them can make: the diff is additive or corrective only — no deleted witness, loosened expectation or weakened assertion.

If the fix is not unambiguous, correct and targeted, yield: the open ticket is already committed and is the record. Say what the fix would need, and do not try a second approach or weaken the witness.

## Under `/loop`

On its own, a run hunts one rule and ends with its report. Under `/loop`, each next run takes a rule this loop has not attacked yet, and the loop ends when a `False` stays open because its fix waits on a decision only the user can make; when a run cannot go on, on a check it cannot make pass; and once every rule has been attacked in it.

## Commits and report

Commit as CLAUDE.md says: the open ticket's message names the flaw found, the fix's the rule now enforced. Close with five fields: part and rule attacked, technique, the ticket filed or the finding proposed, what was committed, what is on the tree uncommitted.
